input_GT_concepts <- function(condition_occurrence_tbl, procedure_occurrence_tbl,
                              observation_tbl, measurement_tbl, visit_occurrence_tbl, PPS_concepts) {
  rename_cols <- function(top_concepts, df, start_date_col, id_col) {
    top_concepts_df <- rename(df, "domain_concept_start_date" = start_date_col, "domain_concept_id" = id_col) %>%
      inner_join(top_concepts, by = "domain_concept_id") %>%
      select(person_id, domain_concept_start_date, domain_concept_id) %>%
      distinct()
    return(top_concepts_df)
  }

  c_o <- rename_cols(PPS_concepts, condition_occurrence_tbl, "condition_start_date", "condition_concept_id")
  p_o <- rename_cols(PPS_concepts, procedure_occurrence_tbl, "procedure_date", "procedure_concept_id")
  o_df <- rename_cols(PPS_concepts, observation_tbl, "observation_date", "observation_concept_id")
  m_df <- rename_cols(PPS_concepts, measurement_tbl, "measurement_date", "measurement_concept_id")
  v_o <- rename_cols(PPS_concepts, visit_occurrence_tbl, "visit_start_date", "visit_concept_id")

  top_preg_related_concepts <- list(c_o, p_o, o_df, m_df, v_o) %>%
    reduce(union_all)

  return(top_preg_related_concepts)
}

records_comparison <- function(personlist, i) {
  # for the below: t = time (actual), c = concept (expected)
  # first do the comparisons to the records PREVIOUS to record i

  # Iterate through the previous records
  for (j in 1:(i - 1)) {
    # Obtain the difference in actual dates of the consecutive patient records
    delta_t <- as.numeric(difftime(personlist$domain_concept_start_date[i], personlist$domain_concept_start_date[i - j], units = "days") / 30)
    # Obtain the max expected month difference based on clinician knowledge of the two concepts (allow two extra months for leniency)
    adjConceptMonths_MaxExpectedDelta <- personlist$max_month[i] - personlist$min_month[i - j] + 2
    # Obtain the min expected month difference based on clinician knowledge of the two concepts (allow two extra months for leniency)
    adjConceptMonths_MinExpectedDelta <- personlist$min_month[i] - personlist$max_month[i - j] - 2
    # Save a boolean indicating whether the actual date difference falls within the max and min expected date differences for the consecutive concepts
    agreement_t_c <- (adjConceptMonths_MaxExpectedDelta >= delta_t) & (delta_t >= adjConceptMonths_MinExpectedDelta)

    # If there is agreement between the concepts, update the return_agreement_t_c variable
    if (agreement_t_c == TRUE) {
      return(TRUE) # return early -- only needs to be true once
    }
  }
  # Next, do the comparisons to the records SURROUNDING record i
  len_to_start <- i - 1
  len_to_end <- nrow(personlist) - i
  bridge_len <- min(len_to_start, len_to_end)
  if (bridge_len == 0) {
    return(FALSE)
  } # no records surrounding

  # Iterate through the bridge records around record i, in case record i was an outlier
  for (s in seq_len(len_to_start)) {
    for (e in seq_len(len_to_end)) {
      # Obtain the time difference in months between the bridge records
      bridge_delta_t <- as.numeric(difftime(personlist$domain_concept_start_date[i + e],
        personlist$domain_concept_start_date[i - s],
        units = "days"
      ) / 30)
      # Obtain the max and min expected month differences based on clinician knowledge of the bridge concepts (allow two extra months for leniency)
      bridge_adjConceptMonths_MaxExpectedDelta <- personlist$max_month[i + e] - personlist$min_month[i - s] + 2
      bridge_adjConceptMonths_MinExpectedDelta <- personlist$min_month[i + e] - personlist$max_month[i - s] - 2
      # Check if there is agreement between the bridge concepts
      bridge_agreement_t_c <- (bridge_adjConceptMonths_MaxExpectedDelta >= bridge_delta_t) &
        (bridge_delta_t >= bridge_adjConceptMonths_MinExpectedDelta)
      # If there is agreement between the bridge concepts, update the return_agreement_t_c variable
      if (bridge_agreement_t_c == TRUE) {
        return(TRUE) # return early -- only needs to be true once
      }
    }
  }

  # Return the final agreement status between the concepts
  return(FALSE)
}

assign_episodes <- function(personlist, ...) {
  if (nrow(personlist) == 1) {
    personlist$person_episode_number <- 1
    return(personlist)
  }

  # Filter to plausible pregnancy timelines and concept month sequences, and number by episode to get person_episode_number

  # Initialize variables for episode numbering and storing episode information
  # # Treat the first record as belonging to the first episode
  person_episode_number <- 1
  person_episodes <- 1
  person_episode_chr <- "1"
  person_episode_dates <- list()

  # Add the date of the current record to the corresponding episode in person_episode_dates
  person_episode_dates[[person_episode_chr]] <- c(personlist$domain_concept_start_date[1])

  for (i in 2:nrow(personlist)) {
    # Calculate the time difference in months between the current record and the previous record
    delta_t <- as.numeric(difftime(personlist$domain_concept_start_date[i],
      personlist$domain_concept_start_date[i - 1],
      units = "days"
    ) / 30)

    # Perform the checks to determine whether this is a continuation of an episode or the start of a new episode
    agreement_t_c <- records_comparison(personlist, i)

    # If there is no agreement between the concepts and the time difference is greater than 2 months,
    # change to 1 month, ie retry period
    # increment the person_episode_number to indicate a new episode
    if ((!agreement_t_c) && (delta_t > 1)) {
      person_episode_number <- person_episode_number + 1
    } else if (delta_t > 10) {
      # If the time difference is greater than 10 months, increment the person_episode_number to indicate a new episode
      person_episode_number <- person_episode_number + 1
    }

    # Append the person_episode_number to the person_episodes list
    person_episodes <- c(person_episodes, person_episode_number)

    person_episode_chr <- as.character(person_episode_number)

    # Check if the person_episode_number is already in the person_episode_dates list
    if (!(person_episode_chr %in% names(person_episode_dates))) {
      person_episode_dates[[person_episode_chr]] <- personlist$domain_concept_start_date[i]
    } else {
      # Add the date of the current record to the corresponding episode in person_episode_dates
      person_episode_dates[[person_episode_chr]] <- c(
        person_episode_dates[[person_episode_chr]],
        personlist$domain_concept_start_date[i]
      )
    }
  }

  # - Check that all the episodes are < 12 mo in length (the 9-10 mo of pregnancy plus the few months of delivery concept ramblings).
  # In the case that any have to be removed, loop through the episodes of the patient again and renumber the remaining episodes
  episodes_to_remove <- c()
  for (episode in names(person_episode_dates)) {
    len_of_episode <- as.numeric(difftime(person_episode_dates[[episode]][length(person_episode_dates[[episode]])],
      person_episode_dates[[episode]][1],
      units = "days"
    ) / 30)
    if (len_of_episode > 12) {
      episodes_to_remove <- c(episodes_to_remove, episode)
    }
  }
  new_person_episodes <- ifelse(as.character(person_episodes) %in% episodes_to_remove, 0, person_episodes)
  numUniqueNonZero <- sum(unique(new_person_episodes) != 0)
  nonZeroNewList <- 1:numUniqueNonZero
  nonZeroOrigList <- unique(new_person_episodes)[unique(new_person_episodes) != 0]
  new_person_episodes <- nonZeroNewList[match(new_person_episodes, nonZeroOrigList)]
  personlist$person_episode_number <- new_person_episodes

  return(personlist)
}

get_PPS_episodes <- function(input_GT_concepts_df, PPS_concepts, person_tbl) {
  patients_with_preg_concepts <- filter(input_GT_concepts_df, !is.na(domain_concept_start_date)) %>%
    left_join(PPS_concepts, by = join_by(domain_concept_id)) %>%
    inner_join(select(person_tbl, person_id, sex_at_birth_concept_id, year_of_birth, day_of_birth, month_of_birth),
      by = "person_id"
    ) %>%
    mutate(
      day_of_birth = if_else(is.na(day_of_birth), 1, day_of_birth),
      month_of_birth = if_else(is.na(month_of_birth), 1, month_of_birth),
      date_of_birth = as.Date(paste0(year_of_birth, "-", month_of_birth, "-", day_of_birth)),
      date_diff = date_diff(domain_concept_start_date, date_of_birth, sql("day")),
      age = date_diff / 365
    ) %>%
    # women of reproductive age
    filter(
      sex_at_birth_concept_id != 45880669,
      age >= 15,
      age < 56
    ) %>%
    select(-ends_with("_of_birth"), -date_diff, -sex_at_birth_concept_id)

  # OBTAIN ALL RELEVANT INPUT PATIENTS AND SAVE GT INFORMATION PER CONCEPT TO A LOOKUP DICTIONARY
  # First we save the women that have gestational timing concepts, and save the gestational timing information for each concept.
  # We add the concepts and their gestational timing months ([min,max]) during pregnancy to a dictionary (hash) in
  # concept key: month value list format e.g. {2211756: [4,8], 2101830: [2,2]...}


  # SAVE EXPECTED GESTATIONAL TIMING MONTH INFORMATION FOR EACH OF THE PATIENT RECORDS
  # Looping over each person with pregnancy concepts, order their concepts by date of each record, and for each concept ID in order, loop through the
  # keys of the dictionary and compare to the concept ID, if there’s a match, save the month value(s) to a list for the record date. You’ll end up with
  # record date: list of matching months, save this to a new dictionary with record dates as the keys. Where no match occurs, put NA
  #   person_dates_dict <- split(person_dates_df$list_col, person_dates_df$person_id)

  person_dates_df <- collect(patients_with_preg_concepts, page_size = 50000) %>%
    group_by(person_id) %>%
    arrange(domain_concept_start_date)

  res <- person_dates_df %>%
    group_modify(assign_episodes)

  return(res)
}

get_episode_max_min_dates <- function(get_PPS_episodes_df) {
  df <- get_PPS_episodes_df %>%
    filter(!is.na(person_episode_number)) %>%
    group_by(person_id, person_episode_number) %>%
    summarise(
      # first time pregnancy concept appears
      episode_min_date = min(domain_concept_start_date),
      # last time a pregnancy concept appears
      episode_max_date = max(domain_concept_start_date),
      episode_max_date_plus_two_months = episode_max_date %m+% months(2),
      # add the number of unique gestational timing concepts per episode
      n_GT_concepts = n_distinct(domain_concept_id)
    ) %>%
    ungroup()

  return(df)
}

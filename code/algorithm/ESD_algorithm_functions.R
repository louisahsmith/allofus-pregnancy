get_timing_concepts <- function(concept_tbl, condition_occurrence_tbl, observation_tbl,
                                measurement_tbl, procedure_occurrence_tbl, final_merged_episode_detailed_df, PPS_concepts) {
  # obtain the gestational timing <= 3 month concept information to use as additional information for precision category designation

  pregnant_dates <- final_merged_episode_detailed_df

  algo2_timing_concepts_id_list <- PPS_concepts %>%
    select(domain_concept_id) %>%
    pull(domain_concept_id) %>%
    as.integer()

  observation_concept_list <- c(3011536, 3026070, 3024261, 4260747, 40758410, 3002549, 43054890, 46234792, 4266763, 40485048, 3048230, 3002209, 3012266)
  measurement_concept_list <- c(3036844, 3048230, 3001105, 3002209, 3050433, 3012266)

  est_date_of_delivery_concepts <- c(1175623, 1175623, 3001105, 3011536, 3024261, 3024973, 3026070, 3036322, 3038318, 3038608, 4059478, 4128833, 40490322, 40760182, 40760183, 42537958)
  est_date_of_conception_concepts <- c(3002314, 3043737, 4058439, 4072438, 4089559, 44817092)
  len_of_gestation_at_birth_concepts <- c(4260747, 43054890, 46234792, 4266763, 40485048)

  # need to find concept names that contain 'gestation period' as well as the specific concepts
  concepts_to_search <- concept_tbl %>%
    filter(
      str_detect(tolower(concept_name), "gestation period") |
        concept_id %in% c(
          observation_concept_list, measurement_concept_list, algo2_timing_concepts_id_list,
          est_date_of_delivery_concepts, est_date_of_conception_concepts,
          len_of_gestation_at_birth_concepts
        )
    ) %>%
    select(concept_id, concept_name)


  get_preg_related_concepts <- function(df, person_id_list, df_date_col) {
    df %>%
      select(person_id, all_of(df_date_col), concept_id, concept_name, value_col) %>%
      rename(all_of(c(domain_concept_start_date = df_date_col))) %>%
      inner_join(person_id_list, by = join_by(
        person_id, domain_concept_start_date >= start_date,
        domain_concept_start_date <= recorded_episode_end
      )) %>%
      select(person_id, domain_concept_start_date,
        domain_concept_id = concept_id, domain_concept_name = concept_name,
        start_date, recorded_episode_end, value_col, episode_number
      )
  }

  # add: change to pregnancy start rather than recorded episode start
  person_id_list <- pregnant_dates %>%
    mutate(start_date = pmin(pregnancy_start, recorded_episode_start, na.rm = TRUE)) %>%
    select(person_id, start_date, recorded_episode_end, episode_number) %>%
    aou_create_temp_table(nchar_batch = 50000)

  c_o <- concepts_to_search %>%
    inner_join(condition_occurrence_tbl, by = c("concept_id" = "condition_concept_id")) %>%
    mutate(value_col = concept_name) %>%
    get_preg_related_concepts(person_id_list, "condition_start_date")
  o_df <- concepts_to_search %>%
    inner_join(observation_tbl, by = c("concept_id" = "observation_concept_id")) %>%
    mutate(value_col = value_as_string) %>%
    get_preg_related_concepts(person_id_list, "observation_date")
  m_df <- concepts_to_search %>%
    inner_join(measurement_tbl, by = c("concept_id" = "measurement_concept_id")) %>%
    mutate(value_col = value_as_number) %>%
    get_preg_related_concepts(person_id_list, "measurement_date")
  p_df <- concepts_to_search %>%
    inner_join(procedure_occurrence_tbl, by = c("concept_id" = "procedure_concept_id")) %>%
    mutate(value_col = concept_name) %>%
    get_preg_related_concepts(person_id_list, "procedure_date")

  preg_related_concepts <- list(c_o, o_df, mutate(m_df, value_col = as.character(value_col)), p_df) %>%
    reduce(union_all)

  algo2_timing_concepts_df <- PPS_concepts %>%
    select(domain_concept_id, min_month, max_month)

  preg_related_concepts_local <- preg_related_concepts %>%
    left_join(algo2_timing_concepts_df, by = "domain_concept_id") %>%
    collect(page_size = 20000) %>%
    mutate(domain_value = str_replace(value_col, "\\|text_result_val:", "")) %>%
    mutate(domain_value = str_replace(domain_value, "\\|mapped_text_result_val:", "")) %>%
    mutate(domain_value = str_replace(domain_value, "Gestation period, ", "")) %>%
    mutate(domain_value = str_replace(domain_value, "gestation period, ", "")) %>%
    mutate(domain_value = str_replace(domain_value, " weeks", "")) %>%
    mutate(domain_value = as.integer(as.numeric(domain_value))) %>%
    mutate(
      keep_value = if_else((str_detect(tolower(domain_concept_name), "gestation period,")) |
        (str_detect(tolower(domain_concept_name), "gestational age")) |
        (domain_concept_id %in% c(3048230, 3002209, 3012266) & domain_value < 44 & domain_value > 0), 1, 0),
      extrapolated_preg_start = if_else(keep_value == 1, domain_concept_start_date - (domain_value * 7), NA_Date_)
    )


  preg_related_concepts_local
}


validate <- function(date_text) {
  tryCatch(
    {
      as.Date(date_text, "%Y-%m-%d")
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

findIntersection <- function(intervals) {
  if (length(intervals) == 1) {
    intervals <- matrix(intervals[[1]], ncol = 2)
  } else {
    # Sort intervals
    intervals <- reduce(intervals, rbind)
  }
  intervals <- as.data.frame(intervals) %>%
    mutate(across(everything(), as.Date)) %>%
    arrange(V1)

  # First remove outlier ranges via the IQR*1.5 approach. Outlier ranges are determined by the number of overlaps each range has with other ranges.
  overlapCountDict <- rep(0, nrow(intervals))
  for (j in 1:nrow(intervals)) {
    for (m in 1:nrow(intervals)) {
      if (j != m) {
        # First interval
        last <- intervals[j, 2]
        first <- intervals[j, 1]
        # 1st condition - does first date equal either the first or last date
        if ((intervals[m, 1] == last) || (intervals[m, 1] == first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
          # 2nd condition - does second date equal either the first or last date
        } else if ((intervals[m, 2] == last) || (intervals[m, 2] == first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
          # 3rd condition - does second date fall between the first and last date
        } else if ((intervals[m, 2] < last) && (intervals[m, 2] > first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
          # 4th condition - does first date fall between the first and last date
        } else if ((intervals[m, 1] < last) && (intervals[m, 1] > first)) {
          overlapCountDict[j] <- overlapCountDict[j] + 1
          next
        }
      }
    }
  }

  intervals$overlapCountDict <- overlapCountDict
  # if there were no overlaps with a given date, overlapCountDict is 0
  # but the other ones will be higher
  allCounts <- overlapCountDict
  # these are both going to be 1 if all overlap
  countsQ1 <- quantile(allCounts, 0.25)
  countsQ3 <- quantile(allCounts, 0.75)
  # and outlier metric will be 0
  outlierMetric <- (countsQ3 - countsQ1) * 1.5
  outlierThreshold <- abs(countsQ1 - outlierMetric)
  if (outlierThreshold == 0) {
    filteredIntervals <- filter(intervals, overlapCountDict > outlierThreshold)
  } else {
    filteredIntervals <- filter(intervals, overlapCountDict >= outlierThreshold)
  }
  filteredIntervals <- arrange(filteredIntervals, desc(overlapCountDict))

  # Now the outliers are removed, proceed with obtaining the overlaps
  N <- nrow(filteredIntervals)

  # If only one interval
  if (N == 1) {
    # First interval
    last <- filteredIntervals[1, 2] # last day of overlapping ranges
    min_start <- filteredIntervals[1, 2] # minimum day of intersection of ranges
    first <- filteredIntervals[1, 1] # first day of overlapping ranges
    max_start <- filteredIntervals[1, 1] # maximum day of intersection of ranges
    intersection_list <- c(last, first, min_start, max_start)
    # If no intervals, take the first only if more than one
  } else if (N == 0) {
    # First interval
    intervalsToSort <- intervals[order(overlapCountDict, decreasing = TRUE), , drop = FALSE]
    last <- intervalsToSort[1, 2]
    min_start <- intervalsToSort[1, 2]
    first <- intervalsToSort[1, 1]
    max_start <- intervalsToSort[1, 1]
    intersection_list <- c(last, first, min_start, max_start)
  } else {
    # First interval
    last <- filteredIntervals[1, 2] # last day of overlapping ranges
    min_start <- filteredIntervals[1, 2] # minimum (last) day of intersection of ranges
    first <- filteredIntervals[1, 1] # first day of overlapping ranges
    max_start <- filteredIntervals[1, 1] # maximum (first) day of intersection of ranges

    # Check rest of the intervals and find the intersection
    for (i in 2:N) {
      if (filteredIntervals[i, 1] < first) {
        first <- filteredIntervals[i, 1]
      }
      if (filteredIntervals[i, 2] > last) {
        last <- filteredIntervals[i, 2]
      }
      if ((filteredIntervals[i, 2] < min_start) && (filteredIntervals[i, 2] > max_start)) {
        min_start <- filteredIntervals[i, 2]
      }
      if ((filteredIntervals[i, 1] > max_start) && (filteredIntervals[i, 1] < min_start)) {
        max_start <- filteredIntervals[i, 1]
      }
    }

    intersection_list <- c(last, first, min_start, max_start)
  }

  return(intersection_list)
}

# check for GW concept overlap to the intervals
remove_GW_outliers <- function(lol_of_GW_concepts) {
  list_of_GW_concepts <- as.Date(unlist(lol_of_GW_concepts))
  median_startdate <- sort(list_of_GW_concepts)[ceiling(length(list_of_GW_concepts) / 2)]
  allDistances <- rep(0, length(list_of_GW_concepts))
  for (j in seq_along(list_of_GW_concepts)) {
    allDistances[j] <- as.numeric(max(list_of_GW_concepts[j], median_startdate) - min(list_of_GW_concepts[j], median_startdate))
  }
  distQ1 <- quantile(allDistances, 0.25)
  distQ3 <- quantile(allDistances, 0.75)
  outlierMetric <- (distQ3 - distQ1) * 1.5
  outlierLowerThreshold <- distQ1 - outlierMetric
  outlierUpperThreshold <- distQ3 + outlierMetric
  filteredDates <- list_of_GW_concepts[allDistances >= outlierLowerThreshold & allDistances <= outlierUpperThreshold]
  return(filteredDates)
}

# definition to get accuracy category
assign_precision_category <- function(precision_days) {
  case_when(
    precision_days == -1 ~ "week_poor-support",
    precision_days >= 0 & precision_days <= 7 ~ "week",
    precision_days > 7 & precision_days <= 14 ~ "two-week",
    precision_days > 14 & precision_days <= 21 ~ "three-week",
    precision_days > 21 & precision_days <= 28 ~ "month",
    precision_days > 28 & precision_days <= 56 ~ "two-month",
    precision_days > 56 & precision_days <= 84 ~ "three-month",
    TRUE ~ "non-specific"
  )
}

# applying udf to the date array column to obtain the new column 'final_timing_info' (a list of [inferred_episode_start, precision_days, precision_category]) for each row
get_gt_timing <- function(dateslist) {
  # Iterate over the dateslist
  timing_arr <- map(dateslist[map_lgl(dateslist, validate)], sort)
  GW_list <- dateslist[sapply(dateslist, function(x) length(x) == 1)]
  GR3m_list <- dateslist[sapply(dateslist, function(x) length(x) == 2)]

  inferred_start_date <- as.Date("2000-01-01")
  precision_days <- 999
  precision_category <- "-999"
  intervalsCount <- 0
  majorityOverlapCount <- 0

  # get length of list with GR3m ranges
  N <- length(GR3m_list)

  if (N > 0) {
    common_GR3m_interval <- findIntersection(GR3m_list)
    # [datetime.date(2014, 11, 2), datetime.date(2014, 7, 3), datetime.date(2014, 10, 2), datetime.date(2014, 8, 17)]
  } else {
    common_GR3m_interval <- NULL
  }

  plausibleDays <- 0
  maxRangeDays <- 0
  range_s <- NULL
  range_e <- NULL
  interval_s <- NULL
  interval_e <- NULL
  daterangeMidpoint <- NULL

  if (!is.null(common_GR3m_interval)) { # if it's not an empty array
    range_e <- as.Date(common_GR3m_interval[1]) # end date of range
    range_s <- as.Date(common_GR3m_interval[2]) # start date of range
    interval_e <- as.Date(common_GR3m_interval[3]) # end date of intersection
    interval_s <- as.Date(common_GR3m_interval[4]) # start date of intersection

    plausibleDays <- as.numeric(difftime(interval_e, interval_s, units = "days"))
    maxRangeDays <- as.numeric(difftime(range_e, range_s, units = "days"))
    daterangeMidpoint <- interval_s + days(as.integer(plausibleDays / 2)) # get midpoint of intersection
    # utilize the overlap more when it gets narrowed down to less than a week by taking the midpoint and adding 3 days either side (otherwise unlikely to overlap much with GW concepts and thus will be ignored)
    if (plausibleDays < 7) {
      interval_s <- daterangeMidpoint - days(3)
      interval_e <- daterangeMidpoint + days(3)
      plausibleDays <- 6
    }
  }

  # there are week-level estimates
  if (length(GW_list) > 0) {
    if (!is.null(interval_s)) { # GR3m interval is not null
      intervalsCount <- intervalsCount + 1
      # check for overlaps with GR3m concept ranges
      gwConceptCount <- length(GW_list)
      overlapping_gwConcepts <- list()

      for (gwlist in GW_list) {
        gw_concept_start_date <- gwlist[[1]]

        if (gw_concept_start_date >= interval_s && gw_concept_start_date <= interval_e) {
          overlapping_gwConcepts <- c(overlapping_gwConcepts, gw_concept_start_date)
        }
      }

      overlapping_gwConcepts_count <- length(overlapping_gwConcepts)
      perc_overlapping <- (overlapping_gwConcepts_count / gwConceptCount) * 100

      if (perc_overlapping > 50) {
        majorityOverlapCount <- majorityOverlapCount + 1
        filtDates <- remove_GW_outliers(overlapping_gwConcepts)
        inferred_start_date <- filtDates[[1]] # latest date
        precision_days <- as.numeric(max(filtDates) - min(filtDates))
      } else {
        filtDates <- remove_GW_outliers(GW_list)
        inferred_start_date <- filtDates[[1]]
        precision_days <- as.numeric(max(filtDates) - min(filtDates))

        if (length(filtDates) == 1) {
          # there's only one gw concept and it doesn't overlap
          precision_days <- -1
        }
      }
    } else {
      filtDates <- remove_GW_outliers(GW_list)
      inferred_start_date <- filtDates[[1]]
      precision_days <- as.numeric(max(filtDates) - min(filtDates))

      if (length(filtDates) == 1) {
        precision_days <- -1
      }
    }
  } else {
    inferred_start_date <- daterangeMidpoint
    precision_days <- maxRangeDays
  }

  precision_category <- assign_precision_category(precision_days)

  single_episode_timingres <- list(
    inferred_start_date = inferred_start_date,
    precision_days = precision_days,
    precision_category = precision_category,
    intervalsCount = intervalsCount,
    majorityOverlapCount = majorityOverlapCount
  )

  return(single_episode_timingres)
}

episodes_with_gestational_timing_info <- function(get_timing_concepts_df) {
  # add on either GW or GR3m designation depending on whether the concept is present
  timing_designation_df <- get_timing_concepts_df %>%
    mutate(
      domain_concept_id = as.integer(domain_concept_id),
      GT_type = case_when(
        str_detect(str_to_lower(domain_concept_name), "gestation period") | domain_concept_id %in% c(
          3048230, 3002209, 3012266,
          # added additional concept
          3050433
        ) ~ "GW",
        !is.na(min_month) ~ "GR3m",
        TRUE ~ NA
      )
    )

  # add on the max and min pregnancy start dates predicted by each concept
  timing_designation_df <- timing_designation_df %>%
    mutate(
      min_days_to_pregnancy_start = if_else(GT_type == "GR3m", round(min_month * 30.4), NA),
      max_days_to_pregnancy_start = if_else(GT_type == "GR3m", round(max_month * 30.4), NA)
    )


  # add the max and min possible pregnancy start dates according to the GR3m concepts
  timing_designation_df <- timing_designation_df %>%
    mutate(
      min_pregnancy_start = if_else(GT_type == "GR3m", domain_concept_start_date - as.integer(min_days_to_pregnancy_start), NA_Date_),
      max_pregnancy_start = if_else(GT_type == "GR3m", domain_concept_start_date - as.integer(max_days_to_pregnancy_start), NA_Date_)
    ) %>%
    select(-min_days_to_pregnancy_start, -max_days_to_pregnancy_start)

  # remove type if null values
  timing_designation_df <- timing_designation_df %>%
    mutate(GT_type = case_when(
      GT_type == "GW" & is.na(domain_value) ~ NA,
      GT_type == "GW" & is.na(extrapolated_preg_start) ~ NA,
      TRUE ~ GT_type
    ))

  # filter to rows with GW or GR3m type
  timing_designation_df <- timing_designation_df %>%
    filter(GT_type == "GW" | GT_type == "GR3m")

  # get start date range for GR3m
  timing_designation_df <- timing_designation_df %>%
    mutate(across(c(extrapolated_preg_start, min_pregnancy_start, max_pregnancy_start), as.character)) %>%
    rowwise() %>%
    mutate(
      preg_start_range = ifelse(is.na(extrapolated_preg_start), paste(
        max_pregnancy_start,
        min_pregnancy_start
      ), extrapolated_preg_start),
      # get start dates for GW
      extr = extrapolated_preg_start
    ) %>%
    ungroup()

  # create list of all dates
  timing_designation_df <- timing_designation_df %>%
    mutate(all_GT_info = ifelse(is.na(extr), preg_start_range, extr))

  # add on a categorization column to ensure gestation week concepts are distinguished as a single entity and others are unique per concept (important for the next steps of removing duplicates)
  timing_designation_df <- timing_designation_df %>%
    # add gt-type here so othe rmeasurements not rolled up
    mutate(domain_concept_name_rollup = ifelse(!is.na(domain_value) & GT_type == "GW",
      "Gestation Week", domain_concept_name
    ))

  # IMPORTANT: sort by domain value (gest week concepts) from highest (latest in pregancy) to lowest (earliest in pregnancy)
  # so that later on the first element of the GW list can be taken for 'latest in pregnancy' concept
  timing_designation_df <- timing_designation_df %>%
    arrange(person_id, episode_number, desc(domain_value)) %>%
    # remove all rows but the first for each concept date ~ GT_type combination (as these are likely to be historical references)
    # so on each date, the highest domain value will be chosen
    group_by(person_id, episode_number, domain_concept_name_rollup, domain_concept_start_date, GT_type) %>%
    slice(1) %>%
    ungroup()

  # group the dataset by patient and episode number and pass relevant columns to a function that adds inferred_start_date and precision
  new_timing_designation_df <- timing_designation_df %>%
    group_by(person_id, episode_number) %>%
    # first is the date with the highest domain value
    summarise(
      GT_info_list = map(list(all_GT_info), ~ str_split(.x, " ")),
      GW_flag = as.numeric(any(GT_type == "GW")),
      GR3m_flag = as.numeric(any(GT_type == "GR3m")),
      .groups = "drop"
    )

  new_timing_designation_df <- new_timing_designation_df %>%
    rowwise() %>%
    mutate(
      final_timing_info = list(get_gt_timing(GT_info_list)),
      inferred_episode_start = final_timing_info[[1]],
      precision_days = final_timing_info[[2]],
      precision_category = final_timing_info[[3]],
      intervalsCount = final_timing_info[[4]],
      majorityOverlapCount = final_timing_info[[5]]
    ) %>%
    ungroup() %>%
    select(
      person_id, episode_number, GT_info_list, GW_flag, GR3m_flag, inferred_episode_start, precision_days,
      precision_category, intervalsCount, majorityOverlapCount
    )

  # print the GW and GR3m concept overlap information to log
  majorityOverlapCountTotal <- sum(new_timing_designation_df$majorityOverlapCount, na.rm = TRUE)
  intervalsCountTotal <- sum(new_timing_designation_df$intervalsCount, na.rm = TRUE)
  percMajority <- (majorityOverlapCountTotal / intervalsCountTotal) * 100

  cat("Number of episodes with GR3m intervals:",
    intervalsCountTotal,
    "Percent of cases that contain a GR3m intersection that ALSO have majority GW overlap:",
    percMajority,
    sep = "\n"
  )

  return(new_timing_designation_df)
}

merged_episodes_with_metadata <- function(episodes_with_gestational_timing_info_df, final_merged_episode_detailed_df, Matcho_term_durations) {
  # Add other pregnancy and demographic related info for each episode.

  # Assign input data frames to variables
  demographics_df <- final_merged_episode_detailed_df
  timing_df <- episodes_with_gestational_timing_info_df %>% select(-GT_info_list)
  term_max_min <- collect(Matcho_term_durations)

  final_df <- demographics_df %>%
    left_join(timing_df, by = c("person_id", "episode_number")) %>%
    distinct()

  # Add missing GW_flag and GR3m_flag
  final_df <- final_df %>%
    mutate(
      GW_flag = if_else(is.na(GW_flag), 0, GW_flag),
      GR3m_flag = if_else(is.na(GR3m_flag), 0, GR3m_flag)
    )

  # Check if categories match between algorithms and dates are within 14 days of each other for outcomes only
  final_df <- final_df %>%
    mutate(outcome_match = case_when(
      HIP_outcome_category == PPS_outcome_category & HIP_outcome_category != "PREG" &
        abs(as.numeric(difftime(HIP_end_date, PPS_end_date, units = "days"))) <= 14 ~ 1,
      HIP_outcome_category == "PREG" & PPS_outcome_category == "PREG" ~ 1,
      TRUE ~ 0
    ))

  # If categories don't match, take the category that occurs second (outcome category from HIP algorithm is prioritized)
  final_df <- final_df %>%
    group_by(person_id) %>%
    arrange(episode_number) %>%
    mutate(
      next_HIP_outcome = lead(HIP_outcome_category),
      final_outcome_category = case_when(
        outcome_match == 1 ~ HIP_outcome_category,
        outcome_match == 0 & is.na(PPS_outcome_category) ~ HIP_outcome_category,
        outcome_match == 0 & is.na(HIP_outcome_category) ~ PPS_outcome_category,
        # if they don't match, but the hip end date is not within 7 days before the PPS outcome
        # add: go with HIP if the PPS is the next one and there's sufficient separation
        outcome_match == 0 & HIP_outcome_category != "PREG" & PPS_outcome_category != "PREG" &
          !is.na(next_HIP_outcome) & PPS_outcome_category == next_HIP_outcome & HIP_end_date <= PPS_end_date - days(14) ~ HIP_outcome_category,
        # but otherwise go with PPS
        outcome_match == 0 & HIP_outcome_category != "PREG" & PPS_outcome_category != "PREG" &
          HIP_end_date <= PPS_end_date - days(7) ~ PPS_outcome_category,
        # or if they're similar timing go with HIP
        TRUE ~ HIP_outcome_category
      )
    ) %>%
    ungroup()

  # If categories don't match, take the end date that occurs second (outcome date from HIP is prioritized)
  final_df <- final_df %>%
    mutate(inferred_episode_end = case_when(
      outcome_match == 1 ~ HIP_end_date,
      outcome_match == 0 & is.na(PPS_outcome_category) ~ HIP_end_date,
      outcome_match == 0 & is.na(HIP_outcome_category) ~ PPS_end_date,
      outcome_match == 0 & HIP_outcome_category != "PREG" & PPS_outcome_category != "PREG" &
        !is.na(next_HIP_outcome) & PPS_outcome_category == next_HIP_outcome &
        HIP_end_date <= PPS_end_date - days(14) ~ HIP_end_date,
      outcome_match == 0 & HIP_outcome_category != "PREG" & PPS_outcome_category != "PREG" &
        HIP_end_date <= PPS_end_date - days(7) ~ PPS_end_date,
      !is.na(HIP_end_date) ~ HIP_end_date,
      !is.na(PPS_end_date) ~ PPS_end_date
    ))

  # Join with term_max_min data frame and drop 'retry' column
  final_df <- final_df %>%
    left_join(term_max_min, by = c("final_outcome_category" = "category")) %>%
    select(-retry)

  # If no start date, subtract the max term in days from inferred episode end
  final_df <- final_df %>%
    mutate(inferred_episode_start = if_else(is.na(inferred_episode_start), inferred_episode_end - days(max_term), inferred_episode_start))

  # Convert precision_days to integer type
  final_df <- final_df %>%
    mutate(precision_days = as.integer(precision_days))

  # Add missing accuracy info for remaining episodes
  final_df <- final_df %>%
    mutate(precision_days = if_else(is.na(precision_days), max_term - min_term, precision_days))

  # Add missing accuracy category for remaining episodes
  final_df <- final_df %>%
    mutate(precision_category = if_else(is.na(precision_category), assign_precision_category(precision_days), precision_category))

  # Calculate gestational age at inferred episode end
  final_df <- final_df %>%
    mutate(gestational_age_days_calculated = as.integer(difftime(inferred_episode_end, inferred_episode_start, units = "days")))

  # Check if outcome aligns with term duration expected of that outcome
  final_df <- final_df %>%
    mutate(term_duration_flag = case_when(
      gestational_age_days_calculated >= min_term & gestational_age_days_calculated <= max_term ~ 1,
      final_outcome_category == "PREG" & gestational_age_days_calculated <= 301 ~ 1,
      TRUE ~ 0
    )) %>%
    select(-min_term, -max_term)

  # Add outcome concordance score - 2 highly concordant, 1 somewhat concordant, 0 not accurate/not enough info
  final_df <- final_df %>%
    mutate(outcome_concordance_score = case_when(
      outcome_match == 1 & term_duration_flag == 1 & GW_flag == 1 ~ 2,
      outcome_match == 0 & term_duration_flag == 1 & GW_flag == 1 ~ 1,
      TRUE ~ 0
    ))

  # Calculate preterm status from calculation
  final_df <- final_df %>%
    mutate(preterm_status_from_calculation = if_else(gestational_age_days_calculated < 259, 1, 0))

  # Print the min episode/pregnancy start and max episode/pregnancy end to check on time period of dataset
  min_episode_date <- min(final_df$recorded_episode_start, na.rm = TRUE)
  max_episode_date <- max(final_df$recorded_episode_end, na.rm = TRUE)
  min_pregnancy_date <- min(final_df$inferred_episode_start, na.rm = TRUE)
  max_pregnancy_date <- max(final_df$inferred_episode_end, na.rm = TRUE)
  cat("Min episode start date:",
    as.character(min_episode_date),
    "Max episode end date:",
    as.character(max_episode_date),
    "Min pregnancy start date:",
    as.character(min_pregnancy_date),
    "Max pregnancy end date:",
    as.character(max_pregnancy_date),
    sep = "\n"
  )

  return(final_df)
}

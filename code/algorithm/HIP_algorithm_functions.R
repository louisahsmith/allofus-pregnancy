initial_pregnant_cohort <- function(procedure_occurrence_tbl, measurement_tbl,
                                    observation_tbl, condition_occurrence_tbl,
                                    person_tbl, HIP_concepts) {
  # Get concepts specific for pregnancy from domain tables.

  observation_df <- observation_tbl %>%
    select(
      person_id,
      concept_id = observation_concept_id,
      visit_date = observation_date,
      value_as_number
    ) %>%
    inner_join(HIP_concepts, by = "concept_id")

  measurement_df <- measurement_tbl %>%
    select(
      person_id,
      concept_id = measurement_concept_id,
      visit_date = measurement_date,
      value_as_number
    ) %>%
    inner_join(HIP_concepts, by = "concept_id")

  procedure_df <- procedure_occurrence_tbl %>%
    select(
      person_id,
      concept_id = procedure_concept_id,
      visit_date = procedure_date
    ) %>%
    inner_join(HIP_concepts, by = "concept_id")

  # filter condition table
  condition_df <- condition_occurrence_tbl %>%
    select(
      person_id,
      concept_id = condition_concept_id,
      visit_date = condition_start_date
    ) %>%
    inner_join(HIP_concepts, by = "concept_id")

  # combine tables
  all_dfs <- list(measurement_df, procedure_df, observation_df, condition_df)
  union_df <- reduce(all_dfs, union_all)

  # get unique person ids for women of reproductive age
  person_df <- person_tbl %>%
    filter(
      # 45878463: Female
      # 46273637: Intersex
      # 45880669: Male
      # 1177221: I prefer not to answer
      # 903096: Skip
      # 4124462: None
      sex_at_birth_concept_id != 45880669
      # the majority of the people in the other non-Female or Male categories
      # also report female gender
    ) %>%
    mutate(
      day_of_birth = if_else(is.na(day_of_birth), 1, day_of_birth),
      month_of_birth = if_else(is.na(month_of_birth), 1, month_of_birth),
      date_of_birth = as.Date(paste0(year_of_birth, "-", month_of_birth, "-", day_of_birth))
    ) %>%
    select(person_id, date_of_birth)

  # keep only person_ids of women of reproductive age at some visit
  union_df <- union_df %>%
    inner_join(person_df, by = "person_id") %>%
    mutate(
      date_diff = date_diff(visit_date, date_of_birth, sql("day")),
      age = date_diff / 365
    ) %>%
    filter(age >= 15, age < 56)

  # return the resulting dataframe
  distinct(union_df)
}

# Note here that for SA and AB, if there is an episode that contains concepts for both,
# only one will be (essentially randomly) chosen
final_visits <- function(initial_pregnant_cohort_df, Matcho_outcome_limits, categories) {
  df <- initial_pregnant_cohort_df %>%
    filter(category %in% categories) %>%
    # only keep one obs per person-date -- they're all in the same category
    # select(person_id, visit_date, category) %>%
    group_by(person_id, visit_date) %>%
    # slicing by minimum concept id just a choice to make the code work
    # could have also done something like filter(row_number() == 1) but doesn't
    # work on databases
    slice_min(order_by = concept_id, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # distinct(person_id, visit_date, .keep_all = TRUE) %>% stopped working?!
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    # Create a new column called "days" that calculates the number of days between each visit for each person.
    mutate(days = date_diff(visit_date, lag(visit_date), sql("day"))) %>%
    ungroup()

  temp_df <- df

  # get minimum days between outcomes
  min_day <- Matcho_outcome_limits %>%
    filter(first_preg_category == categories[1] & outcome_preg_category == categories[1]) %>%
    pull(min_days)

  # identify first visit for each pregnancy episode
  first_df <- df %>%
    group_by(person_id) %>%
    slice_min(visit_date) %>%
    ungroup()

  other_df <- df %>%
    filter(days >= min_day)

  all_df <- union_all(first_df, other_df) %>%
    distinct()

  cat(paste0("Preliminary total number of ", paste(categories, collapse = " and "), " episodes:"),
    tally(all_df) %>% pull(n) %>% as.integer(),
    sep = "\n"
  )

  return(all_df)
}

add_stillbirth <- function(final_stillbirth_visits_df, final_livebirth_visits_df, Matcho_outcome_limits) {
  # Add stillbirth visits to livebirth visits table.

  # get minimum days between outcomes
  before_min <- Matcho_outcome_limits %>%
    filter(first_preg_category == "LB" & outcome_preg_category == "SB") %>%
    pull(min_days)

  after_min <- Matcho_outcome_limits %>%
    filter(first_preg_category == "SB" & outcome_preg_category == "LB") %>%
    pull(min_days)


  # pull out the stillbirth episodes again, but first figure out if it's plausible
  # that they happened relative to a live birth
  final_temp_df <- union_all(final_stillbirth_visits_df, final_livebirth_visits_df) %>%
    select(-any_of(c("gest_value", "value_as_number"))) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(
      # get previous category if available
      previous_category = lag(category),
      # get difference in days with previous episode start date
      after_days = date_diff(visit_date, lag(visit_date), sql("day")),
      next_category = lead(category),
      # and next episode start date
      before_days = date_diff(lead(visit_date), visit_date, sql("day"))
    ) %>%
    filter(category == "SB") %>%
    filter(
      # it's the only episode
      (is.na(before_days) & is.na(after_days)) |
        # the previous category was a stillbirth and there's no next category
        # (those were already checked)
        (previous_category != "LB" & is.na(next_category)) |
        # same but opposite
        (next_category != "LB" & is.na(previous_category)) |
        (previous_category != "LB" & next_category != "LB") |
        # the last episode was a live birth and this one happens after the minimum
        (previous_category == "LB" & after_days >= before_min & is.na(next_category)) |
        # the next episode is a live birth and happens after the minimum
        (next_category == "LB" & before_days >= after_min & is.na(previous_category)) |
        # or surrounded by two live births spaced sufficiently
        (next_category == "LB" & before_days >= after_min & previous_category == "LB" & after_days >= before_min)
    ) %>%
    ungroup()

  # combine with livebirth table and drop columns
  final_df <- union_all(final_livebirth_visits_df, final_temp_df) %>%
    select(-previous_category, -next_category, -before_days, -after_days) %>%
    distinct()

  return(final_df)
}

add_ectopic <- function(add_stillbirth_df, Matcho_outcome_limits, final_ectopic_visits) {
  # get minimum days between outcomes
  # minimum number of days that ECT can follow LB and SB; LB and SB have the same days
  before_min <- Matcho_outcome_limits %>%
    filter(first_preg_category == "LB" & outcome_preg_category == "ECT") %>%
    pull(min_days)
  # minimum number of days that LB can follow ECT
  after_min_lb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "ECT" & outcome_preg_category == "LB") %>%
    pull(min_days)
  # minimum number of days that SB can follow ECT
  after_min_sb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "ECT" & outcome_preg_category == "SB") %>%
    pull(min_days)

  # get difference in days with subsequent visit
  final_temp_df <- union_all(add_stillbirth_df, select(final_ectopic_visits, -any_of(c("gest_value", "value_as_number")))) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(
      # get previous category if available
      previous_category = lag(category),
      # get difference in days with previous episode start date
      after_days = date_diff(visit_date, lag(visit_date), sql("day")),
      next_category = lead(category),
      # and next episode start date
      before_days = date_diff(lead(visit_date), visit_date, sql("day"))
    ) %>%
    # filter to ectopic visits
    # keep visits with days containing null values - indicates single event
    # keep visits not preceded by or followed by LB or SB
    # record is preceded by livebirth/stillbirth only
    # record is followed by livebirth only
    # record is followed by stillbirth only
    # record is followed by LB and preceded by livebirth/stillbirth
    # record is followed by SB and preceded by livebirth/stillbirth
    filter(category == "ECT") %>%
    filter(
      # it's the only episode
      (is.na(before_days) & is.na(after_days)) |
        # the previous category was ectopic and there's no next category
        # (those were already checked)
        # or some configuration
        (!previous_category %in% c("LB", "SB") & is.na(next_category)) |
        (!next_category %in% c("LB", "SB") & is.na(previous_category)) |
        (!previous_category %in% c("LB", "SB") & !next_category %in% c("LB", "SB")) |
        # the last episode was a delivery and this one happens after the minimum
        (previous_category %in% c("LB", "SB") & after_days >= before_min & is.na(next_category)) |
        # there was no previous category and the next live birth happens after the minimum
        (next_category == "LB" & before_days >= after_min_lb & is.na(previous_category)) |
        (next_category == "SB" & before_days >= after_min_sb & is.na(previous_category)) |
        # surrounded by each, appropriately spaced
        (next_category == "LB" & before_days >= after_min_lb & previous_category %in% c("LB", "SB") & after_days >= before_min) |
        (next_category == "SB" & before_days >= after_min_sb & previous_category %in% c("LB", "SB") & after_days >= before_min)
    ) %>%
    ungroup()

  final_df <- union_all(add_stillbirth_df, final_temp_df) %>%
    select(-previous_category, -next_category, -before_days, -after_days) %>%
    distinct()

  final_df
}

add_abortion <- function(add_ectopic_df, Matcho_outcome_limits, final_abortion_visits) {
  # Add abortion visits - SA and AB are treated the same.

  # get minimum days between outcomes
  # minimum number of days that ECT can follow LB and SB; LB and SB have the same days
  before_min_lb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "LB" & outcome_preg_category == "AB") %>%
    pull(min_days)

  before_min_ect <- Matcho_outcome_limits %>%
    filter(first_preg_category == "ECT" & outcome_preg_category == "AB") %>%
    pull(min_days)
  # minimum number of days that LB can follow ECT
  after_min_lb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "AB" & outcome_preg_category == "LB") %>%
    pull(min_days)
  # minimum number of days that SB can follow ECT
  after_min_sb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "AB" & outcome_preg_category == "SB") %>%
    pull(min_days)

  after_min_ect <- Matcho_outcome_limits %>%
    filter(first_preg_category == "AB" & outcome_preg_category == "ECT") %>%
    pull(min_days)

  # get difference in days with subsequent visit
  final_temp_df <- union_all(add_ectopic_df, select(final_abortion_visits, -any_of(c("gest_value", "value_as_number")))) %>%
    mutate(temp_category = ifelse(category == "SA", "AB", category)) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(
      # get previous category if available
      previous_category = lag(temp_category),
      # get difference in days with previous episode start date
      after_days = date_diff(visit_date, lag(visit_date), sql("day")),
      next_category = lead(temp_category),
      # and next episode start date
      before_days = date_diff(lead(visit_date), visit_date, sql("day"))
    ) %>%
    filter(temp_category == "AB") %>%
    filter(
      # don't have to worry about limits
      (is.na(before_days) & is.na(after_days)) |
        (!previous_category %in% c("LB", "SB", "ECT") & is.na(next_category)) |
        (!next_category %in% c("LB", "SB", "ECT") & is.na(previous_category)) |
        (!previous_category %in% c("LB", "SB", "ECT") & !next_category %in% c("LB", "SB", "ECT")) |

        # the last episode was a delivery and this one happens after the minimum
        (previous_category %in% c("LB", "SB") & after_days >= before_min_lb & is.na(next_category)) |
        (next_category == "LB" & before_days >= after_min_lb & is.na(previous_category)) |
        (next_category == "SB" & before_days >= after_min_sb & is.na(previous_category)) |
        (next_category == "LB" & previous_category %in% c("LB", "SB") & before_days >= after_min_lb & after_days >= before_min_lb) |
        (next_category == "SB" & previous_category %in% c("LB", "SB") & before_days >= after_min_sb & after_days >= before_min_lb) |
        (previous_category == "ECT" & after_days >= before_min_ect & is.na(next_category)) |
        (next_category == "ECT" & before_days >= after_min_ect & is.na(previous_category)) |
        (next_category == "ECT" & previous_category == "ECT" & before_days >= after_min_ect & after_days >= before_min_ect) |
        (next_category == "ECT" & previous_category %in% c("LB", "SB") & before_days >= after_min_ect & after_days >= before_min_lb) |
        (next_category == "LB" & previous_category == "ECT" & before_days >= after_min_lb & after_days >= before_min_ect) |
        (next_category == "SB" & previous_category == "ECT" & before_days >= after_min_sb & after_days >= before_min_ect)
    ) %>%
    ungroup()

  final_df <- union_all(add_ectopic_df, final_temp_df) %>%
    select(-previous_category, -next_category, -before_days, -after_days, -temp_category) %>%
    distinct()

  return(final_df)
}

add_delivery <- function(add_abortion_df, Matcho_outcome_limits, final_delivery_visits_df) {
  #  Add delivery record only visits

  # get minimum days between outcomes
  # minimum number of days that DELIV can follow LB and SB; LB and SB have the same days
  before_min_lb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "LB" & outcome_preg_category == "DELIV") %>%
    pull(min_days)

  before_min_ect <- Matcho_outcome_limits %>%
    filter(first_preg_category == "ECT" & outcome_preg_category == "DELIV") %>%
    pull(min_days)
  # minimum number of days that LB can follow
  # to add: if there's LB or SB outcome before then, they should be given this delivery date
  after_min_lb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "DELIV" & outcome_preg_category == "LB") %>%
    pull(min_days)
  # minimum number of days that SB can follow
  after_min_sb <- Matcho_outcome_limits %>%
    filter(first_preg_category == "DELIV" & outcome_preg_category == "SB") %>%
    pull(min_days)

  after_min_ect <- Matcho_outcome_limits %>%
    filter(first_preg_category == "DELIV" & outcome_preg_category == "ECT") %>%
    pull(min_days)

  # get difference in days with subsequent visit
  final_temp_df <- union_all(add_abortion_df, select(final_delivery_visits_df, -any_of(c("gest_value", "value_as_number")))) %>%
    mutate(temp_category = ifelse(category == "SA", "AB", category)) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(
      # get previous category if available
      previous_category = lag(temp_category),
      # get difference in days with previous episode start date
      after_days = date_diff(visit_date, lag(visit_date), sql("day")),
      next_category = lead(temp_category),
      # and next episode start date
      before_days = date_diff(lead(visit_date), visit_date, sql("day"))
    )

  # have the deliveries and all othe others
  # add this: want to move the LB or SB date earlier if there's an earlier delivery date
  add_abortion_df_rev <- final_temp_df %>%
    mutate(visit_date = if_else(
      !is.na(previous_category) &
        previous_category == "DELIV" & category %in% c("LB", "SB") &
        after_days < after_min_sb,
      lag(visit_date), visit_date
    )) %>%
    filter(category != "DELIV") %>%
    ungroup()

  final_temp_df <- final_temp_df %>%
    filter(category == "DELIV") %>%
    filter(
      # don't need to worry about timing
      (is.na(before_days) & is.na(after_days)) |
        (!previous_category %in% c("LB", "SB", "ECT", "AB") & is.na(next_category)) |
        (!next_category %in% c("LB", "SB", "ECT", "AB") & is.na(previous_category)) |
        (!previous_category %in% c("LB", "SB", "ECT", "AB") & !next_category %in% c("LB", "SB", "ECT", "AB")) |
        # timing
        (previous_category %in% c("LB", "SB") & after_days >= before_min_lb & is.na(next_category)) |
        (next_category == "LB" & before_days >= after_min_lb & is.na(previous_category)) |
        (next_category == "SB" & before_days >= after_min_sb & is.na(previous_category)) |
        (next_category == "LB" & previous_category %in% c("LB", "SB") & before_days >= after_min_lb & after_days >= before_min_lb) |
        (next_category == "SB" & previous_category %in% c("LB", "SB") & before_days >= after_min_sb & after_days >= before_min_lb) |
        (previous_category %in% c("ECT", "AB") & after_days >= before_min_ect & is.na(next_category)) |
        (next_category %in% c("ECT", "AB") & before_days >= after_min_ect & is.na(previous_category)) |
        (next_category %in% c("ECT", "AB") & previous_category %in% c("ECT", "AB") & before_days >= after_min_ect & after_days >= before_min_ect) |
        (next_category %in% c("ECT", "AB") & previous_category %in% c("LB", "SB") & before_days >= after_min_ect & after_days >= before_min_lb) |
        (next_category == "LB" & previous_category %in% c("ECT", "AB") & before_days >= after_min_lb & after_days >= before_min_ect) |
        (next_category == "SB" & previous_category %in% c("ECT", "AB") & before_days >= after_min_sb & after_days >= before_min_ect)
    ) %>%
    ungroup()

  final_df <- union_all(add_abortion_df_rev, final_temp_df) %>%
    select(-previous_category, -next_category, -before_days, -after_days, -temp_category) %>%
    distinct()

  counts <- final_df %>%
    count(category) %>%
    collect()

  cat("Total preliminary episodes:\n")
  apply(counts, 1, cat, sep = "\n")

  return(final_df)
}

calculate_start <- function(add_delivery_df, Matcho_term_durations) {
  # Estimate start of pregnancies based on outcome type.

  # join tables
  term_df <- add_delivery_df %>%
    left_join(Matcho_term_durations, by = "category") %>%
    # based only on the outcome, when did pregnancy start
    # calculate latest start start date
    mutate(
      min_start_date = visit_date - as.integer(min_term),
      # calculate earliest start date
      max_start_date = visit_date - as.integer(max_term)
    )

  return(term_df)
}

gestation_visits <- function(initial_pregnant_cohort_df) {
  # Filter to visits with gestation period.
  # Additional gestation concepts to use:
  # 3002209 - Gestational age Estimated
  # 3048230 - Gestational age in weeks
  # 3012266 - Gestational age

  # Get records with gestation period
  gest_df <- initial_pregnant_cohort_df %>% filter(!is.na(gest_value))

  # Get records with gestational age in weeks
  other_gest_df <- initial_pregnant_cohort_df %>%
    filter(
      concept_id %in% c(3002209, 3048230, 3012266),
      !is.na(value_as_number),
      # also filter out 0 -- this is an error
      value_as_number > 0, value_as_number <= 44
    ) %>%
    mutate(gest_value = as.integer(value_as_number))

  # Combine tables
  all_gest_df <- union_all(gest_df, other_gest_df)

  return(all_gest_df)
}

gestation_episodes <- function(gestation_visits_df, min_days = 70, buffer_days = 28) {
  # minimum number of days to be new distinct episode
  # number of days to use as a buffer

  # Define pregnancy episode per patient by gestational records.
  #
  # Any record with a negative change or no change in the gestational age in
  # weeks from the previous record is flagged as the start of a potential
  # episode. This record is then checked if there is at least a separation of
  # 70 days from the previous record. The number of days, 70, was determined
  # by taking the minimum outcome limit in days from Matcho et al. (56) and
  # adding a buffer of 14 days. If the record is not at least 70 days from the
  # previous record, it is no longer flagged as the start of an episode.
  #
  # For all records with a positive change in the gestational age in weeks
  # from the previous record is then checked if the date difference in days is
  # greater than the difference in days between the record's gestational age
  # in weeks and the previous record's gestational age in weeks with a buffer
  # of 28 days. The buffer of 28 days was determined by taking the minimum
  # retry period in days from Matcho et al. (14) and adding 14 days as a
  # buffer. If the date difference in days is greater than the difference in
  # days between the record's gestational age in weeks and the previous
  # record's gestational age in weeks with the buffer, then this record is
  # flagged as a start of a new episode.

  # filter out any empty visit dates
  df <- gestation_visits_df %>%
    filter(
      !is.na(visit_date),
      # remove any records with incorrect gestational weeks (i.e. 9999999)
      gest_value > 0 & gest_value <= 44
    ) %>%
    # keep max gest_value if two or more gestational records share same date
    group_by(person_id, visit_date) %>%
    mutate(gest_week = max(gest_value)) %>%
    ungroup() %>%
    # filter out rows that are not the max gest_value at visit_date
    filter(gest_value == gest_week) %>%
    # add column for gestation period in days
    mutate(gest_day = gest_week * 7) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(
      # get previous gestation week
      prev_week = lag(gest_week, 1),
      # get previous date
      prev_date = lag(visit_date, 1),
      # calculate difference between gestation weeks
      week_diff = gest_week - prev_week,
      # calculate number of days between gestation weeks with buffer
      day_diff = week_diff * 7 + buffer_days,
      # get difference in days between visit date and previous date
      date_diff = date_diff(visit_date, prev_date, sql("day")),
      # check if any negative or zero number in week_diff column corresponds to a new pregnancy episode
      # assume it does if the difference in actual dates is larger than the minimum
      # change to 1 (arbitrary positive number) if not;
      # new_diff = 1 if the next obs has lower gest week and the difference in dates
      # is smaller than the minimum number of days between pregnancies
      # week_diff is negative if at a lower gestational age now
      new_diff = if_else(date_diff < min_days & week_diff <= 0, 1, week_diff),
      # check if any positive number in week_diff column (so at a higher gestational age next time)
      # has a date_diff >= day_diff
      # that means that the difference in time is greater than the difference
      # in gestational age + buffer
      # may correspond to new pregnancy episode, if so change to -1 (negative number)
      new_diff2 = if_else(date_diff >= day_diff & week_diff > 0, -1, new_diff),
      # create new columns, index and episode; any zero or negative number in newdiff2 column indicates a new episode
      index = row_number(),
      # count as an episodes if first row or
      # difference in gest age is negative and date_diff large enough for it to be a new pregnancy or
      # difference in gest age is positive but that difference is larger than the difference in dates
      episode = as.integer(cumsum(ifelse(new_diff2 <= 0 | index == 1, 1, 0))),
      episode_chr = as.character(episode) # for grouping
    ) %>%
    ungroup()

  return(df)
}

get_min_max_gestation <- function(gestation_episodes_df) {
  # Get the min and max gestational age in weeks and the corresponding visit
  # dates per pregnancy episode.
  #
  # Also get the first and last visits and their corresponding gestational age
  # in weeks per pregnancy episode.
  #
  # For minimum gestational age in weeks, the first and last occurrence and
  # their dates were obtained.

  ############ First Visit Date ############

  # identify first visit for each pregnancy episode
  # and get max gestation week at first visit date
  new_first_df <- gestation_episodes_df %>%
    group_by(person_id, episode) %>%
    slice_min(visit_date, n = 1) %>%
    summarise(first_gest_week = max(gest_week), .groups = "drop")

  ############ Min Gestation Week ############

  # identify minimum gestation week for each pregnancy episode
  temp_min_df <- gestation_episodes_df %>%
    group_by(person_id, episode) %>%
    slice_min(gest_week, n = 1) %>%
    mutate(min_gest_week = gest_week) %>%
    ungroup() %>%
    aou_compute()

  # get range of time when that gestational week was recorded
  # get first occurrence of min gestation week
  new_min_df <- temp_min_df %>%
    group_by(person_id, episode, min_gest_week) %>%
    summarize(min_gest_date = min(visit_date), .groups = "drop")

  # get last occurrence of min gestation week
  second_min_df <- temp_min_df %>%
    group_by(person_id, episode, gest_week) %>% # = min_gest_week
    summarize(min_gest_date_2 = max(visit_date), .groups = "drop")

  ############ End Visit Date ############

  # identify end visit for each pregnancy episode
  # keep in mind this could be a month after pregnancy actually ended...
  temp_end_df <- gestation_episodes_df %>%
    group_by(person_id, episode) %>%
    slice_max(visit_date, n = 1) %>%
    mutate(end_gest_date = visit_date)

  # get max gestation week at end visit date
  new_end_df <- temp_end_df %>%
    group_by(person_id, episode, end_gest_date) %>%
    summarize(end_gest_week = max(gest_week), .groups = "drop")

  ############ Max Gestation Week ############

  # identify max gestation week for each pregnancy episode
  temp_max_df <- gestation_episodes_df %>%
    group_by(person_id, episode) %>%
    slice_max(gest_week, n = 1) %>%
    mutate(max_gest_week = gest_week) %>%
    ungroup() %>%
    aou_compute()

  # get first occurrence of max gestation week
  new_max_df <- temp_max_df %>%
    group_by(person_id, episode, max_gest_week) %>%
    summarize(max_gest_date = min(visit_date), .groups = "drop")

  # max_gest_date can be later than min_gest_date_2 (only one GA but multiple dates)

  ############ Join tables ############

  # join first and end tables
  all_df <- new_first_df %>%
    inner_join(new_end_df, by = c("person_id", "episode")) %>%
    inner_join(new_min_df, by = c("person_id", "episode")) %>%
    inner_join(second_min_df, by = c("person_id", "episode")) %>%
    inner_join(new_max_df, by = c("person_id", "episode"))

  return(all_df)
}

### START HERE
add_gestation <- function(calculate_start_df, get_min_max_gestation_df, buffer_days = 28) {
  # Add gestation-based episodes. Any gestation-based episode that overlaps with an outcome-based
  # episode is removed as a distinct episode.
  # add unique id for each outcome visit
  calculate_start_df <- calculate_start_df %>%
    # visit date is the first outcome date for the hierarchically chosen outcome
    mutate(visit_id = sql("concat(person_id, visit_date)"))

  # add unique id for each gestation visit
  get_min_max_gestation_df <- get_min_max_gestation_df %>%
    # max gest date is the first occurrence of the maximum gestational week
    mutate(
      gest_id = sql("concat(person_id, max_gest_date)"),
      # add column for gestation period in days for largest gestation week on record
      max_gest_day = (max_gest_week * 7),
      # add column for gestation period in days for smallest gestation week on record
      min_gest_day = (min_gest_week * 7),
      # get date of estimated start date based on max gestation week on record
      # max_gest_date is the first occurrence of the maximum gestational week
      max_gest_start_date = max_gest_date - as.integer(max_gest_day),
      # get date of estimated start date based on min gestation week on record
      # min_gest_date is the first occurence of the min gestational week
      min_gest_start_date = min_gest_date - as.integer(min_gest_day),
      # which one is earlier
      max_gest_start_date_further = if_else(
        max_gest_start_date > min_gest_start_date,
        min_gest_start_date, max_gest_start_date
      ),
      # and which one is later
      min_gest_start_date = if_else(
        max_gest_start_date > min_gest_start_date,
        max_gest_start_date, min_gest_start_date
      ),
      # so max_gest_start_date will always be earlier
      max_gest_start_date = max_gest_start_date_further,
      # get difference in days between estimated start dates
      gest_start_date_diff = date_diff(max_gest_start_date, min_gest_start_date, sql("day"))
    )


  # join both tables to find overlaps
  # 18679
  both_df <- inner_join(calculate_start_df, get_min_max_gestation_df,
    by = join_by(person_id, overlaps(
      max_start_date, visit_date,
      max_gest_start_date, max_gest_date
    ))
  ) %>%
    # Check for any gestation-based episodes that overlap with more than one outcome-based
    # episode and keep only those episodes where the gestation-based end date is closest to the
    # outcome date.
    # mutate(days_diff = as.numeric(difftime(visit_date, max_gest_date))) %>%
    mutate(
      # add -- these are changed anyway so if there are multiple similar overlaps, choose
      # the one with the better term duration
      # visit date should be the first visit date at which there's an outcome
      gest_at_outcome = date_diff(visit_date, max_gest_start_date, sql("day")),
      # we want it to be under the max
      is_under_max = ifelse(gest_at_outcome <= max_term, 1, 0),
      # and over the min, ie both = 1
      is_over_min = ifelse(gest_at_outcome >= min_term, 1, 0),
      days_diff = date_diff(visit_date, max_gest_date, sql("day")),
      days_diff = if_else(is_over_min == 1 | is_under_max == 1 | days_diff < -buffer_days, 10000, days_diff)
    ) %>%
    group_by(visit_id) %>%
    slice_min(order_by = abs(days_diff), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(gest_id) %>%
    slice_min(order_by = abs(days_diff), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    aou_compute()

  # only outcome-based episodes
  just_outcome_df <- calculate_start_df %>%
    anti_join(select(both_df, visit_id), by = "visit_id")

  # only gestation-based episodes
  just_gestation_df <- get_min_max_gestation_df %>%
    anti_join(select(both_df, gest_id), by = "gest_id") %>%
    mutate(
      category = "PREG",
      # visit date becomes
      visit_date = max_gest_date
    )

  all_df <- reduce(
    list(
      both_df,
      just_outcome_df,
      just_gestation_df
    ),
    union_all
  ) %>%
    select(-all_of("episode")) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(episode = row_number()) %>%
    ungroup() %>%
    # recalculate since I overwrote
    mutate(days_diff = date_diff(visit_date, max_gest_date, sql("day"))) %>%
    aou_compute()


  counts <- count(all_df,
    gestation_based = !is.na(gest_id),
    outcome_based = !is.na(visit_id)
  ) %>%
    collect()

  cat("Total number of outcome-based episodes:",
    tally(calculate_start_df) %>% pull(n) %>% as.integer(),
    "Total number of gestation-based episodes:",
    tally(get_min_max_gestation_df) %>% pull(n) %>% as.integer(),
    "Total number of only outcome-based episodes after merging:",
    counts %>% filter(!gestation_based, outcome_based) %>% pull(n) %>% as.integer(),
    "Total number of only gestation-based episodes after merging:",
    counts %>% filter(gestation_based, !outcome_based) %>% pull(n) %>% as.integer(),
    "Total number of episodes with both after merging:",
    counts %>% filter(gestation_based, outcome_based) %>% pull(n) %>% as.integer(),
    sep = "\n"
  )
  return(all_df)
}

clean_episodes <- function(add_gestation_df, buffer_days = 28) {
  # Clean up episodes by removing duplicate episodes and reclassifying outcome-based episodes
  # as gestation-based episodes if the outcome containing gestational info does not fall within
  # the term durations defined by Matcho et al.
  final_df <- add_gestation_df

  # remove any outcomes where the gestational age based on max_gest_date is over the max term duration defined by Matcho et al.
  over_max_df <- final_df %>%
    # it has both an outcome and a gestation but is over the max
    filter(!is.na(gest_id) & !is.na(visit_id) & is_under_max == 0) %>%
    mutate(
      removed_category = category,
      category = "PREG",
      visit_date = max_gest_date,
      removed_outcome = 1
    )

  # filter out these episodes from main table
  final_df <- final_df %>%
    filter(!(!is.na(gest_id) & !is.na(visit_id) & is_under_max == 0)) %>%
    mutate(
      removed_outcome = 0
    )

  cat("Total number of episodes over maximum term duration:",
    tally(over_max_df) %>% pull(n) %>% as.integer(),
    sep = "\n"
  )

  # join episodes with new values back to main table
  final_df <- union_all(final_df, over_max_df)

  ###### remove any outcomes where the gestational age based on max_gest_date is under the min term duration defined by Matcho et al. ######

  # filter to episodes with max_gest_date is under the min term duration and where the number of days between the visit_date and
  # max_gest_date is negative with buffer
  under_min_df <- final_df %>%
    filter(!is.na(gest_id) & !is.na(visit_id) & is_over_min == 0 & days_diff < -buffer_days) %>%
    mutate(
      removed_category = category,
      category = "PREG",
      visit_date = max_gest_date,
      removed_outcome = 1
    ) %>%
    aou_compute()

  # filter out these episodes from main table
  final_df <- final_df %>%
    filter(!(!is.na(gest_id) & !is.na(visit_id) & is_over_min == 0 & days_diff < -buffer_days))

  cat("Total number of episodes under minimum term duration:",
    tally(under_min_df) %>% pull(n) %>% as.integer(),
    sep = "\n"
  )

  # join episodes with new values to main table
  final_df <- union_all(final_df, under_min_df)

  ###### remove any outcomes where the difference between max_gest_date in days is negative ######
  # filter to episodes with max_gest_date is after the outcome visit_date with buffer
  neg_days_df <- final_df %>%
    filter(!is.na(gest_id) & !is.na(visit_id) & days_diff < -buffer_days) %>%
    mutate(
      removed_category = category,
      category = "PREG",
      visit_date = max_gest_date,
      removed_outcome = 1
    )

  cat("Total number of episodes with negative number of days between outcome and max_gest_date:",
    tally(neg_days_df) %>% pull(n) %>% as.integer(),
    sep = "\n"
  )

  # filter out these episodes from main table
  final_df <- final_df %>%
    filter(!(!is.na(gest_id) & !is.na(visit_id) & days_diff < -buffer_days)) %>%
    # join episodes with new values to main table
    union_all(neg_days_df) %>%
    ###### add columns for quality check ######
    # get new gestational age at visit date
    mutate(
      gest_at_outcome = date_diff(visit_date, max_gest_start_date, sql("day")),
      min_gest_date_diff = date_diff(min_gest_date_2, min_gest_date, sql("day")),
      date_diff_max_end = date_diff(max_gest_date, end_gest_date, sql("day"))
    ) %>%
    # redo column for episode
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(episode = row_number()) %>%
    ungroup()

  return(final_df)
}

remove_overlaps <- function(clean_episodes_df) {
  # Identify episodes that overlap and keep only the latter episode if the previous episode is PREG.
  # If the latter episode doesn't have gestational info, redefine the start date to be the
  # previous episode end date plus the retry period.
  df <- clean_episodes_df

  df <- df %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    # get previous date
    mutate(
      prev_date = lag(visit_date),
      # get previous category
      prev_category = lag(category),
      # get previous retry period
      prev_retry = lag(retry),
      # get previous gest_id
      prev_gest_id = lag(gest_id),
      # get difference in days between start date and previous visit date
      # us gestation-based date if available
      prev_date_diff = ifelse(!is.na(max_gest_start_date),
        date_diff(max_gest_start_date, prev_date, sql("day")),
        date_diff(max_start_date, prev_date, sql("day"))
      ),
      # if the difference in days is negative, indicate overlap of episodes
      has_overlap = ifelse(prev_date_diff < 0, 1, 0)
    ) %>%
    ungroup()

  # overlapped episodes
  overlap_df <- df %>%
    filter(has_overlap == 1 & prev_category == "PREG")

  # get list of gest_ids to remove
  gest_id_list <- overlap_df %>%
    distinct(prev_gest_id) %>%
    pull()

  # remove episodes that overlap
  final_df <- df %>%
    filter(!(gest_id %in% gest_id_list & category == "PREG")) %>%
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    # recalculate
    # get previous date
    mutate(
      prev_date = lag(visit_date),
      # get previous category
      prev_category = lag(category),
      # get previous retry period
      prev_retry = lag(retry),
      # get previous gest_id
      prev_gest_id = lag(gest_id),
      # get difference in days between start date and previous visit date
      # us gestation-based date if available
      prev_date_diff = ifelse(!is.na(max_gest_start_date),
        date_diff(max_gest_start_date, prev_date, sql("day")),
        date_diff(max_start_date, prev_date, sql("day"))
      ),
      # if the difference in days is negative, indicate overlap of episodes
      has_overlap = ifelse(prev_date_diff < 0, 1, 0),
      # get estimated start date
      estimated_start_date = case_when(
        # if there's an overlap and a retry period from the earlier episodes
        # and the last episode was not preg (or else would be in gest_id_list)
        # start date = last visit date + retry period
        has_overlap == 1 & !is.na(prev_retry) ~ prev_date + as.integer(prev_retry),
        is.na(max_gest_start_date) ~ max_start_date,
        TRUE ~ max_gest_start_date
      ),
      # get estimated gestational age in days at outcome_visit_date using estimated_start_date
      gest_at_outcome = date_diff(visit_date, estimated_start_date, sql("day")),
      # add column to check if gest_at_outcome is less than or equal to max_term, 1 indicates yes
      is_under_max = ifelse(gest_at_outcome <= max_term, 1, 0),
      # add column to check if gest_at_outcome is greater than or equal to min_term, 1 indicates yes
      is_over_min = ifelse(gest_at_outcome >= min_term, 1, 0)
    ) %>%
    # redo column for episode
    group_by(person_id) %>%
    dbplyr::window_order(visit_date) %>%
    mutate(
      episode = row_number(),
      # check that there are no more overlapping episodes
      prev_date = lag(visit_date),
      preg_gest_id = lag(gest_id)
    ) %>%
    ungroup() %>%
    mutate(
      prev_date_diff = ifelse(!is.na(estimated_start_date),
        date_diff(estimated_start_date, prev_date, sql("day")),
        date_diff(estimated_start_date, prev_date, sql("day"))
      ),
      # checked, there are no remaining
      has_overlap = ifelse(prev_date_diff < 0, 1, 0)
    )

  # still_overlaps <- final_df %>%
  #     filter(has_overlap == 1)

  # if there are any remaining episodes with gestational age in weeks at
  # outcome date not within the term durations, reclassify as PREG
  temp_df <- final_df %>%
    filter(!is.na(max_gest_week) & !is.na(concept_name) & is_over_min == 0) %>%
    mutate(
      removed_category = category,
      category = "PREG",
      visit_date = max_gest_date,
      removed_outcome = 1
    )

  final_df <- final_df %>%
    filter(!(!is.na(max_gest_week) & !is.na(concept_name) & is_over_min == 0)) %>%
    union_all(temp_df) %>%
    aou_compute()

  cat("Total number of episodes with removed outcome:",
    final_df %>% filter(removed_outcome == 1) %>% tally() %>% pull(n) %>% as.integer(),
    sep = "\n"
  )
  return(final_df)
}

final_episodes <- function(remove_overlaps_df) {
  # Keep subset of columns with episode start and end as well as category.
  # select columns and drop duplicates
  remove_overlaps_df %>%
    distinct(person_id, category, visit_date, estimated_start_date, episode)
}

final_episodes_with_length <- function(final_episodes_df, gestation_visits_df) {
  # Find the first gestation record within an episode and calculate the episode
  # length based on the date of the first gestation record and the visit date.

  df <- final_episodes_df

  # select columns and rename column
  gest_df <- gestation_visits_df %>%
    select(person_id, gest_value, visit_date) %>%
    rename(gest_date = visit_date) %>%
    aou_compute()

  merged <- gest_df %>%
    right_join(df,
      by = join_by(
        person_id,
        between(gest_date, estimated_start_date, visit_date)
      )
    ) %>%
    group_by(person_id, episode) %>%
    slice_min(gest_date, n = 1) %>%
    # keep max gest_value if two or more gestation records share same date
    ungroup() %>%
    aou_compute() %>%
    group_by(person_id, episode) %>%
    slice_max(gest_value, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    distinct() %>%
    # flag episodes with gestational info
    mutate(gest_flag = ifelse(is.na(gest_date), NA, "yes"))

  # get episode length if there is a gestation record date, otherwise impute 1
  final_df <- merged %>%
    mutate(episode_length = if_else(!is.na(gest_date),
      date_diff(visit_date, gest_date, sql("day")), 1
    ))

  # if an episode length is 0, change to 1
  final_df <- final_df %>%
    mutate(episode_length = if_else(episode_length == 0, 1, episode_length)) %>%
    select(-gest_value) %>%
    distinct()

  return(final_df)
}

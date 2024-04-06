outcomes_per_episode <- function(PPS_episodes_df, get_PPS_episodes_df, initial_pregnant_cohort_df) {
  # Get outcomes for Algorithm 2:
  # Outcomes are collected from a 'lookback to lookahead window', which is the
  # episode max date minus 14d to the earliest out of i) the next closest
  # episode start date or ii) a number of months of length (10 months - the
  # earliest concept month that could relate to the end of the episode)

  # To assign a measure of support for outcomes identified in HIP episodes, we
  # inferred pregnancy outcomes in the PPS by checking for any outcome within
  # a window of 14 days before the episode end to up to the earliest date from
  # either (1) the next episode start date, or (2) up to 10 months minus the
  # last record’s expected minimum month after the start of the episode. We
  # then selected the outcome based on Matcho et al’s outcome hierarchy
  # assessment

  pregnant_dates <- PPS_episodes_df %>%
    group_by(person_id) %>%
    arrange(person_episode_number, episode_min_date) %>%
    # not gestational week 1, just when the first concept appears
    mutate(
      next_closest_episode_date = lead(episode_min_date) - days(1),
      previous_episode_date = lag(episode_max_date) + days(1)
    ) %>%
    ungroup()

  # get the max number of months to look ahead from the episode itself, in a new column called 'max_pregnancy_date'
  # do this by saving the concept date relating to the last episode concept
  # (if multiple on the same date then the one containing max month),
  # of which the min month is used out of the tuple of month values where necessary and subtracted from 10,
  # this then is added onto the concept date to get 'max_pregnancy_date'
  tmp_preg_episode_concept_GA <- get_PPS_episodes_df %>%
    group_by(person_id, person_episode_number) %>%
    arrange(desc(domain_concept_start_date), desc(max_month), desc(min_month)) %>%
    # choose the last concept date and the greatest possible gestational age at that time
    slice(1) %>%
    ungroup() %>%
    mutate(
      # the next pregnancy could occur (10 months + buffer)
      months_to_add = 11L - as.integer(min_month),
      # last time we would expect an outcome
      max_pregnancy_date = domain_concept_start_date %m+% months(months_to_add)
    ) %>%
    select(person_id, person_episode_number, max_pregnancy_date)

  pregnant_dates <- left_join(pregnant_dates, tmp_preg_episode_concept_GA,
    by = c("person_id", "person_episode_number")
  ) %>%
    mutate(
      # if there's no next episode impute a distant time
      next_closest_episode_date = if_else(is.na(next_closest_episode_date),
        ymd("2999-01-01"),
        next_closest_episode_date
      ),
      # choose the earliest out of the next episode and the last time we'd expect an outcome
      episode_max_date_plus_lookahead_window = pmin(next_closest_episode_date, max_pregnancy_date, na.rm = TRUE),
      # two weeks before we saw the last concept
      episode_max_date_minus_lookback_window = episode_max_date - days(14)
    )

  # begin searching for outcomes within the relevant lookback and lookahead dates
  preg_related_concepts <- initial_pregnant_cohort_df %>%
    filter(category %in% c("LB", "SB", "DELIV", "ECT", "AB", "SA")) %>%
    collect() %>%
    inner_join(pregnant_dates,
      by = join_by(person_id, between(
        # the max lookahead window stops at the next episode... but only if it's in PPS
        # if there were multiple
        # outcomes and the first date wasn't chosen, there will be problems...
        visit_date, episode_max_date_minus_lookback_window,
        episode_max_date_plus_lookahead_window
      )),
      relationship = "many-to-many"
    )

  preg_related_concepts_lst <- preg_related_concepts %>%
    mutate(lst = paste(visit_date, ",", concept_id, ",", category)) %>%
    group_by(
      person_id, person_episode_number, episode_min_date, episode_max_date,
      episode_max_date_minus_lookback_window, episode_max_date_plus_lookahead_window,
      n_GT_concepts
    ) %>%
    summarise(outcomes_list = list(unique(lst)), .groups = "drop")

  df1 <- preg_related_concepts_lst %>%
    mutate(outcomes_list = discard(outcomes_list, is_empty)) %>%
    mutate(outcomes_list = map(outcomes_list, sort))

  get_outcome_date <- function(x, outcome) {
    if (length(grep(outcome, x)) > 0) {
      strsplit(x[grep(outcome, x)], ",")[[1]][1]
    } else {
      NA
    }
  }

  df1_outcomes <- df1 %>%
    mutate(
      LB_delivery_date = map_chr(outcomes_list, get_outcome_date, "LB"),
      SB_delivery_date = map_chr(outcomes_list, get_outcome_date, "SB"),
      ECT_delivery_date = map_chr(outcomes_list, get_outcome_date, "ECT"),
      SA_delivery_date = map_chr(outcomes_list, get_outcome_date, "SA"),
      AB_delivery_date = map_chr(outcomes_list, get_outcome_date, "AB"),
      DELIV_delivery_date = map_chr(outcomes_list, get_outcome_date, "DELIV"),
      algo2_category = case_when(
        !is.na(LB_delivery_date) ~ "LB",
        !is.na(SB_delivery_date) ~ "SB",
        !is.na(ECT_delivery_date) ~ "ECT",
        !is.na(SA_delivery_date) ~ "SA",
        !is.na(AB_delivery_date) ~ "AB",
        !is.na(DELIV_delivery_date) ~ "DELIV"
      ),
      algo2_outcome_date = case_when(
        !is.na(LB_delivery_date) ~ LB_delivery_date,
        !is.na(SB_delivery_date) ~ SB_delivery_date,
        !is.na(ECT_delivery_date) ~ ECT_delivery_date,
        !is.na(SA_delivery_date) ~ SA_delivery_date,
        !is.na(AB_delivery_date) ~ AB_delivery_date,
        !is.na(DELIV_delivery_date) ~ DELIV_delivery_date
      ),
      algo2_outcome_date = ymd(algo2_outcome_date)
    )

  return(df1_outcomes)
}

add_outcomes <- function(outcomes_per_episode_df, PPS_episodes_df) {
  out_df <- outcomes_per_episode_df %>%
    select(
      person_id, person_episode_number, episode_min_date, algo2_category,
      algo2_outcome_date, n_GT_concepts
    )

  df <- PPS_episodes_df %>%
    left_join(out_df, by = c("person_id", "person_episode_number", "episode_min_date", "n_GT_concepts"))

  return(df)
}

final_merged_episodes <- function(HIP_episodes_local_df, PPS_episodes_with_outcomes_df) {
  # Merge episodes by checking for any overlap of episodes between the two algorithms.
  #
  # algo1 = HIP episodes
  # algo2 = PPS episodes

  # The following is for checking overlap:
  # - complete overlap
  # - algo1 contains algo2
  # - algo2 contains algo1
  # - start in algo1 is within algo2
  # - start in algo1 is within algo2
  # - start in algo2 is within algo1
  # - end in algo1 is within algo2
  # - end in algo2 is within algo1

  algo1_pregnancy <- HIP_episodes_local_df %>%
    rename(
      pregnancy_start = estimated_start_date,
      pregnancy_end = visit_date,
      first_gest_date = gest_date
    ) %>%
    mutate(algo1_id = paste(person_id, episode, "1", sep = "_"))

  algo2 <- PPS_episodes_with_outcomes_df %>%
    mutate(algo2_id = paste(person_id, person_episode_number, "2", sep = "_"))

  all_episodes <- algo1_pregnancy %>%
    full_join(algo2, by = join_by(
      person_id,
      overlaps(
        pregnancy_start, pregnancy_end,
        episode_min_date, episode_max_date_plus_two_months
      )
    )) %>%
    mutate(
      merged_episode_start = pmin(first_gest_date, episode_min_date, pregnancy_end),
      merged_episode_end = pmax(episode_max_date, pregnancy_end),
      merged_episode_length = as.numeric(difftime(merged_episode_end, merged_episode_start, units = "days")) / 30.25
    )

  # check for duplicated algorithm 1 episodes
  # these are HIPPS episodes that overlap multiple PPS episodes
  all_episodes <- all_episodes %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  # Check for duplicated algorithm 2 episodes
  all_episodes <- all_episodes %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  # check overla

  cat("Total initial number of episodes for HIP:",
    algo1_pregnancy %>%
      distinct(person_id, episode) %>%
      tally() %>%
      pull(n),
    "Total initial number of episodes for PPS:",
    algo2 %>%
      distinct(person_id, person_episode_number) %>%
      tally() %>%
      pull(n),
    "Count of HIP episodes that overlap multiple PPS episodes:",
    all_episodes %>%
      filter(algo1_dup != 0) %>%
      distinct(algo1_id) %>%
      tally() %>%
      pull(n),
    "Count of PPS episodes that overlap multiple HIP episodes:",
    all_episodes %>%
      filter(algo2_dup != 0) %>%
      distinct(algo2_id) %>%
      tally() %>%
      pull(n),
    "Total number of HIP episodes after merging:",
    all_episodes %>%
      distinct(algo1_id) %>%
      tally() %>%
      pull(n) - 1, # don't count NA
    "Total number of PPS episodes after merging:",
    all_episodes %>%
      distinct(algo2_id) %>%
      tally() %>%
      pull(n) - 1, # don't count NA
    sep = "\n"
  )

  return(all_episodes)
}

final_merged_episodes_no_duplicates <- function(final_merged_episodes_df) {
  # Remove any episodes that overlap with more than one episode.

  # 1. Keep algorithm 1 episodes with an end date closest to algorithm 2's end
  # date. Starting with duplicated algorithm 1 episodes, find the date
  # difference in days between each algorithm's end date. Find the minimum
  # date difference in days. If an algorithm 1 episode date difference in days
  # does not equal the minimum date difference in days, flag that episode for
  # removal by converting algorithm 1 episode info to null.

  # 2. Any remaining duplicated algorithm 1 episodes may have more than one
  # algorithm 2 episodes with the same date difference in days. Calculate the
  # length of algorithm 2 episodes and keep only the longest algorithm 2
  # episode. For any algorithm 2 episode that doesn't meet this criteria, both
  # the algorithm 1 and 2 episode info are converted to null.

  # 3. Next repeat the same process described in Step 1 for duplicated
  # algorithm 2 episodes.

  no_dup_df <- final_merged_episodes_df %>%
    filter((algo1_dup == 0 & algo2_dup == 0) | (algo1_dup == 0 & is.na(algo2_dup)) | (is.na(algo1_dup) & algo2_dup == 0))

  best_algo1A <- final_merged_episodes_df %>%
    filter(algo1_dup == 1 & !is.na(algo2_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(algo2_category), 10000, date_diff),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(algo2_category) | new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo1_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1) %>%
    ungroup()

  best_algo2A <- final_merged_episodes_df %>%
    filter(algo2_dup == 1 & !is.na(algo1_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo2_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1) %>%
    ungroup()

  best_bothA <- bind_rows(
    best_algo1A,
    best_algo2A
  ) %>%
    select(-contains("date_diff"), -contains("dup")) %>%
    distinct() %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup() %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  keepA <- best_bothA %>%
    filter(!(algo1_dup == 1 & !is.na(algo2_id)) & !(algo2_dup == 1 & !is.na(algo1_id)))

  best_algo1B <- best_bothA %>%
    filter(algo1_dup == 1 & !is.na(algo2_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(algo2_category), 10000, date_diff),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(algo2_category) | new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo1_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_algo2B <- best_bothA %>%
    filter(algo2_dup == 1 & !is.na(algo1_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo2_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_bothB <- bind_rows(
    best_algo1B,
    best_algo2B
  ) %>%
    select(-contains("date_diff"), -contains("dup")) %>%
    distinct() %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup() %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  keepB <- best_bothB %>%
    filter(!(algo1_dup == 1 & !is.na(algo2_id)) & !(algo2_dup == 1 & !is.na(algo1_id)))

  # check to make sure this was sufficient
  # nrow(best_bothB) == nrow(keepB)

  best_algo1C <- best_bothB %>%
    filter(algo1_dup == 1 & !is.na(algo2_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(algo2_category), 10000, date_diff),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(algo2_category) | new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo1_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_algo2C <- best_bothB %>%
    filter(algo2_dup == 1 & !is.na(algo1_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo2_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_bothC <- bind_rows(
    best_algo1C,
    best_algo2C
  ) %>%
    select(-contains("date_diff"), -contains("dup")) %>%
    distinct() %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup() %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  keepC <- best_bothC %>%
    filter(!(algo1_dup == 1 & !is.na(algo2_id)) & !(algo2_dup == 1 & !is.na(algo1_id)))

  # check to make sure this was sufficient
  # nrow(best_bothC) == nrow(keepC)

  best_algo1D <- best_bothC %>%
    filter(algo1_dup == 1 & !is.na(algo2_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(algo2_category), 10000, date_diff),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(algo2_category) | new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo1_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_algo2D <- best_bothC %>%
    filter(algo2_dup == 1 & !is.na(algo1_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo2_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_bothD <- bind_rows(
    best_algo1D,
    best_algo2D
  ) %>%
    select(-contains("date_diff"), -contains("dup")) %>%
    distinct() %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup() %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  keepD <- best_bothD %>%
    filter(!(algo1_dup == 1 & !is.na(algo2_id)) & !(algo2_dup == 1 & !is.na(algo1_id)))

  # check to make sure this was sufficient
  # nrow(best_bothD) == nrow(keepD)

  best_algo1E <- best_bothD %>%
    filter(algo1_dup == 1 & !is.na(algo2_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      # deprioritize algo2 without outcomces
      date_diff = ifelse(is.na(algo2_category), 10000, date_diff),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(is.na(algo2_category) | new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo1_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_algo2E <- best_bothD %>%
    filter(algo2_dup == 1 & !is.na(algo1_id)) %>%
    mutate(
      date_diff = abs(as.numeric(difftime(pregnancy_end, episode_max_date, units = "days"))),
      new_date_diff = abs(as.numeric(difftime(episode_max_date, episode_min_date, units = "days"))),
      new_date_diff = ifelse(new_date_diff > 310, -1, date_diff)
    ) %>%
    group_by(algo2_id) %>%
    slice_min(date_diff, n = 1, with_ties = TRUE) %>%
    slice_max(new_date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_bothE <- bind_rows(
    best_algo1E,
    best_algo2E
  ) %>%
    select(-contains("date_diff"), -contains("dup")) %>%
    distinct() %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup() %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()

  keepE <- best_bothE %>%
    filter(!(algo1_dup == 1 & !is.na(algo2_id)) & !(algo2_dup == 1 & !is.na(algo1_id)))

  # check to make sure this was sufficient
  # nrow(best_bothE) == nrow(keepE)

  all_rows <- bind_rows(no_dup_df, keepA, keepB, keepC, keepD, keepE) %>%
    distinct() %>%
    group_by(algo1_id) %>%
    mutate(
      algo1_dup = if_else(is.na(algo1_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup() %>%
    group_by(algo2_id) %>%
    mutate(
      algo2_dup = if_else(is.na(algo2_id)[1], NA, as.integer(n() > 1))
    ) %>%
    ungroup()


  # check
  unduped_counts <- count(all_rows,
    no_algo1 = is.na(algo1_id), algo1_dup,
    no_algo2 = is.na(algo2_id), algo2_dup
  )

  dup_df <- final_merged_episodes_df %>%
    filter((algo1_dup == 1 & !is.na(algo2_id)) | (algo2_dup == 1 & !is.na(algo1_id)))

  cat("Count of duplicated algorithm 1 episodes",
    dup_df %>%
      filter(algo1_dup != 0) %>%
      distinct(algo1_id) %>%
      tally() %>%
      pull(n),
    "Count of duplicated algorithm 2 episodes",
    dup_df %>%
      filter(algo2_dup != 0) %>%
      distinct(algo2_id) %>%
      tally() %>%
      pull(n),
    sep = "\n",
    "count of unduplicated episodes with both",
    unduped_counts %>%
      filter(!no_algo1, !no_algo2, algo1_dup == 0, algo2_dup == 0) %>% pull(n),
    "count of unduplicated algorithm 1 episodes",
    unduped_counts %>%
      filter(!no_algo1, no_algo2, algo1_dup == 0) %>% pull(n),
    "count of unduplicated algorithm 2 episodes",
    unduped_counts %>%
      filter(no_algo1, !no_algo2, algo2_dup == 0) %>% pull(n),
    "total unduplicated episodes",
    nrow(all_rows)
  )

  # recalculate merged dates, episode number, and episode length
  final_df <- all_rows %>%
    select(any_of(c(
      "algo1_id", "algo2_id", "person_id", "pregnancy_end", "pregnancy_start",
      "first_gest_date", "category", "episode_min_date", "episode_max_date",
      "algo1_dup", "algo2_dup", "algo2_category", "algo2_outcome_date"
    ))) %>%
    mutate(
      merged_episode_start = pmin(first_gest_date, episode_min_date, pregnancy_end, na.rm = TRUE),
      merged_episode_end = pmax(episode_max_date, pregnancy_end, na.rm = TRUE)
    ) %>%
    group_by(person_id) %>%
    arrange(merged_episode_start) %>%
    mutate(
      episode_num = row_number(),
      merged_episode_length = as.numeric(difftime(merged_episode_end, merged_episode_start, units = "days")) / 30.25
    ) %>%
    ungroup()

  return(final_df)
}

final_merged_episode_detailed <- function(final_merged_episodes_no_duplicates_df) {
  # Add demographic details for each patient.

  # ADD: assign PPS episodes without outcomes to PREG
  df <- final_merged_episodes_no_duplicates_df %>%
    mutate(
      algo2_category = if_else(
        !is.na(algo2_id) & is.na(algo2_category), "PREG", algo2_category
      ),
      algo2_outcome_date = if_else(
        !is.na(algo2_id) & is.na(algo2_outcome_date),
        episode_max_date, algo2_outcome_date
      )
    )

  df <- df %>%
    rename(
      HIP_end_date = pregnancy_end,
      HIP_outcome_category = category,
      PPS_outcome_category = algo2_category,
      PPS_end_date = algo2_outcome_date,
      recorded_episode_start = merged_episode_start,
      recorded_episode_end = merged_episode_end,
      recorded_episode_length = merged_episode_length
    )

  # add columns marking if episode was identified either algorithm
  df <- df %>%
    mutate(
      HIP_flag = if_else(!is.na(algo1_id), 1, 0),
      PPS_flag = if_else(!is.na(algo2_id), 1, 0),
      # add PPS outcome category for those without them
      PPS_outcome_category = if_else(PPS_flag == 1 & is.na(PPS_outcome_category), "PREG",
        PPS_outcome_category
      )
    )

  final_df <- df %>%
    group_by(person_id) %>%
    arrange(recorded_episode_start) %>%
    mutate(episode_number = row_number()) %>%
    ungroup()

  return(final_df)
}

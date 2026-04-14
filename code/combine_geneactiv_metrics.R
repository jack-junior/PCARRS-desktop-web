# ============================================================
# COMBINE GENEACTIV METRICS (7-DAY)
# ------------------------------------------------------------
# Combine:
#    - Activity metrics (steps + activity) → FIRST 7 DAYS
#    - Software sleep metrics → FIRST 7 DAYS (recomputed averages)
#
# Output:
# summaries/geneactiv_combined_metrics.csv
# ============================================================

library(tidyverse)
library(yaml)
library(janitor)

# ============================================================
# FILE PATHS (Updated for Config)
# ============================================================

cfg <- yaml::read_yaml("config.yml")

activity_file       <- file.path(cfg$paths$summaries, "activity_metrics.csv")
software_sleep_file <- file.path(cfg$paths$summaries, "software_sleep_metrics.csv")

output_file <- file.path(cfg$paths$summaries, "geneactiv_combined_metrics.csv")

# ============================================================
# HELPER FUNCTIONS
# ============================================================

minutes_to_hhmm <- function(mins){
  ifelse(
    is.na(mins),
    NA_character_,
    sprintf("%02d:%02d",
            as.integer(mins %/% 60),
            as.integer(round(mins %% 60)))
  )
}

hhmm_to_minutes <- function(x){
  
  h <- suppressWarnings(as.numeric(substr(x,1,2)))
  m <- suppressWarnings(as.numeric(substr(x,4,5)))
  
  result <- h*60 + m
  
  # invalid format protection
  result[nchar(x) < 4] <- NA_real_
  
  result
}

# ============================================================
# LOAD DATA
# ============================================================

message("Loading datasets...")

activity <- read_csv(activity_file, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(calendar_date = as.Date(calendar_date)) %>%
  arrange(subject, calendar_date)

# sleep <- read_csv(software_sleep_file, show_col_types = FALSE) %>%
#   clean_names() %>%
#   mutate(
#     across(
#       c(starts_with("software_sleep_day"),
#         software_average_sleep_duration_hhmm),
#       ~ substr(as.character(.),1,5)
#     )
#   )

sleep <- read_csv(software_sleep_file, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    across(
      any_of(c(starts_with("software_sleep_day"), "software_average_sleep_duration_hhmm")),
      ~ if_else(is.na(.), NA_character_, substr(as.character(.), 1, 5))
    )
  )

# ============================================================
# ACTIVITY — FIRST 7 DAYS
# ============================================================

message("Processing activity (first 7 days)...")

activity_7 <- activity %>%
  group_by(subject) %>%
  slice_head(n = 7) %>%
  mutate(day_index = row_number()) %>%
  ungroup()

# ----------------------------
# Step days (REQUIRED FOR REPORT)
# ----------------------------

step_days <- activity_7 %>%
  mutate(day_col = paste0("stepday", day_index)) %>%
  select(subject, day_col, total_steps) %>%
  pivot_wider(
    names_from = day_col,
    values_from = total_steps
  )

# ----------------------------
# Activity summary (7 days)
# ----------------------------

activity_summary <- activity_7 %>%
  group_by(subject) %>%
  summarise(
    
    n_days = n(),
    
    avg_daily_steps_7days = ifelse(
      n_days == 7,
      round(mean(total_steps, na.rm = TRUE), 0),
      NA_real_
    ),
    
    inactive_duration = ifelse(
      n_days == 7,
      round(mean(inactive_duration, na.rm = TRUE), 0),
      NA_real_
    ),
    
    active_duration = ifelse(
      n_days == 7,
      round(mean(active_duration, na.rm = TRUE), 0),
      NA_real_
    ),
    
    step_start_date = min(calendar_date, na.rm = TRUE),
    step_end_date   = max(calendar_date, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    valid_7days_activity = n_days == 7,
    
    inactive_duration_hhmm = minutes_to_hhmm(inactive_duration),
    active_duration_hhmm   = minutes_to_hhmm(active_duration)
  )%>%
  select( -any_of("n_days"))

# ============================================================
# SLEEP — FIRST 7 DAYS ONLY + RECOMPUTE AVERAGES
# ============================================================

message("Processing sleep (first 7 days)...")

sleep_processed <- sleep %>%
  
  # Convert HH:MM → minutes
  mutate(
    across(
      starts_with("software_sleep_day"),
      ~ hhmm_to_minutes(.),
      .names = "{.col}_min"
    )
  ) %>%
  
  rowwise() %>%
  mutate(
    
    n_sleep_days = sum(!is.na(c(
      software_sleep_day1_hhmm_min,
      software_sleep_day2_hhmm_min,
      software_sleep_day3_hhmm_min,
      software_sleep_day4_hhmm_min,
      software_sleep_day5_hhmm_min,
      software_sleep_day6_hhmm_min,
      software_sleep_day7_hhmm_min
    ))),
    
    software_average_sleep_duration_7days_min =
      ifelse(
        n_sleep_days == 7,
        mean(c(
          software_sleep_day1_hhmm_min,
          software_sleep_day2_hhmm_min,
          software_sleep_day3_hhmm_min,
          software_sleep_day4_hhmm_min,
          software_sleep_day5_hhmm_min,
          software_sleep_day6_hhmm_min,
          software_sleep_day7_hhmm_min
        ), na.rm = TRUE),
        NA_real_
      ),
    
    software_average_sleep_efficiency_7days =
      ifelse(
        n_sleep_days == 7,
        round(mean(c(
          software_efficiency_day1,
          software_efficiency_day2,
          software_efficiency_day3,
          software_efficiency_day4,
          software_efficiency_day5,
          software_efficiency_day6,
          software_efficiency_day7
        ), na.rm = TRUE), 1),
        NA_real_
      )
  ) %>%
  ungroup() %>%
  
  mutate(
    valid_7days_sleep = n_sleep_days == 7,
    
    software_average_sleep_duration_7days_hhmm =
      minutes_to_hhmm(software_average_sleep_duration_7days_min)
  ) %>%
  
  select(
    subject,
    age,
    sex,
    data_start_time,
    data_end_time,
    
    valid_7days_sleep,
    
    starts_with("software_sleep_day1"),
    starts_with("software_sleep_day2"),
    starts_with("software_sleep_day3"),
    starts_with("software_sleep_day4"),
    starts_with("software_sleep_day5"),
    starts_with("software_sleep_day6"),
    starts_with("software_sleep_day7"),
    software_average_sleep_duration_7days_hhmm,
    
    starts_with("software_efficiency_day1"),
    starts_with("software_efficiency_day2"),
    starts_with("software_efficiency_day3"),
    starts_with("software_efficiency_day4"),
    starts_with("software_efficiency_day5"),
    starts_with("software_efficiency_day6"),
    starts_with("software_efficiency_day7"),
    
    software_average_sleep_efficiency_7days
  )%>%
  select(-ends_with("_min"), -any_of("n_days"))

# ============================================================
# MERGE ALL
# ============================================================

combined <- sleep_processed %>%
  left_join(step_days, by = "subject") %>%
  left_join(activity_summary, by = "subject") %>%
  mutate(
    valid_7days_overall = valid_7days_activity & valid_7days_sleep,
    
    sleep_start_date = suppressWarnings(as.Date(data_start_time)),
    sleep_end_date   = suppressWarnings(as.Date(data_end_time))
  )

combined <- combined %>%
  mutate(
    date_alignment = case_when(
      
      is.na(step_start_date) | is.na(sleep_start_date) ~ "missing",
      
      step_start_date == sleep_start_date &
        step_end_date   == sleep_end_date ~ "perfect_match",
      
      abs(as.numeric(step_start_date - sleep_start_date)) <= 1 &
        abs(as.numeric(step_end_date   - sleep_end_date)) <= 1 ~ "near_match",
      
      TRUE ~ "mismatch"
    ),
    
    valid_date_alignment = date_alignment %in% c("perfect_match","near_match")
  )

combined <- combined %>%
  mutate(
    valid_date_alignment = date_alignment %in% c("perfect_match","near_match")
  )

# ============================================================
# ORDER COLUMNS
# ============================================================

combined <- combined %>%
  relocate(
    subject,
    age,
    sex,
    
    step_start_date,
    step_end_date,
    data_start_time,
    data_end_time,
    
    valid_7days_activity,
    valid_7days_sleep,
    valid_7days_overall,
    valid_date_alignment,
    
    starts_with("stepday"),
    avg_daily_steps_7days,
    
    inactive_duration,
    inactive_duration_hhmm,
    active_duration,
    active_duration_hhmm,
    
    starts_with("software_sleep_day"),
    software_average_sleep_duration_7days_hhmm,
    
    starts_with("software_efficiency_day"),
    software_average_sleep_efficiency_7days
  )

# ============================================================
# QUALITY CHECK
# ============================================================

n_activity_issue <- sum(!combined$valid_7days_activity, na.rm = TRUE)
n_sleep_issue    <- sum(!combined$valid_7days_sleep, na.rm = TRUE)
n_overall_issue  <- sum(!combined$valid_7days_overall, na.rm = TRUE)
n_mismatch       <- sum(combined$date_alignment == "mismatch", na.rm = TRUE)

if(n_activity_issue > 0){
  warning(paste(n_activity_issue, "participants have incomplete activity data"))
}

if(n_sleep_issue > 0){
  warning(paste(n_sleep_issue, "participants have incomplete sleep data"))
}

if(n_overall_issue > 0){
  warning(paste(n_overall_issue, "participants are not valid for analysis"))
}

if(n_mismatch > 0){
  warning(paste(n_mismatch, "participants have date mismatch between activity and sleep data"))
}

# ============================================================
# SAVE OUTPUT
# ============================================================

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

write_csv(combined, output_file)

message("Combined dataset saved to:")
message(output_file)
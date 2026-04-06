# ============================================================
# PERSON SUMMARY PRESENTATION DATASET
# ------------------------------------------------------------
# This script formats the person-level summary dataset
#
# Modifications:
# 1. Convert InactiveDuration and ActiveDuration from minutes
#    to HH:MM format.
# 2. Add daily step counts as stepday1–stepday7.
# 3. Round estimates to the nearest whole number.
#
# Input files:
#   summaries/person_summary.csv
#   summaries/step_metrics.csv
#   summaries/software_sleep_metrics.csv
#
# Output:
#   summaries/person_summary_presentation.csv
# ============================================================

# ============================================================
# PERSON SUMMARY PRESENTATION DATASET
# ------------------------------------------------------------
# This script formats the person-level summary dataset
# ============================================================

library(tidyverse)
library(here)
library(janitor)

# ============================================================
# FILE PATHS
# ============================================================

person_summary_file <- here("summaries", "person_summary.csv")
step_metrics_file   <- here("summaries", "step_metrics.csv")
software_sleep_file <- here("summaries", "software_sleep_metrics.csv")

output_file <- here("summaries", "person_summary_presentation.csv")

# ============================================================
# HELPER FUNCTIONS
# ============================================================

minutes_to_hhmm <- function(mins) {
  ifelse(
    is.na(mins),
    NA_character_,
    sprintf("%02d:%02d",
            as.integer(mins %/% 60),
            as.integer(round(mins %% 60)))
  )
}

hours_to_hhmm <- function(hours) {
  ifelse(
    is.na(hours),
    NA_character_,
    {
      mins <- round(hours * 60)
      hh <- mins %/% 60
      mm <- mins %% 60
      sprintf("%02d:%02d", hh, mm)
    }
  )
}

# ============================================================
# LOAD DATA
# ============================================================

message("Loading datasets...")

person_summary <- read_csv(person_summary_file, show_col_types = FALSE) %>%
  clean_names()

step_metrics <- read_csv(step_metrics_file, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(calendar_date = as.Date(calendar_date))

software_sleep <- read_csv(software_sleep_file, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    across(
      c(starts_with("software_sleep_day"),
        software_average_sleep_duration_hhmm),
      ~ substr(as.character(.),1,5)
    )
  )

# ============================================================
# STEP 1 — BUILD stepday1–stepday7
# ============================================================

message("Building stepday1–stepday7 columns...")

step_days <- step_metrics %>%
  select(subject, calendar_date, total_steps) %>%
  arrange(subject, calendar_date) %>%
  group_by(subject) %>%
  mutate(day_index = row_number()) %>%
  filter(day_index <= 7) %>%
  mutate(day_col = paste0("stepday", day_index)) %>%
  select(subject, day_col, total_steps) %>%
  pivot_wider(
    names_from  = day_col,
    values_from = total_steps,
    values_fill = NA
  ) %>%
  ungroup()

# ============================================================
# STEP 2 — MERGE DATASETS
# ============================================================

presentation <- person_summary %>%
  left_join(step_days, by = "subject") %>%
  left_join(software_sleep, by = "subject")

# ============================================================
# STEP 3 — ROUND ESTIMATES AND FORMAT VARIABLES
# ============================================================

message("Rounding estimates and formatting variables...")

presentation <- presentation %>%
  mutate(
    total_steps = round(total_steps,0),
    inactive_duration = round(inactive_duration,0),
    active_duration = round(active_duration,0),

    GGIR_sleep_duration_sd =
      round(sleep_duration_in_spt_we_t5a5_sd,0),

    inactive_duration_hhmm =
      minutes_to_hhmm(inactive_duration),

    active_duration_hhmm =
      minutes_to_hhmm(active_duration),

    GGIR_sleep_duration_hhmm =
      hours_to_hhmm(sleep_duration_in_spt_we_t5a5_mn),

    GGIR_sleep_duration_minute =
      round(sleep_duration_in_spt_we_t5a5_mn * 60),

    # convert HH:MM → minutes
    software_average_sleep_duration_minutes =
      as.numeric(substr(software_average_sleep_duration_hhmm,1,2))*60 +
      as.numeric(substr(software_average_sleep_duration_hhmm,4,5)),

    sleep_difference_minutes =
      software_average_sleep_duration_minutes -
      GGIR_sleep_duration_minute
  )

# ============================================================
# COLUMN ORDER
# ============================================================

presentation <- presentation %>%
  select(
    -sleep_duration_in_spt_we_t5a5_mn,
    -sleep_duration_in_spt_we_t5a5_sd
  )

presentation <- presentation %>%
  relocate(
    subject,
    age,
    sex,
    starts_with("stepday"),
    total_steps,
    inactive_duration,
    inactive_duration_hhmm,
    active_duration,
    active_duration_hhmm,

    GGIR_sleep_duration_hhmm,
    GGIR_sleep_duration_minute,
    GGIR_sleep_duration_sd,

    starts_with("software_sleep_day"),
    software_average_sleep_duration_hhmm,
    software_average_sleep_duration_minutes,
    sleep_difference_minutes,

    starts_with("software_efficiency_day"),
    software_average_sleep_efficiency,

  )

# ============================================================
# SAVE OUTPUT
# ============================================================

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

write_csv(presentation, output_file)

message("dataset saved to:")
message(output_file)

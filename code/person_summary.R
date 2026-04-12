#!/usr/bin/env Rscript

library(tidyverse)
library(yaml)

# =============================
# LOAD CONFIGURATION
# =============================
cfg <- yaml::read_yaml("config.yml")

# =============================
# 1. PROCESS ACTIVITY SUMMARY
# =============================
# Read from the combined activity_metrics file generated in previous steps
activity_metrics_path <- file.path(cfg$paths$summaries, "activity_metrics.csv")


if (!file.exists(activity_metrics_path)) {
  stop("activity_metrics.csv not found")
}

activity_summary <- read.csv(activity_metrics_path) %>%
  mutate(subject = as.character(subject)) %>%
  group_by(subject) %>%
  summarize(total_steps = mean(total_steps, na.rm = TRUE),
            InactiveDuration = mean(InactiveDuration, na.rm = TRUE),
            ActiveDuration = mean(ActiveDuration, na.rm = TRUE),
            .groups = 'drop')


# =============================
# 2. PROCESS SLEEP SUMMARY
# =============================
# Path to GGIR Part 4 output
if (!file.exists("current_ggir_folder.txt")) {
  stop("Missing current_ggir_folder.txt")
}

ggir_source_name <- readLines("current_ggir_folder.txt")[1]

sleep_file_path <- file.path(
  cfg$paths$ggir_output,
  paste0("output_", ggir_source_name),
  "results",
  "part4_summary_sleep_cleaned.csv"
)

if (!file.exists(sleep_file_path)) {
  stop(paste("Sleep file not found:", sleep_file_path))
}

sleep_summary <- read.csv(sleep_file_path) %>%
  mutate(subject = as.character(str_sub(ID, 1, 6))) %>%
  select(subject, SleepDurationInSpt_WE_T5A5_mn, SleepDurationInSpt_WE_T5A5_sd)

# =============================
# 3. JOIN AND SAVE
# =============================
log_msg(paste("Activity subject type:", class(activity_summary$subject)))
log_msg(paste("Sleep subject type:", class(sleep_summary$subject)))

summary_df <- activity_summary %>%
  left_join(sleep_summary, by = "subject")

# Save final person summary
output_file <- file.path(cfg$paths$summaries, "person_summary.csv")
write_csv(summary_df, file = output_file)

log_msg(paste("Success:", output_file, "has been generated"))
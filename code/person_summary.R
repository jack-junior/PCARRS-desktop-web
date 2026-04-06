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

activity_summary <- read.csv(activity_metrics_path) %>%
  group_by(subject) %>%
  summarize(total_steps = mean(total_steps, na.rm = TRUE),
            InactiveDuration = mean(InactiveDuration, na.rm = TRUE),
            ActiveDuration = mean(ActiveDuration, na.rm = TRUE),
            .groups = 'drop')

# =============================
# 2. PROCESS SLEEP SUMMARY
# =============================
# Path to GGIR Part 4 output
sleep_file_path <- file.path(cfg$paths$ggir_output, "output_BIN", "results", "part4_summary_sleep_cleaned.csv")

sleep_summary <- read.csv(sleep_file_path) %>%
  mutate(subject = as.numeric(str_extract(ID, "^[^_]+"))) %>%
  select(subject, SleepDurationInSpt_WE_T5A5_mn, SleepDurationInSpt_WE_T5A5_sd)

# =============================
# 3. JOIN AND SAVE
# =============================
summary_df <- activity_summary %>%
  left_join(sleep_summary, by = "subject")

# Save final person summary
output_file <- file.path(cfg$paths$summaries, "person_summary.csv")
write_csv(summary_df, file = output_file)

message("Success: ", output_file, " has been generated.")
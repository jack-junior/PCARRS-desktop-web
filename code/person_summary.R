#!/usr/bin/env Rscript

library(tidyverse)
library(yaml)

# =============================
# LOAD CONFIGURATION
# =============================
# Since this is sourced locally, it uses the config already in memory or reads it here
cfg <- yaml::read_yaml("config.yml")

safe_log <- function(msg, type = "info") {
  prefix <- switch(type, "error" = "❌ ", "warning" = "⚠️ ", "info" = "🔹 ", "")
  full_msg <- paste0(prefix, msg)
  
  if (exists("log_msg") && is.function(log_msg)) {
    log_msg(full_msg)
  } else {
    cat(full_msg, "\n")
  }
}

# Use the batch_name defined in the global environment (from Shiny)
# Default to empty if not found to maintain compatibility
b_name <- if (exists("batch_name")) batch_name else ""


# =============================
# 1. PROCESS ACTIVITY SUMMARY
# =============================
# Updated path to point to the batch-specific summary folder
if (b_name != "") {
  activity_metrics_path <- file.path(cfg$paths$summaries, b_name, "activity_metrics.csv")
} else {
  activity_metrics_path <- file.path(cfg$paths$summaries, "activity_metrics.csv")
}

if (!file.exists(activity_metrics_path)) {
  stop(paste("activity_metrics.csv not found at:", activity_metrics_path))
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

# Updated path to include the batch subfolder in GGIR output
if (b_name != "") {
  sleep_file_path <- file.path(
    cfg$paths$ggir_output,
    #b_name,
    paste0("output_", ggir_source_name),
    "results",
    "part4_summary_sleep_cleaned.csv"
  )
} else {
  sleep_file_path <- file.path(
    cfg$paths$ggir_output,
    paste0("output_", ggir_source_name),
    "results",
    "part4_summary_sleep_cleaned.csv"
  )
}

if (!file.exists(sleep_file_path)) {
  # Note: part4_summary_sleep_cleaned.csv might not exist if part4 didn't run 
  # or was named differently. Ensuring consistency with previous steps.
  #warning(paste("Sleep file not found:", sleep_file_path))
  safe_log(paste("Sleep file not found:", sleep_file_path), type = "warning")
  sleep_summary <- data.frame(subject = character(), 
                              SleepDurationInSpt_WE_T5A5_mn = numeric(), 
                              SleepDurationInSpt_WE_T5A5_sd = numeric())
} else {
  sleep_summary <- read.csv(sleep_file_path) %>%
    # Updated to 7 characters to match the rest of the pipeline
    mutate(subject = as.character(str_sub(ID, 1, 7))) %>%
    select(subject, SleepDurationInSpt_WE_T5A5_mn, SleepDurationInSpt_WE_T5A5_sd)
}

# =============================
# 3. JOIN AND SAVE
# =============================
# Using log_msg if it exists in the environment
# if (exists("log_msg")) {
#   log_msg(paste("Activity subject type:", class(activity_summary$subject)))
#   log_msg(paste("Sleep subject type:", class(sleep_summary$subject)))
# }

# if (exists("log_msg") && is.function(log_msg)) {
#   log_msg(paste("Activity subject type:", class(activity_summary$subject)))
#   log_msg(paste("Sleep subject type:", class(sleep_summary$subject)))
# }

safe_log(paste("Activity subject type:", class(activity_summary$subject)))
safe_log(paste("Sleep subject type:", class(sleep_summary$subject)))

summary_df <- activity_summary %>%
  left_join(sleep_summary, by = "subject")

# Save final person summary inside the batch folder
if (b_name != "") {
  output_dir <- file.path(cfg$paths$summaries, b_name)
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_file <- file.path(output_dir, "person_summary.csv")
} else {
  output_file <- file.path(cfg$paths$summaries, "person_summary.csv")
}

write_csv(summary_df, file = output_file)

# if (exists("log_msg")) {
#   log_msg(paste("Success:", output_file, "has been generated"))
# }

safe_log(paste("Success:", output_file, "has been generated"))
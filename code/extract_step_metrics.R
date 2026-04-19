#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(yaml)
})

# --- DYNAMIC PROJECT ROOT DETECTION ---
config_path <- "config.yml"
if (!file.exists(config_path)) config_path <- "../config.yml"

if (!file.exists(config_path)) {
  stop("FATAL: config.yml not found. Check project structure.")
}

# Define project root based on config file location
base_project <- dirname(normalizePath(config_path, winslash = "/"))

# --- DYNAMIC PYTHON LOCATION ---
path_options <- c(
  file.path(base_project, "resources/python_env/python.exe"),
  file.path(base_project, "resources/python_env/Scripts/python.exe"),
  file.path(base_project, "resources/python_env/bin/python.exe")
)

python_path <- NULL
for (opt in path_options) {
  if (file.exists(opt)) {
    python_path <- normalizePath(opt, winslash = "/", mustWork = FALSE)
    break
  }
}

if (is.null(python_path)) {
  stop("FATAL: python.exe not found in resources/python_env/")
}

# Set environment
Sys.setenv(PYTHON = python_path)
options(python_cmd = python_path)
message("--- SUCCESS: Portable Mode Active. Using: ", python_path)

suppressWarnings(suppressPackageStartupMessages(library(argparse)))

parser <- ArgumentParser()
parser$add_argument("--batch", type="character", help="Batch name")
args <- parser$parse_args()

cfg <- yaml::read_yaml(config_path)

# On utilise base_project pour construire les chemins de données
if (!is.null(args$batch) && args$batch != "") {
  stepcount_root_dir <- file.path(base_project, cfg$paths$summaries, args$batch)
} else {
  stepcount_root_dir <- file.path(base_project, cfg$paths$summaries)
}

output_file <- file.path(stepcount_root_dir, "step_metrics.csv")

# =============================
# FUNCTION TO PROCESS ONE PARTICIPANT FOLDER
# =============================

process_one_stepcount_dir <- function(stepcount_dir) {
  
  participant_id <- basename(stepcount_dir)
  
  # Safety check: skip internal resource folders if they exist in summaries
  if (participant_id %in% c("R_libs", "python_env", "images")) return(tibble())
  
  message("Processing participant folder: ", participant_id)
  
  
  # ---- find Daily file ----
  daily_file <- list.files(
    stepcount_dir,
    pattern = "Daily\\.csv\\.gz$",
    recursive = TRUE,
    full.names = TRUE
  )  
  
  
  if (length(daily_file) != 1L) {
    warning(
      "Skipping ", stepcount_dir, " because Daily.csv.gz not unique.\nFound: ",
      paste(basename(daily_file), collapse = ", ")
    )
    return(tibble())
  }
  
  # ---- find Bouts file ----
  bouts_file <- list.files(
    stepcount_dir,
    pattern = "Bouts\\.csv\\.gz$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(bouts_file) != 1L) {
    warning(
      "Skipping ", stepcount_dir, " because Bouts.csv.gz not unique.\nFound: ",
      paste(basename(bouts_file), collapse = ", ")
    )
    return(tibble())
  }
  
  # ---- read Daily ----
  daily <- suppressWarnings( 
    readr::read_csv(daily_file, show_col_types = FALSE) %>%
    clean_names() %>%
    # keep core fields; adjust names if needed
    select(
      filename,
      date,
      total_steps = steps,
      cadence95 = cadence95th_steps_min
    ) 
  )
  
  # daily <- suppressWarnings(
  #   readr::read_csv(daily_file, show_col_types = FALSE, progress = FALSE)
  # )
  
  # ---- read Bouts ----
  bouts <- suppressWarnings(
    readr::read_csv(bouts_file, show_col_types = FALSE, progress = FALSE) %>%
      clean_names()
  )
  
  # Safety check: if Bouts is empty, return a tibble with NA metrics to avoid errors
  if (nrow(bouts) == 0) {
    message("No bouts found for: ", participant_id)
    return(daily %>% mutate(participant = participant_id, .before = 1))
  }
  
  # ---- derive daily bout metrics ----
  bouts_daily <- bouts %>%
    mutate(
      date         = as.Date(start_time),
      cadence_bout = steps / duration_mins,
      bout_type = case_when(
        cadence_bout >= 130 ~ "run",
        cadence_bout >= 100 ~ "walk_fast",
        cadence_bout >= 60  ~ "walk_slow",
        TRUE                ~ "other"
      )
    ) %>%
    group_by(filename, date) %>%
    summarise(
      # steps
      slow_steps_day = sum(steps[bout_type == "walk_slow"], na.rm = TRUE),
      fast_steps_day = sum(steps[bout_type == "walk_fast"], na.rm = TRUE),
      run_steps_day  = sum(steps[bout_type == "run"],        na.rm = TRUE),
      
      # durations (in minutes)
      walk_duration_min_day = sum(
        duration_mins[bout_type %in% c("walk_slow", "walk_fast")],
        na.rm = TRUE
      ),
      run_duration_min_day = sum(
        duration_mins[bout_type == "run"],
        na.rm = TRUE
      ),
      
      # number of walking bouts (slow + fast)
      n_walking_bouts_day = sum(bout_type %in% c("walk_slow", "walk_fast")),
      
      # SD of walking bout durations (minutes)
      sd_walk_bout_duration_min = if (
        sum(bout_type %in% c("walk_slow", "walk_fast")) >= 2
      ) {
        stats::sd(
          duration_mins[bout_type %in% c("walk_slow", "walk_fast")],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      
      # mean cadence of walking bouts (slow + fast)
      mean_walk_cadence_day = if (
        sum(bout_type %in% c("walk_slow", "walk_fast")) >= 1
      ) {
        mean(
          cadence_bout[bout_type %in% c("walk_slow", "walk_fast")],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      
      .groups = "drop"
    )
  
  # ---- join Daily + bouts_daily ----
  daily_joined <- daily %>%
    left_join(bouts_daily, by = c("filename", "date")) %>%
    mutate(
      participant = participant_id,
      .before = 1
    )
  
  return(daily_joined)
}

# =============================
# MAIN: LOOP OVER ALL PARTICIPANT FOLDERS
# =============================

# List subdirectories INSIDE the batch folder (recursive = FALSE)
# This finds: summaries/250126/2900764_250125, etc.
participant_dirs <- list.dirs(stepcount_root_dir, full.names = TRUE, recursive = FALSE)

# Safety: Filter out resource folders
participant_dirs <- participant_dirs[!basename(participant_dirs) %in% c("R_libs", "python_env", "images")]

# Handle case where files might be directly in the batch (no subfolders)
if (length(participant_dirs) == 0) {
  message("No subdirectories found. Treating current directory as participant folder.")
  participant_dirs <- stepcount_root_dir
}

# Run the processing for each participant folder found
all_daily <- purrr::map_dfr(participant_dirs, process_one_stepcount_dir)

if (nrow(all_daily) > 0) {
  all_daily <- all_daily %>%
    # Updated to 7 characters for subject ID consistency
    mutate(subject = str_sub(participant, 1, 7)) %>% 
    select(-participant, -filename) %>%
    rename(calendar_date = date)
  
  # Save master dataset
  readr::write_csv(all_daily, output_file)
}
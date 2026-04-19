################################################################################
# Script to extract GGIR activity metrics from GGIR output (DYNAMIC VERSION)
################################################################################

#!/usr/bin/env Rscript

# --- DYNAMIC PROJECT ROOT DETECTION ---
config_path <- "config.yml"
if (!file.exists(config_path)) config_path <- "../config.yml"

if (!file.exists(config_path)) {
  stop("FATAL: config.yml not found. Check project structure.")
}

# Define project root based on config file location
base_project <- dirname(normalizePath(config_path, winslash = "/"))

# --- DYNAMIC PYTHON LOCATION (PORTABLE MODE) ---
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

if (!is.null(python_path)) {
  Sys.setenv(PYTHON = python_path)
  options(python_cmd = python_path)
  message("--- SUCCESS: Portable Python Active for Activity Metrics: ", python_path)
}

# --- NOW LOAD LIBRARIES ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
  library(haven) 
  library(argparse)
})

if (!exists("log_msg")) {
  log_msg <- function(msg) { cat(paste0(Sys.time(), " - ", msg, "\n")) }
}

# =============================
# 1. LOAD CONFIGURATION
# =============================
parser <- ArgumentParser()
parser$add_argument("--batch", type="character", help="Batch name")
args <- parser$parse_args()

# Load config to get dynamic paths
config_path <- "config.yml"

if (!file.exists(config_path)) {
  config_path <- "../config.yml"
}

if (!file.exists(config_path)) {
  stop("FATAL: config.yml not found. Current directory: ", getwd())
}
cfg <- yaml::read_yaml(config_path)

get_daily_activity = function(GGIR_output = cfg$paths$ggir_output, batch_name = args$batch){
  
  # --- DYNAMIC BATCH LOGIC ---
  
  # If a batch is provided, point to GGIR/batch_name
  # current_ggir_base <- if (!is.null(batch_name) && batch_name != "") {
  #   file.path(GGIR_output, batch_name)
  # } else {
  #   GGIR_output
  # }
  
  # 1. Read the source folder name used by GGIR
  if (!file.exists("current_ggir_folder.txt")) {
    stop("ERROR: 'current_ggir_folder.txt' missing. Run GGIR first.")
  }
  ggir_source_name <- readLines("current_ggir_folder.txt")
  
  # 2. Build path to QC folder
  qc_dir <- file.path(GGIR_output, paste0("output_", ggir_source_name), "results", "QC")
  
  if (!dir.exists(qc_dir)) {
    stop("GGIR results directory not found: ", qc_dir)
  }
  
  # 3. Search for part5_daysummary file
  all_files <- list.files(qc_dir, full.names = TRUE)
  
  candidates <- all_files[grep("part5_daysummary_full", all_files)]
  
  if (length(candidates) == 0) {
    stop("No 'part5_daysummary_full' file found in: ", qc_dir)
  }
  
  # avoid empty files
  candidates <- candidates[file.size(candidates) > 0]
  
  if (length(candidates) == 0) {
    stop("Files 'part5_daysummary_full' found but they are empty")
  }
  
  filename <- candidates[1]
  
  log_msg(paste(">>> Using GGIR file:", basename(filename)))
  log_msg(paste("--- Data Extraction from :", basename(filename), "---"))
  
  # =============================
  # 2. PROCESSING DATA
  # =============================
  activity_df_raw <- read_csv(filename, show_col_types = FALSE)
  if (nrow(activity_df_raw) == 0) {
    stop("GGIR file loaded but contains 0 rows")
  }
  
  log_msg(paste(">>> Rows loaded:", nrow(activity_df_raw)))
  
  # =============================
  # COLUMN VALIDATION
  # =============================
  required_cols <- c(
    "dur_day_min",
    "dur_day_total_IN_min",
    "dur_day_total_MOD_min",
    "dur_day_total_VIG_min",
    "ACC_day_mg",
    "ACC_day_total_IN_mg",
    "Nblocks_day_total_LIG",
    "Nblocks_day_total_MOD",
    "Nblocks_day_total_VIG",
    "FRAG_CoV_dur_IN_day",
    "FRAG_CoV_dur_PA_day",
    "ID",
    "weekday",
    "calendar_date"
  )
  
  missing_cols <- setdiff(required_cols, names(activity_df_raw))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in GGIR output:", paste(missing_cols, collapse=", ")))
  }
  
  if (!"GGIRversion" %in% names(activity_df_raw)) {
    activity_df_raw$GGIRversion <- NA
    log_msg(">>> GGIRversion column missing → filled with NA")
  }
  
  # =============================
  # PIPELINE
  # =============================
  activity_df = activity_df_raw %>%
    mutate(
      ActiveDuration = dur_day_min - dur_day_total_IN_min,
      MVPADuration = dur_day_total_MOD_min + dur_day_total_VIG_min,
      ActivityIntensity = ACC_day_mg - ACC_day_total_IN_mg,
      ActiveVolume = ActivityIntensity * ActiveDuration,
      ActiveBoutCount = Nblocks_day_total_LIG + Nblocks_day_total_MOD + Nblocks_day_total_VIG,
      # Update ID to 7 characters to match pipeline logic
      id = str_sub(ID, 1, 7)
    ) %>%
    rename(InactiveDuration = dur_day_total_IN_min,
           LightActivityDuration = dur_day_total_LIG_min,
           ModerateActivityDuration = dur_day_total_MOD_min,
           VigorousActivityDuration = dur_day_total_VIG_min,
           InactiveBoutCount = Nblocks_day_total_IN,
           InactiveBoutDurationVariance = FRAG_CoV_dur_IN_day,
           ActiveBoutDurationVariance = FRAG_CoV_dur_PA_day) %>%
    dplyr::select(subject = id, weekday, calendar_date, GGIRversion, ActiveDuration,
                  MVPADuration, ActivityIntensity, ActiveVolume, ActiveBoutCount, InactiveDuration,
                  LightActivityDuration, ModerateActivityDuration, VigorousActivityDuration,
                  InactiveBoutCount, InactiveBoutDurationVariance, ActiveBoutDurationVariance
    )
  
  # Define output folder (summaries/batch_name)
  output_folder <- if (!is.null(batch_name) && batch_name != "") {
    file.path(cfg$paths$summaries, batch_name)
  } else {
    cfg$paths$summaries
  }
  
  if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  
  output_path <- file.path(output_folder, "activity_metrics.csv")
  
  write_csv(activity_df, file = output_path)
  log_msg(paste("Success: activity_metrics.csv generated in", output_folder))
  
  return(activity_df)
}

# Execute the function
get_daily_activity()
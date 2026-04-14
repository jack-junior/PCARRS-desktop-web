# #!/usr/bin/env Rscript
# 
suppressPackageStartupMessages({
   library(tidyverse)
   library(janitor)
   library(yaml)
 })
 
# =============================
 # USER SETTINGS (Updated for Config)
 # =============================
 
 # Load config to get dynamic paths
 config_path <- "config.yml"
 
 if (!file.exists(config_path)) {
   # Si on ne le trouve pas, on tente de remonter d'un cran (au cas où le script tourne DEPUIS le dossier code/)
   config_path <- "../config.yml"
 }
 
 if (!file.exists(config_path)) {
   stop("FATAL: config.yml not found. Current directory: ", getwd())
 }
 
 cfg <- yaml::read_yaml(config_path)
 # Root folder containing one subfolder per participant (e.g. 111111_311025)
 stepcount_root_dir <- cfg$paths$summaries
# Output file for the combined master dataset
output_file <- file.path(cfg$paths$summaries, "step_metrics.csv")



# # Utilisation de suppressPackageStartupMessages pour nettoyer le log
# suppressPackageStartupMessages({
#   library(tidyverse)
#   library(janitor)
#   library(yaml)
# })
# 
# # --- TEST DE SURVIE IMMÉDIAT ---
# cat("\n[CHECK 1] Libraries loaded successfully\n")
# cat("[CHECK 2] Working Directory:", getwd(), "\n")
# 
# # --- CHARGEMENT SÉCURISÉ DU YAML ---
# config_path <- "config.yml"
# if (!file.exists(config_path)) {
#   config_path <- "../config.yml"
# }
# 
# if (!file.exists(config_path)) {
#   cat("[ERROR] config.yml non trouvé !\n")
#   quit(status = 1)
# }
# 
# cfg <- yaml::read_yaml(config_path)
# cat("[CHECK 3] YAML loaded. Summary path:", cfg$paths$summaries, "\n")
# 
# stepcount_root_dir <- cfg$paths$summaries
# output_file <- file.path(cfg$paths$summaries, "step_metrics.csv")


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
  daily <- readr::read_csv(daily_file, show_col_types = FALSE) %>%
    clean_names() %>%
    # keep core fields; adjust names if needed
    select(
      filename,
      date,
      total_steps = steps,
      cadence95 = cadence95th_steps_min
    )
  
  # ---- read Bouts ----
  bouts <- readr::read_csv(bouts_file, show_col_types = FALSE) %>%
    clean_names()
  
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

# immediate subdirectories of the root, each assumed to be a participant folder
participant_dirs <- list.dirs(stepcount_root_dir, full.names = TRUE, recursive = FALSE)

# Filter out non-participant directories
participant_dirs <- participant_dirs[!basename(participant_dirs) %in% c("R_libs", "python_env", "images")]

# Process each participant folder and row-bind results
if (length(participant_dirs) == 0) {
  message("⚠️ No participant directories found in: ", stepcount_root_dir)
  quit(status = 0) # On sort proprement sans erreur pour ne pas bloquer le pipeline
}

all_daily <- purrr::map_dfr(participant_dirs, process_one_stepcount_dir)

if (nrow(all_daily) > 0) {
  all_daily <- all_daily %>%
    mutate(subject = str_sub(participant, 1, 6)) %>%
    select(-participant, -filename) %>%
    rename(calendar_date = date)
  
  # Save master dataset
  readr::write_csv(all_daily, output_file)
}
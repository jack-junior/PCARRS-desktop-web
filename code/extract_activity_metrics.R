################################################################################
# Script to extract GGIR activity metrics from GGIR output (DYNAMIC VERSION)
################################################################################

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
  library(haven) 
})


if (!exists("log_msg")) {
  log_msg <- function(msg) { cat(paste0(Sys.time(), " - ", msg, "\n")) }
}

# =============================
# 1. LOAD CONFIGURATION
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

get_daily_activity = function(GGIR_output = cfg$paths$ggir_output){
  
  # --- NOUVELLE LOGIQUE DYNAMIQUE ---
  
  # 1. On lit le nom du dossier source utilisé par GGIR (créé par run_GGIR.R)
  if (!file.exists("current_ggir_folder.txt")) {
    stop("Erreur : Fichier témoin 'current_ggir_folder.txt' absent. Relancez GGIR.")
  }
  ggir_source_name <- readLines("current_ggir_folder.txt")
  
  # 2. On construit le chemin vers le dossier QC
  # Cela gère automatiquement 'output_BIN' ou 'output_ggir_subset'
  qc_dir <- file.path(GGIR_output, paste0("output_", ggir_source_name), "results", "QC")
  
  if (!dir.exists(qc_dir)) {
    stop("Le dossier de résultats GGIR est introuvable : ", qc_dir)
  }
  
  # 3. On cherche le fichier part5_daysummary (le nom change selon les seuils)
  all_files <- list.files(qc_dir, full.names = TRUE)
  
  candidates <- all_files[grep("part5_daysummary_full", all_files)]
  
  if (length(candidates) == 0) {
    stop("Aucun fichier 'part5_daysummary_full' trouvé dans : ", qc_dir)
  }
  
  # éviter fichiers vides
  candidates <- candidates[file.size(candidates) > 0]
  
  if (length(candidates) == 0) {
    stop("Fichiers 'part5_daysummary_full' trouvés mais vides")
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
  log_msg(paste(">>> Columns:", paste(names(activity_df_raw), collapse=", ")))
  
  # =============================
  # VALIDATION COLONNES (AVANT PIPELINE)
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
      id = str_sub(ID, 1, 6)
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
  
  # Save output to the summaries folder defined in config
  output_path <- file.path(cfg$paths$summaries, "activity_metrics.csv")
  if(!dir.exists(cfg$paths$summaries)) dir.create(cfg$paths$summaries, recursive = TRUE)
  
  write_csv(activity_df, file = output_path)
  log_msg(paste("Success: activity_metrics.csv generated in", cfg$paths$summaries))
  
  return(activity_df)
}

# Execute the function
get_daily_activity()
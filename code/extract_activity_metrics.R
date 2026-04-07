################################################################################
# Script to extract GGIR activity metrics from GGIR output (DYNAMIC VERSION)
################################################################################

library(tidyverse)
library(haven)
library(yaml)

# =============================
# 1. LOAD CONFIGURATION
# =============================
cfg <- yaml::read_yaml("config.yml")

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
  filename <- all_files[grep("part5_daysummary", all_files)][1] # On prend le premier trouvé
  
  if (is.na(filename) || !file.exists(filename)) {
    stop("Rapport part5_daysummary introuvable dans : ", qc_dir)
  }
  
  message("--- Extraction des données depuis : ", basename(filename), " ---")
  
  # =============================
  # 2. PROCESSING DATA
  # =============================
  activity_df = read_csv(filename, show_col_types = FALSE) %>%
    mutate(ActiveDuration = dur_day_min - dur_day_total_IN_min,
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
  message("Succès : activity_metrics.csv généré dans ", cfg$paths$summaries)
  
  return(activity_df)
}

# Execute the function
get_daily_activity()
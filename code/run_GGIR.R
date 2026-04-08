# ==============================================================================
# GGIR Pipeline 
# ==============================================================================

library(GGIR)
library(yaml)
library(tidyverse)

# 1. LOAD CONFIGURATION
if (!file.exists("config.yml")) {
  stop("Configuration file (config.yml) not found. Please save settings in the App first.")
}
cfg <- yaml::read_yaml("config.yml")

# 2. DEFINE DYNAMIC PATHS
data_dir   <- cfg$paths$raw_bin
output_dir <- cfg$paths$ggir_output

if (data_dir == "" || is.null(data_dir)) {
  stop("No input directory found in config. Please select the .bin folder in the App.")
}

if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# 3. SELECTIVE PROCESSING LOGIC (Unified)
final_data_dir <- data_dir
is_selective <- FALSE
temp_subset_dir <- file.path(tempdir(), "ggir_subset") # Stockage du chemin pour nettoyage

if (exists("selected_filenames") && !is.null(selected_filenames) && length(selected_filenames) > 0) {
  message("--- Selective Mode Active ---")
  
  if(dir.exists(temp_subset_dir)) unlink(temp_subset_dir, recursive = TRUE)
  dir.create(temp_subset_dir, recursive = TRUE)
  
  files_to_copy <- file.path(data_dir, selected_filenames)
  existing_files <- files_to_copy[file.exists(files_to_copy)]
  
  if (length(existing_files) > 0) {
    success_copy <- file.copy(existing_files, temp_subset_dir)
    if(all(success_copy)) {
      final_data_dir <- temp_subset_dir
      is_selective <- TRUE
      message(paste("Files successfully copied to temp folder:", length(existing_files), "files."))
    }
  } 
} 

writeLines(basename(final_data_dir), "current_ggir_folder.txt")

message("--- Starting GGIR Analysis ---")

# --- 4. RUN GGIR (PARAMÈTRES ALIGNÉS SUR L'ORIGINEL) ---
GGIR(
  mode = c(1, 2, 3, 4, 5),
  datadir = final_data_dir, 
  outputdir = output_dir,
  overwrite = FALSE,           # CORRIGÉ : Aligné sur l'originel (évite les calculs redondants)
  studyname = "CARRS Brain",   # CORRIGÉ : Nom exact du projet originel
  idloc = 6,
  printsummary = TRUE, 
  do.imp = FALSE,              # Confirmé : pas d'imputation
  epochvalues2csv = TRUE, 
  timewindow = c("WW"), 
  
  save_ms5rawlevels = TRUE, 
  save_ms5raw_format = "csv", 
  save_ms5raw_without_invalid = FALSE, 
  part5_agg2_60seconds = FALSE, 
  nonwear_approach = "2023", 
  nonwear_range_threshold = 150, 
  
  viewingwindow = 2, 
  do.report = c(2, 4, 5), 
  do.part3.pdf = TRUE, 
  dofirstpage = TRUE, 
  visualreport_without_invalid = FALSE, 
  visualreport_hrsPerRow = 24,
  visualreport_focus = "night",
  
  frag.metrics = "all", 
  cosinor = TRUE, 
  do.neishabouricounts = TRUE, 
  
  do.enmoa = TRUE,
  do.enmo = FALSE,
  acc.metric = "ENMOa",
  threshold.lig = 105.6,
  threshold.mod = 174.2,
  threshold.vig = 330,
  
  nonwearFiltermaxHours = 3,
  nonwearFilterWindow = c(20, 10),
  verbose = TRUE,      
  visualreport = TRUE, 
  do.parallel = FALSE,
  
  LUXthresholds = c(0, 1, 10, 100, 300, 1000),
  LUX_day_segments = seq(0, 24, by = 1)
)

# --- 5. NETTOYAGE (AJOUTÉ) ---
if (is_selective && dir.exists(temp_subset_dir)) {
  unlink(temp_subset_dir, recursive = TRUE)
  message("--- Cleanup: Temporary selective folder removed ---")
}

message("==========================================")
message("GGIR SUMMARY REPORT")
message("Status: COMPLETED")
message("Mode: ", if(is_selective) "Selective (Subset)" else "Full Directory")
message("Check folder: ", output_dir)
message("==========================================")
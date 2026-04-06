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

# 3. SELECTIVE PROCESSING LOGIC (New)
# These variables are passed from the Shiny Server global environment
target_files <- NULL
overwrite_mode <- FALSE

if (exists("selected_ids_vector") && length(selected_ids_vector) > 0) {
  message("Selective Mode Active. Target IDs: ", paste(selected_ids_vector, collapse=", "))
  
  # List all .bin files and filter by the IDs provided
  all_bin_files <- list.files(data_dir, pattern = "\\.bin$", recursive = TRUE)
  
  # Match filenames containing the IDs
  target_files <- all_bin_files[unique(unlist(lapply(selected_ids_vector, function(id) grep(id, all_bin_files))))]
  
  if (length(target_files) == 0) {
    stop("None of the specified IDs were found in the .bin folder.")
  }
  
  # In selective mode, we usually want to overwrite to re-process errors
  overwrite_mode <- TRUE
  message("Files found for processing: ", length(target_files))
}

message("--- Starting GGIR Analysis ---")

# 4. RUN GGIR
GGIR(
  mode = c(1, 2, 3, 4, 5),
  datadir = data_dir,
  outputdir = output_dir,
  
  # SELECTIVE FILTRATION
  selectfiles = target_files, # NULL means process all
  overwrite = overwrite_mode,  # TRUE if selective, FALSE if full (to skip existing)
  
  studyname = cfg$project$name, 
  idloc = 6, 
  printsummary = TRUE, 
  do.imp = FALSE, 
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
  
  LUXthresholds = c(0, 1, 10, 100, 300, 1000),
  LUX_day_segments = c(seq(0, 24, by = 1))
)

message("==========================================")
message("GGIR SUMMARY REPORT")
message("Status: COMPLETED")
message("Files Targeted: ", if(is.null(target_files)) "Full Folder" else length(target_files))
message("Check folder: ", output_dir, "/output_", cfg$project$name)
message("==========================================")
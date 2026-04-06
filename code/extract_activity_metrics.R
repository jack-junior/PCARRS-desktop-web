################################################################################
# Script to extract GGIR activity metrics from GGIR output
#
# Inputs:
#    GGIR_output: this should be a folder where the GGIR output is stored.
#
# Outputs:
#    activity_metrics.csv: a file of activity metrics for each subject and day
################################################################################

library(tidyverse)
library(haven)
library(yaml)

# =============================
# LOAD CONFIGURATION
# =============================
cfg <- yaml::read_yaml("config.yml")

get_daily_activity = function(GGIR_output = cfg$paths$ggir_output){
  
  # Construct the path to the specific GGIR part 5 summary file
  # Note: The subfolder 'output_BIN' is standard GGIR naming
  filename = file.path(GGIR_output, "output_BIN", "results", "QC",
                       "part5_daysummary_full_WW_L105.6M174.2V330_T5A5.csv")
  
  if (!file.exists(filename)) {
    stop("GGIR output file not found at: ", filename)
  }
  
  activity_df = read_csv(filename, show_col_types = FALSE) %>%
    mutate(ActiveDuration = dur_day_min - dur_day_total_IN_min,
           MVPADuration = dur_day_total_MOD_min + dur_day_total_VIG_min,
           ActivityIntensity = ACC_day_mg - ACC_day_total_IN_mg,
           ActiveVolume = ActivityIntensity * ActiveDuration,
           ActiveBoutCount = Nblocks_day_total_LIG + Nblocks_day_total_MOD + Nblocks_day_total_VIG,
           id = str_sub(ID, 1,6),
           #valid_day = ifelse(cleaningcode %in% c(0,1), TRUE, FALSE)
    ) %>%
    rename(InactiveDuration = dur_day_total_IN_min,
           LightActivityDuration = dur_day_total_LIG_min,
           ModerateActivityDuration = dur_day_total_MOD_min,
           VigorousActivityDuration = dur_day_total_VIG_min,
           InactiveBoutCount = Nblocks_day_total_IN,
           InactiveBoutDurationVariance = FRAG_CoV_dur_IN_day,
           ActiveBoutDurationVariance = FRAG_CoV_dur_PA_day) %>%
    dplyr::select(filename, subject = id, weekday, calendar_date, GGIRversion, ActiveDuration,
                  MVPADuration, ActivityIntensity, ActiveVolume, ActiveBoutCount, InactiveDuration,
                  LightActivityDuration, ModerateActivityDuration, VigorousActivityDuration,
                  InactiveBoutCount, InactiveBoutDurationVariance, ActiveBoutDurationVariance,
                  #valid_day, cleaning_code
    )
  
  
  # Save output to the summaries folder defined in config
  output_path <- file.path(cfg$paths$summaries, "activity_metrics.csv")
  write_csv(activity_df, file = output_path)
  
  activity_df
}

# Execute the function
get_daily_activity()
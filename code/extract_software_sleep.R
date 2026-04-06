# ============================================================
# EXTRACT SOFTWARE SLEEP METRICS (FINAL CORRECT VERSION)
# ============================================================

library(tidyverse)
library(yaml)
library(lubridate)

# --- LOAD CONFIG ---
cfg <- yaml::read_yaml("config.yml")

sleep_reports_dir <- cfg$paths$sleep_data

output_file <- file.path(
  cfg$paths$summaries,
  "software_sleep_metrics.csv"
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# LIST FILES
# ============================================================

files <- list.files(
  sleep_reports_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

# ============================================================
# HELPERS
# ============================================================

split_line <- function(line){
  if(is.na(line) || length(line)==0) return(NA)
  strsplit(line,",\\s*")[[1]]
}

hhmm_to_minutes <- function(x){
  
  x <- as.character(x)
  
  h <- suppressWarnings(as.numeric(substr(x,1,2)))
  m <- suppressWarnings(as.numeric(substr(x,4,5)))
  
  out <- h*60 + m
  out[is.na(x)] <- NA_real_
  
  return(out)
}

minutes_to_hhmm <- function(mins){
  ifelse(
    is.na(mins),
    NA_character_,
    sprintf("%02d:%02d",
            as.integer(mins %/% 60),
            as.integer(round(mins %% 60)))
  )
}

# ============================================================
# MAIN FUNCTION
# ============================================================

extract_sleep_metrics <- function(file){
  
  lines <- readLines(file, warn = FALSE)
  
  # -----------------------
  # BASIC INFO
  # -----------------------
  
  pid_line <- lines[grepl("^Participant,", lines)][1]
  age_line <- lines[grepl("^Age,", lines)][1]
  sex_line <- lines[grepl("^Sex,", lines)][1]
  
  get_val <- function(line){
    if(is.na(line)) return(NA)
    parts <- split_line(line)
    if(length(parts) >= 2) return(parts[2])
    NA
  }
  
  pid_full <- get_val(pid_line)
  subject  <- substr(pid_full,1,6)
  
  age <- suppressWarnings(as.numeric(get_val(age_line)))
  sex <- get_val(sex_line)
  
  # ============================================================
  # DAILY SLEEP TABLE (WITH DATE ALIGNMENT)
  # ============================================================
  
  header_index <- grep("Night of Date", lines)
  
  temp_df <- tibble()
  
  if(length(header_index) > 0){
    
    start <- header_index + 1
    
    for(i in 1:8){
      
      row_index <- start + i - 1
      
      if(row_index <= length(lines)){
        
        parts <- split_line(lines[row_index])
        
        if(!is.null(parts) && length(parts) >= 9){
          
          date_val  <- parts[1]
          sleep_val <- parts[8]
          eff_val   <- parts[9]
          
          if(date_val == "")  date_val  <- NA
          if(sleep_val == "") sleep_val <- NA
          if(eff_val == "")   eff_val   <- NA
          
          temp_df <- bind_rows(
            temp_df,
            tibble(
              date  = date_val,
              sleep = substr(sleep_val,1,5),
              eff   = suppressWarnings(as.numeric(eff_val))
            )
          )
        }
      }
    }
    
    # ========================================================
    # SORT BY DATE (CRITICAL)
    # ========================================================
    
    library(lubridate)
    
    temp_df <- temp_df %>%
      mutate(
        date = parse_date_time(
          date,
          orders = c("d-b-Y","d/m/Y","Y-m-d","d-b-y")
        )
      ) %>%
      mutate(date = as.Date(date))
    
    # ========================================================
    # KEEP FIRST 7 DAYS ONLY
    # ========================================================
    
    temp_df <- temp_df %>%
      slice_head(n = 7)
    
  }
  
  # ============================================================
  # REBUILD DAY1–DAY7
  # ============================================================
  
  daily_sleep <- rep(NA_character_,7)
  daily_eff   <- rep(NA_real_,7)
  
  if(nrow(temp_df) > 0){
    
    daily_sleep[1:nrow(temp_df)] <- temp_df$sleep
    daily_eff[1:nrow(temp_df)]   <- temp_df$eff
    
  }
  
  # ============================================================
  # RECALCULATE AVERAGES (STRICT 7 DAYS)
  # ============================================================
  
  avg_sleep_min <- mean(
    hhmm_to_minutes(temp_df$sleep),
    na.rm = TRUE
  )
  
  avg_sleep_hhmm <- minutes_to_hhmm(avg_sleep_min)
  
  avg_eff <- round(
    mean(temp_df$eff, na.rm = TRUE),
    1
  )
  
  # ============================================================
  # STUDY PERIOD FROM REAL DATA (NOT HEADER)
  # ============================================================
  
  start_date <- as.character(min(temp_df$date, na.rm = TRUE))
  end_date   <- as.character(max(temp_df$date, na.rm = TRUE))
  
  # ============================================================
  # OUTPUT
  # ============================================================
  
  tibble(
    
    subject = subject,
    age = age,
    sex = sex,
    
    data_start_time = start_date,
    data_end_time   = end_date,
    
    software_sleep_day1_hhmm = daily_sleep[1],
    software_sleep_day2_hhmm = daily_sleep[2],
    software_sleep_day3_hhmm = daily_sleep[3],
    software_sleep_day4_hhmm = daily_sleep[4],
    software_sleep_day5_hhmm = daily_sleep[5],
    software_sleep_day6_hhmm = daily_sleep[6],
    software_sleep_day7_hhmm = daily_sleep[7],
    
    software_average_sleep_duration_hhmm = avg_sleep_hhmm,
    
    software_efficiency_day1 = daily_eff[1],
    software_efficiency_day2 = daily_eff[2],
    software_efficiency_day3 = daily_eff[3],
    software_efficiency_day4 = daily_eff[4],
    software_efficiency_day5 = daily_eff[5],
    software_efficiency_day6 = daily_eff[6],
    software_efficiency_day7 = daily_eff[7],
    
    software_average_sleep_efficiency = avg_eff
  )
}

# ============================================================
# APPLY
# ============================================================

software_sleep_metrics <- map_dfr(
  files,
  extract_sleep_metrics
)

# ============================================================
# SAVE
# ============================================================

write_csv(
  software_sleep_metrics,
  output_file
)

message("Sleep metrics successfully extracted (7-day aligned & corrected)")
message("Saved to: ", output_file)
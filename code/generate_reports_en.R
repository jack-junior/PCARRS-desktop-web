# =====================================================
# 1. CONFIGURATION AND LOADING
# =====================================================
library(tidyverse)
library(officer)
library(flextable)
library(here)
library(readxl)


cfg <- yaml::read_yaml("config.yml")

path_data  <- file.path(cfg$paths$summaries, "geneactiv_combined_metrics.csv")
path_demo  <- file.path(cfg$paths$raw_data, "Demo_Participant_Detail.xlsx") # Vérifie l'extension .xlsx ou .csv

path_img   <- "resources/images"
path_logo1 <- file.path(path_img, "logo1.png")
path_logo2 <- file.path(path_img, "logo2.png")


# Color Palette
col_primary   <- "#003366" # Deep Navy
col_secondary <- "#008080" # Teal
col_light_bg  <- "#F2F2F2" # Light Grey

# Typography Styles
fp_title  <- fp_text(font.size = 22, bold = TRUE, color = col_primary)
fp_bold   <- fp_text(bold = TRUE, color = col_primary)
fp_italic <- fp_text(font.size = 9, italic = TRUE, color = "#666666")
fp_line   <- fp_text(color = "#CCCCCC", bold = TRUE)
fp_subtitle <- fp_text(font.size = 16, bold = TRUE, color = col_primary)

# =====================================================
# 1.a DATA PREPARATION
# =====================================================
data_metrics <- read_csv(path_data, show_col_types = FALSE) %>%
  mutate(subject = as.character(subject))

data_demo <- read_excel(here("data", "Demo_Participant_Detail.xlsx")) %>%
  mutate(intnl_test_id = as.character(intnl_test_id))

data_full <- data_metrics %>%
  left_join(data_demo, by = c("subject" = "intnl_test_id"))

# =====================================================
# 2. UTILITY FUNCTIONS
# =====================================================

# Function to convert HH:MM:SS to "HH:MM (X.X h)"
format_duration <- function(x) {
  if (is.na(x) || x == "" || x == "0") return("-")
  parts <- as.numeric(strsplit(as.character(x), ":")[[1]])
  if(length(parts) < 2) return(x)

  #hours_dec <- round(parts[1] + (parts[2]/60), 1)

  # Reconstruct HH:MM format
  hhmm <- sprintf("%02d:%02d", parts[1], parts[2])

  return(paste(hhmm, "hours"))
}

# Simple decimal converter for charts
convert_to_hours_dec <- function(x) {
  if (is.na(x) || x == "") return(0)
  parts <- as.numeric(strsplit(as.character(x), ":")[[1]])
  if(length(parts) < 2) return(0)
  return(round(parts[1] + (parts[2]/60), 1))
}

# =====================================================
# 3. REPORT GENERATION FUNCTION
# =====================================================
generate_officer_report <- function(pid, data_full, template_path) {

  # Filter data for the specific participant
  person <- data_full %>% filter(subject == pid)

  # --- LOGOS PREPARATION ---
  img1 <- external_img(src = path_logo1, width = 0.6, height = 0.7)
  img2 <- external_img(src = path_logo2, width = 1.3, height = 0.7)

  # Header properties
  header_prop <- fp_par(
    text.align = "left",
    padding.bottom = 20,
    tabs = fp_tabs(fp_tab(pos = 6.8, style = "right"))
  )
  header_fp <- fpar(img1, ftext("\t"), img2, fp_p = header_prop)

  # --- DATA EXTRACTION ---
  steps_vec <- as.numeric(c(person$stepday1, person$stepday2, person$stepday3,
                            person$stepday4, person$stepday5, person$stepday6, person$stepday7))
  avg_steps <- round(person$avg_daily_steps_7days[1])

  sleep_hhmm_list <- c(person$software_sleep_day1_hhmm, person$software_sleep_day2_hhmm,
                       person$software_sleep_day3_hhmm, person$software_sleep_day4_hhmm,
                       person$software_sleep_day5_hhmm, person$software_sleep_day6_hhmm,
                       person$software_sleep_day7_hhmm)
  sleep_vec <- sapply(sleep_hhmm_list, convert_to_hours_dec)

  # --- FORMAT DATES ---
  start_dt <- format(as.Date(person$data_start_time[1]), "%d %b %Y")
  end_dt   <- format(as.Date(person$data_end_time[1]), "%d %b %Y")
  period_txt <- paste(start_dt, "to", end_dt)

  # --- TABLE 1: PARTICIPANT INFO (Zebra + Traits verticaux) ---
  info_df <- data.frame(
    Var = c("Household ID:", "Deidentified Participant ID:", "CEB Code:", "Name:", "Age:", "Gender:", "Study Period:"),
    Val = c(
      ifelse(is.na(person$hhp_id[1]), "-", as.character(person$hhp_id[1])),
      as.character(person$subject[1]),
      ifelse(is.na(person$ceb_code[1]), "-", as.character(person$ceb_code[1])),
      ifelse(is.na(person$part_name[1]), "N/A", as.character(person$part_name[1])), # Le nom vient du merge
      as.character(person$age[1]),
      as.character(person$sex[1]),
      period_txt
    )
  )

  ft_info <- flextable(info_df) %>%
    delete_part(part = "header") %>%
    bold(j = 1, part = "body") %>%
    color(j = 1, color = col_primary, part = "body") %>%

    theme_zebra(odd_body = "#E6F0FA", even_body = "transparent") %>%
    fontsize(size = 10, part = "body") %>%

    border_outer(border = fp_border(color = col_primary, width = 1.5)) %>%
    border_inner_h(border = fp_border(color = col_primary, width = 0.5)) %>%
    border_inner_v(border = fp_border(color = col_primary, width = 0.5)) %>%

    set_table_properties(layout = "autofit", width = 1)



  # --- TABLE 2: DASHBOARD ---
  dash_df <- data.frame(
    Icon = c("steps", "inactive", "sleep", "efficiency"),
    Metric = c("Daily Steps", "Daily Inactive time", "Nightly sleep duration", "Sleep efficiency"),
    Value = c(
      paste(format(avg_steps, big.mark = ","), "steps"),
      format_duration(person$inactive_duration_hhmm[1]),
      format_duration(person$software_average_sleep_duration_7days_hhmm[1]),
      paste0(person$software_average_sleep_efficiency_7days[1], " %")
    ),
    Interpretation = trimws(c(
      "Any amount of physical activity is better than none, and more is better. For people under 60, taking at least 10,000 steps per day is recommended. For people older than 60 years, please consult your healthcare provider regarding appropriate levels of physical activity.",
      "Sitting and being inactive for long periods of time can be unhealthy. It can increase the risk of heart disease, cancer, and type-2 diabetes. Sitting for 8 hours continuously is especially injurious to health. Limiting sedentary time and being physically active is good for health.",
      "Most adults function best with more than 7 hours of sleep per night.",
      "Generally, a sleep efficiency of less than 85% indicates poorer sleep. Increased time awake during the night may be an indicator of poor sleep quality."
    ))
  )

  ft_dash <- flextable(dash_df) %>%
    set_header_labels(Icon = "", Metric = "Behavior", Value = "Your Average", Interpretation = "Health Context") %>%
    bg(bg = col_primary, part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%

    compose(j = 1, value = as_paragraph(as_image(src = file.path(path_img, paste0(Icon, ".png")), width = 0.6, height = 0.6)), part = "body") %>%
    compose(j = 4, value = as_paragraph(
      as_chunk("Interpretation: ", props = fp_text(bold = TRUE, color = col_primary)),
      as_chunk(Interpretation)
    ), part = "body") %>%

    bold(j = 2, part = "body") %>%
    fontsize(size = 10, part = "all") %>%
    align(j = 1:3, align = "center", part = "all") %>%

    align(j = 4, align = "justify", part = "body") %>%
    valign(valign = "center", part = "all") %>%

    theme_zebra(
      odd_header = col_primary,
      odd_body = "#E6F0FA",
      even_body = "transparent"
    ) %>%

    border_inner_h(border = fp_border(color = col_primary, width = 0.5), part = "body") %>%
    border_outer(border = fp_border(color = col_primary, width = 1.5)) %>%
    border_inner_v(border = fp_border(color = col_primary, width = 0.5), part = "all") %>%
    width(j = 1, width = 1.2) %>% width(j = 2, width = 1.6) %>% width(j = 3, width = 1.4) %>% width(j = 4, width = 3.1) %>%
    border_outer(border = fp_border(color = col_primary, width = 1.5)) %>%
    padding(padding = 5, part = "all")


  # --- CHARTS ---
  p1 <- ggplot(data.frame(d = paste("Day", 1:7), s = steps_vec), aes(x = d, y = s)) +
    geom_col(fill = col_secondary, color = col_primary, width = 0.7) +
    theme_minimal() + labs(x = "Day of Study", y = "Daily Steps") +
    theme(panel.border = element_rect(color = col_primary, fill = NA), axis.title = element_text(face = "bold", color = col_primary))

  p2 <- ggplot(data.frame(n = paste("Night", 1:7), s = sleep_vec), aes(x = n, y = s)) +
    geom_col(fill = col_primary, color = col_primary, width = 0.7) +
    theme_minimal() + labs(x = "Night of Study", y = "Nightly Hours Sleep") +
    theme(panel.border = element_rect(color = col_primary, fill = NA), axis.title = element_text(face = "bold", color = col_primary))

  tmp1 <- tempfile(fileext = ".png"); ggsave(tmp1, p1, width=5, height=3.5, dpi=300)
  tmp2 <- tempfile(fileext = ".png"); ggsave(tmp2, p2, width=5, height=3.5, dpi=300)

  # --- CHART LAYOUTS ---
  layout_steps <- flextable(data.frame(A="", B="")) %>%
    delete_part(part = "header") %>% border_remove() %>%
    compose(j=1, i=1, value=as_paragraph(as_image(file.path(path_img, "steps.png"), width=1.2, height=1.2)), part = "body") %>%
    compose(j=2, i=1, value=as_paragraph(as_image(tmp1, width=4.5, height=3.2)), part = "body") %>%
    width(j=1, width=1.5) %>% width(j=2, width=4.5)

  layout_sleep <- flextable(data.frame(A="", B="")) %>%
    delete_part(part = "header") %>% border_remove() %>%
    compose(j=1, i=1, value=as_paragraph(as_image(file.path(path_img, "sleep.png"), width=1.2, height=1.2)), part = "body") %>%
    compose(j=2, i=1, value=as_paragraph(as_image(tmp2, width=4.5, height=3.2)), part = "body") %>%
    width(j=1, width=1.5) %>% width(j=2, width=4.5)


  # --- FINAL ASSEMBLY ---
  doc <- read_docx() %>%
    # Define section with high header and standard margins
    body_set_default_section(
      value = prop_section(
        page_mar = page_mar(header = 0.3, top = 0.6, bottom = 0.5, left = 1.0, right = 1.0),
        header_default = block_list(header_fp)
      )
    ) %>%

    # Centered Title and separator
    body_add_fpar(fpar(ftext("🏥 P-CARRS Health Behavior Report", prop = fp_title), fp_p = fp_par(text.align = "center"))) %>%

    body_add_fpar(fpar(ftext("__________________________________________________________________", prop = fp_line),
                       fp_p = fp_par(text.align = "center"))) %>%


    # Introduction
    body_add_fpar(fpar(
      ftext("Thank you for your participation in the Precision CARRS 7-day monitoring study!", prop = fp_bold),
      fp_p = fp_par(text.align = "center", padding.top = 10)
    )) %>%
    body_add_fpar(fpar(
      ftext("Here are your results. The GENEActiv wristband monitors and measures your movements. Based on your movements, we make estimates of the time you spend waking activity and sleep. This generally provides good estimates of your sleeping and that can help you know more about your activity and sleep patterns. The findings are presented as averages over the days you wore the GENEActiv wristband. Please contact us if you want further information.",
            prop = fp_text(font.size = 10.5)),
      fp_p = fp_par(
        text.align = "justify",
        padding.top = 5,
        padding.bottom = 10,
        line_spacing = 1.15
      )
    ))%>%

    # Participant Info
    body_add_fpar(fpar(ftext("👤 Participant Information", prop = fp_subtitle),
                       fp_p = fp_par(padding.top = 10, padding.bottom = 5))) %>%
    body_add_flextable(ft_info) %>%

    body_add_par("", style = "Normal") %>%
    body_add_fpar(fpar( ftext("📋 Health Behavior Report – Week Average", prop = fp_subtitle),
                        fp_p = fp_par(padding.top = 10, padding.bottom = 5))) %>%
    body_add_flextable(ft_dash) %>%
    body_add_par("", style = "Normal") %>%
    # body_add_break() %>%


    body_add_fpar(fpar(ftext("📊 Daily Charts", prop = fp_subtitle),
                       fp_p = fp_par(padding.top = 15, padding.bottom = 7))) %>%
    body_add_flextable(layout_steps) %>%
    body_add_fpar(fpar(ftext("This graph shows the number of steps you took over the days of the study.", prop = fp_text(bold = TRUE)),
                       fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10))) %>%
    body_add_par("", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_flextable(layout_sleep) %>%
    body_add_fpar(fpar(ftext("This graph shows the number of hours you slept each night.", prop = fp_text(bold = TRUE)),
                       fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10))) %>%

    body_add_par("", style = "Normal") %>%

    # Centered Bottom Separator Line
    body_add_fpar(fpar(ftext("__________________________________________________________________", prop = fp_line),
                       fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10))) %>%


    # Final Research Note
    body_add_fpar(fpar(ftext("Note: The activity and sleep information is preliminary and for research purpose. This is not a clinical assessment. Please contact us if you want further information.", prop = fp_italic), fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10)))
  
  
  report_out <- file.path(cfg$paths$summaries, "reports_English")
  if(!dir.exists(report_out)) dir.create(report_out, recursive = TRUE)
  
  final_path <- file.path(report_out, paste0("Report_English_", pid, ".docx"))
  print(doc, target = final_path)
  return(final_path)
}

# =====================================================
# 4. EXECUTION INDIVIDUAL REPORT GENERATION
# =====================================================
#data_full <- read_csv(path_data, show_col_types = FALSE)
if(!dir.exists(report_out)) dir.create(report_out, recursive = TRUE)

for(pid in unique(data_full$subject)) {
  tryCatch({
    generate_officer_report(pid, data_full)
    cat("Generated Report for PID:", pid, "✔\n")
  }, error = function(e) cat("Error for PID:", pid, "-", e$message, "\n"))
}

# =====================================================
# 5. COMBINED REPORT GENERATION
# =====================================================

report_files <- list.files(here("reports_English"), pattern = "Report_English_.*\\.docx", full.names = TRUE)

combined_doc <- read_docx() %>%
  body_set_default_section(
    value = prop_section(
      page_mar = page_mar(header = 0.3, top = 0.6, bottom = 0.5, left = 1.0, right = 1.0)
    )
  )

cat("--- Starting Combined Report Assembly ---\n")

for(i in seq_along(report_files)) {
  current_file <- report_files[i]

  tryCatch({
    combined_doc <- body_add_docx(combined_doc, src = current_file)

    if(i < length(report_files)) {
      combined_doc <- body_add_break(combined_doc)
    }

    cat("Merged:", basename(current_file), "[✔]\n")

  }, error = function(e) cat("Error merging file:", current_file, "-", e$message, "\n"))
}

final_combined_path <- here("reports_English", "Full_Study_Combined_Report.docx")
print(combined_doc, target = final_combined_path)

cat("--- Assembly Complete! ---\n")

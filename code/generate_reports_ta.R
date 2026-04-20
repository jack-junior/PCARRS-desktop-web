# =====================================================
# 1. CONFIGURATION AND LOADING
# =====================================================
library(tidyverse)
library(officer)
library(flextable)
library(here)
library(readxl)
library(doconv)
library(yaml) # Ajouté pour lire le config.yml

# --- SÉCURITÉ SHINY ---
if (!exists("target_id")) {
  stop("target_id is missing. This script must be run from the Shiny App.")
}

b_name <- if (exists("batch_name")) batch_name else ""

cfg <- yaml::read_yaml("config.yml")

# Chemins basés sur config.yml
if (b_name != "") {
  path_data <- file.path(cfg$paths$summaries, b_name, "geneactiv_combined_metrics.csv")
  report_out <- file.path(cfg$paths$reports, b_name, "reports_Tamil")
} else {
  path_data <- file.path(cfg$paths$summaries, "geneactiv_combined_metrics.csv")
  report_out <- file.path(cfg$paths$reports, "reports_Tamil")
}

path_demo <- cfg$paths$participant_files$info
path_img   <- "resources/images"
path_logo1 <- file.path(path_img, "logo1.png")
path_logo2 <- file.path(path_img, "mdrf-logo.png")

# Dossier de sortie automatique
#report_out <- file.path(cfg$paths$reports, "reports_Tamil")
if(!dir.exists(report_out)) dir.create(report_out, recursive = TRUE)



# Color Palette
col_primary   <- "#003366" # Deep Navy
col_secondary <- "#008080" # Teal
col_light_bg  <- "#F2F2F2" # Light Grey

# Typography Styles
fp_title  <- fp_text(font.size = 18, bold = TRUE, color = col_primary)
fp_bold   <- fp_text(bold = TRUE, color = col_primary)
fp_italic <- fp_text(font.size = 8, italic = TRUE, color = "#666666")
fp_line   <- fp_text(color = "#CCCCCC", bold = TRUE)
fp_subtitle <- fp_text(font.size = 11, bold = TRUE, color = col_primary)

# =====================================================
# 1.a DATA PREPARATION
# =====================================================
# Clean TARGET_ID
clean_target_id <- substr(trimws(target_id), 1, 7)

# --- DATA PREPARATION CIBLÉE ---
data_metrics <- read_csv(path_data, show_col_types = FALSE) %>%
  mutate(subject = as.character(subject)) %>%
  filter(subject == clean_target_id) # On utilise l'ID nettoyé ici

# Détection auto Excel ou CSV pour la démo
if(grepl("\\.xlsx$", path_demo)) {
  data_demo <- read_excel(path_demo)
} else {
  data_demo <- read_csv(path_demo, show_col_types = FALSE)
}

data_demo <- data_demo %>% 
  mutate(did = as.character(did)) %>%
  filter(did == clean_target_id) 

data_full <- data_metrics %>%
  select(-age) %>%
  left_join(data_demo, by = c("subject" = "did"))

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

  return(paste(hhmm,
               "மணி நேரம்"))
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
generate_officer_report <- function(pid, data_full) {
  

  # Filter data for the specific participant
  person <- data_full 
  
  if (is.null(person) || nrow(person) == 0) {
    stop(paste("No data found for participant:", pid))
  }
  

  # --- LOGOS PREPARATION ---
  img1 <- external_img(src = path_logo1, width = 0.6, height = 0.7)
  img2 <- external_img(src = path_logo2, width = 0.8, height = 0.8)

  # Header properties
  header_prop <- fp_par(
    text.align = "left",
    padding.bottom = 14,
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
  
  steps_vec[is.na(steps_vec)] <- 0
  sleep_vec[is.na(sleep_vec)] <- 0

  # --- FORMAT DATES ---
  start_dt <- format(as.Date(person$data_start_time[1]), "%d %b %Y")
  end_dt   <- format(as.Date(person$data_end_time[1]), "%d %b %Y")
  period_txt <- paste(start_dt, "முதல்",
                      end_dt , "வரை")

  # --- TABLE 1: PARTICIPANT INFO (Zebra + Traits verticaux) ---
  info_df <- data.frame(
    Var = c("குடும்ப அடையாள எண்:",
            "டி-அடையாளம் நீக்கப்பட்ட ஐடி:",
            "சிஇபி குறியீடு:",
            "பெயர்:",
            "வயது:",
            "பாலினம்:",
            "ஆய்வு காலம்:"),
    Val = c(
      ifelse(is.na(person$HHID[1]), "-", as.character(person$HHID[1])),
      as.character(person$subject[1]),
      ifelse(is.na(person$ceb_code[1]), "-", as.character(person$ceb_code[1])),
      ifelse(is.na(person$`Name of Participant`[1]), "N/A", as.character(person$Name_of_Participant[1])),
      #as.character(person$AGE[1]),
      # Dans info_df
      Age = ifelse(is.na(person$AGE[1]), "N/A", as.character(person$AGE[1])),
      case_when(
        as.character(person$sex[1]) == "Male"   ~ "ஆண்",
        as.character(person$sex[1]) == "Female" ~ "பெண்",
        TRUE                                    ~ as.character(person$sex[1])
      ),
      period_txt
    )
  )

  ft_info <- flextable(info_df) %>%
    delete_part(part = "header") %>%
    bold(j = 1, part = "body") %>%
    color(j = 1, color = col_primary, part = "body") %>%

    theme_zebra(odd_body = "#E6F0FA", even_body = "transparent") %>%
    fontsize(size = 8, part = "body") %>%

    border_outer(border = fp_border(color = col_primary, width = 1.5)) %>%
    border_inner_h(border = fp_border(color = col_primary, width = 0.5)) %>%
    border_inner_v(border = fp_border(color = col_primary, width = 0.5)) %>%

    set_table_properties(layout = "autofit", width = 1)



  # --- TABLE 2: DASHBOARD ---
  dash_df <- data.frame(
    Icon = c("steps", "inactive", "sleep", "efficiency"),
    Metric = c("ेதினசரி நடைகளின் எண்ணிக்கை",
               "தினசரி செயலற்ற நேரம்",
               "இரவு தூக்கத்தின் நேரம்",
               "தூக்க திறன்"),
    Value = c(
      paste(format(avg_steps, big.mark = ","), "நடைகள்"),
      format_duration(person$inactive_duration_hhmm[1]),
      format_duration(person$software_average_sleep_duration_7days_hhmm[1]),
      paste0(person$software_average_sleep_efficiency_7days[1], " %")
    ),
    Interpretation = trimws(c(
      "குறைந்த அளவு உடல் செயல்பாடு, எதுவும் செய்யாததை விட சிறந்தது மற்றும் அதீத உடல் செயல்பாடு மிகவும் சிறந்தது. 60 வயதிற்குட்பட்டவர்கள், ஒரு நாளைக்கு குறைந்தது 10000 நடைகள் நடக்க பரிந்துரைக்கப்படுகிறது. . 60 வயதிற்கு மேற்பட்டவர்கள், சரியான அளவிலான உடல் செயல்பாடுகள் குறித்து உங்கள் சுகாதார  ஆலோசகரை அணுகவும்.",
      "நீண்ட நேரம் உட்கார்ந்திருப்பது மற்றும் உடல் செயல்பாடு இல்லாமல் இருப்பது ஆரோக்கியமற்றதாக இருக்கும். இது இதய நோய், புற்றுநோய் மற்றும் நீரிழிவு நோய் அபாயத்தை அதிகரிக்கும். தொடர்ந்து 8 மணி நேரம் உட்கார்ந்திருப்பது உங்கள் உடல் ஆரோக்கியத்திற்கு  மிகவும் தீங்கு விளைவிக்கும்.உட்கார்ந்திருக்கும் நேரத்தை கட்டுபடுத்துவது மற்றும் சுறுசுறுப்பாக இருப்பது ஆரோக்கியத்திற்கு நல்லது.",
      "பெரும்பாலான மக்கள் இரவில் 7 மணி நேரத்திற்கும் மேலாக தூங்குவதன் மூலம் சிறப்பாக செயல்படுகிறார்கள்.",
      "பொதுவாக, 85%க்கும் குறைவான தூக்கத் திறனை, தூக்கமின்மையை
 குறிக்கிறது. இரவில் அதிக நேரம் விழித்திருப்பது, தூக்கத்தின் தரம் குறைவாக இருப்பதற்கான ஓர் அறிகுறியாக இருக்கலாம்.
"
    ))
  )

  ft_dash <- flextable(dash_df) %>%
    set_header_labels(Icon = "", Metric = "நடத்தை",
                      Value = "உங்கள் சராசரி",
                      Interpretation = "ஆரோக்கியம்") %>%
    bg(bg = col_primary, part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%

    #compose(j = 1, value = as_paragraph(as_image(src = here("data", "img", paste0(Icon, ".png")), width = 0.6, height = 0.6)), part = "body") %>%
    compose(j = 1, value = as_paragraph(as_image(src = file.path(path_img, paste0(Icon, ".png")), width = 0.6, height = 0.6)), part = "body") %>%
    compose(j = 4, value = as_paragraph(
      as_chunk("விளக்கம்:", props = fp_text(bold = TRUE, color = col_primary)),
      as_chunk(Interpretation)
    ), part = "body") %>%

    bold(j = 2, part = "body") %>%
    fontsize(size = 8, part = "all") %>%
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
    padding(padding = 4, part = "all")


  # --- CHARTS ---
  
  # plot_data_steps <- data.frame(d = paste("நாள்", 1:7), s = steps_vec) %>%
  #   filter(!is.na(s) & s > 0) 
  # 
  # p1 <- ggplot(plot_data_steps, aes(x = d, y = s)) +
  #   geom_col(fill = col_secondary, color = col_primary, width = 0.7) +
  
  p1 <- ggplot(data.frame(d = paste("நாள்", 1:7), s = steps_vec), aes(x = d, y = s)) +
    geom_col(fill = col_secondary, color = col_primary, width = 0.7) +
    theme_minimal() + labs(x = "ஆய்வு நாள்", y =  "தினசரி அடிகள் எண்ணிக்கை") +
    theme(panel.border = element_rect(color = col_primary, fill = NA), axis.title = element_text(face = "bold", color = col_primary))

  
  # plot_data_sleep <- data.frame(n = paste("இரவு", 1:7), s = sleep_vec) %>%
  #   filter(!is.na(s) & s > 0) 
  # 
  # p2 <- ggplot(plot_data_sleep, aes(x = n, y = s)) +
  #   geom_col(fill = col_primary, color = col_primary, width = 0.7) +
  
  p2 <- ggplot(data.frame(n = paste("இரவு", 1:7), s = sleep_vec), aes(x = n, y = s)) +
    geom_col(fill = col_primary, color = col_primary, width = 0.7) +
    theme_minimal() + labs(x = "ஆய்வு இரவு", y = "ஒரு இரவில் உறங்கிய மணிநேரங்கள்") +
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
        page_mar = page_mar(header = 0.3, top = 0.6, bottom = 0.3, left = 1.0, right = 1.0),
        header_default = block_list(header_fp)
      )
    ) %>%

    # Centered Title and separator
    body_add_fpar(fpar(ftext("🏥 பி-கார்ஸ் சுகாதார செயல்பாடு அறிக்கை", prop = fp_title), fp_p = fp_par(text.align = "center"))) %>%

    body_add_fpar(fpar(ftext("__________________________________________________________________", prop = fp_line),
                       fp_p = fp_par(text.align = "center"))) %>%


    # Introduction
    body_add_fpar(fpar(
      ftext("Precision CARRS 7-நாள் கண்காணிப்பு ஆய்வில் பங்கேற்றதற்கு நன்றி!  ", prop = fp_bold),
      fp_p = fp_par(text.align = "center", padding.top = 8)
    )) %>%
    body_add_fpar(fpar(
      ftext("ஜெனிஆக்டிவ்  சாதனம் உங்கள் அசைவுகளை கண்காணித்து மதிப்பீடு செய்கிறது. உங்கள் அசைவுகளின் அடிப்படையில், நீங்கள் விழித்திருக்கும் மற்றும் தூங்கும் நேரத்தை மதிப்பீடு செய்கிறது. இது பொதுவாக உங்கள் தூக்கத்தின் சரியான மதிப்பீடுகளை வழங்குகிறது. மேலும் இது உங்கள் உடல் செயல்பாடு மற்றும் தூக்க முறைகள் பற்றி மேலும் அறிய உதவுகிறது. நீங்கள் ஜெனிஆக்டிவ் கைகடிகாரத்தை அணிந்த நாட்களில் கண்டறியப்பட்ட தகவல்கள் சராசரியாக வழங்கப்படுகின்றன. உங்களுக்கு ஏதேனும் கேள்விகள் இருந்தால், எங்களை தொடர்பு கொள்ளவும். ",
            prop = fp_text(font.size = 9)),
      fp_p = fp_par(
        text.align = "justify",
        padding.top = 5,
        padding.bottom = 8,
        line_spacing = 1
      )
    ))%>%

    # Participant Info
    body_add_fpar(fpar(ftext("👤 பங்கேற்பாளர் தகவல்", prop = fp_subtitle),
                       fp_p = fp_par(padding.top = 10, padding.bottom = 5))) %>%
    body_add_flextable(ft_info) %>%

    body_add_fpar(fpar( ftext("📋 சுகாதார செயல்பாடு அறிக்கை - வார சராசரி", prop = fp_subtitle),
                        fp_p = fp_par(padding.top = 10, padding.bottom = 5))) %>%
    body_add_flextable(ft_dash) %>%
    body_add_par("", style = "Normal") %>%
    # body_add_break() %>%


    body_add_fpar(fpar(ftext("📊 தினசரி விளக்கப்படம்", prop = fp_subtitle),
                       fp_p = fp_par(padding.top = 7, padding.bottom = 3))) %>%
    body_add_flextable(layout_steps) %>%
    body_add_fpar(
      fpar(
        ftext("இந்த வரைபடம், ஆய்வு நாட்களில் நீங்கள் நடந்த நடைகளின் எண்ணிக்கையைக் காட்டுகிறது.",
              prop = fp_text(bold = TRUE)),

        fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10)
      )
    )%>%
    body_add_par("", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_flextable(layout_sleep) %>%
    body_add_fpar(
      fpar(
        ftext("இந்த வரைபடம் நீங்கள் ஒவ்வொரு இரவும் உறங்கிய மணிநேரங்களைக் காட்டுகிறது",
              prop = fp_text(bold = TRUE)),
        fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 15)
      )
    )%>%


    # Centered Bottom Separator Line
    body_add_fpar(fpar(ftext("__________________________________________________________________", prop = fp_line),
                       fp_p = fp_par(text.align = "center"))) %>%


    body_add_par("", style = "Normal") %>%

    # Final Research Note
    body_add_fpar(fpar(ftext("குறிப்பு : இந்த அறிக்கை உடல் செயல்பாடு மற்றும் தூக்கத்தை பற்றிய முதற் கட்ட தகவல்கள் ஆராய்ச்சி நோக்கத்திற்கானது. இது மருத்துவ மதிப்பீடு அல்ல. தகவல் ஏதேனும் வேண்டுமானால் தயவு செய்து எங்களை தொடர்பு கொள்ளவும்.", prop = fp_italic), fp_p = fp_par(text.align = "center")))

  #report_out <- file.path(cfg$paths$reports, "reports_Tamil")
  #if(!dir.exists(report_out)) dir.create(report_out, recursive = TRUE)
  
  # --- SAVE AND EXPORT ---
  docx_path <- file.path(report_out, paste0("Report_Tamil_", pid, ".docx"))
  pdf_path  <- file.path(report_out, paste0("Report_Tamil_", pid, ".pdf"))
  
  # Sauvegarde du Word
  print(doc, target = docx_path)
  
  # Conversion en PDF
  tryCatch({
    doconv::to_pdf(docx_path, output = pdf_path)
  }, error = function(e) {
    message("PDF conversion failed for ", pid, ": ", e$message)
  })
  
  return(pdf_path)
}

# =====================================================
# 4. EXECUTION
# =====================================================
cat("--- Generating Individual Report (TAMIL) for ID:", clean_target_id, "---\n") # Changé ici

person_data <- data_full %>% filter(subject == clean_target_id)

if(nrow(person_data) > 0) {
  docx_final <- generate_officer_report(clean_target_id, person_data)
  cat("✅ Individual Tamil Report Done for:", clean_target_id, "\n") # Changé ici
} else {
  stop("ID not found in combined metrics or participant info (Tamil).") # Changé ici
}
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

cfg <- yaml::read_yaml("config.yml")

# Chemins basés sur config.yml
path_data  <- file.path(cfg$paths$summaries, "geneactiv_combined_metrics.csv")
path_demo <- cfg$paths$participant_files$hi
path_img   <- "resources/images"
path_logo1 <- file.path(path_img, "logo1.png")
path_logo2 <- file.path(path_img, "logo2.png")

# Dossier de sortie automatique
report_out <- file.path(cfg$paths$reports, "reports_Hindi")
if(!dir.exists(report_out)) dir.create(report_out, recursive = TRUE)


# Color Palette
col_primary   <- "#003366" # Deep Navy
col_secondary <- "#008080" # Teal
col_light_bg  <- "#F2F2F2" # Light Grey

# Typography Styles
fp_title  <- fp_text(font.size = 20, bold = TRUE, color = col_primary)
fp_bold   <- fp_text(bold = TRUE, color = col_primary)
fp_italic <- fp_text(font.size = 9, italic = TRUE, color = "#666666")
fp_line   <- fp_text(color = "#CCCCCC", bold = TRUE)
fp_subtitle <- fp_text(font.size = 14, bold = TRUE, color = col_primary)

# =====================================================
# 1.a DATA PREPARATION
# =====================================================
# Clean TARGET_ID
clean_target_id <- substr(trimws(target_id), 1, 6)

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
  mutate(intnl_test_id = as.character(intnl_test_id)) %>%
  filter(intnl_test_id == clean_target_id) 

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

  return(paste(hhmm, "घंटे"))
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
  
  steps_vec[is.na(steps_vec)] <- 0
  sleep_vec[is.na(sleep_vec)] <- 0

  # --- FORMAT DATES ---
  start_dt <- format(as.Date(person$data_start_time[1]), "%d %b %Y")
  end_dt   <- format(as.Date(person$data_end_time[1]), "%d %b %Y")
  period_txt <- paste(start_dt, "से",
                      end_dt , "तक")

  # --- TABLE 1: PARTICIPANT INFO (Zebra + Traits verticaux) ---
  info_df <- data.frame(
    Var = c("हाउसहोल्ड आईडी:",
            "डी-आइडेंटिफाइड आईडी:",
            "सीइबी कोड:",
            "नाम:",
            "उम्र:",
            "लिंग:",
            "अध्ययन की अवधि:"),
    Val = c(
      ifelse(is.na(person$hhp_id[1]), "-", as.character(person$hhp_id[1])),
      as.character(person$subject[1]),
      ifelse(is.na(person$ceb_code[1]), "-", as.character(person$ceb_code[1])),
      ifelse(is.na(person$part_name[1]), "N/A", as.character(person$part_name[1])), # Le nom vient du merge
      as.character(person$age[1]),
      case_when(
        as.character(person$sex[1]) == "Male"   ~ "पुरुष",
        as.character(person$sex[1]) == "Female" ~ "महिला",
        TRUE                                    ~ as.character(person$sex[1]) # Garde la valeur d'origine si NA ou autre
      ),
      period_txt
    )
  )

  ft_info <- flextable(info_df) %>%
    delete_part(part = "header") %>%
    bold(j = 1, part = "body") %>%
    color(j = 1, color = col_primary, part = "body") %>%

    theme_zebra(odd_body = "#E6F0FA", even_body = "transparent") %>%
    fontsize(size = 9, part = "body") %>%

    border_outer(border = fp_border(color = col_primary, width = 1.5)) %>%
    border_inner_h(border = fp_border(color = col_primary, width = 0.5)) %>%
    border_inner_v(border = fp_border(color = col_primary, width = 0.5)) %>%

    set_table_properties(layout = "autofit", width = 1)



  # --- TABLE 2: DASHBOARD ---
  dash_df <- data.frame(
    Icon = c("steps", "inactive", "sleep", "efficiency"),
    Metric = c("रोज़ाना चले जाने वाले कदम",
               "दिनभर में बिना गतिविधि का समय",
               "रात की नींद का समय",
               "नींद की दक्षता"),
    Value = c(
      paste(format(avg_steps, big.mark = ","), "कदम"),
      format_duration(person$inactive_duration_hhmm[1]),
      format_duration(person$software_average_sleep_duration_7days_hhmm[1]),
      paste0(person$software_average_sleep_efficiency_7days[1], " %")
    ),
    Interpretation = trimws(c(
      "थोड़ी बहुत शारीरिक गतिविधि करना,किसी भी तरह की शारीरिक गतिविधि न करने से तो बेहतर है । 60 साल से कम उम्र के लोगों के लिए, रोज़ाना कम से कम 10,000 कदम चलने की सलाह दी जाती है। 60 साल से ज़्यादा उम्र के लोग, अपने लिए सही शारीरिक गतिविधि के बारे में कृपया अपने डॉक्टर से सलाह लें।",
      "लंबे समय तक बैठे रहना और सक्रिय न होना सेहत के लिए हानिकारक हो सकता है। इससे दिल की बीमारी, कैंसर और टाइप-2 मधुमेह (डायबिटीज) का खतरा बढ़ सकता है। लगातार 8 घंटे तक बैठे रहना स्वास्थ्य के लिए विशेष रूप से नुकसानदेह है। बैठने के समय को कम करना और शारीरिक रूप से सक्रिय रहना सेहत के लिए अच्छा है।",
      "ज़्यादातर वयस्क लोग रात में 7 घंटे से अधिक सोने पर सबसे अच्छी तरह काम कर पाते हैं।",
      "आमतौर पर, 85% से कम नींद की दक्षता खराब नींद का संकेत देती है। रात के दौरान जागने का समय बढ़ना खराब नींद की गुणवत्ता का सूचक हो सकता है।"
    ))
  )

  ft_dash <- flextable(dash_df) %>%
    set_header_labels(Icon = "", Metric = "व्यवहार",
                      Value = "आपका औसत",
                      Interpretation = "स्वास्थ्य संदर्भ") %>%
    bg(bg = col_primary, part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%

    #compose(j = 1, value = as_paragraph(as_image(src = here("data", "img", paste0(Icon, ".png")), width = 0.6, height = 0.6)), part = "body") %>%
    compose(j = 1, value = as_paragraph(as_image(src = file.path(path_img, paste0(Icon, ".png")), width = 0.6, height = 0.6)), part = "body") %>%
    compose(j = 4, value = as_paragraph(
      as_chunk("व्याख्या: ", props = fp_text(bold = TRUE, color = col_primary)),
      as_chunk(Interpretation)
    ), part = "body") %>%

    bold(j = 2, part = "body") %>%
    fontsize(size = 9, part = "all") %>%
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
  p1 <- ggplot(data.frame(d = paste("दिन", 1:7), s = steps_vec), aes(x = d, y = s)) +
    geom_col(fill = col_secondary, color = col_primary, width = 0.7) +
    theme_minimal() + labs(x = "अध्ययन का दिन", y =  "दैनिक कदमs") +
    theme(panel.border = element_rect(color = col_primary, fill = NA), axis.title = element_text(face = "bold", color = col_primary))

  p2 <- ggplot(data.frame(n = paste("रात", 1:7), s = sleep_vec), aes(x = n, y = s)) +
    geom_col(fill = col_primary, color = col_primary, width = 0.7) +
    theme_minimal() + labs(x = "अध्ययन की रात", y = "रात की नींद ") +
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
    body_add_fpar(fpar(ftext("🏥 पी-कार्स स्वास्थ्य व्यवहार रिपोर्ट", prop = fp_title), fp_p = fp_par(text.align = "center"))) %>%

    body_add_fpar(fpar(ftext("__________________________________________________________________", prop = fp_line),
                       fp_p = fp_par(text.align = "center"))) %>%


    # Introduction
    body_add_fpar(fpar(
      ftext("प्रीसिजन कार्स के 7-दिवसीय निगरानी अध्ययन में भाग लेने के लिए आपका धन्यवाद!  ", prop = fp_bold),
      fp_p = fp_par(text.align = "center", padding.top = 10)
    )) %>%
        body_add_fpar(fpar(
      ftext("यहाँ आपके परिणाम दिए गए हैं। जिनिअक्टिव रिस्टबैंड आपकी गतिविधियों पर नज़र रखता है और उन्हें मापता है। आपकी गतिविधियों के आधार पर, हम आपके जागने और सोने के समय का अनुमान लगाते हैं। यह आमतौर पर आपकी नींद का सही अनुमान देता है, जिससे आपको अपनी एक्टिविटी और सोने के तरीकों (पैटर्न) के बारे में बेहतर जानकारी मिल सकती है। ये नतीजे उन दिनों के औसत पर आधारित हैं जब आपने जिनिअक्टिव रिस्टबैंड पहना था। आपको प्रश्न पूछना है, तो कृपया हमसे पर संपर्क करें।",
            prop = fp_text(font.size = 10.5)),
      fp_p = fp_par(
        text.align = "justify",
        padding.top = 5,
        padding.bottom = 10,
        line_spacing = 1.15
      )
    ))%>%

    # Participant Info
    body_add_fpar(fpar(ftext("👤 प्रतिभागी की जानकारी", prop = fp_subtitle),
                       fp_p = fp_par(padding.top = 10, padding.bottom = 5))) %>%
    body_add_flextable(ft_info) %>%

    body_add_par("", style = "Normal") %>%
    body_add_fpar(fpar( ftext("📋 स्वास्थ्य व्यवहार रिपोर्ट – साप्ताहिक औसत", prop = fp_subtitle),
                        fp_p = fp_par(padding.top = 10, padding.bottom = 5))) %>%
    body_add_flextable(ft_dash) %>%
    body_add_par("", style = "Normal") %>%
   # body_add_break() %>%


    body_add_fpar(fpar(ftext("📊 दैनिक चार्ट", prop = fp_subtitle),
                       fp_p = fp_par(padding.top = 15, padding.bottom = 7))) %>%
    body_add_flextable(layout_steps) %>%
    body_add_fpar(
      fpar(
        ftext("यह ग्राफ अध्ययन के दिनों के दौरान आपके द्वारा चले गए कदमों की संख्या को दर्शाता है।",
              prop = fp_text(bold = TRUE)),

        fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10)
      )
    )%>%
    body_add_par("", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_flextable(layout_sleep) %>%
    body_add_fpar(
      fpar(
        ftext("यह ग्राफ दिखाता है कि आप हर रात कितने घंटे सोते हैं।को दर्शाता है।",
              prop = fp_text(bold = TRUE)),
        fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10)
      )
    )%>%

    body_add_par("", style = "Normal") %>%

    # Centered Bottom Separator Line
    body_add_fpar(fpar(ftext("__________________________________________________________________", prop = fp_line),
                       fp_p = fp_par(text.align = "center", padding.top = 10, padding.bottom = 10))) %>%



    # Final Research Note
    body_add_fpar(fpar(ftext("नोट: गतिविधि और नींद की यह जानकारी शुरुआती है और केवल शोध के उद्देश्य से है। यह कोई क्लिनिकल जांच (डॉक्टरी रिपोर्ट) नहीं है। यदि आप और अधिक जानकारी चाहते हैं, तो कृपया हमसे संपर्क करें।", prop = fp_italic), fp_p = fp_par(text.align = "center")))

  report_out <- file.path(cfg$paths$reports, "reports_Hindi")
  if(!dir.exists(report_out)) dir.create(report_out, recursive = TRUE)
  
  # --- SAVE AND EXPORT ---
  docx_path <- file.path(report_out, paste0("Report_Hindi_", pid, ".docx"))
  pdf_path  <- file.path(report_out, paste0("Report_Hindi_", pid, ".pdf"))
  
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
cat("--- Generating Individual Report (HINDI) for ID:", clean_target_id, "---\n")

person_data <- data_full %>% filter(subject == clean_target_id)

if(nrow(person_data) > 0) {
  # Note: on passe person_data (qui est déjà filtré) à la fonction
  docx_final <- generate_officer_report(clean_target_id, person_data)
  cat("✅ Individual Hindi Report Done for:", clean_target_id, "\n")
} else {
  stop("ID not found in combined metrics or participant info (Hindi).")
}
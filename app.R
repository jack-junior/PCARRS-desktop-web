local_lib <- "resources/R_libs"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)

.libPaths(c(local_lib, .libPaths()))


library(shiny)
library(bslib)
library(httr)
library(shinyFiles) 
library(yaml)       
library(tidyverse) 

# --- 1. AUTO-INSTALLER R (Vers dossier local) ---
required_packages <- c("shiny", "bslib", "httr", "shinyFiles", "yaml", "tidyverse", "GGIR", "ActCR", "actilifecounts")

# Vérifier ce qui manque dans la librairie locale
missing_pkgs <- required_packages[!(required_packages %in% installed.packages(lib.loc=local_lib)[, "Package"])]

if (length(missing_pkgs) > 0) {
  message("Installing R packages to local library...")
  install.packages(missing_pkgs, lib = local_lib, repos = "https://cloud.r-project.org")
}

# --- 2. AUTO-INSTALLER PYTHON (Portable) ---
py_env_path <- "resources/python_env"

setup_python_env <- function() {
  if (!dir.exists(py_env_path)) {
    message("Creating Python Virtual Environment...")
    if (!dir.exists("resources")) dir.create("resources", showWarnings = FALSE)
    
    # Create the environment
    system(paste("python -m venv", py_env_path))
    
    # Path to pip based on OS
    pip_path <- if (Sys.info()['sysname'] == "Windows") {
      file.path(py_env_path, "Scripts", "pip.exe")
    } else {
      file.path(py_env_path, "bin", "pip")
    }
    # Install stepcount and PyYAML
    system(paste(shQuote(pip_path), "install stepcount PyYAML"))
  }
}

setup_python_env()

# Load the updater logic
if (file.exists("updater.R")) source("updater.R")

ui <- page_navbar(
  title = "GeneActiv Analysis Suite",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("System Setup",
            layout_column_wrap(
              card(
                card_header("Pipeline & Resource Update"),
                p("Click below to sync the latest analysis scripts, logos, and report templates."),
                actionButton("btn_update", "Update System from GitHub", 
                             icon = icon("github"), class = "btn-warning"),
                span(textOutput("update_time"), style = "font-size: 0.8em; color: gray; margin-top: 10px;")
              ),
              card(
                card_header("System Status"),
                verbatimTextOutput("sys_status"),
                actionButton("btn_check_sys", "Check Dependencies", class = "btn-secondary")
              )
            )
  ),
  
  nav_panel("Data Selection",
            card(
              card_header("Folder Configuration"),
              p("Select the local folders for this analysis:"),
              layout_column_wrap(
                width = 1/3,
                div(
                  tags$label("1. Raw .bin Files"),
                  shinyDirButton("dir_bin", "Browse...", "Select Folder containing .bin files"),
                  verbatimTextOutput("path_bin", placeholder = TRUE)
                ),
                div(
                  tags$label("2. Software Sleep Reports"),
                  shinyDirButton("dir_sleep", "Browse...", "Select Sleep Reports folder"),
                  verbatimTextOutput("path_sleep", placeholder = TRUE)
                ),
                div(
                  tags$label("3. Participant Info Folder"),
                  shinyDirButton("dir_participants", "Browse...", "Select folder containing EN/TA/HI CSVs"),
                  verbatimTextOutput("path_participants", placeholder = TRUE)
                )
              ),
              hr(),
              actionButton("btn_save_config", "Save All Configurations", class = "btn-success w-100")
            )
  ),
  
  nav_panel("Processing",
            card(
              card_header("Pipeline Execution"),
              radioButtons("run_mode", "Select Run Mode:",
                           choices = c("Full Folder (Skip existing)" = "full", 
                                       "Specific Participant IDs" = "selective"),
                           selected = "full"),
              
              conditionalPanel(
                condition = "input.run_mode == 'selective'",
                textInput("selected_ids", "Enter Participant IDs (separated by comma):", 
                          placeholder = "e.g. 1001, 1002, 1005")
              ),
              
              actionButton("run_full_pipeline", "START ANALYSIS", 
                           class = "btn-success btn-lg", icon = icon("play")),
              hr(),
              tags$style("#log_console { background: #222; color: #0f0; font-family: monospace; height: 400px; overflow-y: scroll; }"),
              verbatimTextOutput("log_console")
            )
  ),
  
  nav_panel("Reports",
            card(
              card_header("Generate Multi-language Reports"),
              selectInput("report_lang", "Select Language", 
                          choices = c("English", "Tamil", "Hindi")),
              actionButton("gen_report", "Generate Final Reports", class = "btn-info")
            )
  )
)

server <- function(input, output, session) {
  
  roots <- getVolumes()()
  shinyDirChoose(input, "dir_bin", roots = roots)
  shinyDirChoose(input, "dir_sleep", roots = roots)
  shinyDirChoose(input, "dir_participants", roots = roots)
  
  output$path_bin <- renderText({ if (is.integer(input$dir_bin)) "Not selected" else parseDirPath(roots, input$dir_bin) })
  output$path_sleep <- renderText({ if (is.integer(input$dir_sleep)) "Not selected" else parseDirPath(roots, input$dir_sleep) })
  output$path_participants <- renderText({ if (is.integer(input$dir_participants)) "Not selected" else parseDirPath(roots, input$dir_participants) })
  
  observeEvent(input$btn_save_config, {
    part_dir <- if (is.integer(input$dir_participants)) "" else parseDirPath(roots, input$dir_participants)
    
    config_data <- list(
      paths = list(
        raw_bin = if (is.integer(input$dir_bin)) "" else parseDirPath(roots, input$dir_bin), # Renamed for script compatibility
        sleep_data = if (is.integer(input$dir_sleep)) "" else parseDirPath(roots, input$dir_sleep),
        participant_folder = part_dir,
        participant_files = list(
          en = file.path(part_dir, "participants_en.csv"),
          ta = file.path(part_dir, "participants_ta.csv"),
          hi = file.path(part_dir, "participants_hi.csv")
        ),
        img_folder = "resources/images",
        summaries = "summaries",
        ggir_output = "GGIR"
      )
    )
    write_yaml(config_data, "config.yml")
    if(!dir.exists("summaries")) dir.create("summaries")
    if(!dir.exists("GGIR")) dir.create("GGIR")
    showNotification("Configuration Saved!", type = "message")
  })
  
  observeEvent(input$btn_update, {
    showNotification("Connecting to GitHub...", type = "message")
    if (check_and_update()) {
      showModal(modalDialog(title = "Update Success", "System is up to date. Please restart app to apply core changes.", easyClose = TRUE))
      output$update_time <- renderText({ paste("Last update:", Sys.time()) })
    }
  })
  
  observeEvent(input$run_full_pipeline, {
    withProgress(message = 'Starting Pipeline...', value = 0, {
      
      selected_ids_vector <<- NULL
      if (input$run_mode == "selective" && input$selected_ids != "") {
        selected_ids_vector <<- trimws(unlist(strsplit(input$selected_ids, ",")))
      }
      
      all_logs <- c("--- STARTING PIPELINE ---")
      
      # 1. GGIR
      setProgress(0.1, detail = "Step 1: GGIR (R)...")
      # Capture R output
      ggir_out <- capture.output({ source("code/run_GGIR.R", local = FALSE) })
      all_logs <- c(all_logs, ">> GGIR Log:", ggir_out)
      
      # 2. Python
      setProgress(0.3, detail = "Step 2: Steps (Python)...")
      local_python <- if (Sys.info()['sysname'] == "Windows") {
        file.path("resources", "python_env", "Scripts", "python.exe")
      } else {
        file.path("resources", "python_env", "bin", "python")
      }
      
      ids_arg <- if(!is.null(selected_ids_vector)) paste0("--ids ", shQuote(input$selected_ids)) else ""
      
      # BUG FIX: Added 'intern = TRUE' to capture output and assigned to py_out
      py_out <- system(paste(shQuote(local_python), "code/get_steps.py", ids_arg), intern = TRUE)
      all_logs <- c(all_logs, ">> Python Log:", py_out)
      
      # 3-7. R Merging Scripts
      setProgress(0.5, detail = "Step 3-5: Merging Data...")
      source("code/extract_step_metrics.R", local = FALSE)
      source("code/extract_activity_metrics.R", local = FALSE)
      
      # Ensure combined data is handled
      activity <- read_csv("summaries/activity_metrics.csv")
      steps    <- read_csv("summaries/step_metrics.csv")
      combined <- left_join(steps, activity, by = c("subject", "calendar_date"))
      write_csv(combined, "summaries/activity_metrics.csv")
      
      setProgress(0.8, detail = "Step 6-7: Finalizing...")
      source("code/person_summary.R", local = FALSE)
      source("code/extract_software_sleep.R", local = FALSE)
      source("code/combine_geneactiv_metrics.R", local = FALSE)
      
      all_logs <- c(all_logs, "--- PIPELINE FINISHED ---")
      output$log_console <- renderPrint({ cat(paste(all_logs, collapse="\n")) })
      setProgress(1)
    })
    showNotification("Full Pipeline Completed!", type = "message")
  })
  
  observeEvent(input$gen_report, {
    selected_lang <- input$report_lang 
    withProgress(message = paste("Generating", selected_lang, "Reports..."), value = 0.5, {
      if (selected_lang == "English") source("code/generate_reports_en.R", local = FALSE)
      else if (selected_lang == "Tamil") source("code/generate_reports_ta.R", local = FALSE)
      else if (selected_lang == "Hindi") source("code/generate_reports_hi.R", local = FALSE)
      setProgress(1)
    })
    showNotification("Reports Generated!", type = "message")
  })
}

shinyApp(ui, server)
# --- Gestion des chemins de librairie locale ---
local_lib <- "resources/R_libs"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))

# --- Chargement des bibliothèques de base ---
library(shiny)
library(bslib)
library(httr)
library(shinyFiles) 
library(yaml)        
library(tidyverse) 
library(shinyWidgets) 

message("--- Checking R Dependencies ---")

# --- 1. AUTO-INSTALLER R (Vers dossier local) ---
required_packages <- c(
  "shiny", "bslib", "httr", "shinyFiles", "yaml", 
  "tidyverse", "GGIR", "ActCR", "actilifecounts", 
  "shinyWidgets", "shinycssloaders"
)

installed_pkgs <- installed.packages(lib.loc=local_lib)[, "Package"]
missing_pkgs <- required_packages[!(required_packages %in% installed_pkgs)]

if (length(missing_pkgs) > 0) {
  message(">>> Installing missing R packages to local library: ", paste(missing_pkgs, collapse=", "))
  install.packages(missing_pkgs, lib = local_lib, repos = "https://cloud.r-project.org")
  message(">>> R packages installed successfully.")
} else {
  message(">>> All R dependencies are present.")
}

library(GGIR)
library(ActCR)
library(actilifecounts)



# --- 2. PORTABLE PYTHON AUTO-INSTALLER ---
# Define paths for the portable environment
py_env_path <- file.path(getwd(), "resources", "python_env")
mamba_exe   <- file.path(getwd(), "resources", "bin", "micromamba.exe")

setup_python_env <- function() {
  # Check if the environment folder already exists
  if (!dir.exists(py_env_path)) {
    message("--- Initializing Portable Python Environment (Python 3.9 + Java) ---")
    
    # Critical Check: Is the Micromamba engine present?
    if (!file.exists(mamba_exe)) {
      stop("CRITICAL ERROR: Micromamba executable not found in resources/bin/. 
            Please ensure 'micromamba.exe' is present for portable setup.")
    }
    
    # Step A: Create the base environment using Micromamba
    # We include python, openjdk (for stepcount), and pip
    message(">>> Creating Conda environment... This may take a few minutes.")
    cmd_create <- paste(
      shQuote(mamba_exe), "create", 
      "-p", shQuote(py_env_path), 
      "-c conda-forge", 
      "python=3.9 openjdk pip -y"
    )
    system(cmd_create)
    
    # Step B: Locate the portable Pip executable based on OS
    pip_path <- if (Sys.info()['sysname'] == "Windows") {
      file.path(py_env_path, "Scripts", "pip.exe")
    } else {
      file.path(py_env_path, "bin", "pip")
    }
    
    # Step C: Install Python libraries (stepcount and PyYAML)
    if (file.exists(pip_path)) {
      message(">>> Installing stepcount and dependencies via Pip...")
      system(paste(shQuote(pip_path), "install stepcount PyYAML"))
      message(">>> Python environment setup complete.")
    } else {
      message("❌ ERROR: Pip not found. Python environment creation might have failed.")
    }
  } else {
    message(">>> Python environment detected at: ", py_env_path)
  }
}

# Execute the setup
setup_python_env()

# Load updater logic if present
if (file.exists("updater.R")) source("updater.R")

ui <- page_navbar(
  title = "GeneActiv Analysis Suite",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  header = tags$head(
    tags$style("
      /* 1. GREEN TERMINAL STYLE */
      #log_console { 
        background: #1e1e1e; 
        color: #00ff00; 
        font-family: 'Courier New', monospace; 
        height: 600px; 
        overflow-y: auto; 
        padding: 15px;
        border-radius: 5px;
        font-size: 13px;
        line-height: 1.2;
        white-space: pre-wrap;
      }
      
      /* 2. MOVE PROGRESS BAR TO TOP CENTER */
      .shiny-progress-container {
        top: 50px !important; 
        left: 50% !important;
        transform: translateX(-50%);
        bottom: auto !important;
        width: 400px !important;
        position: fixed !important;
      }
      .shiny-progress .bar {
        background-color: #2c3e50 !important;
      }
      
      /* 3. MOVE NOTIFICATIONS TO TOP RIGHT */
      #shiny-notification-panel {
        top: 20px !important;
        right: 20px !important;
        left: auto !important;
        bottom: auto !important;
        width: 300px !important;
        position: fixed !important;
      }
    "),
    
    tags$script("
      /* Auto-scroll de la console vers le bas */
      setInterval(function() {
        var el = document.getElementById('log_console');
        if (el) { el.scrollTop = el.scrollHeight; }
      }, 1000);
    ")
  ),
  
  # --- TAB 1: SYSTEM SETUP ---
  nav_panel("System Setup",
            layout_column_wrap(
              card(
                card_header("Pipeline & Resource Update"),
                p("Sync the latest analysis scripts, logos, and report templates from GitHub."),
                actionButton("btn_update", "Update System", 
                             icon = icon("github"), class = "btn-warning"),
                span(textOutput("update_time"), style = "font-size: 0.8em; color: gray; margin-top: 10px;")
              ),
              card(
                card_header("System Status"),
                verbatimTextOutput("sys_status"),
                actionButton("btn_check_sys", "Re-check Dependencies", class = "btn-secondary")
              )
            )
  ),
  
  # --- TAB 2: DATA SELECTION ---
  nav_panel("Data Selection",
            card(
              card_header("Folder Configuration"),
              p("Select the local directories required for the analysis pipeline:"),
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
                  shinyDirButton("dir_participants", "Browse...", "Select folder with EN/TA/HI CSVs"),
                  verbatimTextOutput("path_participants", placeholder = TRUE)
                )
              ),
              hr(),
              actionButton("btn_save_config", "Save All Configurations", class = "btn-success w-100")
            )
  ),
  
  # --- TAB 3: PROCESSING ---
  nav_panel("Processing",
            card(
              card_header("Pipeline Execution"),
              radioButtons("run_mode", "Select Run Mode:",
                           choices = c("Full Folder (Skip existing)" = "full", 
                                       "Specific Participant IDs" = "selective"),
                           selected = "full"),
              
              conditionalPanel(
                condition = "input.run_mode == 'selective'",
                shinyWidgets::pickerInput(
                  "selected_files", "Select Files to Process:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)
                )
              ),
              
              actionButton("run_full_pipeline", "START ANALYSIS", 
                           class = "btn-success btn-lg", icon = icon("play")),
              hr(),
              
              card(
                card_header(
                  div(class = "d-flex justify-content-between align-items-center",
                      span(icon("terminal"), " Execution Live Log"),
                      actionButton("clear_logs", "Clear Console", 
                                   icon = icon("trash"), 
                                   class = "btn-sm btn-outline-danger")
                  )
                ),
                verbatimTextOutput("log_console")
              )
            )
  ),
  
  # --- TAB 4: REPORTS ---
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
  
  # --- 1. DIRECTORY SELECTION LOGIC ---
  # Get available drives (C:, D:, etc. on Windows / Volumes on Mac)
  roots <- getVolumes()()
  
  # Initialize folder pickers
  shinyDirChoose(input, "dir_bin", roots = roots)
  shinyDirChoose(input, "dir_sleep", roots = roots)
  shinyDirChoose(input, "dir_participants", roots = roots)
  
  # Reactive display of selected paths
  output$path_bin <- renderText({ 
    if (is.integer(input$dir_bin)) "No folder selected" else parseDirPath(roots, input$dir_bin) 
  })
  output$path_sleep <- renderText({ 
    if (is.integer(input$dir_sleep)) "No folder selected" else parseDirPath(roots, input$dir_sleep) 
  })
  output$path_participants <- renderText({ 
    if (is.integer(input$dir_participants)) "No folder selected" else parseDirPath(roots, input$dir_participants) 
  })
  
  # --- 2. CONFIGURATION SAVING ---
  observeEvent(input$btn_save_config, {
    # Resolve paths (default to empty string if not selected)
    raw_path   <- if (is.integer(input$dir_bin)) "" else parseDirPath(roots, input$dir_bin)
    sleep_path <- if (is.integer(input$dir_sleep)) "" else parseDirPath(roots, input$dir_sleep)
    part_dir   <- if (is.integer(input$dir_participants)) "" else parseDirPath(roots, input$dir_participants)
    
    config_data <- list(
      paths = list(
        raw_bin = raw_path,
        sleep_data = sleep_path,
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
    
    # Save to YAML
    write_yaml(config_data, "config.yml")
    
    # Ensure output directories exist
    if(!dir.exists("summaries")) dir.create("summaries")
    if(!dir.exists("GGIR")) dir.create("GGIR")
    
    showNotification("Configuration Saved Successfully!", type = "message")
  })
  
  # --- 3. DYNAMIC FILE LISTING (For Selective Mode) ---
  observe({
    req(input$dir_bin) 
    path <- parseDirPath(roots, input$dir_bin)
    
    if (is.null(path) || length(path) == 0 || path == "") return(NULL)
    
    if (dir.exists(path)) {
      # Scan recursively for .bin files (case-insensitive)
      files <- list.files(path, pattern = "\\.(bin|BIN)$", recursive = TRUE, full.names = FALSE)
      
      if (length(files) > 0) {
        shinyWidgets::updatePickerInput(session, "selected_files", choices = files)
      } else {
        shinyWidgets::updatePickerInput(session, "selected_files", choices = "No .bin files found")
      }
    }
  })
  
  # --- 4. SYSTEM STATUS UPDATES ---
  output$sys_status <- renderText({
    req(input$btn_check_sys)
    
    status_msg <- c(
      sprintf("Current Time: %s", Sys.time()),
      "---------------------------",
      sprintf("[ ] Micromamba Engine: %s", if(file.exists(mamba_exe)) "FOUND" else "MISSING"),
      sprintf("[ ] Python Virtual Env: %s", if(dir.exists(py_env_path)) "READY" else "NOT CREATED"),
      sprintf("[ ] R Local Libraries: %s", if(length(missing_pkgs) == 0) "ALL INSTALLED" else paste("MISSING:", length(missing_pkgs))),
      "---------------------------"
    )
    paste(status_msg, collapse = "\n")
  })
  
  # Re-run setup if button clicked
  observeEvent(input$btn_check_sys, {
    showNotification("Re-checking system environment...", type = "message")
    setup_python_env() 
    showNotification("System check complete.", type = "message")
  })
  
  # --- 5. LOGGING ENGINE (Defined Once) ---
  # This polls the log file for changes every 2 seconds
  log_reader <- reactivePoll(1000, session,
                             checkFunc = function() {
                               if (file.exists("pipeline_log.txt")) file.info("pipeline_log.txt")$mtime else NULL
                             },
                             valueFunc = function() {
                               if (file.exists("pipeline_log.txt")) {
                                 lines <- readLines("pipeline_log.txt", warn = FALSE)
                                 if (length(lines) == 0) return("💻 Console ready. Waiting for analysis...")
                                 return(lines)
                               } else {
                                 return("💻 Console ready. Waiting for analysis...")
                               }
                             }
  )
  
  # Send the polled lines to the UI
  output$log_console <- renderPrint({
    cat(paste(log_reader(), collapse = "\n"))
  })
  
  # Clear console logic
  observeEvent(input$clear_logs, {
    if (file.exists("pipeline_log.txt")) {
      writeLines("💻 Console cleared. Ready for new analysis.", "pipeline_log.txt")
      showNotification("Console cleared", type = "message")
    }
  })
  
  # --- 6. MAIN PIPELINE EXECUTION (UPDATED) ---
  observeEvent(input$run_full_pipeline, {
    
    # Notification de démarrage
    showNotification("🚀 Analysis Started", type = "message", duration = 5)
    
    # Variables globales pour les sous-scripts
    selected_filenames <<- if(input$run_mode == "selective") input$selected_files else NULL
    
    log_file <- "pipeline_log.txt"
    if(file.exists(log_file)) file.remove(log_file) 
    file.create(log_file)
    
    withProgress(message = 'Processing Data...', value = 0, {
      
      con <- file(log_file, open = "wt")
      
      sink(con, type = "message", split = TRUE)
      sink(con, type = "output", split = TRUE)
      
      log_msg <- function(msg) {
        cat(msg, "\n")
        flush(con)
        # FORCE SHINY À METTRE À JOUR L'INTERFACE
        Sys.sleep(0.1) 
      }
      
      
      
      tryCatch({
        log_msg("==========================================")
        log_msg(paste("🚀 PIPELINE STARTED -", Sys.time()))
        log_msg(paste("Mode:", toupper(input$run_mode)))
        log_msg("==========================================\n")
        
        # --- RÉSOLUTION DES CHEMINS ---
        suffix <- if(Sys.info()['sysname'] == "Windows") "python.exe" else "bin/python"
        # On s'assure que le chemin vers python_env est correct
        full_py_path <- normalizePath(file.path(getwd(), "resources", "python_env", suffix), mustWork = FALSE)
        
        # STEP 1: GGIR
        setProgress(0.1, detail = "Step 1: Analyzing movement (GGIR)...")
        log_msg("📂 STEP 1/5: Running GGIR Analysis...")
        source("code/run_GGIR.R", local = TRUE)
        log_msg("✅ GGIR Analysis complete.")
        flush(con)
        Sys.sleep(0.5)        
        # STEP 2: PYTHON STEPCOUNT
        setProgress(0.3, detail = "Step 2: Calculating daily steps...")
        log_msg("\n📂 STEP 2/5: Running Stepcount (Python)...")
        
        if (!file.exists(full_py_path)) {
          log_msg(paste("❌ ERROR: Python environment not found at:", full_py_path))
        } else {
          py_script <- normalizePath("code/get_steps.py")
          
          # Prépare les arguments
          args <- c(shQuote(py_script))
          
          if (!is.null(selected_filenames)) {
            # On extrait les IDs (noms de fichiers sans extension)
            ids_only <- gsub("\\..*$", "", selected_filenames)
            ids_str <- paste(ids_only, collapse=",")
            # Utilise --ids car c'est ce que ton script Python attend (argparse)
            args <- c(args, "--ids", shQuote(ids_str))
          }
          
          # Exécution
          system(paste(shQuote(full_py_path), paste(args, collapse=" ")))
          log_msg("✅ Step calculation complete.")
          flush(con)
          Sys.sleep(0.5)
        }
        
        # STEP 3: METRICS EXTRACTION
        setProgress(0.5, detail = "Step 3: Organizing activity data...")
        log_msg("\n📂 STEP 3/5: Extracting calculated metrics...")
        source("code/extract_step_metrics.R", local = TRUE)
        source("code/extract_activity_metrics.R", local = TRUE)
        log_msg("✅ Metrics extracted.")
        flush(con)
        Sys.sleep(0.5)
        
        # STEP 4: DATA INTEGRATION
        setProgress(0.7, detail = "Step 4: Merging all data sources...")
        log_msg("\n📂 STEP 4/5: Integrating all results...")
        
        act_file <- "summaries/activity_metrics.csv"
        stp_file <- "summaries/step_metrics.csv"
        
        if(file.exists(act_file) && file.exists(stp_file)) {
          activity <- read_csv(act_file, show_col_types = FALSE)
          steps    <- read_csv(stp_file, show_col_types = FALSE)
          
          # Fusion sécurisée
          combined <- left_join(steps, activity, by = c("subject", "calendar_date"))
          write_csv(combined, act_file)
          log_msg("✅ Data successfully merged.")
        } else {
          log_msg("⚠️ One or more metric files missing. Integration skipped.")
        }
        
        # STEP 5: FINAL REPORTS
        setProgress(0.8, detail = "Step 5: Finalizing summaries...")
        log_msg("\n📂 STEP 5/5: Finalizing summaries and sleep logs...")
        
        # Utilisation de try() pour éviter que le pipeline crash si un script de fin échoue
        try(source("code/person_summary.R", local = TRUE))
        try(source("code/extract_software_sleep.R", local = TRUE))
        try(source("code/combine_geneactiv_metrics.R", local = TRUE))
        
        log_msg("\n==========================================")
        log_msg("🎉 PIPELINE SUCCESSFULLY FINISHED")
        log_msg("==========================================")
        
      }, error = function(e) {
        log_msg("\n******************************************")
        log_msg("❌ CRITICAL ERROR DETECTED")
        log_msg(paste("Error Details:", e$message))
        log_msg("******************************************")
      }, finally = {
        # Libération systématique des flux
        sink(type = "message")
        sink(type = "output")
        if (isOpen(con)) close(con)
      })
      
      setProgress(1)
    })
    
    showNotification("✅ Pipeline Finished", type = "message")
  })
  
  # --- 7. REPORT GENERATION ---
  observeEvent(input$gen_report, {
    selected_lang <- input$report_lang 
    withProgress(message = paste("Generating", selected_lang, "Reports..."), value = 0.5, {
      # Map selection to script names
      lang_code <- switch(selected_lang, "English" = "en", "Tamil" = "ta", "Hindi" = "hi")
      script_path <- paste0("code/generate_reports_", lang_code, ".R")
      
      if(file.exists(script_path)) {
        source(script_path, local = TRUE)
      } else {
        showNotification(paste("Error: Script", script_path, "not found."), type = "error")
      }
      setProgress(1)
    })
    showNotification("Reports Generated!", type = "message")
  })
} 

shinyApp(ui, server)
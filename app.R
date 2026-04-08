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
  "shinyWidgets", "shinycssloaders", "flextable", "officer", "doconv", "readxl", "R6", "jsonlite", "curl", "digest", "readxl"
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
  # --- BLOC A : CRÉATION ---
  if (!dir.exists(py_env_path)) {
    message("--- 🛠️ CREATING NEW PYTHON 3.9 ENVIRONMENT ---")
    if (!file.exists(mamba_exe)) stop("micromamba.exe missing!")
    
    # Suppression de l'argument problématique --no-default-packages
    cmd_create <- paste(
      shQuote(mamba_exe), "create", 
      "-p", shQuote(py_env_path), 
      "-c conda-forge", 
      "python=3.9 openjdk=11 pip setuptools wheel -y"
    )
    
    message(">>> Executing Micromamba (Creating environment)...")
    system(cmd_create)
  }
  
  # --- BLOC B : VÉRIFICATION (Chemin Intelligent) ---
  path_root <- file.path(py_env_path, "python.exe")
  path_scripts <- file.path(py_env_path, "Scripts", "python.exe")
  
  # On choisit celui qui existe vraiment
  local_python <- if (file.exists(path_root)) {
    path_root
  } else if (file.exists(path_scripts)) {
    path_scripts
  } else {
    NULL
  }
  
  if (!is.null(local_python)) {
    message(">>> 🔍 Python found at: ", local_python)
    message(">>> Installing/Verifying stepcount & PyYAML...")
    
    # On lance l'installation via le moteur Python trouvé
    cmd_repair <- paste(shQuote(local_python), "-m pip install stepcount PyYAML")
    system(cmd_repair)
    
    message("--- ✅ PYTHON LIBRARIES READY ---")
  } else {
    message("❌ FATAL ERROR: python.exe introuvable dans le dossier env.")
    message("Contenu du dossier pour diagnostic : ", paste(list.files(py_env_path), collapse=", "))
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
      
            /* 1. Structure Globale */
      body {
        height: 100vh !important;
        overflow: hidden !important;
        display: flex;
        flex-direction: column;
      }
      
      .tab-content {
        flex-grow: 1;
        overflow-y: auto !important;
        height: calc(100vh - 70px);
        padding: 20px;
        background-color: #f8f9fa;
      }
      
      /* 2. Style Terminal (60% de l'écran) */
      #log_console { 
        height: 60vh !important; 
        background: #1e1e1e !important; 
        color: #00ff00 !important; 
        font-family: 'Courier New', monospace; 
        overflow-y: auto; 
        padding: 15px;
        border-radius: 5px;
        box-shadow: inset 0 0 15px #000;
        font-size: 13px;
        line-height: 1.4;
        white-space: pre-wrap;
        border: 1px solid #333;
      }
      
     /* 3. Barre de progression animée ZÉBRA */
      .progress-bar {
        background-image: linear-gradient(
          45deg, 
          rgba(255, 255, 255, .15) 25%, 
          transparent 25%, 
          transparent 50%, 
          rgba(255, 255, 255, .15) 50%, 
          rgba(255, 255, 255, .15) 75%, 
          transparent 75%, 
          transparent
        ) !important;
        background-size: 1rem 1rem !important; /* Taille des rayures */
        animation: progress-bar-stripes 1s linear infinite !important;
      }

      @-webkit-keyframes progress-bar-stripes {
        from { background-position: 1rem 0; }
        to { background-position: 0 0; }
      }

      @keyframes progress-bar-stripes {
        from { background-position: 1rem 0; }
        to { background-position: 0 0; }
      }
      
    "),
    
    tags$script(HTML("
  Shiny.addCustomMessageHandler('update_log', function(message) {
    var el = document.getElementById('log_console_js');
    if (el) {
      // On met à jour uniquement le contenu texte
      el.textContent = message.text + (message.cursor ? ' █' : ' _');
      // Auto-scroll fluide
      // el.scrollTop = el.scrollHeight;
    }
  });
"))
    
    
   # tags$script("
  #    /* Auto-scroll de la console vers le bas */
   #   setInterval(function() {
  #      var el = document.getElementById('log_console');
   #     if (el) { el.scrollTop = el.scrollHeight; }
    #  }, 1000);
    #")
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
              
              card(
                card_header(
                  div(class = "d-flex align-items-center justify-content-between",
                      div(icon("terminal"), span(" Execution Live Log", style = "font-weight: bold; margin-left: 10px;")),
                      actionButton("clear_logs", "Clear Console", 
                                   icon = icon("trash"), 
                                   class = "btn-sm btn-outline-danger")
                  )
                ),
                # On crée une zone HTML brute qui ne sera jamais redessinée par Shiny
                tags$div(id = "log_console_js", 
                         style = "height: 70vh !important; background: #1e1e1e; color: #00ff00; 
                    font-family: 'Courier New', monospace; overflow-y: auto; 
                    padding: 15px; border-radius: 5px; font-size: 13px; 
                    line-height: 1.4; white-space: pre-wrap; border: 1px solid #333;")
              )
            )
  ),
  
  
  # --- TAB 4: REPORTS ---
  nav_panel("Reports",
            layout_column_wrap(
              width = 1/2,
              # Colonne Gauche : Sélection des individus
              card(
                card_header("1. Select Participants"),
                p("Only participants with completed analysis (Step 1-5) are listed here."),
                shinyWidgets::pickerInput(
                  "report_selected_ids", "Available for Reporting:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "Scan summaries first...")
                ),
                actionButton("refresh_report_list", "Refresh List", icon = icon("sync"), class = "btn-outline-secondary btn-sm")
              ),
              # Colonne Droite : Options de langues et Génération
              card(
                card_header("2. Language Options"),
                checkboxGroupInput("report_languages", "Generate reports in:",
                                   choices = c("English" = "en", "Tamil" = "ta", "Hindi" = "hi"),
                                   selected = "en", inline = TRUE),
                hr(),
                actionButton("gen_report_multi", "GENERATE SELECTED REPORTS", 
                             class = "btn-primary btn-lg", icon = icon("file-pdf")),
                hr(), # Une petite ligne de séparation
                actionButton("merge_selected_reports", "MERGE SELECTED LANGUAGES", 
                             class = "btn-outline-success", icon = icon("layer-group")),
                span(textOutput("report_status_msg"), style = "margin-top:10px; color: #2c3e50; font-weight: bold;")
              )
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
  
  # --- LOGGING ENGINE CORRIGÉ ---
  # Logique de lecture du fichier (sans clignotement UI)
  observe({
    invalidateLater(800) # Fréquence de rafraîchissement (800ms)
    
    # 1. Lire le fichier
    log_text <- if (file.exists("pipeline_log.txt")) {
      paste(readLines("pipeline_log.txt", warn = FALSE), collapse = "\n")
    } else {
      "💻 Console ready. Awaiting system log..."
    }
    
    # 2. Gérer le curseur
    show_cursor <- as.numeric(Sys.time()) %% 1.6 < 0.8
    
    # 3. Envoyer au JavaScript
    session$sendCustomMessage("update_log", list(
      text = log_text,
      cursor = show_cursor
    ))
  })
  

  
  # Clear console logic
  observeEvent(input$clear_logs, {
    if (file.exists("pipeline_log.txt")) {
      # Message plus dynamique
      writeLines(paste0("------------------------------------------\n",
                        "🖥️ LOG TERMINAL RESET - ", Sys.time(), "\n",
                        "READY: Awaiting system log...\n",
                        "------------------------------------------\n"), 
                 "pipeline_log.txt")
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
      
      sink(con, type = "message")
      sink(con, type = "output")
      
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
        # Chemin correct pour un venv Python sur Windows
        full_py_path <- normalizePath(
          file.path(getwd(), "resources", "python_env", "Scripts", "python.exe"), 
          mustWork = FALSE
        )
        
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
        
        # --- DÉTECTION DYNAMIQUE DU PYTHON ---
        path_root <- file.path(getwd(), "resources", "python_env", "python.exe")
        path_scripts <- file.path(getwd(), "resources", "python_env", "Scripts", "python.exe")
        
        full_py_path <- if (file.exists(path_root)) {
          path_root
        } else if (file.exists(path_scripts)) {
          path_scripts
        } else {
          NULL
        }
        
        if (is.null(full_py_path)) {
          log_msg("❌ ERROR: Python environment not found. Please run 'System Setup' first.")
        } else {
          log_msg(paste(">>> Using Python at:", full_py_path))
          py_script <- normalizePath("code/get_steps.py")
          
          # Prépare les arguments
          args <- c(shQuote(py_script))
          
          if (!is.null(selected_filenames)) {
            ids_only <- basename(selected_filenames)
            ids_only <- gsub("\\.(bin|BIN)$", "", ids_only)
            
            ids_str <- paste(ids_only, collapse=",")
            args <- c(args, "--ids", shQuote(ids_str))
            
            message(">>> IDs sent to Python: ", ids_str)
          }
          
          # Exécution avec le chemin détecté
          #system(paste(shQuote(full_py_path), paste(args, collapse=" ")))
          cmd_python <- paste(shQuote(full_py_path), paste(args, collapse=" "), ">> pipeline_log.txt 2>&1")
          
          log_msg(">>> Python is running in background, check below for updates...")
          
          # Exécution
          system(cmd_python, wait = TRUE)
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

  # Dynamic list of ready IDs (based on the 'summaries' folder)
  observe({
   # invalidateLater(2000) # Refresh the list every 2 seconds
    summary_path <- "summaries"
    if (dir.exists(summary_path)) {
      # Identify folders that contain at least one file (completed analysis)
      dirs <- list.dirs(summary_path, full.names = FALSE, recursive = FALSE)
      ready_ids <- dirs[sapply(dirs, function(d) length(list.files(file.path(summary_path, d))) > 0)]
      
      # Update the picker input in the UI
      shinyWidgets::updatePickerInput(session, "report_selected_ids", choices = ready_ids)
    }
  })
  
  # Trigger when the "GENERATE SELECTED REPORTS" button is clicked
  observeEvent(input$gen_report_multi, {
    req(input$report_selected_ids) # Ensure at least one ID is selected
    req(input$report_languages)    # Ensure at least one language is checked
    
    selected_ids <- input$report_selected_ids
    selected_langs <- input$report_languages
    
    withProgress(message = 'Generating Reports...', value = 0, {
      total_steps <- length(selected_ids) * length(selected_langs)
      step_count <- 0
      
      for (current_id in selected_ids) {
        for (lang in selected_langs) {
          step_count <- step_count + 1
          
          # Update the progress bar details
          setProgress(step_count / total_steps, 
                      detail = paste("Processing:", current_id, "(", toupper(lang), ")"))
          
          # 1. Define the Global variable 'target_id' for the sub-scripts to use
          target_id <<- current_id 
          
          # 2. Path to the specific language script
          script_path <- paste0("code/generate_reports_", lang, ".R")
          
          if(file.exists(script_path)) {
            tryCatch({
              # Run the script in the local environment
              source(script_path, local = TRUE)
            }, error = function(e) {
              message("Error in ", lang, " report for ", current_id, ": ", e$message)
            })
          } else {
            message("Script not found: ", script_path)
          }
        }
      }
    })
    showNotification("✅ Selected reports have been generated!", type = "message")
  })
  
  # Logique pour fusionner les rapports
  # Logique pour fusionner les rapports selon les langues sélectionnées
  observeEvent(input$merge_selected_reports, {
    req(input$report_languages) # On vérifie qu'au moins une langue est cochée
    
    selected_langs <- input$report_languages
    
    withProgress(message = 'Merging Reports...', value = 0, {
      
      for (lang in selected_langs) {
        # Mise à jour de la barre de progression
        setProgress(value = (which(selected_langs == lang) / length(selected_langs)), 
                    detail = paste("Merging:", toupper(lang)))
        
        # Le nom du script doit correspondre (ex: code/merge_reports_hindi.R)
        merge_script_path <- paste0("code/merge_reports_", lang, ".R")
        
        if(file.exists(merge_script_path)) {
          tryCatch({
            source(merge_script_path, local = TRUE)
            showNotification(paste("✅ Combined Report", toupper(lang), "created!"), type = "message")
          }, error = function(e) {
            showNotification(paste("❌ Error merging", lang, ":", e$message), type = "error")
          })
        } else {
          showNotification(paste("⚠️ Script missing:", merge_script_path), type = "warning")
        }
      }
    })
  })
  
  
  # --- LOGIQUE DU BOUTON UPDATE AVEC BARRE DE PROGRESSION ---
  observeEvent(input$btn_update, {
    withProgress(message = 'Updating Analysis Scripts...', value = 0, {
      success <- tryCatch({
        # APPEL SPECIFIQUE : Code = TRUE, Roots = FALSE (ou TRUE selon ton choix)
        check_and_update(update_roots = FALSE, update_code = TRUE, shiny_progress = setProgress)
      }, error = function(e) {
        FALSE
      })
      
      if (success) showNotification("Scripts updated successfully!", type = "message")
    })
  })
  
} 

shinyApp(ui, server)
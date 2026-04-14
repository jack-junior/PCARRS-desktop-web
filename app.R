# --- 1. STRICT PORTABLE PATHS ---
# Get the absolute path to the project root
if (!interactive()) {
  # Si lancé via Rscript.exe
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  if (length(script.name) > 0) setwd(dirname(normalizePath(script.name)))
}
project_root <- getwd()

# Force Local R Libraries
local_lib <- normalizePath(file.path(project_root, "resources", "R_libs"), mustWork = FALSE)
.libPaths(local_lib)

# Define Portable Executables
# We use normalizePath to ensure Windows-compatible backslashes
r_exe_path <- normalizePath(file.path(project_root, "R-portable", "bin", "x64", "Rscript.exe"), mustWork = FALSE)
mamba_exe  <- normalizePath(file.path(project_root, "resources", "bin", "micromamba.exe"), mustWork = FALSE)
py_env_path <- normalizePath(file.path(project_root, "resources", "python_env"), mustWork = FALSE)

# Detect Python Executable Location
path_root <- file.path(py_env_path, "python.exe")
path_scripts <- file.path(py_env_path, "Scripts", "python.exe")
full_py_path <- if (file.exists(path_root)) path_root else if (file.exists(path_scripts)) path_scripts else NULL

# --- Chargement des bibliothèques de base ---
library(shiny)
library(bslib)
library(httr)
library(shinyFiles) 
library(yaml)        
library(tidyverse) 
library(shinyWidgets) 
library(shinyjs)
library(processx)


# message("--- Checking R Dependencies ---")
# # 
# #  # --- 1. AUTO-INSTALLER R (Vers dossier local) ---
# required_packages <- c(
#   # --- SHINY UI ---
#   "shiny", "bslib", "shinyWidgets", "shinyjs", "shinycssloaders", "shinyFiles",
#   
#   # --- DATA / MANIPULATION ---
#   "tidyverse", "dplyr", "readr", "tibble", "stringr", "lubridate",
#   "janitor",
#   
#   # --- VISUALIZATION ---
#   "ggplot2", "scales",
#   
#   # --- GGIR + ACTIVITY ---
#   "GGIR", "ActCR", "actilifecounts",
#   
#   # --- REPORTING ---
#   "officer", "flextable", "doconv",
#   
#   # --- FILE I/O ---
#   "readxl", "writexl",
#   
#   # --- CONFIG / SYSTEM ---
#   "yaml", "processx", "R6", "jsonlite", "httr", "curl", "digest",
#   
#   # --- GRAPHICS ENGINE (CRITICAL FIX) ---
#   "ragg", "systemfonts", "textshaping",
#   
#   # --- DATE/TIME + HMS ---
#   "hms",
#   
#   # --- OPTIONAL BUT SAFE ---
#   "here"
# )
# 
# 
# #installed_pkgs <- installed.packages(lib.loc=local_lib)[, "Package"]
# #missing_pkgs <- required_packages[!(required_packages %in% installed_pkgs)]
# 
# installed <- installed.packages(lib.loc = local_lib)[, "Package"]
# 
# if (!all(required_packages %in% installed)) {
#   stop("Missing required packages. Please reinstall environment.")
#   
# } else {
#   message(">>> All R dependencies are present.")
# }


library(GGIR)
library(ActCR)
library(actilifecounts)



# --- 2. PORTABLE PYTHON AUTO-INSTALLER ---
setup_python_env <- function(shiny_session = NULL) {
  log_file <- "pipeline_log.txt"
  if(!file.exists(log_file)) file.create(log_file)
  
  if (!file.exists(mamba_exe)) {
    write(paste(Sys.time(), "❌ ERROR: micromamba.exe missing"), file=log_file, append=TRUE)
    return(FALSE)
  }
  
  if (!dir.exists(py_env_path) || length(list.files(py_env_path)) == 0) {
    if(dir.exists(py_env_path)) unlink(py_env_path, recursive = TRUE, force = TRUE)
    dir.create(py_env_path, recursive = TRUE)
    
    write(paste(Sys.time(), "--- 🛠️ CREATING PYTHON ENV ---"), file=log_file, append=TRUE)
    
    # Using --always-copy for portable stability
    if (!is.null(shiny_session)) {
      # On informe l'utilisateur que le téléchargement commence
      log_msg("🛠️ Executing Micromamba create command...")
    }
    cmd_create <- paste(shQuote(mamba_exe), "create", "-p", shQuote(py_env_path), 
                        "-c conda-forge python=3.9 openjdk=11 pip setuptools wheel -y --always-copy")
    
    system(cmd_create, wait = TRUE, invisible = FALSE)
    
    # MONITORING BLOC: Update Progress Bar while micromamba is running
    if (!is.null(shiny_session)) {
      withProgress(message = 'Downloading & Installing Python', value = 0, {
        while(TRUE) {
          is_running <- any(grepl("micromamba.exe", system("tasklist", intern = TRUE)))
          if(!is_running) break
          incProgress(0.02, detail = "Downloading packages from conda-forge...")
          Sys.sleep(2) 
        }
        setProgress(1)
      })
    }
  }
  
  if (!file.exists(full_py_path)) {
    write(paste(Sys.time(), "❌ ERROR: python.exe not found"), file=log_file, append=TRUE)
    return(FALSE)
  }
  
  # Pip Installation with progress
  write(paste(Sys.time(), ">>> Installing Stepcount & PyYAML..."), file=log_file, append=TRUE)
  cmd_pip <- paste(shQuote(full_py_path), "-m pip install stepcount PyYAML")
  
  if (!is.null(shiny_session)) {
    withProgress(message = 'Finalizing Libraries', value = 0.8, {
      shell(cmd_pip, wait = TRUE)
    })
  }
  
  return(TRUE)
}

# Load updater logic if present
if (file.exists("updater.R")) source("updater.R")

ui <- page_navbar(
  title = span(
    tags$img(src = "logo.png", style = "height:50px; margin-right:10px; vertical-align:middle;"),
    "GeneActiv Analysis Suite"
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE, # Prevents "jumping" layouts by allowing natural heights
  
  # --- UI HEADER: CSS & JS ---
  header = tags$head(
    tags$link(rel = "shortcut icon", href = "app_icon.ico"),
    shinyjs::useShinyjs(),
    
    tags$style(HTML("
      /* 1. Global Structure */
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
        padding: 0px; /* Padding handled inside nav_panels */
        background-color: #f8f9fa;
      }
      
      /* 2. Progress Bar & Notifications Position */
      .shiny-progress-container {
        top: 50px !important; 
        left: 50% !important;
        transform: translateX(-50%);
        bottom: auto !important;
        width: 400px !important;
        position: fixed !important;
        z-index: 9999;
      }
      .shiny-progress .bar {
        background-color: #2c3e50 !important;
      }
      
      #shiny-notification-panel {
        top: 20px !important;
        right: 20px !important;
        left: auto !important;
        bottom: auto !important;
        width: 300px !important;
        position: fixed !important;
      }
      
      /* 3. Terminal Style */
      #log_console_js { 
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
      
      /* 4. Animated Zebra Progress Bar */
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
        background-size: 1rem 1rem !important;
        animation: progress-bar-stripes 1s linear infinite !important;
      }

      @keyframes progress-bar-stripes {
        from { background-position: 1rem 0; }
        to { background-position: 0 0; }
      }
      
      /* 5. Right-side Logos */
      .navbar-nav.ms-auto {
        align-items: center;
        margin-right: 15px;
      }
      .logo-right {
        height: 50px; 
        margin-left: 15px;
      }
      
      /* Supprime l'espace blanc forcé par le spinner */
      .shiny-spinner-output-container {
        margin-top: 0 !important;
        display: block !important;
      }
      
      /* Force le terminal à coller au header de la carte */
      .card-body {
        padding-top: 5px !important;
      }
      #log_console_js {
        margin-top: 0 !important;
      }
    ")),
    
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update_log', function(message) {
        var el = document.getElementById('log_console_js');
        if (el) {
          var isAtBottom = (el.scrollHeight - el.scrollTop - el.clientHeight) < 50;
          var newDiv = document.createElement('div');
          newDiv.style.marginBottom = '2px';
          newDiv.innerText = message.text;
          el.appendChild(newDiv);
          
          if (isAtBottom) {
            el.scrollTop = el.scrollHeight;
          }
          
          if (el.childNodes.length > 1000) {
            el.removeChild(el.firstChild);
          }
        }
      });

      Shiny.addCustomMessageHandler('clear_log', function(message) {
        var el = document.getElementById('log_console_js');
        if (el) { el.innerHTML = ''; }
      });
    "))
  ),
  
  # --- TAB 1: SYSTEM SETUP ---
  nav_panel("System Setup",
            div(style = "padding: 20px;",
                layout_column_wrap(
                  width = 1/2,
                  heights_equal = "row",
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
                    layout_column_wrap(
                      width = 1/2,
                      actionButton("btn_check_sys", "Check Dependencies", class = "btn-primary w-100"),
                      uiOutput("install_button_ui") 
                    )
                  )
                )
            )
  ),
  
  # --- TAB 2: DATA SELECTION ---
  nav_panel("Data Selection",
            div(style = "padding: 20px;",
                card(
                  card_header("Folder Configuration"),
                  p("Select the local directories required for the analysis pipeline:"),
                  layout_column_wrap(
                    width = 1/3,
                    div(
                      tags$label("1. Raw .bin Files"),
                      shinyDirButton("dir_bin", "Browse...", "Select .bin folder", class = "btn-primary w-100"),
                      verbatimTextOutput("path_bin", placeholder = TRUE)
                    ),
                    div(
                      tags$label("2. Software Sleep Reports"),
                      shinyDirButton("dir_sleep", "Browse...", "Select Sleep folder", class = "btn-primary w-100"),
                      verbatimTextOutput("path_sleep", placeholder = TRUE)
                    ),
                    div(
                      tags$label("3. Participant Info Folder"),
                      shinyDirButton("dir_participants", "Browse...", "Select Info folder", class = "btn-primary w-100"),
                      verbatimTextOutput("path_participants", placeholder = TRUE)
                    )
                  ),
                  hr(),
                  actionButton("btn_save_config", "Save All Configurations", class = "btn-primary w-100")
                )
            )
  ),
  
  # --- TAB 3: PROCESSING ---
  nav_panel("Processing",
            div(style = "padding: 20px;",
                layout_sidebar(
                  sidebar = sidebar(
                    title = "Execution Controls",
                    width = 400,
                    radioButtons("run_mode", "Run Mode:",
                                 choices = c("Full Folder (Skip existing)" = "full", 
                                             "Specific Participant IDs" = "selective"),
                                 selected = "full"),
                    conditionalPanel(
                      condition = "input.run_mode == 'selective'",
                      pickerInput("selected_files", "Select Files to Process:", choices = NULL, multiple = TRUE,
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE))
                    ),
                    actionButton("run_full_pipeline", "START ANALYSIS", 
                                 class = "btn-primary btn-lg w-100", icon = icon("play"))
                  ),
                  # Main Terminal Area
                  card(
                    full_screen = TRUE,
                    card_header(
                      div(class = "d-flex justify-content-between align-items-center",
                          span(icon("terminal"), " Execution Live Log                            "),
                          div(style = "flex-grow: 1;"), # Optionnel : un vide qui pousse les bords
                          actionButton("clear_logs", "Clear Console", 
                                       icon = icon("trash"), class = "btn-sm btn-outline-danger")
                      )
                    ),
                    tags$div(id = "log_console_js", style = "height: 100%;"),
                    
                    # div(style = "position: relative; min-height: 400px;", 
                    #     shinycssloaders::withSpinner(
                    #       tags$div(id = "log_console_js", style = "height: 100%;"),
                    #       type = 8, 
                    #       color = "#00ff00",
                    #       size = 0.5,
                    #       proxy.height = "50px" # Force une petite zone de chargement
                    #     )
                    # )
                  )
                )
            )
  ),
  
  # --- TAB 4: REPORTS ---
  nav_panel("Reports",
            div(style = "padding: 20px;",
                layout_column_wrap(
                  width = 1/2,
                  card(
                    card_header("1. Select Participants"),
                    p("Only participants with completed analysis (Steps 1-5) are listed here."),
                    pickerInput("report_selected_ids", "Available for Reporting:", choices = NULL, multiple = TRUE,
                                options = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "Scan summaries first...")),
                    actionButton("refresh_report_list", "Refresh List", icon = icon("sync"), class = "btn-outline-secondary btn-sm")
                  ),
                  card(
                    card_header("2. Language Options"),
                    checkboxGroupInput("report_languages", "Generate reports in:",
                                       choices = c("English" = "en", "Tamil" = "ta", "Hindi" = "hi"),
                                       selected = "en", inline = TRUE),
                    hr(),
                    actionButton("gen_report_multi", "GENERATE SELECTED REPORTS", class = "btn-primary btn-lg w-100", icon = icon("file-pdf")),
                    hr(),
                    actionButton("merge_selected_reports", "MERGE SELECTED LANGUAGES", class = "btn-outline-success w-100", icon = icon("layer-group")),
                    span(textOutput("report_status_msg"), style = "margin-top:10px; display:block; color: #2c3e50; font-weight: bold;")
                  )
                )
            )
  ),
  
  # --- NAVBAR LOGOS ---
  nav_spacer(),
  nav_item(tags$img(src = "logo1.png", class = "logo-right")),
  nav_item(tags$img(src = "logo2.png", class = "logo-right")),
  nav_item(tags$img(src = "logo3.png", class = "logo-right"))
)
  
server <- function(input, output, session) {
    
  # --- 0. SESSION CLEANUP ---
  active_processes <- reactiveVal(list())
  
  session$onSessionEnded(function() {
    message("Closing session. Killing active background processes...")
    
    # On utilise isolate() pour lire la valeur réactive en dehors du contexte normal
    procs <- isolate(active_processes())
    
    if (length(procs) > 0) {
      lapply(procs, function(p) {
        if (!is.null(p) && p$is_alive()) {
          try(p$kill_tree(), silent = TRUE) 
        }
      })
    }
    
    stopApp()
    # q("no") est commenté ici pour tes tests, 
    # réactive-le uniquement quand tout fonctionne parfaitement.
     q("no") 
  })
    
    # --- 1. UNIVERSAL LOGGING ENGINE ---
    log_file <- "pipeline_log.txt"
    
    log_msg <- function(msg) {
      if (is.null(msg) || msg == "") return()
      
      # 1. Write to local file for history
      cat(msg, "\n", file = log_file, append = TRUE)
      
      # 2. Clean for JS Terminal transmission
      clean_msg <- gsub("[\r\n]", " ", msg)
      clean_msg <- gsub("\\\\", "/", clean_msg) # Fix Windows paths for JS
      
      session$sendCustomMessage("update_log", list(text = paste0(clean_msg, "\n")))
    }
    
    # --- 2. UNIVERSAL ASYNC RUNNER (The Master Engine) ---
    run_step_async <- function(command, args, step_name, on_success = NULL, check_fn = NULL) {
      log_msg(paste0("\n🔹 START: ", step_name))
      start_time <- Sys.time()
      last_heartbeat <- Sys.time()
      
      px <- process$new(
        command = command,
        args = args,
        stdout = "|",
        stderr = "|",
        cleanup = TRUE
      )
      
      # On utilise isolate pour les reactiveVal en dehors du contexte réactif
      current_list <- isolate(active_processes())
      current_list[[step_name]] <- px
      active_processes(current_list)
      
      check_status <- function() {
        # 1. Vérifier si le processus existe encore
        if (is.null(px)) return()
        
        # 2. TENTATIVE DE LECTURE (Sécurisée)
        # On capture l'erreur "Invalid connection" si le processus vient de mourir
        out <- tryCatch(px$read_output_lines(), error = function(e) NULL)
        err <- tryCatch(px$read_error_lines(), error = function(e) NULL)
        
        if (length(out) > 0) {
          lapply(out, log_msg)
          last_heartbeat <<- Sys.time()
        }
        if (length(err) > 0) {
          lapply(err, function(x) log_msg(paste("⚠️", x)))
        }
        
        # 3. HEARTBEAT
        elapsed <- as.numeric(difftime(Sys.time(), last_heartbeat, units = "secs"))
        if (elapsed > 15) {
          log_msg(paste0("... ", step_name, " in progress ..."))
          last_heartbeat <<- Sys.time()
        }
        
        # 4. VÉRIFICATION DE FIN
        if (!px$is_alive()) {
          # Dernière tentative de lecture des restes du buffer avant de fermer
          final_out <- tryCatch(px$read_all_output_lines(), error = function(e) NULL)
          if (length(final_out) > 0) lapply(final_out, log_msg)
          
          exit_code <- px$get_exit_status()
          duration <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
          
          is_valid <- (exit_code == 0)
          if (!is.null(check_fn)) is_valid <- is_valid && check_fn()
          
          if (is_valid) {
            log_msg(paste0("✅ SUCCESS: ", step_name, " (", duration, "s)"))
            if (!is.null(on_success)) on_success() 
          } else {
            log_msg(paste0("❌ ERROR: ", step_name, " failed (Code ", exit_code, ")"))
            shiny::showNotification(paste("Error in", step_name), type = "error")
          }
          return() # On arrête le cycle later
        }
        
        # 5. CONTINUER LE CYCLE
        later::later(check_status, 0.5)
      }
      
      check_status()
    }
    
    # --- 3. DIRECTORY & CONFIGURATION LOGIC ---
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
    
    observeEvent(input$btn_save_config, {
      raw_path   <- if (is.integer(input$dir_bin)) "" else parseDirPath(roots, input$dir_bin)
      sleep_path <- if (is.integer(input$dir_sleep)) "" else parseDirPath(roots, input$dir_sleep)
      part_dir   <- if (is.integer(input$dir_participants)) "" else parseDirPath(roots, input$dir_participants)
      
      config_data <- list(
        paths = list(
          raw_bin = raw_path,
          sleep_data = sleep_path,
          participant_folder = part_dir,
          participant_files = list(
            en = file.path(part_dir, "participants_en.xlsx"),
            ta = file.path(part_dir, "participants_ta.xlsx"),
            hi = file.path(part_dir, "participants_hi.xlsx")
          ),
          img_folder = "resources/images",
          summaries = "summaries",
          ggir_output = "GGIR",
          reports = "reports"
        )
      )
      write_yaml(config_data, "config.yml")
      lapply(c("summaries", "GGIR", "reports"), function(d) if(!dir.exists(d)) dir.create(d))
      showNotification("Configuration Saved!", type = "default")
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
    
    # --- 4. SYSTEM STATUS & INSTALLER ---
    py_ready <- reactiveVal(FALSE)
    check_trigger <- reactiveVal(0)
    
    output$sys_status <- renderText({
      check_trigger() # Dépendance réactive
      
      # Vérification réelle des fichiers
      env_exists <- dir.exists(py_env_path) && length(list.files(py_env_path)) > 0
      lib_exists <- dir.exists(local_lib) && length(list.files(local_lib)) > 0
      
      # Mise à jour de l'état global
      py_ready(env_exists)
      
      paste(c(sprintf("Current Time: %s", Sys.time()), 
              "---------------------------",
              sprintf("[ %s ] Micromamba Engine", if(file.exists(mamba_exe)) "✅ READY" else "❌ MISSING"),
              sprintf("[ %s ] Python Virtual Env", if(env_exists) "✅ READY" else "⚠️ MISSING"),
              sprintf("[ %s ] R Local Libraries", if(lib_exists) "✅ READY" else "❌ MISSING"),
              "---------------------------"), collapse = "\n")
    })
    
    # --- POPUP DE CONFIRMATION ---
    observeEvent(input$btn_install_py, {
      showModal(modalDialog(
        title = "Initial Setup",
        tags$div(
          tags$p("This will download and install Python (approx. 1.5GB - 2GB)."),
          tags$b("Estimated time: 25-35 minutes depending on your internet connection."),
          tags$p(style = "color: red; margin-top: 10px;", "Please do not close the app during the process.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_install", "Start Download", class = "btn-primary")
        )
      ))
    })
    
    # --- EXÉCUTION DE L'INSTALLATION ---
    observeEvent(input$confirm_install, {
      removeModal()
      showNotification("Installation started. Check the progress bar above.", type = "warning", duration = 15)
      
      # On force un premier log dans la console JS
      log_msg("🚀 Starting Python environment setup...")
      
      # Lancement de la fonction définie en haut du script
      success <- setup_python_env(shiny_session = session)
      
      if(success) {
        # On déclenche la mise à jour de l'UI
        check_trigger(check_trigger() + 1) 
        py_ready(TRUE) 
        showNotification("✅ Python is ready!", type = "default")
        log_msg("✅ Python environment successfully initialized.")
      } else {
        showNotification("❌ Python installation failed. See pipeline_log.txt", type = "error")
        log_msg("❌ Installation failed.")
      }
    })
    
    # --- 5. MAIN PIPELINE EXECUTION (CHAINED) ---
    observeEvent(input$run_full_pipeline, {
      req(input$run_mode)
      
      # 1. Capture values
      mode_selected  <- input$run_mode
      files_selected <- input$selected_files
      
      # 2. UI Reset
      shiny::showNotification("🚀 Analysis Started", type = "message")
      session$sendCustomMessage("clear_log", list()) 
      
      if(file.exists(log_file)) file.remove(log_file)
      file.create(log_file)
      
      current_python_exe   <- full_py_path
      current_r_script_exe <- r_exe_path 
      
      if (!file.exists(current_r_script_exe)) {
        log_msg("❌ ERROR: R-portable not found at expected path.")
        return()
      }
      
      # 3. Persistent Progress Object (NO on.exit here!)
      prog <- shiny::Progress$new(session)
      prog$set(message = "Executing Pipeline", value = 0)
      
      # Helper function to update progress safely
      safe_prog <- function(val, det) {
        try({
          if (!is.null(prog)) prog$set(value = val, detail = det)
        }, silent = TRUE)
      }
      
      # --- 4. PIPELINE FUNCTIONS ---
      
      finish_pipeline <- function() {
        safe_prog(1, "All steps completed")
        log_msg("\n==========================================\n🎉 PIPELINE FINISHED\n==========================================")
        
        # On sécurise l'appel à get_ready_ids
        tryCatch({
          ids <- get_ready_ids()
          shinyWidgets::updatePickerInput(session, "report_selected_ids", choices = ids)
        }, error = function(e) {
          log_msg(paste("⚠️ Note: UI update skipped -", e$message))
        })
        
        shiny::showNotification("✅ Pipeline Finished", type = "message")
        prog$close() 
      }
      
      
      step5_final <- function() {
        safe_prog(0.9, "Step 5: Finalizing summaries")
        log_msg("\n--- Step 5: Finalizing summaries ---")
        tryCatch({
          # On utilise local = TRUE pour que les scripts voient les variables de la session
          # mais on les exécute séquentiellement
          source("code/person_summary.R", local = TRUE)
          source("code/extract_software_sleep.R", local = TRUE)
          
          # C'est ici qu'on lance ton script de combinaison corrigé
          source("code/combine_geneactiv_metrics.R", local = TRUE)
          
          finish_pipeline()
        }, error = function(e) {
          log_msg(paste("❌ Error Step 5:", e$message))
          # Si l'erreur est "non-function", c'est souvent un problème de parenthèse
          # ou de package mal chargé dans un des sous-scripts.
          prog$close()
        })
      }
      
      step4_integration <- function() {
        safe_prog(0.8, "Step 4: Merging data sources")
        log_msg("\n--- Step 4: Merging all data sources ---")
        tryCatch({
          activity <- read_csv("summaries/activity_metrics.csv", show_col_types = FALSE)
          steps <- read_csv("summaries/step_metrics.csv", show_col_types = FALSE)
          write_csv(left_join(steps, activity, by = c("subject", "calendar_date")), "summaries/activity_metrics.csv")
          step5_final()
        }, error = function(e) {
          log_msg(paste("❌ Error Step 4:", e$message))
          prog$close()
        })
      }
      
      step3_extraction <- function() {
        safe_prog(0.6, "Step 3: Extracting metrics")
        run_step_async(current_r_script_exe, "code/extract_step_metrics.R", "Step Metrics", on_success = function() {
          run_step_async(current_r_script_exe, "code/extract_activity_metrics.R", "Activity Metrics", 
                         on_success = step4_integration, 
                         check_fn = function() file.exists("summaries/activity_metrics.csv"))
        }, check_fn = function() file.exists("summaries/step_metrics.csv"))
      }
      
      step2_python <- function() {
        safe_prog(0.4, "Step 2: Python Stepcount")
        py_script <- normalizePath("code/get_steps.py", mustWork = FALSE)
        args <- c(py_script) 
        
        if (mode_selected == "selective" && !is.null(files_selected)) {
          ids <- gsub("\\.(bin|BIN)$", "", basename(files_selected))
          args <- c(args, "--ids", paste(ids, collapse=","))
        }
        
        run_step_async(current_python_exe, args, "Python Stepcount", on_success = step3_extraction)
      }
      
      # --- 5. KICK-OFF ---
      safe_prog(0.1, "Step 1: GGIR Analysis")
      run_step_async(current_r_script_exe, c("code/run_GGIR.R"), "GGIR Analysis", 
                     on_success = step2_python, 
                     check_fn = function() dir.exists("GGIR"))
      
    })
        
        
    
    # Dynamic UI for the Install Button
    output$install_button_ui <- renderUI({
      if (!py_ready()) {
        actionButton("btn_install_py", "Initialize Python Environment", 
                     class = "btn-danger w-100", icon = icon("download"))
      } else {
        actionButton("btn_install_py", "Reinstall / Repair Python", 
                     class = "btn-outline-primary w-100")
      }
    })
    
    observe({
      if(!py_ready()) {
        shinyjs::disable("run_full_pipeline") # Nécessite library(shinyjs)
      } else {
        shinyjs::enable("run_full_pipeline")
      }
    })
    
    
    # --- 6. REPORT GENERATION ---
    
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
      showNotification("✅ Selected reports have been generated!", type = "default")
    })
    
    # Logique pour fusionner les rapports
    observeEvent(input$merge_selected_reports, {
      req(input$report_languages) # On vérifie qu'au moins une langue est cochée
      
      selected_langs <- input$report_languages
      
      withProgress(message = 'Merging Reports...', value = 0, {
        
        for (lang in selected_langs) {
          # Mise à jour de la barre de progression
          setProgress(value = (which(selected_langs == lang) / length(selected_langs)), 
                      detail = paste("Merging:", toupper(lang)))
          
          merge_script_path <- paste0("code/merge_reports_", lang, ".R")
          
          if(file.exists(merge_script_path)) {
            tryCatch({
              source(merge_script_path, local = TRUE)
              showNotification(paste("✅ Combined Report", toupper(lang), "created!"), type = "default")
            }, error = function(e) {
              showNotification(paste("❌ Error merging", lang, ":", e$message), type = "error")
            })
          } else {
            showNotification(paste("⚠️ Script missing:", merge_script_path), type = "warning")
          }
        }
      })
    })
    
    # --- 7. UTILS & UPDATES ---
    # get_ready_ids <- function() {
    #   file <- file.path("summaries", "geneactiv_combined_metrics.csv")
    #   if (!file.exists(file)) return(character(0))
    #   df <- read.csv(file)
    #   valid_flag <- if (is.logical(df$valid_7days_activity)) df$valid_7days_activity else df$valid_7days_activity == 1
    #   return(sort(unique(as.character(df$subject[valid_flag]))))
    # }
    
    # On crée une fonction réutilisable pour scanner les dossiers
    get_ready_ids <- function() {
      
      file <- file.path("summaries", "geneactiv_combined_metrics.csv")
      
      # 1. Check existence
      if (!file.exists(file)) return(character(0))
      
      # 2. Read safely
      df <- tryCatch({
        read.csv(file)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(df)) return(character(0))
      
      # 3. Check required columns
      required_cols <- c("subject", "valid_7days_activity")
      
      if (!all(required_cols %in% names(df))) return(character(0))
      
      # 4. Normalize valid column (robust)
      valid_flag <- df$valid_7days_activity
      
      valid_flag <- if (is.logical(valid_flag)) {
        valid_flag
      } else if (is.numeric(valid_flag)) {
        valid_flag == 1
      } else {
        tolower(as.character(valid_flag)) == "true"
      }
      
      # 5. Filter valid participants
      df_valid <- df[valid_flag, ]
      
      # 6. Extract unique IDs
      ids <- unique(df_valid$subject)
      
      # 7. Clean
      ids <- as.character(ids)
      ids <- ids[!is.na(ids) & ids != ""]
      
      # 8. Sort (UX)
      ids <- sort(ids)
      
      return(ids)
    }
    
    observeEvent(input$refresh_report_list, {
      shinyWidgets::updatePickerInput(session, "report_selected_ids", choices = get_ready_ids())
    })
    
    # observeEvent(input$btn_update, {
    #   withProgress(message = 'Updating Scripts...', value = 0, {
    #     check_and_update(update_roots = FALSE, update_code = TRUE, shiny_progress = setProgress)
    #   })
    # })
    
    observeEvent(input$btn_update, {
      withProgress(message = 'Updating Analysis Scripts...', value = 0, {
        success <- tryCatch({
          # APPEL SPECIFIQUE : Code = TRUE, Roots = FALSE (ou TRUE selon ton choix)
          check_and_update(update_roots = FALSE, update_code = TRUE, shiny_progress = setProgress)
        }, error = function(e) {
          FALSE
        })
        
        if (success) showNotification("Scripts updated successfully!", type = "default")
      })
    })
    
    # --- AUTO-REFRESH LIST ON START (ADAPTED) ---
    # This ensures the IDs are loaded immediately when the session opens
    observe({
      ids <- get_ready_ids()
      if (length(ids) > 0) {
        shinyWidgets::updatePickerInput(session, "report_selected_ids", choices = ids)
      }
    })
    
    # 1. Mise à jour au clic sur le bouton "Refresh List"
    observeEvent(input$refresh_report_list, {
      ids <- get_ready_ids()
      
      if (length(ids) > 0) {
        shinyWidgets::updatePickerInput(session, "report_selected_ids", choices = ids)
        showNotification("List updated: Participants found.", type = "default")
      } else {
        shinyWidgets::updatePickerInput(session, "report_selected_ids", choices = character(0))
        showNotification("No completed analyses found in 'summaries/'.", type = "warning")
      }
    })
    
    # --- SYSTEM CHECK LOGIC (ADAPTED) ---
    observeEvent(input$btn_check_sys, {
      showNotification("Re-checking system environment...", type = "default")
      setup_python_env(shiny_session = session) # Pass session for the progress bar
      check_trigger(check_trigger() + 1)
      showNotification("System check complete.", type = "default")
    })
    
    # Clear console logic
    observeEvent(input$clear_logs, {
      # 1. Réinitialiser le fichier physique
      if (file.exists("pipeline_log.txt")) {
        writeLines(paste0("------------------------------------------\n",
                          "🖥️ LOG TERMINAL RESET - ", Sys.time(), "\n",
                          "READY: Awaiting system log...\n",
                          "------------------------------------------\n"), 
                   "pipeline_log.txt")
      }
      
      # 2. CRITIQUE : Envoyer l'ordre d'effacement à l'interface (JavaScript)
      session$sendCustomMessage("clear_log", list())
      
      # 3. Optionnel : Afficher un message de bienvenue dans la console vide
      log_msg(paste0("🖥️ TERMINAL RESET - ", Sys.time()))
      
      showNotification("Console cleared", type = "default")
    })
}

shinyApp(ui, server)
  
  
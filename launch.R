# --- launch.R ---

# --- 1. ISOLATION TOTALE (Anti-OneDrive / Anti-System) ---
# On redéfinit le HOME de R sur le dossier du projet
Sys.setenv(HOME = getwd())
Sys.setenv(R_USER = getwd())

# On force un dossier temporaire local pour éviter d'écrire dans AppData
if(!dir.exists("temp")) dir.create("temp", showWarnings = FALSE)
Sys.setenv(TMPDIR = file.path(getwd(), "temp"))
Sys.setenv(TMP = file.path(getwd(), "temp"))
Sys.setenv(TEMP = file.path(getwd(), "temp"))

# --- 2. GESTION DES CHEMINS ---
project_path <- getwd()
local_lib    <- file.path(project_path, "resources", "R_libs")
.libPaths(c(normalizePath(local_lib), .libPaths()))

message(">>> R_USER redirected to: ", Sys.getenv("R_USER"))

# A. DEFINE ABSOLUTE PATHS
initial_options <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_name <- sub(file_arg, "", initial_options[grep(file_arg, initial_options)])

if (length(script_name) > 0) {
  # On force le dossier de travail sur le dossier contenant launch.R
  script_path <- dirname(normalizePath(script_name, winslash = "/", mustWork = TRUE))
  setwd(script_path)
}

project_path <- getwd()
local_lib    <- file.path(project_path, "resources", "R_libs")

# B. DIRECTORY PREPARATION
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}

# C. FORCE R TO USE LOCAL LIBRARIES
Sys.setenv(R_LIBS_USER = local_lib)
.libPaths(c(local_lib, .libPaths()))
print("=== LIB PATHS ===")
print(.libPaths())

# D. LOAD CORE DEPENDENCIES
suppressPackageStartupMessages({
  tryCatch({
    library(digest, quietly = TRUE, warn.conflicts = FALSE)
    library(httr, quietly = TRUE, warn.conflicts = FALSE)
    library(shiny, quietly = TRUE, warn.conflicts = FALSE)
    library(later, quietly = TRUE, warn.conflicts = FALSE)
    
  }, error = function(e) {
    message(">>> Package loading issue: ", e$message)
  })
})

# E. SMART UPDATER LOGIC
if (file.exists("updater.R")) {
  tryCatch({
    source("updater.R") # Plus propre que readLines + eval
    check_and_update(update_roots = TRUE, update_code = FALSE) 
  }, error = function(e) {
    message(">>> Update system skipped: ", e$message)
  })
}

# F. LAUNCH SHINY APP
#port <- 1234
port <- 3456
url  <- paste0("http://127.0.0.1:", port)

# Détection du navigateur avec priorité Chrome -> Edge -> Default
browser_path <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
if(!file.exists(browser_path)) {
  browser_path <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
}

# Si aucun des deux, on utilise la commande 'start' générique de Windows
later::later(function() {
  if(file.exists(browser_path)) {
    launch_cmd <- sprintf('start "" %s --app=%s', shQuote(browser_path), url)
  } else {
    launch_cmd <- sprintf('start %s', url)
  }
  shell(launch_cmd, wait = FALSE)
}, delay = 2)

# Lancement de l'app
shiny::runApp(appDir = project_path, port = port, host = "127.0.0.1", launch.browser = FALSE)
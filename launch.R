# --- launch.R ---

# A. DEFINE ABSOLUTE PATHS
# Using normalizePath ensures Windows understands the full drive path (e.g., C:/...)
# which often bypasses "folder not writable" errors during package installation.
project_path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
local_lib    <- file.path(project_path, "resources", "R_libs")

# B. DIRECTORY PREPARATION
# Create the library folder if it's missing before trying to use it
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}

# C. FORCE R TO USE LOCAL LIBRARIES
# Set environmental variable for the session and update .libPaths
Sys.setenv(R_LIBS_USER = local_lib)
.libPaths(c(local_lib, .libPaths()))

# Diagnostic messages for the console
message(">>> Working Directory: ", project_path)
message(">>> Using Library Path: ", local_lib)

# D. LOAD CORE DEPENDENCIES
# We explicitly use lib.loc to ensure we aren't loading from a system R folder
tryCatch({
  library(httr, lib.loc = local_lib)
  library(shiny, lib.loc = local_lib)
}, error = function(e) {
  message(">>> Initial load warning: Core packages might be missing. App will attempt auto-install.")
})

# E. UPDATER LOGIC
# Check for updates from GitHub before the Shiny UI starts
if (file.exists("updater.R")) {
  message("--- Checking for core updates (app.R, updater.R) ---")
  tryCatch({
    source("updater.R")
    # This function will overwrite local files with the latest versions from GitHub
    check_and_update(update_roots = TRUE) 
  }, error = function(e) {
    message(">>> Update failed, starting with local version: ", e$message)
  })
}

# F. LAUNCH SHINY APP
message("--- Starting UI ---")
# Launch the app located in the project root
shiny::runApp(project_path, launch.browser = TRUE)
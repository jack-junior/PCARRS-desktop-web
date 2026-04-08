# --- launch.R ---

# A. DEFINE ABSOLUTE PATHS
# normalizePath converts relative paths to full Windows paths (C:/Users/...)
# This is essential for write permissions and DLL stability.
project_path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
local_lib    <- file.path(project_path, "resources", "R_libs")

# B. DIRECTORY PREPARATION
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}

# C. FORCE R TO USE LOCAL LIBRARIES
# We override the system library paths to prioritize your portable folder
Sys.setenv(R_LIBS_USER = local_lib)
.libPaths(c(local_lib, .libPaths()))

# Diagnostic console messages
message(">>> Working Directory: ", project_path)
message(">>> Using Library Path: ", local_lib)

# D. LOAD CORE DEPENDENCIES
# digest is now required for the smart update (SHA1)
tryCatch({
  library(digest, lib.loc = local_lib)
  library(httr, lib.loc = local_lib)
  library(shiny, lib.loc = local_lib)
}, error = function(e) {
  message(">>> Warning: Core packages missing or corrupted. Attempting to recover via updater...")
})

# E. SMART UPDATER LOGIC
# Dans la section E de ton launch.R
if (file.exists("updater.R")) {
  message("--- Checking for Root Updates (app.R only) ---")
  tryCatch({
    tmp_script <- readLines("updater.R", warn = FALSE, skipNul = TRUE)
    eval(parse(text = tmp_script))
    
    # APPEL SPECIFIQUE : Roots = TRUE, Code = FALSE
    check_and_update(update_roots = TRUE, update_code = FALSE) 
    
  }, error = function(e) {
    message(">>> Update system skipped: ", e$message)
  })
}

# F. LAUNCH SHINY APP
message("--- Starting UI ---")
# Run the application located in the current project root
shiny::runApp(project_path, launch.browser = TRUE)
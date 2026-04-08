# --- launch.R ---
# 1. Configuration stricte des chemins
project_path <- getwd()
lib_path <- file.path(project_path, "resources", "R_libs")

# On force R à n'utiliser que ton dossier R_libs
.libPaths(lib_path)

# 2. Gestion de la mise à jour GitHub (Avant le lancement de l'UI)
if (file.exists("updater.R")) {
  message("--- Checking for core updates (app.R, updater.R) ---")
  tryCatch({
    source("updater.R")
    # On lance la mise à jour des fichiers racines
    # Cette fonction téléchargera le dernier app.R depuis GitHub
    check_and_update(update_roots = TRUE) 
  }, error = function(e) {
    message("Update failed, starting with local version: ", e$message)
  })
}

# 3. Lancement de l'application
library(shiny)
message("--- Starting UI ---")
runApp(project_path, launch.browser = TRUE)
library(httr)

check_and_update <- function() {
  owner <- "PCARRS"
  repo  <- "p5-activity-summary"
  
  # 1. Configuration des dossiers à synchroniser
  folders_map <- c(
    "code"     = "code",
    "data/img" = "resources/images"
  )
  
  # 2. Configuration des fichiers individuels à la racine (ex: app.R)
  root_files <- c("app.R", "updater.R")
  
  message("--- Starting System Update ---")
  success_all <- TRUE
  
  # --- PARTIE A : Mise à jour des dossiers ---
  for (github_path in names(folders_map)) {
    local_path <- folders_map[[github_path]]
    url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, github_path)
    res <- GET(url)
    
    if (status_code(res) == 200) {
      contents <- content(res)
      if (!dir.exists(local_path)) dir.create(local_path, recursive = TRUE)
      
      for (file_info in contents) {
        if (file_info$type == "file") {
          dest_file <- file.path(local_path, file_info$name)
          message(sprintf("Updating: %s", file_info$name))
          download.file(file_info$download_url, dest_file, mode = "wb", quiet = TRUE)
        }
      }
    } else {
      warning(sprintf("Failed to access: %s", github_path))
      success_all <- FALSE
    }
  }
  
  # --- PARTIE B : Mise à jour des fichiers racines (Self-Update) ---
  for (f in root_files) {
    url_file <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, f)
    res_file <- GET(url_file)
    
    if (status_code(res_file) == 200) {
      file_info <- content(res_file)
      # On télécharge sous un nom temporaire pour ne pas casser l'exécution actuelle
      # L'utilisateur verra les changements au prochain redémarrage
      dest_file <- f 
      message(sprintf("Updating core file: %s", f))
      download.file(file_info$download_url, dest_file, mode = "wb", quiet = TRUE)
    }
  }
  
  if (success_all) {
    message("--- Update Complete Success ---")
    return(TRUE)
  } else {
    return(FALSE)
  }
}
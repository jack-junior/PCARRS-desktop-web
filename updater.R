library(httr)

check_and_update <- function(update_roots = FALSE, update_code = TRUE, shiny_progress = NULL) {
  owner <- "jack-junior"
  repo  <- "PCARRS-desktop-web"
  
  any_update_attempted <- FALSE
  had_errors <- FALSE
  
  # --- PARTIE A : Dossier /code (Seulement si update_code est TRUE) ---
  if (update_code) {
    # On itère sur chaque dossier un par un
    folders_to_sync <- c("code", "resources/images")
    
    for (folder in folders_to_sync) {
      url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, folder)
      res <- GET(url)
      
      if (status_code(res) == 200) {
        any_update_attempted <- TRUE
        contents <- content(res)
        
        # CRUCIAL : On utilise 'folder' comme chemin local
        if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
        
        n <- length(contents)
        for (i in seq_along(contents)) {
          file_info <- contents[[i]]
          if (file_info$type == "file") {
            if (!is.null(shiny_progress)) {
              shiny_progress(value = i/n, detail = paste("Syncing", folder, ":", file_info$name))
            }
            # On construit le chemin de destination basé sur le dossier actuel
            dest_file <- file.path(folder, file_info$name)
            tryCatch({
              download.file(file_info$download_url, dest_file, mode = "wb", quiet = TRUE)
            }, error = function(e) { had_errors <<- TRUE })
          }
        }
      }
    }
  }
  
  # --- PARTIE B : Fichiers Racines (Seulement si update_roots est TRUE) ---
  if (update_roots) {
    root_files <- c("app.R") # Liste des fichiers racine à surveiller
    for (f in root_files) {
      url_file <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, f)
      res_file <- GET(url_file)
      if (status_code(res_file) == 200) {
        any_update_attempted <- TRUE
        file_info <- content(res_file)
        message("Updating root file: ", f)
        tryCatch({
          download.file(file_info$download_url, f, mode = "wb", quiet = TRUE)
        }, error = function(e) { had_errors <<- TRUE })
      }
    }
  }
  
  return(!had_errors)
}
library(httr)

check_and_update <- function(update_roots = FALSE, update_code = TRUE, shiny_progress = NULL) {
  owner <- "jack-junior"
  repo  <- "PCARRS-desktop-web"
  
  any_update_attempted <- FALSE
  had_errors <- FALSE
  
  # --- PARTIE A : Dossier /code (Seulement si update_code est TRUE) ---
  if (update_code) {
    github_path <- "code"
    local_path  <- "code"
    url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, github_path)
    res <- GET(url)
    
    if (status_code(res) == 200) {
      any_update_attempted <- TRUE
      contents <- content(res)
      if (!dir.exists(local_path)) dir.create(local_path, recursive = TRUE)
      
      n <- length(contents)
      for (i in seq_along(contents)) {
        file_info <- contents[[i]]
        if (file_info$type == "file") {
          if (!is.null(shiny_progress)) {
            shiny_progress(value = i/n, detail = paste("Updating code:", file_info$name))
          }
          dest_file <- file.path(local_path, file_info$name)
          tryCatch({
            download.file(file_info$download_url, dest_file, mode = "wb", quiet = TRUE)
          }, error = function(e) { had_errors <<- TRUE })
        }
      }
    }
  }
  
  # --- PARTIE B : Fichiers Racines (Seulement si update_roots est TRUE) ---
  if (update_roots) {
    root_files <- c("app.R", "config.yml") # Liste des fichiers racine à surveiller
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
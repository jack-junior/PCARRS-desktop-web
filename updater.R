library(httr)

check_and_update <- function() {
  owner <- "jack-junior"
  repo  <- "PCARRS-desktop-web"
  
  # Dossiers à synchroniser (GitHub = Local)
  folders_map <- c("code" = "code")
  #root_files  <- c("app.R", "updater.R", "config.yml.example")
  
  # 2. Fichiers racines (seulement si demandé au démarrage)
  root_files <- if(update_roots) c("app.R", "updater.R", "config.yml") else c()
  

  message("--- Starting System Update from GitHub ---")
  
  # On initialise à FALSE et on passe à TRUE uniquement si on fait des actions réelles
  any_update_attempted <- FALSE
  had_errors <- FALSE
  
  # --- PARTIE A : Mise à jour des dossiers ---
  for (i in seq_along(folders_map)) {
    github_path <- names(folders_map)[i]
    local_path  <- folders_map[[i]]
    
    url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, github_path)
    res <- GET(url)
    
    if (status_code(res) == 200) {
      any_update_attempted <- TRUE
      contents <- content(res)
      if (!dir.exists(local_path)) dir.create(local_path, recursive = TRUE)
      
      for (file_info in contents) {
        if (file_info$type == "file") {
          dest_file <- file.path(local_path, file_info$name)
          message(sprintf("Updating: %s", file_info$name))
          tryCatch({
            download.file(file_info$download_url, dest_file, mode = "wb", quiet = TRUE)
          }, error = function(e) { 
            message("Error downloading ", file_info$name)
            had_errors <<- TRUE 
          })
        }
      }
    } else {
      message(sprintf("Skipping: Folder '%s' not found on GitHub (Status: %s)", github_path, status_code(res)))
      had_errors <- TRUE # On marque une erreur car un dossier attendu est absent
    }
  }
  
  # --- PARTIE B : Fichiers racines ---
   for (f in root_files) {
     url_file <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, f)
     res_file <- GET(url_file)
     
     if (status_code(res_file) == 200) {
       any_update_attempted <- TRUE
       file_info <- content(res_file)
       message(sprintf("Updating core file: %s", f))
       tryCatch({
         download.file(file_info$download_url, f, mode = "wb", quiet = TRUE)
       }, error = function(e) { had_errors <<- TRUE })
     }
   }
   
  # LOGIQUE DE RETOUR FINALE
  if (any_update_attempted && !had_errors) {
    message("--- Update Complete! ---")
    return(TRUE)
  } else {
    message("--- Update Failed or Partial (Check Console) ---")
    return(FALSE)
  }
}
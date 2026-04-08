# --- updater.R ---
library(httr)
library(digest)

# Robust SHA1 calculation to avoid "nul character" errors
git_sha1 <- function(filepath) {
  if (!file.exists(filepath) || file.info(filepath)$size == 0) return("")
  
  sha <- tryCatch({
    con <- file(filepath, "rb")
    size <- file.info(filepath)$size
    content <- readBin(con, "raw", n = size)
    close(con)
    
    header <- charToRaw(paste0("blob ", size, "\0"))
    digest(c(header, content), algo = "sha1", serialize = FALSE)
  }, error = function(e) {
    return("error_reading_file")
  })
  return(sha)
}

check_and_update <- function(update_roots = FALSE) {
  owner <- "jack-junior"
  repo  <- "PCARRS-desktop-web"
  
  folders_map <- c("code" = "code")
  root_files <- if(update_roots) c("app.R", "updater.R", "config.yml") else c()
  
  message("--- Checking GitHub for Updates ---")
  update_performed <- FALSE
  had_errors <- FALSE
  
  # A. Folders Update
  for (i in seq_along(folders_map)) {
    g_path <- names(folders_map)[i]
    l_path <- folders_map[[i]]
    
    res <- GET(sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, g_path))
    
    if (status_code(res) == 200) {
      if (!dir.exists(l_path)) dir.create(l_path, recursive = TRUE)
      for (file_info in content(res)) {
        if (file_info$type == "file") {
          dest <- file.path(l_path, file_info$name)
          if (git_sha1(dest) != file_info$sha) {
            message("Updating: ", file_info$name)
            tryCatch({
              download.file(file_info$download_url, dest, mode = "wb", quiet = TRUE)
              update_performed <- TRUE
            }, error = function(e) { had_errors <<- TRUE })
          }
        }
      }
    }
  }
  
  # B. Root Files Update
  for (f in root_files) {
    res <- GET(sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, f))
    if (status_code(res) == 200) {
      f_info <- content(res)
      if (git_sha1(f) != f_info$sha) {
        message("Updating core file: ", f)
        tryCatch({
          download.file(f_info$download_url, f, mode = "wb", quiet = TRUE)
          update_performed <- TRUE
        }, error = function(e) { had_errors <<- TRUE })
      }
    }
  }
  
  if (had_errors) message("--- Update finished with some errors ---")
  else if (update_performed) message("--- System updated successfully ---")
  else message("--- System already up to date ---")
  
  return(TRUE)
}
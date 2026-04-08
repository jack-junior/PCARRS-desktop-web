library(httr)
library(digest)

# --- Internal Helper: Calculate Git-style SHA1 for a local file ---
# This matches the SHA format used by the GitHub API for comparison
git_sha1 <- function(filepath) {
  if (!file.exists(filepath)) return("")
  size <- file.info(filepath)$size
  # GitHub calculates SHA1 as: "blob [size]\0[content]"
  con <- file(filepath, "rb")
  content <- readBin(con, "raw", n = size)
  close(con)
  
  header <- charToRaw(paste0("blob ", size, "\0"))
  return(digest(c(header, content), algo = "sha1", serialize = FALSE))
}

check_and_update <- function(update_roots = FALSE) {
  owner <- "jack-junior"
  repo  <- "PCARRS-desktop-web"
  
  # Folders to sync (GitHub Folder Name = Local Folder Name)
  folders_map <- c("code" = "code")
  
  # Core files (only updated if update_roots is TRUE)
  root_files <- if(update_roots) c("app.R", "updater.R", "config.yml") else c()
  
  message("--- Starting System Update Check (GitHub) ---")
  
  update_performed <- FALSE
  had_errors <- FALSE
  
  # --- PART A: Folder Synchronization (Smart Diff) ---
  for (i in seq_along(folders_map)) {
    github_path <- names(folders_map)[i]
    local_path  <- folders_map[[i]]
    
    url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, github_path)
    res <- GET(url)
    
    if (status_code(res) == 200) {
      contents <- content(res)
      if (!dir.exists(local_path)) dir.create(local_path, recursive = TRUE)
      
      for (file_info in contents) {
        if (file_info$type == "file") {
          dest_file <- file.path(local_path, file_info$name)
          
          # Check if the file actually changed
          remote_sha <- file_info$sha
          local_sha  <- git_sha1(dest_file)
          
          if (local_sha != remote_sha) {
            message(sprintf("Diff detected: Updating %s...", file_info$name))
            tryCatch({
              download.file(file_info$download_url, dest_file, mode = "wb", quiet = TRUE)
              update_performed <- TRUE
            }, error = function(e) { 
              message("Error downloading: ", file_info$name)
              had_errors <<- TRUE 
            })
          } else {
             message(sprintf("Up to date: %s", file_info$name)) # Optional verbose
          }
        }
      }
    } else {
      message(sprintf("Skipping: Folder '%s' not found (Status: %s)", github_path, status_code(res)))
      had_errors <- TRUE
    }
  }
  
  # --- PART B: Core Files Synchronization (Smart Diff) ---
  for (f in root_files) {
    url_file <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, f)
    res_file <- GET(url_file)
    
    if (status_code(res_file) == 200) {
      file_info <- content(res_file)
      remote_sha <- file_info$sha
      local_sha  <- git_sha1(f)
      
      if (local_sha != remote_sha) {
        message(sprintf("Diff detected: Updating core file %s...", f))
        tryCatch({
          download.file(file_info$download_url, f, mode = "wb", quiet = TRUE)
          update_performed <- TRUE
        }, error = function(e) { 
          message("Error updating core file: ", f)
          had_errors <<- TRUE 
        })
      }
    }
  }
  
  # FINAL STATUS LOGIC
  if (had_errors) {
    message("--- Update Finished with Warnings (Check Console) ---")
    return(FALSE)
  } else if (update_performed) {
    message("--- Update Complete: Files synchronized! ---")
    return(TRUE)
  } else {
    message("--- System is already up to date (No diffs) ---")
    return(TRUE)
  }
}
library(officer)
library(yaml)
library(tidyverse)

# ============================================================
# 1. BATCH CONFIGURATION
# ============================================================
cfg <- yaml::read_yaml("config.yml")

# Retrieve the batch name
b_name <- if (exists("batch_name")) batch_name else ""

if (b_name == "") {
  stop("❌ ERROR: No batch_name defined.")
}

report_dir <- file.path(cfg$paths$reports, b_name, "reports_English")

# Output files also go into this batch-specific English folder
final_filename <- paste0("Full_Combined_Report_", b_name)
final_docx <- file.path(report_dir, paste0(final_filename, ".docx"))
final_pdf  <- file.path(report_dir, paste0(final_filename, ".pdf"))

# ============================================================
# 2. FILE DETECTION
# ============================================================
# List all individual Word reports
report_files <- list.files(report_dir, pattern = "Report_English_.*\\.docx", full.names = TRUE)

if(length(report_files) == 0) {
  stop(paste("❌ ERROR: No individual reports found in:", report_dir))
}

cat("--- Merging", length(report_files), "reports for Batch:", b_name, "---\n")

# ============================================================
# 3. CREATE MASTER DOCUMENT
# ============================================================
combined_doc <- read_docx() %>%
  body_set_default_section(
    value = prop_section(
      page_mar = page_mar(header = 0.3, top = 0.6, bottom = 0.5, left = 1.0, right = 1.0)
    )
  )

# Loop to append each report with a page break
for(i in seq_along(report_files)) {
  # Add the external docx content
  combined_doc <- body_add_docx(combined_doc, src = report_files[i])
  
  # Add a page break only between documents (not after the last one)
  if(i < length(report_files)) {
    combined_doc <- body_add_break(combined_doc)
  }
}

# ============================================================
# 4. SAVE OUTPUTS IN BATCH FOLDER
# ============================================================
final_filename <- paste0("Full_Combined_Report_", b_name)
final_docx <- file.path(report_dir, paste0(final_filename, ".docx"))
final_pdf  <- file.path(report_dir, paste0(final_filename, ".pdf"))

# Write the Word file
print(combined_doc, target = final_docx)

# ============================================================
# 5. PDF CONVERSION
# ============================================================
if(require(doconv)) {
  tryCatch({
    to_pdf(final_docx, output = final_pdf)
    cat("✅ Merge complete! PDF available at:", final_pdf, "\n")
  }, error = function(e) {
    cat("⚠️ PDF conversion failed, but Word file was saved.\n")
  })
} else {
  cat("✅ Merge complete! (Word only, 'doconv' package not found for PDF)\n")
}
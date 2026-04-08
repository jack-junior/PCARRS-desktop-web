library(officer)
library(yaml)
library(tidyverse)

# Chargement de la config pour trouver les dossiers
cfg <- yaml::read_yaml("config.yml")
report_dir <- file.path(cfg$paths$reports, "reports_Hindi")

# 1. Lister tous les fichiers Word individuels
report_files <- list.files(report_dir, pattern = "Report_Hindi_.*\\.docx", full.names = TRUE)

if(length(report_files) == 0) {
  stop("Aucun rapport individuel trouvé pour la fusion.")
}

cat("--- Fusion de", length(report_files), "rapports en cours ---\n")

# 2. Créer le document maître
combined_doc <- read_docx() %>%
  body_set_default_section(
    value = prop_section(
      page_mar = page_mar(header = 0.3, top = 0.6, bottom = 0.5, left = 1.0, right = 1.0)
    )
  )

# 3. Boucler pour ajouter chaque rapport avec un saut de page
for(i in seq_along(report_files)) {
  combined_doc <- body_add_docx(combined_doc, src = report_files[i])
  if(i < length(report_files)) {
    combined_doc <- body_add_break(combined_doc)
  }
}

# 4. Sauvegarde finale
final_docx <- file.path(report_dir, "Full_Study_Combined_Report_Hindi.docx")
final_pdf  <- file.path(report_dir, "Full_Study_Combined_Report_Hindi.pdf")

print(combined_doc, target = final_docx)

# 5. Conversion en PDF
if(require(doconv)) {
  to_pdf(final_docx, output = final_pdf)
}

cat("--- Fusion terminée ! Fichier disponible dans :", final_pdf, "---\n")
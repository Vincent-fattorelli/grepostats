# Point d'entrée de la collecte.
#   Rscript collecte.R            -> tous les mondes de config.yml
#   Rscript collecte.R fr180      -> uniquement le monde indiqué (doit figurer dans config.yml)

# --- Se place automatiquement dans le dossier de ce script -------------------
# (plus besoin de régler le répertoire de travail à la main)
dossier_script <- function() {
  # Lancé avec Rscript
  f <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(f) == 1) return(dirname(normalizePath(f)))
  # Lancé avec source() ou le bouton "Source" de RStudio
  ofile <- sys.frames()[[1]]$ofile
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  # Lancé ligne par ligne dans RStudio (Ctrl + Entrée)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    chemin <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(chemin)) return(dirname(normalizePath(chemin)))
  }
  getwd()
}
setwd(dossier_script())
# -----------------------------------------------------------------------------

source("R/fonctions.R", encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
cfg <- charger_config("config.yml")

mondes <- cfg$mondes
if (length(args) > 0) {
  mondes <- Filter(function(m) m$id %in% args, mondes)
  if (length(mondes) == 0) stop("Monde introuvable dans config.yml : ", paste(args, collapse = ", "))
}

for (m in mondes) collecter_monde(m, dossier_donnees = "data")

# Page d'accueil du site (site/index.html) : mène au monde, ou liste les mondes s'il y en a plusieurs.
ecrire_accueil(vapply(cfg$mondes, function(m) m$id, character(1)), "site")
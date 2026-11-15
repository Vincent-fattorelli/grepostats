# Fonctions de Grepostats : téléchargement, préparation, domination, historique.
# Dépendances : dplyr, yaml, jsonlite

suppressPackageStartupMessages(library(dplyr))

# ---------------------------------------------------------------- Configuration

charger_config <- function(chemin = "config.yml") {
  cfg <- yaml::yaml.load(paste(readLines(chemin, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
  defauts <- cfg$parametres
  # Chaque monde hérite des paramètres communs, qu'il peut surcharger.
  cfg$mondes <- lapply(cfg$mondes, function(m) {
    for (nom in names(defauts)) if (is.null(m[[nom]])) m[[nom]] <- defauts[[nom]]
    # Réglages ajoutés après la première version : valeurs par défaut si absents de config.yml
    internes <- list(archiver_historique = FALSE, conquetes_jours_page = 30,
                     conquetes_jours_max = 90, rayons_historique = c(60, 80, 100, 120, 140),
                     top_alliances_historique = 15)
    for (nom in names(internes)) if (is.null(m[[nom]])) m[[nom]] <- internes[[nom]]
    if (is.null(m$centre)) m$centre <- c(500, 500)
    m$centre <- as.numeric(unlist(m$centre))
    m$alliances_suivies <- as.character(unlist(m$alliances_suivies))
    m$alliances_ennemies <- as.character(unlist(m$alliances_ennemies))
    m
  })
  cfg
}

# ---------------------------------------------------------------- Téléchargement

# Les fichiers de l'API publique n'ont PAS d'en-tête : header = FALSE.
# (Avec header = TRUE, la première ligne de chaque fichier est avalée.)
telecharger <- function(monde, fichier, noms_colonnes, tentatives = 3) {
  url <- sprintf("https://%s.grepolis.com/data/%s.txt", monde, fichier)
  for (essai in seq_len(tentatives)) {
    res <- tryCatch(
      read.csv(url, header = FALSE, stringsAsFactors = FALSE,
               encoding = "UTF-8", na.strings = c("", "NA")),
      error = function(e) NULL
    )
    if (!is.null(res) && nrow(res) > 0) {
      colnames(res) <- noms_colonnes[seq_len(ncol(res))]
      return(res)
    }
    Sys.sleep(2 * essai)
  }
  stop(sprintf("Impossible de télécharger %s", url))
}

# Variante pour les fichiers non indispensables : renvoie NULL s'ils sont indisponibles.
telecharger_optionnel <- function(monde, fichier, noms_colonnes) {
  tryCatch(telecharger(monde, fichier, noms_colonnes, tentatives = 2),
           error = function(e) { message("  (", fichier, " indisponible, ignoré)"); NULL })
}

# Remplace l'ancienne cascade de str_replace_all : gère tous les caractères.
decoder <- function(x) {
  x <- gsub("+", " ", x, fixed = TRUE)
  out <- vapply(x, function(s) {
    if (is.na(s)) NA_character_ else tryCatch(URLdecode(s), error = function(e) s)
  }, character(1), USE.NAMES = FALSE)
  Encoding(out) <- "UTF-8"
  out
}

charger_monde <- function(monde) {
  list(
    joueurs   = telecharger(monde, "players",
                  c("ID_joueur", "Joueur", "ID_alliance", "Points_joueur",
                    "Classement_joueur", "Nb_villes_joueur")),
    alliances = telecharger(monde, "alliances",
                  c("ID_alliance", "Alliance", "Points_alliance",
                    "Nb_villes_alliance", "Nb_membres", "Classement_alliance")),
    villes    = telecharger(monde, "towns",
                  c("ID_ville", "ID_joueur", "Nom_ville", "CoordsX", "CoordsY",
                    "Position_Ile", "Points_ville")),
    iles      = telecharger(monde, "islands",
                  c("ID_Ile", "CoordsX", "CoordsY", "Type_ile", "Empl_disp",
                    "Ressource_plus", "Ressource_moins")),
    conquetes = telecharger(monde, "conquers",
                  c("ID_ville", "Horodatage", "ID_joueur_nouveau",
                    "ID_joueur_ancien", "ID_alliance_nouveau",
                    "ID_alliance_ancien", "Points_ville")),
    # Points de combat (fichiers optionnels : rang, id, points)
    player_kills_all   = telecharger_optionnel(monde, "player_kills_all",   c("Rang", "ID_joueur", "Points")),
    player_kills_att   = telecharger_optionnel(monde, "player_kills_att",   c("Rang", "ID_joueur", "Points")),
    player_kills_def   = telecharger_optionnel(monde, "player_kills_def",   c("Rang", "ID_joueur", "Points")),
    alliance_kills_all = telecharger_optionnel(monde, "alliance_kills_all", c("Rang", "ID_alliance", "Points")),
    alliance_kills_att = telecharger_optionnel(monde, "alliance_kills_att", c("Rang", "ID_alliance", "Points")),
    alliance_kills_def = telecharger_optionnel(monde, "alliance_kills_def", c("Rang", "ID_alliance", "Points"))
  )
}

# ---------------------------------------------------------------- Préparation

# Construit la table complète des villes : île, joueur, alliance, distance au centre.
preparer_villes <- function(d, cfg_monde) {
  joueurs   <- d$joueurs   |> mutate(Joueur = decoder(Joueur))
  alliances <- d$alliances |> mutate(Alliance = decoder(Alliance))
  villes    <- d$villes    |> mutate(Nom_ville = decoder(Nom_ville))

  iles <- d$iles |>
    transmute(CoordsX, CoordsY, Type_ile,
              Domination = Type_ile %in% cfg_monde$types_iles_domination)

  centre <- cfg_monde$centre
  villes |>
    left_join(iles, by = c("CoordsX", "CoordsY")) |>
    left_join(joueurs |> select(ID_joueur, Joueur, ID_alliance), by = "ID_joueur") |>
    left_join(alliances |> select(ID_alliance, Alliance), by = "ID_alliance") |>
    mutate(
      Distance = round(sqrt((CoordsX - centre[1])^2 + (CoordsY - centre[2])^2), 4),
      # Ville sans joueur = ville fantôme ; joueur sans alliance = "Sans alliance"
      Joueur   = coalesce(Joueur, "(ville fantôme)"),
      Alliance = case_when(
        is.na(ID_joueur)    ~ "(ville fantôme)",
        is.na(Alliance)     ~ "Sans alliance",
        TRUE                ~ Alliance
      )
    )
}

# ---------------------------------------------------------------- Domination

# Part de chaque alliance (ou joueur) sur les villes de domination
# situées à une distance <= r du centre, pour chaque rayon r.
domination_par_rayon <- function(villes, rayons, par = c("Alliance", "Joueur")) {
  par <- match.arg(par)
  vp <- villes |> filter(Domination %in% TRUE)
  bind_rows(lapply(rayons, function(r) {
    v <- vp |> filter(Distance <= r)
    total <- nrow(v)
    if (total == 0) return(NULL)
    v |>
      count(.data[[par]], name = "Nb_villes") |>
      arrange(desc(Nb_villes)) |>
      mutate(Rayon = r, Total_villes = total,
             Pourcentage = round(100 * Nb_villes / total, 2))
  }))
}

# ---------------------------------------------------------------- Historique

# Ligne(s) à archiver pour cette collecte (compacte : top N + alliances suivies).
instantane_historique <- function(villes, cfg_monde, horodatage) {
  dom <- domination_par_rayon(villes, cfg_monde$rayons_historique, "Alliance")
  garder <- dom |>
    group_by(Rayon) |>
    mutate(rang = row_number()) |>
    ungroup() |>
    filter(rang <= cfg_monde$top_alliances_historique |
             Alliance %in% cfg_monde$alliances_suivies) |>
    select(-rang)
  garder$Horodatage <- horodatage
  garder |> select(Horodatage, Rayon, Alliance, Nb_villes, Total_villes, Pourcentage)
}

# Ajoute à un CSV existant sans doublon (clé = colonnes `cle`).
ajouter_csv <- function(nouveau, chemin, cle) {
  if (file.exists(chemin)) {
    ancien <- read.csv(chemin, stringsAsFactors = FALSE, encoding = "UTF-8")
    nouveau <- bind_rows(ancien, nouveau)
  }
  nouveau <- nouveau |> distinct(across(all_of(cle)), .keep_all = TRUE)
  write.csv(nouveau, chemin, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(nouveau)
}

# ---------------------------------------------------------------- Page web

# Écrit site/<monde>/donnees.js (les données) et index.html (la page, copiée
# depuis R/modele_site.html). La page s'ouvre d'un double-clic, sans serveur.
exporter_site <- function(d, villes, cfg_monde, horodatage, dossier_site) {
  dir.create(dossier_site, recursive = TRUE, showWarnings = FALSE)

  # Villes de domination dans le rayon maximum (sert aux conquêtes récentes)
  vp <- villes |> filter(Domination %in% TRUE, Distance <= cfg_monde$rayon_max)

  # Conquêtes récentes sur ces villes. Contrôle de cohérence : l'horodatage doit
  # ressembler à un temps Unix (secondes).
  conq <- d$conquetes
  if (nrow(conq) > 0 && !all(conq$Horodatage > 1.2e9 & conq$Horodatage < as.numeric(Sys.time()) + 86400, na.rm = TRUE)) {
    warning("conquers.txt : la colonne 'Horodatage' ne ressemble pas à un temps Unix, vérifier le format.")
  }
  joueurs   <- d$joueurs   |> transmute(ID_joueur, Joueur = decoder(Joueur))
  alliances <- d$alliances |> transmute(ID_alliance, Alliance = decoder(Alliance))
  limite <- as.numeric(Sys.time()) - cfg_monde$conquetes_jours_page * 86400

  c_ <- conq |>
    filter(Horodatage >= limite) |>
    inner_join(vp |> select(ID_ville, CoordsX, CoordsY, Distance), by = "ID_ville") |>
    left_join(joueurs   |> rename(nj = Joueur),   by = c("ID_joueur_nouveau"   = "ID_joueur")) |>
    left_join(joueurs   |> rename(oj = Joueur),   by = c("ID_joueur_ancien"    = "ID_joueur")) |>
    left_join(alliances |> rename(alnew = Alliance), by = c("ID_alliance_nouveau" = "ID_alliance")) |>
    left_join(alliances |> rename(alold = Alliance), by = c("ID_alliance_ancien"  = "ID_alliance")) |>
    mutate(
      nj = case_when(!is.na(nj) ~ nj, is.na(ID_joueur_nouveau) ~ "(ville fantôme)", TRUE ~ "(joueur supprimé)"),
      oj = case_when(!is.na(oj) ~ oj, is.na(ID_joueur_ancien)  ~ "(ville fantôme)", TRUE ~ "(joueur supprimé)"),
      alnew = case_when(!is.na(alnew) ~ alnew, is.na(ID_joueur_nouveau) ~ "(ville fantôme)",
                        is.na(ID_alliance_nouveau) ~ "Sans alliance", TRUE ~ "(alliance dissoute)"),
      alold = case_when(!is.na(alold) ~ alold, is.na(ID_joueur_ancien) ~ "(ville fantôme)",
                        is.na(ID_alliance_ancien) ~ "Sans alliance", TRUE ~ "(alliance dissoute)")
    ) |>
    transmute(i = ID_ville, t = Horodatage, x = CoordsX, y = CoordsY, d = Distance,
              oj = oj, oa = alold, nj = nj, na = alnew, p = Points_ville)

  # Toutes les villes du monde sont exportées : de joueurs (fiche joueur), fantômes
  # (leur liste, y compris sur les cailloux) et de domination. Pour chacune, dernière
  # prise connue (ancien propriétaire, date), seulement si le nouveau propriétaire
  # de la conquête est bien le propriétaire actuel.
  derniere <- conq |>
    group_by(ID_ville) |>
    slice_max(Horodatage, n = 1, with_ties = FALSE) |>
    ungroup() |>
    left_join(joueurs   |> rename(oj = Joueur),      by = c("ID_joueur_ancien" = "ID_joueur")) |>
    left_join(alliances |> rename(alold = Alliance), by = c("ID_alliance_ancien" = "ID_alliance")) |>
    mutate(
      oj = case_when(!is.na(oj) ~ oj, is.na(ID_joueur_ancien) ~ "(ville fantôme)", TRUE ~ "(joueur supprimé)"),
      alold = case_when(!is.na(alold) ~ alold, is.na(ID_joueur_ancien) ~ "(ville fantôme)",
                        is.na(ID_alliance_ancien) ~ "Sans alliance", TRUE ~ "(alliance dissoute)")
    ) |>
    select(ID_ville, ID_nouveau = ID_joueur_nouveau, o_nom = oj, o_all = alold, o_t = Horodatage)

  v <- villes |>
    left_join(derniere, by = "ID_ville") |>
    mutate(ok = (!is.na(ID_nouveau) & !is.na(ID_joueur) & ID_nouveau == ID_joueur) %in% TRUE) |>
    transmute(id = ID_ville, n = Nom_ville, x = CoordsX, y = CoordsY,
              s = coalesce(Position_Ile, 0L), d = Distance, j = Joueur, a = Alliance,
              p = Points_ville, dom = as.integer(Domination %in% TRUE),
              o = ifelse(ok, o_nom, NA_character_), oa = ifelse(ok, o_all, NA_character_),
              t = ifelse(ok, o_t, NA_real_))

  # Classements : points, villes, points de combat (total / attaque / défense)
  jd <- d$joueurs   |> mutate(Joueur = decoder(Joueur))
  ad <- d$alliances |> mutate(Alliance = decoder(Alliance))
  ajouter_pc <- function(base, cle, prefixe) {
    for (s in c("all", "att", "def")) {
      k <- d[[paste0(prefixe, "_kills_", s)]]
      if (is.null(k)) {
        base[[paste0("pc_", s)]] <- NA_real_
        base[[paste0("rg_", s)]] <- NA_real_
        next
      }
      names(k) <- c(paste0("rg_", s), cle, paste0("pc_", s))
      base <- left_join(base, k[!duplicated(k[[cle]]), ], by = cle)
    }
    base
  }
  cls_joueurs <- jd |>
    left_join(ad |> select(ID_alliance, Alliance), by = "ID_alliance") |>
    ajouter_pc("ID_joueur", "player") |>
    transmute(n = Joueur, a = coalesce(Alliance, "Sans alliance"),
              p = Points_joueur, rg = Classement_joueur, v = Nb_villes_joueur,
              pa = pc_all, ra = rg_all, pt = pc_att, rt = rg_att, pd = pc_def, rd = rg_def)
  cls_alliances <- ad |>
    ajouter_pc("ID_alliance", "alliance") |>
    transmute(n = Alliance, p = Points_alliance, rg = Classement_alliance,
              v = Nb_villes_alliance, m = Nb_membres,
              pa = pc_all, ra = rg_all, pt = pc_att, rt = rg_att, pd = pc_def, rd = rg_def)

  # Îles de domination (dans le rayon maximum) qui ont encore des emplacements de fondation libres.
  # Une île de domination a 20 emplacements : villes présentes + Empl_disp = 20.
  centre <- cfg_monde$centre
  iles_libres <- d$iles |>
    filter(Type_ile %in% cfg_monde$types_iles_domination, Empl_disp > 0) |>
    mutate(Distance = round(sqrt((CoordsX - centre[1])^2 + (CoordsY - centre[2])^2), 4)) |>
    filter(Distance <= cfg_monde$rayon_max) |>
    transmute(id = ID_Ile, x = CoordsX, y = CoordsY, l = Empl_disp, d = Distance)

  meta <- list(monde = cfg_monde$id, centre = cfg_monde$centre,
               alliances_suivies = cfg_monde$alliances_suivies,
               alliances_ennemies = cfg_monde$alliances_ennemies,
               rayon_min = cfg_monde$rayon_min, rayon_max = cfg_monde$rayon_max,
               conquetes_jours = cfg_monde$conquetes_jours_page,
               maj_utc = horodatage)
  js <- paste0("window.DONNEES = ",
               jsonlite::toJSON(list(meta = meta, villes = v, conquetes = c_, iles = iles_libres,
                                     classements = list(joueurs = cls_joueurs, alliances = cls_alliances)),
                                auto_unbox = TRUE, dataframe = "rows", na = "null", digits = NA),
               ";")
  writeLines(js, file.path(dossier_site, "donnees.js"), useBytes = TRUE)
  file.copy("R/modele_site.html", file.path(dossier_site, "index.html"), overwrite = TRUE)

  # Version en un seul fichier (page + données), à envoyer telle quelle (Discord...) :
  # elle s'ouvre sur téléphone sans dézipper.
  modele <- paste(readLines("R/modele_site.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
  balise <- '<script src="donnees.js"></script>'
  if (!grepl(balise, modele, fixed = TRUE)) stop("modele_site.html : balise donnees.js introuvable")
  morceaux <- strsplit(modele, balise, fixed = TRUE)[[1]]
  # "</" échappé pour qu'un nom de ville contenant "</script>" ne coupe pas la page.
  js_inline <- gsub("</", "<\\/", js, fixed = TRUE)
  autonome <- paste0(morceaux[1], "<script>", js_inline, "</script>", paste(morceaux[-1], collapse = balise))
  writeLines(enc2utf8(autonome), file.path(dirname(dossier_site), paste0("grepostats-", cfg_monde$id, ".html")), useBytes = TRUE)

  invisible(file.path(dossier_site, "index.html"))
}

ecrire_accueil <- function(mondes, dossier_site = "site") {
  dir.create(dossier_site, recursive = TRUE, showWarnings = FALSE)
  redirection <- if (length(mondes) == 1)
    sprintf('<meta http-equiv="refresh" content="0; url=%s/">', mondes) else ""
  liens <- paste0(sprintf('<li><a href="%s/">%s</a></li>', mondes, mondes), collapse = "\n")
  html <- sprintf('<!doctype html>
<html lang="fr"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Grepostats</title>%s
<style>body{font:16px system-ui,sans-serif;background:#0f1520;color:#e6e9ef;padding:24px}a{color:#56B4E9}</style>
</head><body><h1>Grepostats</h1><ul>%s</ul></body></html>', redirection, liens)
  writeLines(enc2utf8(html), file.path(dossier_site, "index.html"), useBytes = TRUE)
}

# ---------------------------------------------------------------- Orchestration

collecter_monde <- function(cfg_monde, dossier_donnees = "data") {
  monde <- cfg_monde$id
  message(sprintf("[%s] Téléchargement...", monde))
  d <- charger_monde(monde)

  message(sprintf("[%s] Préparation...", monde))
  villes <- preparer_villes(d, cfg_monde)
  horodatage <- format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC")

  sortie <- file.path(dossier_donnees, monde)
  dir.create(file.path(sortie, "latest"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(sortie, "history"), recursive = TRUE, showWarnings = FALSE)

  rayons <- seq(cfg_monde$rayon_min, cfg_monde$rayon_max)

  message(sprintf("[%s] Calcul de la domination...", monde))
  ecrire <- function(x, nom) write.csv(x, file.path(sortie, "latest", nom),
                                       row.names = FALSE, fileEncoding = "UTF-8")
  ecrire(villes |> filter(Domination %in% TRUE), "villes_domination.csv")
  ecrire(domination_par_rayon(villes, rayons, "Alliance"), "domination_alliances.csv")
  ecrire(domination_par_rayon(villes, rayons, "Joueur"),   "domination_joueurs.csv")

  # Métadonnées lues par le site : monde, centre, alliances suivies, date.
  meta <- list(monde = monde, centre = cfg_monde$centre,
               alliances_suivies = cfg_monde$alliances_suivies,
               alliances_ennemies = cfg_monde$alliances_ennemies,
               rayon_min = cfg_monde$rayon_min, rayon_max = cfg_monde$rayon_max,
               maj_utc = horodatage)
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
             file.path(sortie, "latest", "meta.json"))

  message(sprintf("[%s] Page web...", monde))
  page <- exporter_site(d, villes, cfg_monde, horodatage, file.path("site", monde))

  # Archivage de ses propres instantanés : désactivé par défaut (l'API fournit
  # déjà l'historique des conquêtes). Activer avec archiver_historique: true.
  if (isTRUE(cfg_monde$archiver_historique)) {
    message(sprintf("[%s] Historique...", monde))
    ajouter_csv(instantane_historique(villes, cfg_monde, horodatage),
                file.path(sortie, "history", "domination_historique.csv"),
                cle = c("Horodatage", "Rayon", "Alliance"))
    limite <- as.numeric(Sys.time()) - cfg_monde$conquetes_jours_max * 86400
    ajouter_csv(d$conquetes |> filter(Horodatage >= limite),
                file.path(sortie, "history", "conquetes.csv"),
                cle = c("ID_ville", "Horodatage"))
  }

  message(sprintf("[%s] OK (%s) - page : %s", monde, horodatage, normalizePath(page)))
  invisible(villes)
}

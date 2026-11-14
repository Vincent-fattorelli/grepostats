# Grepostats

Collecte automatique des données Grepolis et calcul de la domination, pour n'importe quel monde.

## Changer de monde

Tout se règle dans `config.yml` :

```yaml
mondes:
  - id: fr171                      # <- le monde (fr180, en120, de95...)
    centre: [500, 500]             # centre de la carte
    alliances_suivies: ["Tamiastan", "Tamiastan 2.0"]
```

Tu peux ajouter plusieurs blocs pour suivre plusieurs mondes. Les données de chaque monde sont rangées dans `data/<monde>/`.
`Rscript collecte.R fr180` ne collecte que ce monde.

## Utilisation

Double-clic sur `mettre-a-jour.bat` (ou `Rscript collecte.R`). Le dossier `site/` s'ouvre ensuite :
`grepostats-<monde>.html` est la version en un seul fichier, à envoyer telle quelle (Discord…), elle s'ouvre sur téléphone.

Packages requis : `dplyr`, `yaml`, `jsonlite`.

## Ce qui est produit (`data/<monde>/`)

| Fichier | Contenu |
|---|---|
| `latest/villes_domination.csv` | villes des îles de domination, avec joueur, alliance, distance au centre |
| `latest/domination_alliances.csv` | % de domination de chaque alliance, pour chaque rayon |
| `latest/domination_joueurs.csv` | idem par joueur |
| `latest/meta.json` | monde, centre, alliances suivies, date de mise à jour |
| `history/domination_historique.csv` | archive de chaque collecte (rayons clés, top alliances + alliances suivies) |
| `history/conquetes.csv` | conquêtes sur une fenêtre glissante (90 jours par défaut) |

## Mise à jour automatique

Sur GitHub, le workflow `.github/workflows/collecte.yml` collecte toutes les 6 heures et publie le site sur GitHub Pages (réglage du dépôt : *Settings → Pages → Source : GitHub Actions*). Il se lance aussi à chaque envoi de modifications, ou à la main (*Actions → Collecte et publication → Run workflow*). Grepolis rafraîchit ses fichiers chaque heure, ne descends pas sous cet intervalle.

GitHub met en pause les tâches planifiées d'un dépôt sans activité depuis 60 jours : il suffit alors de cliquer sur « Enable workflow » dans l'onglet Actions.

## Points à vérifier

- `types_iles_domination` reprend ta règle d'origine (types 1 à 10 et 37 à 46) : à confirmer selon les règles du monde.
- Les villes sans joueur sont comptées dans le total sous « (ville fantôme) » ; les joueurs sans alliance sous « Sans alliance ».

# Bechdel Test Visualization

Projet de visualisation autour du test de Bechdel. L'objectif est de charger les données existantes sur les films ayant (ou non) passé le test, puis de fournir des visualisations interactives pour explorer les tendances temporelles, les genres, les réalisateurs, etc.

## Structure

- `data/` : données brutes (stocks). Les données viennent du site [bechdeltest.com](https://bechdeltest.com), elles ont été agrégées une première fois en 2023 quand le site proposait une API, et nous avons scrappé les nouvelles données début 2026. Les données sont ensuite fusionnées avec des données d'IMDB, ce qui donne all_movies.csv. Cette ajout a été fait à l'extérieur du repos. En clair movies.csv + last_import.csv (scraping) + imdb.csv (pas présent) = all_movies.csv.
- `src/` : scripts Python pour importer et préparer les données (scraping, fusion).
- `shiny_app/` : code de l’application Shiny pour visualiser les données (en R).

## Instructions

1. Gardez les données que nous avons mis à votre disposition dans le dossier `data/`.
2. Installer les packages nécessaires
3. Lancer l’application Shiny (`shiny::runApp('shiny_app')`).
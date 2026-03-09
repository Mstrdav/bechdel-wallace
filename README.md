# Bechdel Test Visualization

Projet de visualisation autour du test de Bechdel. L'objectif est de charger les données existantes sur les films ayant (ou non) passé le test, puis de fournir des visualisations interactives pour explorer les tendances temporelles, les genres, les réalisateurs, etc.

## Structure

- `data/` : données brutes (stocks). On dispose aujourd’hui de deux sources : le CSV public FiveThirtyEight (importé depuis `src/import_data.py`) et le dépôt supplémentaire `bechdel_movies_2023_FEB.csv` que tu viens de fournir (10k films, colonnes `title/year/rating/dubious/imdbid`).
- `src/` : scripts Python pour importer et préparer les données (analyse/visualisation à faire ensuite).

## Import des données

Un script d'import (`src/import_data.py`) télécharge le CSV des films maintenu par FiveThirtyEight et le sauvegarde en local dans `data/raw/movies.csv`. Le script affiche un petit résumé (nombre de lignes, années couvertes) mais ne produit pas encore de visualisation.

## Étapes suivantes

1. Construire des visualisations (dashboards, plots) sur la base du dataset importé.
2. Ajouter la couche d’analyse : filtrage par décennie, comparaison genres, etc.
3. Déployer le rendu (web, notebook ou dashboard).
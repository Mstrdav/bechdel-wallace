# Bechdel Test Visualization

Projet de visualisation autour du test de Bechdel. L'objectif est de charger les données existantes sur les films ayant (ou non) passé le test, puis de fournir des visualisations interactives pour explorer les tendances temporelles, les genres, les réalisateurs, etc.

## Structure

- `data/` : données brutes (stocks). Pour l'instant, nous utilisons les données publiques de FiveThirtyEight.
- `src/` : scripts Python pour importer et préparer les données (analyse/visualisation à faire ensuite).

## Import des données

Un script d'import (`src/import_data.py`) télécharge le CSV des films maintenu par FiveThirtyEight et le sauvegarde en local dans `data/raw/movies.csv`. Le script affiche un petit résumé (nombre de lignes, années couvertes) mais ne produit pas encore de visualisation.

## Étapes suivantes

1. Construire des visualisations (dashboards, plots) sur la base du dataset importé.
2. Ajouter la couche d’analyse : filtrage par décennie, comparaison genres, etc.
3. Déployer le rendu (web, notebook ou dashboard).
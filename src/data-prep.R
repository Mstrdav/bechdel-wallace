# --- data_prep.R ---
library(data.table)
library(arrow)

print("Début du pré-processing...")

# Chargement du dataset initial
raw_data <- fread("data/raw/all_movies.csv")

# 1. Nettoyage HTML (fait une seule fois pour toutes)
if ("title" %in% names(raw_data)) {
  raw_data[, title := gsub("&#39;", "'", title, fixed = TRUE)]
  raw_data[, title := gsub("&quot;", "\"", title, fixed = TRUE)]
  raw_data[, title := gsub("&amp;", "&", title, fixed = TRUE)]
}

# 2. Renommage et calculs
if ("rating_bw" %in% names(raw_data)) setnames(raw_data, "rating_bw", "rating_val")
if ("ratings" %in% names(raw_data)) setnames(raw_data, "ratings", "rating_val")
if ("year" %in% names(raw_data)) raw_data[, decade := paste0(floor(year / 10) * 10, "s")]
if ("imdbid" %in% names(raw_data)) {
  raw_data[, imdbid := sprintf("%07d", as.integer(gsub("^tt", "", as.character(imdbid))))]
}
if ("genres" %in% names(raw_data)) setnames(raw_data, "genres", "genre")
if ("Genre" %in% names(raw_data)) setnames(raw_data, "Genre", "genre")
if ("countries" %in% names(raw_data)) setnames(raw_data, "countries", "country")

# 3. Pré-calcul des tables dérivées (Genres et Pays)
genres_dt <- data.table()
if("genre" %in% names(raw_data)) {
  genres_dt <- raw_data[!is.na(genre) & genre != "", 
                        .(genre = trimws(unlist(strsplit(genre, ",\\s*|\\|")))), 
                        by = .(imdbid, title, rating_val, year, decade)]
}

countries_dt <- data.table()
if("country" %in% names(raw_data)) {
  countries_dt <- raw_data[!is.na(country) & country != "", 
                           .(country = trimws(unlist(strsplit(country, ",\\s*|\\|")))), 
                           by = .(imdbid, rating_val, year, decade)]
}

# 4. Création du dossier et Sauvegarde en format Parquet (arrow)
dir.create("data/processed", showWarnings = FALSE)

write_parquet(raw_data, "data/processed/movies_clean.parquet")
if(nrow(genres_dt) > 0) write_parquet(genres_dt, "data/processed/genres.parquet")
if(nrow(countries_dt) > 0) write_parquet(countries_dt, "data/processed/countries.parquet")

print("Pré-processing terminé et sauvegardé en Parquet dans le dossier data/processed/ !")
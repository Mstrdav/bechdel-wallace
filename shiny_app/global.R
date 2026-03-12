# --- global.R ---
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(data.table)
library(arrow) 
library(magrittr)

# 1. Chargement ultra-rapide des données Parquet
# setDT() garantit que l'objet importé se comporte comme un data.table dans le serveur
raw_data <- setDT(read_parquet("../data/processed/movies_clean.parquet"))

genres_dt <- data.table()
if (file.exists("../data/processed/genres.parquet")) {
  genres_dt <- setDT(read_parquet("../data/processed/genres.parquet"))
}

countries_dt <- data.table()
if (file.exists("../data/processed/countries.parquet")) {
  countries_dt <- setDT(read_parquet("../data/processed/countries.parquet"))
}

# 2. Indexation (setkey) pour accélérer massivement les filtres par la suite
setkey(raw_data, year, decade, rating_val)
if(nrow(genres_dt) > 0) setkey(genres_dt, year, decade, rating_val)
if(nrow(countries_dt) > 0) setkey(countries_dt, year, decade, rating_val)

# 3. Variables globales utiles pour l'UI et le Serveur
all_genres <- if(nrow(genres_dt) > 0) sort(unique(genres_dt$genre)) else NULL
all_decades <- sort(unique(raw_data$decade))
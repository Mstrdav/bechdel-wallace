# --- global.R ---
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(data.table)
library(arrow)
library(cachem) # Ajout pour la gestion de la mémoire cache

# 1. Chargement ultra-rapide des données Parquet
raw_data <- setDT(read_parquet("../data/processed/movies_clean.parquet"))

genres_dt <- data.table()
if (file.exists("../data/processed/genres.parquet")) {
  genres_dt <- setDT(read_parquet("../data/processed/genres.parquet"))
}

countries_dt <- data.table()
if (file.exists("../data/processed/countries.parquet")) {
  countries_dt <- setDT(read_parquet("../data/processed/countries.parquet"))
}

# 2. Indexation (setkey)
setkey(raw_data, year, decade, rating_val)
if(nrow(genres_dt) > 0) setkey(genres_dt, year, decade, rating_val)
if(nrow(countries_dt) > 0) setkey(countries_dt, year, decade, rating_val)

# 3. Variables globales
all_genres <- if(nrow(genres_dt) > 0) sort(unique(genres_dt$genre)) else NULL
all_decades <- sort(unique(raw_data$decade))

# 4. NOUVEAU : Configuration d'un cache global de 500 Mo en RAM
# Partagé entre toutes les sessions utilisateurs
shinyOptions(cache = cachem::cache_mem(max_size = 500 * 1024^2))
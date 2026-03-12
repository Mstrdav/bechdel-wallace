# --- global.R ---
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(data.table)
library(arrow)
library(cachem) 

# 1. Chargement des données Parquet
raw_data <- setDT(read_parquet("../data/processed/movies_clean.parquet"))
genres_dt <- if (file.exists("../data/processed/genres.parquet")) setDT(read_parquet("../data/processed/genres.parquet")) else data.table()
countries_dt <- if (file.exists("../data/processed/countries.parquet")) setDT(read_parquet("../data/processed/countries.parquet")) else data.table()

# 2. Indexation pour performances data.table
setkey(raw_data, year, decade, rating_val)
if(nrow(genres_dt) > 0) setkey(genres_dt, year, decade, rating_val)
if(nrow(countries_dt) > 0) setkey(countries_dt, year, decade, rating_val)

# 3. Listes globales pour les filtres UI
all_genres <- if(nrow(genres_dt) > 0) sort(unique(genres_dt$genre)) else NULL
all_decades <- sort(unique(raw_data$decade))

# 4. Cache partagé global de 500 Mo
shinyOptions(cache = cachem::cache_mem(max_size = 500 * 1024^2))
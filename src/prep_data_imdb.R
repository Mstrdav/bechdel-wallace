library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

# Chargement & sélection des colonnes
df <- fromJSON(read_file("_all_movies.json"), flatten = TRUE)

colonnes_cibles <- c(
  "id", "title", "release_year", "runtime_minutes", "certificate",
  "is_adult", "rating", "vote_count", "metascore", "current_rank",
  "total_nominations", "review_count", "genres",
  "languages", "countries", "enhanced_directors"
)
df_final <- df[, intersect(colonnes_cibles, colnames(df))]

# collapse une colonne liste en string 
collapse_col <- function(x) {
  sapply(x, function(val) {
    if (is.null(val) || length(val) == 0) return(NA_character_)
    if (is.data.frame(val)) val <- unlist(val)
    paste(trimws(as.character(val[!is.na(val)])), collapse = ", ")
  })
}

# Colonnes simples (listes de strings)
for (col in c("genres", "languages", "countries")) {
  if (col %in% colnames(df_final))
    df_final[[col]] <- collapse_col(df_final[[col]])
}

# enhanced_directors -> extraire juste le nom
if ("enhanced_directors" %in% colnames(df_final)) {
  df_final$director <- sapply(df_final$enhanced_directors, function(val) {
    if (is.null(val) || length(val) == 0) return(NA_character_)
    if (is.data.frame(val) && "name" %in% colnames(val))
      return(paste(trimws(val$name), collapse = ", "))
    NA_character_
  })
  df_final$enhanced_directors <- NULL
}

# Types
df_final <- df_final %>%
  mutate(
    release_year      = as.integer(release_year),
    runtime_minutes   = as.numeric(runtime_minutes),
    rating            = as.numeric(rating),
    vote_count        = as.integer(vote_count),
    metascore         = as.numeric(metascore),
    current_rank      = as.integer(current_rank),
    total_nominations = as.integer(total_nominations),
    review_count      = as.integer(review_count),
    is_adult          = as.logical(is_adult),
    across(where(is.character), ~ stri_trans_general(., "Latin-ASCII"))
  )

# Join et tri
df2 <- read.csv("movies.csv", header = TRUE) %>% 
  mutate(imdbid = paste0("tt", str_pad(as.character(imdbid), width = 7, pad = "0"))) %>% #Format ID
  group_by(imdbid) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>% 
  ungroup()

df_joint <- inner_join(df_final, df2, by = c("id" = "imdbid"))

df_final <- df_joint %>%
  select(
    imdbid          = id,
    id              = id.y,
    submitterid,
    title           = title.x,
    year,
    countries,
    rating_bw       = rating.y,
    dubious,
    runtime_minutes,
    rating          = rating.x,
    # On garde tout le reste...
    everything()
  ) %>% 
  # on retire ce qui est inutile
  select(-title.y, -release_year)

#Export
df_final <- df_final %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))
write_csv(df_final, "all_movies.csv")

View(df_final) 

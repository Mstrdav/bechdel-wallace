# 1. Graphique : Histogramme empilé par décennie
build_hist_plot <- function(df, is_dark) {
  hist_data <- df[, .(count = .N), by = .(decade, rating_val)]
  hist_data[, percentage := (count / sum(count)) * 100, by = decade]
  hist_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
  
  plot_ly(hist_data, x = ~decade, y = ~percentage, color = ~rating_val, 
          colors = c('3'='#2ecc71', '2'='#f1c40f', '1'='#e67e22', '0'='#e74c3c'), 
          type = "bar", text = ~paste0(round(percentage, 1), "%"), 
          textfont = list(color = get_theme_cols(is_dark)$txt), hoverinfo = "text+name") |>
    style_plotly_axes(is_dark, x_title = "Décennie", y_title = "%", y_range = c(0, 100), barmode = "stack")
}

# 2. Graphique : Taux de réussite par genre (100% Empilé)
build_genre_plot <- function(df_filt, is_dark) {
  # Filtre pour garder les genres ayant un minimum de représentation (>= 5 films)
  totals <- df_filt[, .(total = .N), by = genre]
  valid_genres <- totals[total >= 5]
  
  if(nrow(valid_genres) == 0) return(NULL)
  
  # Préparation des données complètes pour l'empilement
  g_data <- df_filt[genre %in% valid_genres$genre, .(count = .N), by = .(genre, rating_val)]
  g_data[, percentage := (count / sum(count)) * 100, by = genre]
  g_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
  
  # Logique de tri : l'axe Y est trié selon le % de réussite (Score 3) croissant de bas en haut
  order_ref <- g_data[rating_val == "3", .(genre, percentage)]
  valid_genres <- merge(valid_genres, order_ref, by = "genre", all.x = TRUE)
  valid_genres[is.na(percentage), percentage := 0] # Ceux qui n'ont aucun score 3
  ordered_genres <- valid_genres[order(percentage)]$genre 
  
  g_data[, genre := factor(genre, levels = ordered_genres)]
  
  plot_ly(g_data, y = ~genre, x = ~percentage, color = ~rating_val, 
          colors = c('3'='#2ecc71', '2'='#f1c40f', '1'='#e67e22', '0'='#e74c3c'), 
          type = "bar", orientation = 'h', text = ~paste0(round(percentage, 1), "%"), 
          textfont = list(color = get_theme_cols(is_dark)$txt), hoverinfo = "text+name") |>
    style_plotly_axes(is_dark, x_title = "% de films", y_title = "", barmode = "stack", margin = list(l = 120))
}

# 3. Graphique : Tendance d'évolution temporelle
build_trend_plot <- function(df, is_dark) {
  trend_data <- df[, .(pct_pass = (sum(rating_val == 3) / .N) * 100), by = year][order(year)]
  
  plot_ly(trend_data, x = ~year, y = ~pct_pass, type = 'scatter', mode = 'lines+markers', 
          line = list(color = '#2ecc71'), marker = list(size = 4)) |>
    style_plotly_axes(is_dark, x_title = "Année", y_title = "% Réussite")
}

# 4. Graphique : Carte géographique mondiale
build_map_plot <- function(c_data, is_dark) {
  plot_geo(c_data[total >= 3]) |>
    add_trace(z = ~pct_pass, color = ~pct_pass, colors = "Greens", locations = ~country, locationmode = 'country names',
              text = ~paste0(country, "<br>Films: ", total, "<br>Réussite: ", round(pct_pass,1), "%")) |>
    style_plotly_map(is_dark)
}

# 5. Graphique : Top 20 des pays (100% Empilé)
build_country_bar_plot <- function(df_filt, is_dark) {
  # Extraire le top 20 des pays les plus productifs
  totals <- df_filt[, .(total = .N), by = country]
  top_countries <- head(totals[order(-total)], 20)
  
  if(nrow(top_countries) == 0) return(NULL)
  
  # Préparation des données complètes pour l'empilement
  c_data <- df_filt[country %in% top_countries$country, .(count = .N), by = .(country, rating_val)]
  c_data[, percentage := (count / sum(count)) * 100, by = country]
  c_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
  
  # Logique de tri : l'axe X est trié selon le % de réussite (Score 3) décroissant (de gauche à droite)
  order_ref <- c_data[rating_val == "3", .(country, percentage)]
  top_countries <- merge(top_countries, order_ref, by = "country", all.x = TRUE)
  top_countries[is.na(percentage), percentage := 0]
  ordered_countries <- top_countries[order(-percentage)]$country
  
  c_data[, country := factor(country, levels = ordered_countries)]
  
  plot_ly(c_data, x = ~country, y = ~percentage, color = ~rating_val, 
          colors = c('3'='#2ecc71', '2'='#f1c40f', '1'='#e67e22', '0'='#e74c3c'), 
          type = "bar", text = ~paste0(round(percentage, 1), "%"), 
          textfont = list(color = get_theme_cols(is_dark)$txt), hoverinfo = "text+name") |>
    style_plotly_axes(is_dark, x_title = "Pays", y_title = "%", barmode = "stack")
}

### 6. NOUVEAU: Camembert classification d'âge
build_cert_plot <- function(df, is_dark) {
  color_map <- c("Enfants" = "#2ecc71", "Adolescents" = "#f1c40f",
                 "Adultes" = "#e74c3c", "Non classé" = "#95a5a6")
  all_groups <- data.table(age_group = names(color_map))
  cert_data <- df[, .(count = .N), by = age_group]
  cert_data <- merge(all_groups, cert_data, by = "age_group", all.x = TRUE)
  cert_data[is.na(count), count := 0]
  cert_data[, color := color_map[age_group]]
  plot_ly(cert_data, labels = ~age_group, values = ~count, type = "pie",
          marker = list(colors = ~color),
          textinfo = "label+percent",
          hoverinfo = "label+value+percent") %>%
    layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
           font = list(color = get_theme_cols(is_dark)$txt),
           showlegend = TRUE) %>%
    plotly::config(displayModeBar = FALSE)
}

### 7. NOUVEAU: Courbe scores moyens IMDb & Metascore
build_scores_year_plot <- function(df, is_dark) {
  df <- df[!is.na(rating) | !is.na(metascore)]
  if(nrow(df) == 0) return(NULL)
  scores_data <- df[, .(
    imdb_moy = mean(rating, na.rm = TRUE),
    meta_moy = mean(metascore / 10, na.rm = TRUE)
  ), by = year][order(year)]
  plot_ly(scores_data, x = ~year) %>%
    add_trace(y = ~imdb_moy, name = "Note IMDb", type = "scatter", mode = "lines+markers",
              line = list(color = "#f1c40f"), marker = list(size = 4, color = "#f1c40f")) %>%
    add_trace(y = ~meta_moy, name = "Metascore (/10)", type = "scatter", mode = "lines+markers",
              line = list(color = "#3498db"), marker = list(size = 4, color = "#3498db")) %>%
    style_plotly_axes(is_dark, x_title = "Année", y_title = "Score moyen") %>%
    layout(legend = list(orientation = "h", y = -0.2))
}

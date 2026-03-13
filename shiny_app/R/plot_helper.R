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
  totals <- df_filt[, .(total = .N), by = genre]
  valid_genres <- totals[total >= 5]
  
  if(nrow(valid_genres) == 0) return(NULL)
  
  g_data <- df_filt[genre %in% valid_genres$genre, .(count = .N), by = .(genre, rating_val)]
  g_data[, percentage := (count / sum(count)) * 100, by = genre]
  g_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
  
  order_ref <- g_data[rating_val == "3", .(genre, percentage)]
  valid_genres <- merge(valid_genres, order_ref, by = "genre", all.x = TRUE)
  valid_genres[is.na(percentage), percentage := 0]
  ordered_genres <- valid_genres[order(percentage)]$genre 
  
  g_data[, genre := factor(genre, levels = ordered_genres)]
  
  plot_ly(g_data, y = ~genre, x = ~percentage, color = ~rating_val, 
          colors = c('3'='#2ecc71', '2'='#f1c40f', '1'='#e67e22', '0'='#e74c3c'), 
          type = "bar", orientation = 'h', text = ~paste0(round(percentage, 1), "%"), 
          textfont = list(color = get_theme_cols(is_dark)$txt), hoverinfo = "text+name") |>
    style_plotly_axes(is_dark, x_title = "% de films", y_title = "", barmode = "stack", margin = list(l = 120))
}

# 3. Graphique : Tendance d'évolution temporelle LISSÉE
build_trend_plot <- function(df, is_dark) {
  trend_data <- df[, .(pct_pass = (sum(rating_val == 3) / .N) * 100), by = year][order(year)]
  
  plot_ly(trend_data, x = ~year, y = ~pct_pass, type = 'scatter', mode = 'lines+markers', 
          line = list(color = '#2ecc71', shape = 'spline', smoothing = 1), # <-- Effet lissé ici
          marker = list(size = 4, color = '#2ecc71')) |>
    style_plotly_axes(is_dark, x_title = "Année", y_title = "% Réussite")
}

# 4. Graphique : Carte géographique mondiale
build_map_plot <- function(c_data, is_dark) {
  plot_geo(c_data[total >= 3]) |>
    add_trace(z = ~pct_pass, color = ~pct_pass, 
              colors = c('#e74c3c', '#f1c40f', '#2ecc71'), # Échelle Rouge -> Jaune -> Vert
              zmin = 0, zmax = 100, # Bloque l'échelle de 0 à 100%
              locations = ~country, locationmode = 'country names',
              text = ~paste0(country, "<br>Films: ", total, "<br>Réussite: ", round(pct_pass,1), "%"),
              colorbar = list(title = "Réussite (%)", ticksuffix = "%")) |> # Légende propre
    style_plotly_map(is_dark)
}

# 5. Graphique : Top 20 des pays (100% Empilé)
build_country_bar_plot <- function(df_filt, is_dark) {
  totals <- df_filt[, .(total = .N), by = country]
  top_countries <- head(totals[order(-total)], 20)
  
  if(nrow(top_countries) == 0) return(NULL)
  
  c_data <- df_filt[country %in% top_countries$country, .(count = .N), by = .(country, rating_val)]
  c_data[, percentage := (count / sum(count)) * 100, by = country]
  c_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
  
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

# 6. Graphique : Certification
build_cert_plot <- function(df, is_dark) {
  cert_data <- df[!is.na(age_group) & age_group != "", .(count = .N), by = .(age_group, rating_val)]
  if(nrow(cert_data) == 0) return(NULL)
  
  cert_data[, percentage := (count / sum(count)) * 100, by = age_group]
  cert_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
  
  order_ref <- cert_data[rating_val == "3", .(age_group, percentage)]
  all_groups <- unique(cert_data[, .(age_group)])
  all_groups <- merge(all_groups, order_ref, by = "age_group", all.x = TRUE)
  all_groups[is.na(percentage), percentage := 0]
  ordered_groups <- all_groups[order(-percentage)]$age_group
  
  cert_data[, age_group := factor(age_group, levels = ordered_groups)]
  
  plot_ly(cert_data, x = ~percentage, y = ~age_group, color = ~rating_val, 
          colors = c('3'='#2ecc71', '2'='#f1c40f', '1'='#e67e22', '0'='#e74c3c'), 
          type = "bar", orientation = 'h', text = ~paste0(round(percentage, 1), "%"), 
          textfont = list(color = get_theme_cols(is_dark)$txt), hoverinfo = "text+name") |>
    style_plotly_axes(is_dark, x_title = "% de films", y_title = "", barmode = "stack", margin = list(l = 100))
}

# 7. Graphique : Corrélation IMDb
build_scores_boxplot <- function(df, is_dark) {
  df <- df[!is.na(rating) & !is.na(rating_val)]
  if(nrow(df) == 0) return(NULL)
  
  df[, rating_val_fac := factor(rating_val, levels = c("0", "1", "2", "3"))]
  
  # Calcul de la moyenne pour tracer la ligne de tendance
  means <- df[, .(mean_rating = mean(rating)), by = rating_val_fac][order(rating_val_fac)]
  
  plot_ly() |>
    # 1. Ajout de la boite à moustache classique
    add_trace(data = df, x = ~rating_val_fac, y = ~rating, type = "box", 
              color = ~rating_val_fac,
              colors = c('0'='#e74c3c', '1'='#e67e22', '2'='#f1c40f', '3'='#2ecc71'),
              boxpoints = "outliers", marker = list(opacity = 0.5), name = "Distribution") |>
    # 2. Ajout de la ligne de moyenne (pour rendre la corrélation évidente)
    add_trace(data = means, x = ~rating_val_fac, y = ~mean_rating, type = 'scatter', mode = 'lines+markers',
              line = list(color = get_theme_cols(is_dark)$txt, width = 2, dash = 'dash'),
              marker = list(color = get_theme_cols(is_dark)$txt, size = 8),
              name = "Moyenne IMDb") |>
    style_plotly_axes(is_dark, x_title = "Score Bechdel", y_title = "Note IMDb") |>
    layout(showlegend = FALSE)
}
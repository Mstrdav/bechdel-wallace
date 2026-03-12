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

# 2. Graphique : Taux de réussite par genre
build_genre_plot <- function(df_filt, is_dark) {
  genre_data <- df_filt[, .(total = .N, pass_count = sum(rating_val == 3, na.rm = TRUE)), by = genre]
  genre_data <- genre_data[, pct_pass := (pass_count / total) * 100][total >= 5][order(-pct_pass)] 
  
  if(nrow(genre_data) == 0) return(NULL)
  
  plot_ly(genre_data, y = ~reorder(genre, pct_pass), x = ~pct_pass, type = 'bar', orientation = 'h',
          marker = list(color = '#2ecc71'), text = ~paste0(round(pct_pass, 1), "% (n=", total, ")"), 
          textfont = list(color = get_theme_cols(is_dark)$txt), hoverinfo = "text") |>
    style_plotly_axes(is_dark, x_title = "% de réussite", margin = list(l = 120))
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

# 5. Graphique : Top 20 des pays (Barplot)
build_country_bar_plot <- function(c_data, is_dark) {
  plot_ly(head(c_data[order(-total)], 20), x = ~reorder(country, -pct_pass), y = ~pct_pass, type = 'bar', marker = list(color = '#2ecc71')) |>
    style_plotly_axes(is_dark, x_title = "Pays", y_title = "% de réussite")
}
# Détermine les couleurs globales selon le mode clair/sombre
get_theme_cols <- function(is_dark) {
  list(
    txt = if (is_dark) "#ffffff" else "#2c3e50",
    grid = if (is_dark) "#444444" else "#e9ecef"
  )
}

# Applique un layout Plotly standardisé pour les graphiques classiques (Bar, Scatter)
style_plotly_axes <- function(p, is_dark, x_title = "", y_title = "", x_range = NULL, y_range = NULL, barmode = NULL, margin = NULL) {
  cols <- get_theme_cols(is_dark)
  
  # Définition des axes
  ax_x <- list(title = x_title, gridcolor = cols$grid, tickfont = list(color = cols$txt), titlefont = list(color = cols$txt))
  ax_y <- list(title = y_title, gridcolor = cols$grid, tickfont = list(color = cols$txt), titlefont = list(color = cols$txt))
  
  if (!is.null(x_range)) ax_x$range <- x_range
  if (!is.null(y_range)) ax_y$range <- y_range
  
  # Arguments de base du layout
  layout_args <- list(
    p = p, xaxis = ax_x, yaxis = ax_y, 
    paper_bgcolor = "transparent", plot_bgcolor = "transparent", 
    font = list(color = cols$txt), hoverlabel = list(font = list(color = cols$txt))
  )
  
  if (!is.null(barmode)) layout_args$barmode <- barmode
  if (!is.null(margin)) layout_args$margin <- margin
  
  # Exécution du layout et masquage de la barre d'outils
  do.call(plotly::layout, layout_args) |> 
    plotly::config(displayModeBar = FALSE)
}

# Applique un layout spécifique pour la carte géographique
style_plotly_map <- function(p, is_dark) {
  cols <- get_theme_cols(is_dark)
  p |> 
    plotly::layout(
      geo = list(showframe = FALSE, projection = list(type = 'equirectangular'), bgcolor = "transparent"),
      paper_bgcolor = "transparent", plot_bgcolor = "transparent",
      font = list(color = cols$txt), hoverlabel = list(font = list(color = cols$txt))
    ) |> 
    plotly::config(displayModeBar = FALSE)
}
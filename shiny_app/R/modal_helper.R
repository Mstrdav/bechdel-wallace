# --- R/modal_helper.R ---

# Modale pour afficher les résultats de la recherche IMDb
show_search_modal <- function(results, query) {
  showModal(modalDialog(
    title = paste("Résultats pour :", query),
    size = "l", easyClose = TRUE,
    lapply(1:nrow(results), function(i) {
      movie <- results[i, ]
      div(class = "card movie-card p-3 mb-2 shadow-sm",
          layout_column_wrap(
            width = 1/2,
            div(h4(movie$title), p(strong("Année :"), movie$year)),
            div(class = "text-end",
                span(class = paste0("badge badge-", movie$rating_val), paste("Score :", movie$rating_val, "/3")),
                br(), br(),
                a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), "Voir sur IMDb", target = "_blank", class = "btn btn-sm btn-outline-primary"))
          )
      )
    }),
    footer = modalButton("Fermer")
  ))
}

# Modale repensée pour le bouton "Film au hasard"
show_random_movie_modal <- function(movie) {
  # Définit la couleur d'accentuation en fonction du score Bechdel
  score_color <- switch(as.character(movie$rating_val), 
                        "3" = "#2ecc71", 
                        "2" = "#f1c40f", 
                        "1" = "#e67e22", 
                        "0" = "#e74c3c")
  
  showModal(modalDialog(
    title = NULL, # On masque le titre par défaut pour un design plus pur
    header = NULL,
    div(style = "text-align: center; padding: 30px 10px;",
        
        # Titre et info de base
        h6("🎬 Film aléatoire", style = "margin-bottom: 20px; font-weight: bold; color: var(--bs-primary); text-transform: uppercase; letter-spacing: 2px;"),
        h2(movie$title, style = "font-weight: 900; letter-spacing: -1px; margin-bottom: 5px;"), 
        p(style = "font-size: 1.1rem; color: var(--bs-secondary);", movie$year, " • ", movie$genre),
        
        hr(style = "width: 40%; margin: 25px auto; opacity: 0.15;"),
        
        # Le Score en forme de pilule
        div(style = sprintf("display: inline-block; padding: 10px 25px; border-radius: 50px; background-color: %s; color: white; font-size: 1.4rem; font-weight: bold; margin-bottom: 20px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);", score_color),
            paste("Score Bechdel :", movie$rating_val, "/ 3")),
        br(),
        
        # Note IMDb
        if ("rating" %in% names(movie)) {
          p(style = "font-size: 1.1rem;", strong("⭐ Note IMDb : "), movie$rating, "/ 10") 
        } else { "" },
        
        br(),
        
        # Bouton d'action call-to-action
        a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), 
          icon("external-link-alt"), " Voir la fiche IMDb", target = "_blank", 
          class = "btn btn-outline-primary rounded-pill mt-3 shadow-sm px-4 py-2", 
          style = "font-weight: bold; transition: all 0.3s;")
    ),
    easyClose = TRUE,
    footer = NULL # On masque le footer pour ne garder que la fiche
  ))
}
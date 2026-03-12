# --- R/helpers_modals.R ---

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

# Modale pour le bouton "Film au hasard"
show_random_movie_modal <- function(movie) {
  showModal(modalDialog(
    title = "🎬 Film tiré au sort",
    div(style = "text-align: center;",
        h2(movie$title), 
        hr(),
        p(strong("Année : "), movie$year),
        p(strong("Note Bechdel : "), movie$rating_val, "/3"),
        br(),
        a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), 
          "Voir sur IMDb", target = "_blank", class = "btn btn-primary shadow-sm")),
    easyClose = TRUE,
    footer = modalButton("Fermer")
  ))
}
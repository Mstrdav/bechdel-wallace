library(shiny)
library(DT)
library(plotly)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinythemes)

ui <- page_navbar(
  title = "Explorateur Test Bechdel",
  theme = bs_theme(bootswatch = "flatly"),
  
  header = tags$style(HTML("
    .shiny-output-error-validation { color: #e74c3c; font-weight: bold; padding: 20px; }
    .bslib-value-box { transition: transform 0.3s ease; }
    .bslib-value-box:hover { transform: translateY(-5px); cursor: pointer; }
  ")),
  
  sidebar = sidebar(
    title = "Contrôles",
    bg = "#f8f9fa",
    selectInput("source", "Dataset", choices = c("Bechdel 2026")),
    
    pickerInput(
      inputId = "selected_decades",
      label = "Sélectionner les décennies :",
      choices = NULL, 
      options = list(`actions-box` = TRUE, `none-selected-text` = "Toutes"),
      multiple = TRUE
    ),
    
    sliderInput("year_range", "Période (Années) :", 
                min = 1880, max = 2023, value = c(1990, 2023), sep = ""),
    
    textInput("title_search", "Rechercher un titre", placeholder = "Ex: Star Wars"),
    
    accordion(
      accordion_panel(
        "Filtres avancés",
        checkboxGroupInput(
          "ratings_filter",
          "Notes à afficher :",
          choices = list("0 : Pas de femmes" = 0, 
                         "1 : Se parlent" = 1, 
                         "2 : Pas d'hommes" = 2, 
                         "3 : Réussite totale" = 3),
          selected = 0:3
        )
      )
    ),
    hr(),
    actionButton("random_btn", "🎲 Film au hasard", class = "btn-info w-100 shadow-sm"),
    actionButton("reset_filters", "🔄 Réinitialiser filtres", class = "btn-outline-secondary btn-sm w-100 mt-2")
  ),
  
  nav_panel(
    title = "Tableau de Bord",
    icon = icon("chart-line"),
    layout_column_wrap(
      width = 1/3,
      value_box(title = "Films filtrés", value = textOutput("box_total_val"), showcase = bs_icon("film"), theme = "primary"),
      value_box(title = "Taux de réussite", value = textOutput("box_percent_val"), showcase = bs_icon("check2-circle"), theme = "success"),
      value_box(title = "Score moyen (/3)", value = textOutput("box_score_val"), showcase = bs_icon("star-half"), theme = "info")
    ),
    
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Répartition des scores par décennie (%)"), 
        plotlyOutput("hist_plot")
      ),
      card(
        card_header("Analyse de la tendance historique (Points & Lissage)"), 
        plotlyOutput("trend_analysis_plot", height = "400px"),
        card_footer("Chaque point est un film. La ligne courbe montre l'évolution de la moyenne.")
      )
    ),
    card(card_header("Évolution temporelle (% de réussite annuelle)"), plotlyOutput("trend_plot"))
  ),
  
  nav_panel(
    title = "Simulateur",
    icon = icon("vials"),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Testez votre film"),
        card_body(
          textInput("sim_title", "Nom du film", "Mon Film Incroyable"),
          switchInput("q1", "Y a-t-il au moins deux femmes ?", value = FALSE, onLabel = "Oui", offLabel = "Non"),
          conditionalPanel(condition = "input.q1 == true", switchInput("q2", "Se parlent-elles entre elles ?", value = FALSE, onLabel = "Oui", offLabel = "Non")),
          conditionalPanel(condition = "input.q1 == true && input.q2 == true", switchInput("q3", "Parlent-elles d'autre chose qu'un homme ?", value = FALSE, onLabel = "Oui", offLabel = "Non"))
        )
      ),
      card(card_header("Résultat du Test"), div(style = "text-align: center; padding: 20px;", uiOutput("sim_gauge"), h3(textOutput("sim_verdict"))))
    )
  ),
  
  nav_panel(title = "Explorateur", icon = icon("table"), card(DTOutput("table")))
)
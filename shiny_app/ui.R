library(shiny)
library(DT)
library(plotly)
library(bslib)
library(bsicons)
library(thematic)
library(shinyWidgets)

# Active le design automatique des graphiques pour correspondre au thème Bootstrap
thematic_shiny(font = "auto")

# Définition du thème Minty (Bootstrap 5)
my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#2ecc71",
  base_font = font_google("Inter")
)

ui <- page_navbar(
  title = "Explorateur Test Bechdel",
  theme = my_theme,
  fillable = FALSE,
  
  # Optimisation CSS pour les performances visuelles
  header = tags$head(
    tags$style(HTML("
      .shiny-output-error-validation { color: #e74c3c; font-weight: bold; padding: 20px; }
      .bslib-value-box { transition: transform 0.2s ease; }
      .bslib-value-box:hover { transform: translateY(-3px); }
      .movie-card { border-left: 5px solid #2ecc71; margin-bottom: 15px; }
      .badge-3 { background-color: #2ecc71; }
      .badge-2 { background-color: #f1c40f; }
      .badge-1 { background-color: #e67e22; }
      .badge-0 { background-color: #e74c3c; }
      /* Loader pour indiquer un calcul en cours */
      #hist_plot.recalculating, #trend_analysis_plot.recalculating { opacity: 0.6; }
    "))
  ),
  
  # --- Onglet principal : Statistiques ---
  nav_panel(
    title = "Tableau de Bord",
    icon = icon("chart-line"),
    
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Films filtrés", 
        value = textOutput("box_total_val"), 
        showcase = bs_icon("film"), 
        theme = "primary"
      ),
      value_box(
        title = "Taux de réussite", 
        value = textOutput("box_percent_val"), 
        showcase = bs_icon("check2-circle"), 
        theme = "success"
      ),
      value_box(
        title = "Score moyen (/3)", 
        value = textOutput("box_score_val"), 
        showcase = bs_icon("star-half"), 
        theme = "info"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = TRUE,
        card_header("Répartition des scores par décennie (%)"), 
        plotlyOutput("hist_plot")
      ),
      card(
        full_screen = TRUE,
        card_header("Analyse de la tendance historique"), 
        plotlyOutput("trend_analysis_plot", height = "400px"),
        card_footer("Chaque point est un film. Rendu optimisé via WebGL.")
      )
    ),
    
    card(
      card_header("Évolution temporelle (% de réussite annuelle)"), 
      plotlyOutput("trend_plot")
    )
  ),
  
  # --- Onglet Explorateur : Table de données ---
  nav_panel(
    title = "Explorateur", 
    icon = icon("table"), 
    card(
      full_screen = TRUE,
      DTOutput("table")
    )
  ),
  
  # --- Barre de recherche (Haut Droite) ---
  nav_spacer(), 
  
  nav_item(
    tags$div(
      style = "width: 280px; padding: 5px 15px 0 0;",
      # Utilisation d'un textInput simple, le délai est géré par debounce() dans server.R
      textInput("search_query", NULL, placeholder = "🔍 Chercher un titre (ex: Matrix)...", width = "100%")
    )
  ),
  
  # --- Barre latérale : Filtres ---
  sidebar = sidebar(
    title = "Filtres globaux",
    bg = "#f8f9fa",
    
    pickerInput(
      inputId = "selected_decades",
      label = "Décennies :",
      choices = NULL, # Rempli dynamiquement par le serveur
      options = list(
        `actions-box` = TRUE, 
        `none-selected-text` = "Toutes",
        `live-search` = TRUE
      ),
      multiple = TRUE
    ),
    
    sliderInput(
      "year_range", 
      "Période :", 
      min = 1880, max = 2024, 
      value = c(1990, 2024), 
      sep = ""
    ),
    
    checkboxGroupInput(
      "ratings_filter",
      "Notes Bechdel :",
      choices = list(
        "3 : Réussite totale" = 3,
        "2 : Pas d'hommes" = 2, 
        "1 : Se parlent" = 1, 
        "0 : Pas de femmes" = 0
      ),
      selected = 0:3
    ),
    
    hr(),
    actionButton(
      "random_btn", 
      "🎲 Film au hasard", 
      class = "btn-success w-100 shadow-sm fw-bold"
    )
  )
)
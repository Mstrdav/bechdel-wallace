library(shiny)
library(DT)
library(plotly)
library(bslib)
library(bsicons)
library(thematic)
library(shinyWidgets)

thematic_shiny(font = "auto")

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
  
  header = tags$head(
    tags$style(HTML("
      /* Adaptations dynamiques pour le thème sombre/clair */
      .shiny-output-error-validation { color: #e74c3c; font-weight: bold; padding: 20px; }
      
      .bslib-sidebar-layout .sidebar { 
        background-color: var(--bs-tertiary-bg) !important; 
        color: var(--bs-body-color) !important;
        border-right: 1px solid var(--bs-border-color);
      }
      
      .sidebar-section-title { 
        font-weight: bold; 
        color: var(--bs-primary); 
        text-transform: uppercase; 
        font-size: 0.75rem; 
        margin-top: 15px; 
        margin-bottom: 10px; 
        display: block; 
        letter-spacing: 0.5px;
        opacity: 0.8;
      }

      .bslib-value-box { transition: transform 0.2s ease; }
      .bslib-value-box:hover { transform: translateY(-3px); }
      .movie-card { border-left: 5px solid #2ecc71; margin-bottom: 15px; background-color: var(--bs-card-bg); }
      .badge-3 { background-color: #2ecc71; }
      .badge-2 { background-color: #f1c40f; }
      .badge-1 { background-color: #e67e22; }
      .badge-0 { background-color: #e74c3c; }
    "))
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
      card(full_screen = TRUE, card_header("Répartition des scores par décennie (%)"), plotlyOutput("hist_plot")),
      card(full_screen = TRUE, card_header("Corrélations par Genre (Taux de réussite)"), plotlyOutput("genre_plot", height = "400px"))
    ),
    card(card_header("Évolution temporelle (% de réussite annuelle)"), plotlyOutput("trend_plot"))
  ),
  
  nav_panel(
    title = "Analyse Géographique",
    icon = icon("globe"),
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE, 
        card_header("Carte mondiale de la parité (Taux de réussite par pays)"), 
        plotlyOutput("map_plot", height = "500px")
      )
    ),
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE, 
        card_header("Top 20 des pays les plus représentés (Performance Bechdel)"), 
        plotlyOutput("country_bar_plot")
      )
    )
  ),
  
  nav_panel(
    title = "Explorateur", 
    icon = icon("table"), 
    card(full_screen = TRUE, DTOutput("table"))
  ),
  
  nav_spacer(), 
  nav_item(tags$div(style = "width: 280px; padding: 5px 15px 0 0;", textInput("search_query", NULL, placeholder = "🔍 Chercher un titre...", width = "100%"))),
  
  # --- Extrait de ui.R (la partie sidebar) ---
  sidebar = sidebar(
    title = "Filtres & Outils",
    
    span(class = "sidebar-section-title", icon("calendar-days"), " Période"),
    # NOUVEAU : On utilise directement all_decades
    pickerInput("selected_decades", "Décennies :", choices = all_decades, selected = all_decades, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
    sliderInput("year_range", "Années :", min = 1880, max = 2026, value = c(1990, 2026), sep = ""),
    
    hr(),
    
    span(class = "sidebar-section-title", icon("masks-theater"), " Genres"),
    # NOUVEAU : On utilise directement all_genres
    pickerInput("selected_genres", "Filtrer par genre :", choices = all_genres, selected = all_genres, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
    
    hr(),
    
    span(class = "sidebar-section-title", icon("filter"), " Score Bechdel"),
    checkboxGroupInput("ratings_filter", label = NULL, choices = list("3 : Réussite totale" = 3, "2 : Pas d'hommes" = 2, "1 : Se parlent" = 1, "0 : Pas de femmes" = 0), selected = 0:3),
    
    hr(),
    
    span(class = "sidebar-section-title", icon("screwdriver-wrench"), " Actions"),
    actionButton("random_btn", "🎲 Film au hasard", class = "btn-success w-100 mb-2 shadow-sm fw-bold"),
    downloadButton("download_data", "📥 Exporter (CSV)", class = "btn-outline-primary w-100 mb-2"),
    
    hr(),
    
    span(class = "sidebar-section-title", icon("palette"), " Apparence"),
    input_dark_mode(id = "dark_mode_toggle")
  )
)
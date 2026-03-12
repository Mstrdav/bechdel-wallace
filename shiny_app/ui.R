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
  title = "Explorateur Bechdel",
  theme = my_theme,
  fillable = FALSE,
  
  header = tags$head(
    tags$style(HTML("
      /* Ajustements pour un rendu compact et élégant */
      .shiny-output-error-validation { color: #e74c3c; font-weight: bold; padding: 20px; }
      .bslib-sidebar-layout .sidebar { background-color: var(--bs-tertiary-bg) !important; color: var(--bs-body-color) !important; border-right: 1px solid var(--bs-border-color); }
      .sidebar-section-title { font-weight: bold; color: var(--bs-primary); text-transform: uppercase; font-size: 0.75rem; margin-top: 10px; margin-bottom: 5px; display: block; letter-spacing: 0.5px; opacity: 0.8; }
      .bslib-value-box { transition: transform 0.2s ease; }
      .bslib-value-box:hover { transform: translateY(-3px); }
      .movie-card { border-left: 5px solid #2ecc71; margin-bottom: 15px; background-color: var(--bs-card-bg); }
      .badge-3 { background-color: #2ecc71; } .badge-2 { background-color: #f1c40f; } .badge-1 { background-color: #e67e22; } .badge-0 { background-color: #e74c3c; }
      /* Corrige les marges du champ de recherche dans la navbar */
      .navbar .shiny-input-container { margin-bottom: 0 !important; }
    "))
  ),
  
  # --- PAGE 1 : ACCUEIL ---
  nav_panel(
    title = "Accueil",
    icon = icon("house"),
    
    # Les Stats Boxes en haut
    layout_column_wrap(
      width = 1/3,
      value_box(title = "Films filtrés", value = textOutput("box_total_val"), showcase = bs_icon("film"), theme = "primary"),
      value_box(title = "Taux de réussite", value = textOutput("box_percent_val"), showcase = bs_icon("check2-circle"), theme = "success"),
      value_box(title = "Score moyen (/3)", value = textOutput("box_score_val"), showcase = bs_icon("star-half"), theme = "info")
    ),
    
    # Le texte explicatif
    card(
      card_header(strong("À propos de ce projet")),
      card_body(
        markdown("
        Bienvenue dans l'**Explorateur du Test de Bechdel** !
        
        Ce tableau de bord interactif analyse la représentation féminine dans l'industrie cinématographique. Au-delà des données, cette application a été **entièrement optimisée techniquement** pour garantir des temps de réponse instantanés, même sur de très gros volumes de données :
        
        * ⚙️ **Architecture MVC** : Code modulaire découpé en fonctions utilitaires (`helpers`) pour une maintenance aisée.
        * 🚀 **Hyper-Vélocité** : Les données ont été nettoyées en amont et sérialisées en format binaire **Parquet** (avec `{arrow}`), divisant le temps de chargement par dix.
        * 🧠 **Moteur `{data.table}`** : Remplacement des opérations classiques par des jointures et indexations en mémoire ultra-performantes.
        * ⚡ **Mise en cache** : Les graphiques interactifs sont mémorisés en RAM (`{cachem}`) pour éviter des calculs redondants au serveur.
        * 🎨 **Design Réactif** : Interface propulsée par `{bslib}` gérant de manière fluide un mode sombre dynamique sur tous les graphiques Plotly.
        ")
      )
    )
  ),
  
  # --- PAGE 2 : ANALYSES ---
  nav_panel(
    title = "Analyses",
    icon = icon("chart-pie"),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Évolution temporelle"),
                          p("Ce graphique illustre la proportion de films réussissant totalement le test de Bechdel (score de 3) année après année. La courbe permet d'identifier la tendance et l'impact de la prise de conscience récente de l'industrie cinématographique.")
        ),
        plotlyOutput("trend_plot")
      )
    ),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Répartition par décennie"),
                          p("Nous regardons ici la proportion de chaque score (de 0 à 3) agrégée par décennie. Cela permet d'analyser à quelle étape précise les films échouent historiquement (ex: le film n'a aucune femme nommée = score 0).")
        ),
        plotlyOutput("hist_plot")
      )
    ),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Performance par Genre"),
                          p("Certains genres cinématographiques sont-ils de meilleurs élèves que d'autres ? Ce classement montre le taux de réussite au test selon le genre du film. Les genres contenant trop peu d'œuvres sont exclus pour préserver la pertinence statistique.")
        ),
        plotlyOutput("genre_plot", height = "400px")
      )
    )
  ),
  
  # --- PAGE 3 : GÉOGRAPHIE ---
  nav_panel(
    title = "Géographie",
    icon = icon("globe"),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Carte mondiale de la parité"),
                          p("La couleur indique le taux de réussite moyen des films produits dans chaque pays. Survolez un pays pour voir son nombre exact de productions dans la base.")
        ),
        plotlyOutput("map_plot", height = "450px")
      )
    ),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Top 20 des producteurs"),
                          p("Un zoom quantitatif sur les 20 pays produisant le plus de films dans notre base de données, classés par leur performance finale au test de Bechdel.")
        ),
        plotlyOutput("country_bar_plot")
      )
    )
  ),
  
  # --- PAGE 4 : EXPLORATEUR ---
  nav_panel(
    title = "Explorateur", 
    icon = icon("table"), 
    card(full_screen = TRUE, DTOutput("table"))
  ),
  
  # --- ÉLÉMENTS DE LA NAVBAR (En haut à droite) ---
  nav_spacer(), 
  nav_item(
    div(class = "d-flex align-items-center gap-2", style = "margin-top: 4px;",
        textInput("search_query", NULL, placeholder = "🔍 Chercher un titre...", width = "220px"),
        actionButton("random_btn", "🎲", class = "btn-success rounded-circle", title = "Tirer un film au hasard"),
        input_dark_mode(id = "dark_mode_toggle")
    )
  ),
  
  # --- SIDEBAR COMPACTE (A gauche) ---
  sidebar = sidebar(
    title = "Filtres globaux",
    width = 300,
    
    span(class = "sidebar-section-title", icon("calendar-days"), " Période"),
    sliderInput("year_range", NULL, min = 1880, max = 2026, value = c(1990, 2026), sep = ""),
    
    span(class = "sidebar-section-title", icon("star"), " Note IMDb"),
    sliderInput("imdb_range", NULL, min = 0, max = 10, value = c(0, 10), step = 0.5),
    
    span(class = "sidebar-section-title", icon("masks-theater"), " Genres"),
    pickerInput("selected_genres", NULL, choices = all_genres, selected = all_genres, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
    
    br(),
    downloadButton("download_data", "📥 Exporter les données", class = "btn-outline-primary w-100")
  )
)
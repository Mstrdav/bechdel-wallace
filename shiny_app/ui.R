library(shiny)
library(DT)
library(plotly)
library(bslib)
library(bsicons)
library(thematic)
library(shinyWidgets)

# thematic shiny est supposé appliquer le theme aux graphiques. Pas convaincu
thematic_shiny(font = "auto")

my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#2ecc71",
  base_font = font_google("Inter")
)

# ui principale
ui <- page_navbar(
  title = "Explorateur Bechdel",
  theme = my_theme,
  fillable = FALSE,
  
  header = tags$head(
    tags$style(HTML("
      /* Ajustements structurels */
      .shiny-output-error-validation { color: #e74c3c; font-weight: bold; padding: 20px; }
      .bslib-sidebar-layout .sidebar { background-color: var(--bs-tertiary-bg) !important; color: var(--bs-body-color) !important; border-right: 1px solid var(--bs-border-color); display: flex; flex-direction: column; }
      .sidebar-section-title { font-weight: bold; color: var(--bs-primary); text-transform: uppercase; font-size: 0.75rem; margin-top: 10px; margin-bottom: 5px; display: block; letter-spacing: 0.5px; opacity: 0.8; }
      .navbar .shiny-input-container { margin-bottom: 0 !important; }
      
      /* Code Couleur du Score Bechdel & Accents */
      .text-accent { color: var(--bs-primary); font-weight: 700; }
      .s3 { color: #2ecc71; font-weight: bold; }
      .s2 { color: #f1c40f; font-weight: bold; }
      .s1 { color: #e67e22; font-weight: bold; }
      .s0 { color: #e74c3c; font-weight: bold; }
      
      /* Animation et design des éléments */
      .bslib-value-box { transition: transform 0.2s ease; }
      .bslib-value-box:hover { transform: translateY(-3px); }
      .movie-card { border-left: 5px solid #2ecc71; margin-bottom: 15px; background-color: var(--bs-card-bg); }
      .badge-3 { background-color: #2ecc71; } .badge-2 { background-color: #f1c40f; } .badge-1 { background-color: #e67e22; } .badge-0 { background-color: #e74c3c; }
    "))
  ),
  
  # --- PAGE 1 : ACCUEIL ---
  nav_panel(
    title = "Accueil",
    icon = icon("house"),
    
    layout_column_wrap(
      width = 1/3,
      value_box(title = "Films filtrés", value = textOutput("box_total_val"), showcase = bs_icon("film"), theme = "primary"),
      value_box(title = "Taux de réussite", value = textOutput("box_percent_val"), showcase = bs_icon("check2-circle"), theme = "success"),
      value_box(title = "Score moyen (/3)", value = textOutput("box_score_val"), showcase = bs_icon("star-half"), theme = "info")
    ),
    
    layout_column_wrap(
      width = 1/2,
      class = "mt-3",
      card(
        card_header(strong(icon("circle-info"), " Qu'est-ce que le test de Bechdel ?")),
        card_body(
          HTML("
            <p>Le <strong>Test de Bechdel-Wallace</strong> est un indicateur simple visant à évaluer la représentation féminine dans les œuvres de fiction.</p>
            <p>Pour qu'un film obtienne la note maximale (<span class='s3'>Score 3</span>), il doit valider trois critères successifs :</p>
            <ul class='list-group list-group-flush mb-3'>
              <li class='list-group-item bg-transparent border-0 py-1'><span class='s1'>Score 1</span> : Le film comporte au moins deux femmes nommées.</li>
              <li class='list-group-item bg-transparent border-0 py-1'><span class='s2'>Score 2</span> : Ces deux femmes parlent ensemble.</li>
              <li class='list-group-item bg-transparent border-0 py-1'><span class='s3'>Score 3</span> : Leur conversation porte sur <span class='text-accent'>un autre sujet qu'un personnage masculin</span>.</li>
            </ul>
            <p>S'il ne valide même pas le premier critère, le film obtient un <span class='s0'>Score 0</span>.</p>
            <p class='text-muted small'><em>Attention : Réussir le test ne signifie pas qu'un film est féministe, ni qu'il est bon. Il s'agit uniquement d'une mesure de la présence active des personnages féminins.</em></p>
          ")
        )
      ),
      card(
        card_header(strong(icon("rocket"), " À propos de cet Explorateur")),
        card_body(
          HTML("
            <p>Ce tableau de bord interactif analyse la représentation féminine dans l'industrie cinématographique à partir de milliers de films.</p>
            <p>Cette application a été <strong>entièrement optimisée techniquement</strong> pour garantir des temps de réponse instantanés :</p>
            <ul>
              <li>⚙️ <strong>Architecture MVC</strong> : Code modulaire découpé en fonctions utilitaires (`helpers`).</li>
              <li>🚀 <strong>Hyper-Vélocité</strong> : Données sérialisées en format binaire <strong>Parquet</strong> (`{arrow}`).</li>
              <li>🧠 <strong>Moteur {data.table}</strong> : Indexations et jointures en mémoire ultra-performantes.</li>
              <li>⚡ <strong>Mise en cache</strong> : Graphiques mémorisés en RAM (`{cachem}`) pour éviter la redondance.</li>
              <li>🎨 <strong>Design Réactif</strong> : Interface `{bslib}` gérant de manière fluide un mode sombre dynamique.</li>
            </ul>
          ")
        )
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
                          HTML("<p>Ce graphique illustre la proportion de films réussissant <span class='text-accent'>totalement le test</span> année après année.</p>
               <p>La courbe permet d'identifier la tendance globale et l'impact de la prise de conscience récente de l'industrie cinématographique sur la parité.</p>")
        ),
        plotlyOutput("trend_plot")
      )
    ),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "right",
                          h5("Répartition par décennie"),
                          HTML("<p>Nous regardons ici la proportion de chaque score agrégée par décennie.</p>
               <p>Cela permet d'analyser à quelle étape précise les films échouent historiquement :</p>
               <ul class='list-unstyled ms-2'>
                <li><span class='s0'>0</span> : Aucune femme nommée</li>
                <li><span class='s1'>1</span> : Elles sont présentes</li>
                <li><span class='s2'>2</span> : Elles se parlent</li>
                <li><span class='s3'>3</span> : Sujet indépendant</li>
               </ul>")
        ),
        plotlyOutput("hist_plot")
      )
    ),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Performance par Genre"),
                          HTML("<p>Décomposition à 100% par genre cinématographique.</p>
               <p>Les barres montrent la répartition complète des scores (du <span class='s0'>0</span> au <span class='s3'>3</span>) pour mesurer de manière granulaire quels genres sont les <span class='text-accent'>meilleurs élèves</span>.</p>
               <p class='small text-muted'>Les genres contenant moins de 5 œuvres sont exclus pour préserver la pertinence statistique.</p>")
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
        sidebar = sidebar(width = 300, bg = "transparent", position = "right",
                          h5("Carte mondiale"),
                          HTML("<p>La couleur de la carte indique le <span class='text-accent'>taux de réussite moyen</span> des films produits dans chaque pays.</p>
               <p>Survolez un pays pour voir son nombre exact de productions enregistrées dans la base.</p>")
        ),
        plotlyOutput("map_plot", height = "450px")
      )
    ),
    
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(width = 300, bg = "transparent", position = "left",
                          h5("Top 20 des producteurs"),
                          HTML("<p>Un zoom quantitatif sur les 20 pays produisant le plus de films dans notre base de données.</p>
               <p>Comme pour les genres, les barres sont <span class='text-accent'>empilées à 100%</span> pour révéler la répartition exacte des scores au sein de chaque pays.</p>")
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
  
  # --- ÉLÉMENTS DE LA NAVBAR ---
  nav_spacer(), 
  nav_item(
    div(class = "d-flex align-items-center gap-1", style = "margin-top: 4px;",
        textInput("search_query", NULL, placeholder = "🔍 Chercher un titre...", width = "220px"),
        # Bouton random avec un design "ghost" (sans fond)
        actionButton("random_btn", "🎲", class = "btn border-0 bg-transparent fs-4", title = "Tirer un film au hasard"),
        input_dark_mode(id = "dark_mode_toggle")
    )
  ),
  
  # --- SIDEBAR COMPACTE ---
  sidebar = sidebar(
    title = "Filtres globaux",
    width = 300,
    
    span(class = "sidebar-section-title", icon("calendar-days"), " Période"),
    sliderInput("year_range", NULL, min = 1880, max = 2026, value = c(1990, 2026), sep = ""),
    
    span(class = "sidebar-section-title", icon("star"), " Note IMDb"),
    sliderInput("imdb_range", NULL, min = 0, max = 10, value = c(0, 10), step = 0.5),
    
    span(class = "sidebar-section-title", icon("masks-theater"), " Genres"),
    pickerInput("selected_genres", NULL, choices = all_genres, selected = all_genres, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
    
    # Mt-auto pousse le bouton tout en bas de la sidebar
    div(class = "mt-auto pt-3",
        downloadButton("download_data", "📥 Exporter (CSV)", class = "btn-outline-primary w-100 fw-bold")
    )
  )
)
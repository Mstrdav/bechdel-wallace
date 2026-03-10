library(shiny)
library(readr)
library(DT)

# Chargement minimal des datasets
movie_paths <- list(
  'FiveThirtyEight' = file.path('data', 'raw', 'movies.csv'),
  'Bechdel 2023' = file.path('data', 'raw', 'bechdel_movies_2023_FEB.csv')
)

load_dataset <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  read_csv(path, show_col_types = FALSE)
}

ui <- fluidPage(
  titlePanel('Bechdel test explorer'),
  sidebarLayout(
    sidebarPanel(
      selectInput('source', 'Dataset', choices = names(movie_paths)),
      numericInput('max_rows', 'Afficher', value = 10, min = 5, max = 100)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Aperçu', DTOutput('table')),
        tabPanel('Statuts', verbatimTextOutput('summary'))
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    load_dataset(movie_paths[[input$source]])
  })

  output$table <- renderDT({
    df <- dataset()
    if (nrow(df) == 0) return(datatable(df))
    datatable(head(df, input$max_rows), options = list(pageLength = input$max_rows))
  })

  output$summary <- renderPrint({
    df <- dataset()
    if (nrow(df) == 0) {
      return('Dataset introuvable (ajoute les CSV dans data/raw/).')
    }
    cat('Nombre total de films :', nrow(df), '\n')
    cat('Colonnes disponibles :', paste(names(df), collapse = ', '), '\n')
  })
}

shinyApp(ui, server)

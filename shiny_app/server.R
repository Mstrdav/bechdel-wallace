library(shiny)
library(readr)
library(DT)
library(plotly)
library(xml2)
library(dplyr)
library(ggplot2)
library(data.table)

decode_html <- function(text_vector) {
  maybe_html <- grepl("&", text_vector)
  text_vector[maybe_html] <- sapply(text_vector[maybe_html], function(txt) {
    if (is.na(txt) || txt == "") return(txt)
    txt <- gsub("&#39;", "'", txt, fixed = TRUE)
    txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
    txt <- gsub("&amp;", "&", txt, fixed = TRUE)
    tryCatch({
      return(xml_text(read_html(paste0("<x>", txt, "</x>"))))
    }, error = function(e) return(txt))
  }, USE.NAMES = FALSE)
  return(text_vector)
}

# Chargement des données
raw_data <- as.data.table(read_csv("../data/raw/all_movies2.csv", show_col_types = FALSE))

# Pré-traitement
if ("title" %in% names(raw_data)) raw_data[, title := decode_html(title)]
if ("rating_bw" %in% names(raw_data)) setnames(raw_data, "rating_bw", "rating_val")
if ("ratings" %in% names(raw_data)) setnames(raw_data, "ratings", "rating_val")
if ("year" %in% names(raw_data)) raw_data[, decade := paste0(floor(year / 10) * 10, "s")]
if ("imdbid" %in% names(raw_data)) {
  raw_data[, imdbid := sprintf("%07d", as.integer(gsub("^tt", "", as.character(imdbid))))]
}

function(input, output, session) {
  
  # Initialisation des filtres
  observe({
    decades <- sort(unique(raw_data$decade))
    updatePickerInput(session, "selected_decades", choices = decades, selected = decades)
  })
  
  # Dataset réactif
  filtered_dataset <- reactive({
    req(input$year_range, input$ratings_filter)
    df_filt <- raw_data[year >= input$year_range[1] & year <= input$year_range[2]]
    df_filt <- df_filt[rating_val %in% as.numeric(input$ratings_filter)]
    if(!is.null(input$selected_decades)) {
      df_filt <- df_filt[decade %in% input$selected_decades]
    }
    return(df_filt)
  })
  
  # Recherche de titre
  observeEvent(input$search_query, {
    query <- input$search_query
    req(nchar(query) >= 2)
    results <- raw_data[grepl(query, title, ignore.case = TRUE)][1:5]
    if(nrow(results) > 0) {
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
                    span(class = paste0("badge badge-", movie$rating_val), 
                         paste("Score :", movie$rating_val, "/3")),
                    br(), br(),
                    a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), 
                      "Voir sur IMDb", target = "_blank", class = "btn btn-sm btn-outline-primary"))
              )
          )
        }),
        footer = modalButton("Fermer")
      ))
    }
  })
  
  # Stats Value Boxes
  output$box_total_val <- renderText({ nrow(filtered_dataset()) })
  output$box_percent_val <- renderText({
    df <- filtered_dataset()
    if(nrow(df) == 0) return("0%")
    paste0(round(nrow(df[rating_val == 3]) / nrow(df) * 100, 1), "%")
  })
  output$box_score_val <- renderText({
    df <- filtered_dataset()
    if(nrow(df) == 0) return("-")
    round(mean(df$rating_val, na.rm = TRUE), 2)
  })
  
  # Histogramme avec VERT EN BAS
  output$hist_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) == 0) return(NULL)
    
    hist_data <- df[, .(count = .N), by = .(decade, rating_val)]
    hist_data[, percentage := (count / sum(count)) * 100, by = decade]
    
    # Inversion de l'ordre pour mettre le score 3 à la base
    hist_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
    
    colors <- c('3' = '#2ecc71', '2' = '#f1c40f', '1' = '#e67e22', '0' = '#e74c3c')
    
    plot_ly(hist_data, x = ~decade, y = ~percentage, color = ~rating_val, colors = colors, type = "bar",
            text = ~paste0(round(percentage, 1), "%"), hoverinfo = "text+name") %>%
      layout(xaxis = list(title = "Décennie"), yaxis = list(title = "%", range = c(0, 100)), barmode = "stack")
  })
  
  # Autres Graphiques
  output$trend_analysis_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 5) return(NULL)
    plot_df <- if(nrow(df) > 2000) df[sample(.N, 2000)] else df
    p <- ggplot(plot_df, aes(x = year, y = rating_val)) +
      geom_jitter(aes(text = paste("Film:", title, "<br>Score:", rating_val)), 
                  alpha = 0.2, color = "#34495e", width = 0.4, height = 0.2) +
      geom_smooth(method = "loess", color = "#2ecc71", fill = "#2ecc71", alpha = 0.2) +
      theme_minimal() + labs(x = "Année", y = "Score")
    ggplotly(p, tooltip = "text") %>% toWebGL()
  })
  
  output$trend_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 2) return(NULL)
    trend_data <- df[, .(pct_pass = (sum(rating_val == 3) / .N) * 100), by = year][order(year)]
    plot_ly(trend_data, x = ~year, y = ~pct_pass, type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#2ecc71')) %>%
      layout(yaxis = list(title = "% Réussite"), xaxis = list(title = "Année"))
  })
  
  # Table de données
  output$table <- renderDT({ 
    datatable(filtered_dataset(), 
              options = list(pageLength = 10, scrollX = TRUE, server = TRUE), 
              rownames = FALSE) 
  })
  
  # Exportation CSV
  output$download_data <- downloadHandler(
    filename = function() { paste("bechdel_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write_csv(filtered_dataset(), file) }
  )
  
  # Film au hasard
  observeEvent(input$random_btn, {
    df <- filtered_dataset()
    if(nrow(df) > 0) {
      movie <- df[sample(.N, 1)]
      showModal(modalDialog(
        title = "🎬 Film tiré au sort",
        div(style = "text-align: center;",
            h2(movie$title), hr(),
            p(strong("Année : "), movie$year),
            p(strong("Note Bechdel : "), movie$rating_val, "/3"),
            a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), "IMDb", target = "_blank", class = "btn btn-primary")),
        easyClose = TRUE
      ))
    }
  })
}
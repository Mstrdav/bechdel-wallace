library(shiny)
library(readr)
library(DT)
library(plotly)
library(xml2)
library(dplyr)
library(ggplot2)

# --- FONCTIONS UTILITAIRES ---
movie_paths <- list("Bechdel 2026" = "/data/raw/all_movies.csv")

decode_html <- function(text_vector) {
  sapply(text_vector, function(txt) {
    if (is.na(txt) || txt == "") return(txt)
    txt <- gsub("&#39;", "'", txt, fixed = TRUE)
    txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
    txt <- gsub("&amp;", "&", txt, fixed = TRUE)
    if (grepl("&", txt)) {
      tryCatch({
        return(xml_text(read_html(paste0("<x>", txt, "</x>"))))
      }, error = function(e) return(txt))
    }
    return(txt)
  }, USE.NAMES = FALSE)
}

load_dataset <- function(path) {
  if (!file.exists(path)) return(data.frame())
  df <- read_csv(path, show_col_types = FALSE)
  if ("title" %in% names(df)) df$title <- decode_html(df$title)
  if ("rating" %in% names(df)) df <- df %>% rename(rating_val = rating)
  else if ("ratings" %in% names(df)) df <- df %>% rename(rating_val = ratings)
  
  if ("year" %in% names(df)) df <- df %>% mutate(decade = paste0(floor(year / 10) * 10, "s"))
  
  if ("imdbid" %in% names(df)) {
    df$imdbid <- sapply(df$imdbid, function(x) {
      num_id <- as.integer(gsub("^tt", "", as.character(x)))
      if (is.na(num_id)) return(as.character(x))
      sprintf("%07d", num_id)
    })
  }
  return(df)
}

function(input, output, session) {
  
  dataset <- reactive({ load_dataset(movie_paths[[input$source]]) })
  
  observe({
    df <- dataset()
    if(nrow(df) > 0) {
      decades <- sort(unique(df$decade))
      updatePickerInput(session, "selected_decades", choices = decades, selected = decades)
    }
  })
  
  filtered_dataset <- reactive({
    df <- dataset()
    if (nrow(df) == 0) return(df)
    df_filt <- df %>% filter(year >= input$year_range[1], year <= input$year_range[2], rating_val %in% as.numeric(input$ratings_filter))
    if(!is.null(input$selected_decades)) df_filt <- df_filt %>% filter(decade %in% input$selected_decades)
    if(input$title_search != "") df_filt <- df_filt %>% filter(grepl(input$title_search, title, ignore.case = TRUE))
    return(df_filt)
  })
  
  observeEvent(input$random_btn, {
    df <- filtered_dataset()
    if(nrow(df) > 0) {
      movie <- df[sample(1:nrow(df), 1), ]
      showModal(modalDialog(
        title = "🎬 Film tiré au sort", size = "m", easyClose = TRUE,
        div(style = "text-align: center;",
            h2(movie$title, style = "color: #2c3e50;"), hr(),
            p(strong("Année : "), movie$year),
            p(strong("Note Bechdel : "), movie$rating_val, "/3"), br(),
            a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), "Voir sur IMDb", target = "_blank", class = "btn btn-outline-primary")),
        footer = modalButton("Fermer")
      ))
    }
  })
  
  output$hist_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) == 0) return(NULL)
    hist_data <- df %>% group_by(decade, rating_val) %>% summarise(count = n(), .groups = 'drop') %>%
      group_by(decade) %>% mutate(percentage = (count / sum(count)) * 100)
    
    colors <- c('0' = '#e74c3c', '1' = '#e67e22', '2' = '#f1c40f', '3' = '#2ecc71')
    plot_ly(hist_data, x = ~decade, y = ~percentage, color = ~factor(rating_val), colors = colors, type = "bar",
            text = ~paste0(round(percentage, 1), "%"), hoverinfo = "text+name") %>%
      layout(xaxis = list(title = "Décennie"), yaxis = list(title = "%", range = c(0, 100)), barmode = "stack")
  })
  
  # NOUVEAU GRAPHIQUE DE TENDANCE (Remplace le Joy Plot)
  output$trend_analysis_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 5) return(NULL)
    
    p <- ggplot(df, aes(x = year, y = rating_val)) +
      geom_jitter(aes(text = paste("Film:", title, "<br>Année:", year, "<br>Score:", rating_val)), 
                  alpha = 0.25, color = "#34495e", width = 0.4, height = 0.2) +
      geom_smooth(method = "loess", color = "#2ecc71", fill = "#2ecc71", alpha = 0.2, size = 1.2) +
      theme_minimal() +
      labs(x = "Année de sortie", y = "Score Bechdel (0 à 3)") +
      scale_y_continuous(breaks = 0:3)
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$box_total_val <- renderText({ nrow(filtered_dataset()) })
  output$box_percent_val <- renderText({
    df <- filtered_dataset()
    if(nrow(df) == 0) return("0%")
    paste0(round(sum(df$rating_val == 3) / nrow(df) * 100, 1), "%")
  })
  output$box_score_val <- renderText({
    df <- filtered_dataset()
    if(nrow(df) == 0) return("-")
    round(mean(df$rating_val, na.rm = TRUE), 2)
  })
  
  output$trend_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 2) return(NULL)
    trend_data <- df %>% group_by(year) %>% summarise(pct_pass = (sum(rating_val == 3) / n()) * 100)
    plot_ly(trend_data, x = ~year, y = ~pct_pass, type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#2ecc71'), marker = list(size = 4)) %>%
      layout(yaxis = list(title = "% Réussite", range = c(0, 105)), xaxis = list(title = "Année"))
  })
  
  output$sim_gauge <- renderUI({
    score <- if(!input$q1) 0 else if(!input$q2) 1 else if(!input$q3) 2 else 3
    colors <- c("#e74c3c", "#e67e22", "#f1c40f", "#2ecc71")
    div(bs_icon(if(score == 3) "check-circle-fill" else "exclamation-triangle-fill", size = "5rem", class = if(score==3) "text-success" else "text-danger"),
        h1(paste0(score, "/3"), style = paste0("color:", colors[score+1], "; font-size: 4rem; font-weight: bold;")))
  })
  
  output$sim_verdict <- renderText({
    score <- if(!input$q1) 0 else if(!input$q2) 1 else if(!input$q3) 2 else 3
    if(score == 3) paste("Félicitations, '", input$sim_title, "' réussit !") else paste("'", input$sim_title, "' échoue.")
  })
  
  output$table <- renderDT({ datatable(filtered_dataset(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
}
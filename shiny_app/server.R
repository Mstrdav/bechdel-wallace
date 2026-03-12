library(shiny)
library(readr)
library(DT)
library(plotly)
library(xml2)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)

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

# Standardisation colonnes
if ("genres" %in% names(raw_data)) setnames(raw_data, "genres", "genre")
if ("Genre" %in% names(raw_data)) setnames(raw_data, "Genre", "genre")
if ("countries" %in% names(raw_data)) setnames(raw_data, "countries", "country")

# Extraction des genres uniques
all_genres <- if("genre" %in% names(raw_data)) {
  genres_clean <- as.character(raw_data$genre[!is.na(raw_data$genre)])
  sort(unique(trimws(unlist(strsplit(genres_clean, ",\\s*|\\|")))))
} else {
  NULL
}

function(input, output, session) {
  
  # Initialisation des filtres sidebar
  observe({
    decades <- sort(unique(raw_data$decade))
    updatePickerInput(session, "selected_decades", choices = decades, selected = decades)
    if(!is.null(all_genres)) {
      updatePickerInput(session, "selected_genres", choices = all_genres, selected = all_genres)
    }
  })
  
  # Dataset réactif
  filtered_dataset <- reactive({
    req(input$year_range, input$ratings_filter)
    df_filt <- raw_data[year >= input$year_range[1] & year <= input$year_range[2]]
    df_filt <- df_filt[rating_val %in% as.numeric(input$ratings_filter)]
    
    if(!is.null(input$selected_decades)) {
      df_filt <- df_filt[decade %in% input$selected_decades]
    }
    
    if(!is.null(input$selected_genres) && "genre" %in% names(df_filt)) {
      regex_genres <- paste(input$selected_genres, collapse = "|")
      df_filt <- df_filt[grepl(regex_genres, genre, ignore.case = TRUE)]
    }
    
    return(df_filt)
  })
  
  # Stats Boxes
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
  
  # Plots existants
  output$hist_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) == 0) return(NULL)
    hist_data <- df[, .(count = .N), by = .(decade, rating_val)]
    hist_data[, percentage := (count / sum(count)) * 100, by = decade]
    hist_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
    colors <- c('3' = '#2ecc71', '2' = '#f1c40f', '1' = '#e67e22', '0' = '#e74c3c')
    plot_ly(hist_data, x = ~decade, y = ~percentage, color = ~rating_val, colors = colors, type = "bar",
            text = ~paste0(round(percentage, 1), "%"), hoverinfo = "text+name") %>%
      layout(xaxis = list(title = "Décennie"), yaxis = list(title = "%", range = c(0, 100)), barmode = "stack")
  })
  
  output$genre_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) == 0 || !"genre" %in% names(df)) return(NULL)
    genre_data <- as.data.frame(df) %>%
      select(genre, rating_val) %>%
      filter(!is.na(genre) & genre != "") %>%
      separate_rows(genre, sep = ",\\s*|\\|") %>% 
      mutate(genre = trimws(genre)) %>%
      group_by(genre) %>%
      summarise(total = n(), pass_count = sum(rating_val == 3, na.rm = TRUE), pct_pass = (pass_count / total) * 100, .groups = 'drop') %>%
      filter(total >= 5) %>% arrange(desc(pct_pass))
    
    plot_ly(genre_data, y = ~reorder(genre, pct_pass), x = ~pct_pass, type = 'bar', orientation = 'h',
            marker = list(color = '#2ecc71'), text = ~paste0(round(pct_pass, 1), "% (n=", total, ")"), hoverinfo = "text") %>%
      layout(xaxis = list(title = "% de réussite"), yaxis = list(title = ""), margin = list(l = 120))
  })
  
  output$trend_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 2) return(NULL)
    trend_data <- df[, .(pct_pass = (sum(rating_val == 3) / .N) * 100), by = year][order(year)]
    plot_ly(trend_data, x = ~year, y = ~pct_pass, type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#2ecc71'), marker = list(size = 4)) %>%
      layout(yaxis = list(title = "% Réussite"), xaxis = list(title = "Année"))
  })
  
  # Logique Pays
  reactive_country_data <- reactive({
    df <- filtered_dataset()
    if(nrow(df) == 0 || !"country" %in% names(df)) return(NULL)
    as.data.frame(df) %>%
      select(country, rating_val) %>%
      filter(!is.na(country)) %>%
      separate_rows(country, sep = ",\\s*|\\|") %>%
      mutate(country = trimws(country)) %>%
      group_by(country) %>%
      summarise(total = n(), pass_count = sum(rating_val == 3, na.rm = TRUE), pct_pass = (pass_count / total) * 100, avg_score = mean(rating_val, na.rm = TRUE), .groups = 'drop')
  })
  
  output$map_plot <- renderPlotly({
    c_data <- reactive_country_data()
    if(is.null(c_data)) return(NULL)
    plot_geo(c_data %>% filter(total >= 3)) %>%
      add_trace(z = ~pct_pass, color = ~pct_pass, colors = "Greens", locations = ~country, locationmode = 'country names',
                text = ~paste0(country, "<br>Films: ", total, "<br>Réussite: ", round(pct_pass,1), "%")) %>%
      layout(geo = list(showframe = FALSE, projection = list(type = 'equirectangular')))
  })
  
  output$country_bar_plot <- renderPlotly({
    c_data <- reactive_country_data()
    if(is.null(c_data)) return(NULL)
    top_countries <- c_data %>% arrange(desc(total)) %>% head(20)
    plot_ly(top_countries, x = ~reorder(country, -pct_pass), y = ~pct_pass, type = 'bar', marker = list(color = '#2ecc71')) %>%
      layout(xaxis = list(title = "Pays"), yaxis = list(title = "% de réussite"))
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
                    span(class = paste0("badge badge-", movie$rating_val), paste("Score :", movie$rating_val, "/3")),
                    br(), br(),
                    a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), "Voir sur IMDb", target = "_blank", class = "btn btn-sm btn-outline-primary"))
              )
          )
        }),
        footer = modalButton("Fermer")
      ))
    }
  })
  
  # Table & Export
  output$table <- renderDT({ 
    datatable(filtered_dataset(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) 
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("bechdel_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write_csv(filtered_dataset(), file) }
  )
  
  # --- FIX : Bouton hasard avec bouton IMDb ---
  observeEvent(input$random_btn, {
    df <- filtered_dataset()
    if(nrow(df) > 0) {
      movie <- df[sample(.N, 1)]
      showModal(modalDialog(
        title = "🎬 Film tiré au sort",
        div(style = "text-align: center;",
            h2(movie$title), 
            hr(),
            p(strong("Année : "), movie$year),
            p(strong("Note Bechdel : "), movie$rating_val, "/3"),
            br(),
            # Retour du lien IMDb ici :
            a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), 
              "Voir sur IMDb", target = "_blank", class = "btn btn-primary shadow-sm")),
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
    }
  })
}
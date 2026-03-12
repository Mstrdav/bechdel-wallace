function(input, output, session) {
  # Dataset réactif principal
  filtered_dataset <- reactive({
    req(input$year_range, input$ratings_filter)
    
    # filtrage rapide grâce à data.table et indexation préalable
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
  
  # Info box
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
  
  # Graphiques
  output$hist_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) == 0) return(NULL)
    
    # 1. Détection du mode sombre directement via ton bouton UI
    is_dark <- identical(input$dark_mode_toggle, "dark")
    
    # 2. Définition des couleurs en "dur" pour que Plotly les comprenne parfaitement
    txt_col <- if(is_dark) "#ffffff" else "#2c3e50"
    grid_col <- if(is_dark) "#444444" else "#e9ecef"
    
    hist_data <- df[, .(count = .N), by = .(decade, rating_val)]
    hist_data[, percentage := (count / sum(count)) * 100, by = decade]
    hist_data[, rating_val := factor(rating_val, levels = c("3", "2", "1", "0"))]
    colors <- c('3' = '#2ecc71', '2' = '#f1c40f', '1' = '#e67e22', '0' = '#e74c3c')
    
    plot_ly(hist_data, x = ~decade, y = ~percentage, color = ~rating_val, colors = colors, type = "bar",
            text = ~paste0(round(percentage, 1), "%"), 
            textfont = list(color = txt_col), # Texte des barres
            hoverinfo = "text+name") |>
      layout(
        xaxis = list(
          title = "Décennie", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),   # Années X
          titlefont = list(color = txt_col)   # Titre X
        ), 
        yaxis = list(
          title = "%", 
          range = c(0, 100), 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),   # Valeurs Y
          titlefont = list(color = txt_col)   # Titre Y
        ), 
        barmode = "stack",
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = txt_col),         # Police globale
        legend = list(font = list(color = txt_col)), # Légende
        hoverlabel = list(font = list(color = txt_col)) # Infobulle
      ) |> 
      config(displayModeBar = FALSE) 
    
  }) |> bindCache(input$year_range, input$ratings_filter, input$selected_decades, input$selected_genres, input$dark_mode_toggle)
  
  output$genre_plot <- renderPlotly({
    if(nrow(genres_dt) == 0) return(NULL)
    
    # 1. Détection du mode sombre
    is_dark <- identical(input$dark_mode_toggle, "dark")
    txt_col <- if(is_dark) "#ffffff" else "#2c3e50"
    grid_col <- if(is_dark) "#444444" else "#e9ecef"
    
    df_filt <- genres_dt[year >= input$year_range[1] & year <= input$year_range[2]]
    df_filt <- df_filt[rating_val %in% as.numeric(input$ratings_filter)]
    if(!is.null(input$selected_decades)) df_filt <- df_filt[decade %in% input$selected_decades]
    if(nrow(df_filt) == 0) return(NULL)
    
    genre_data <- df_filt[, .(
      total = .N, 
      pass_count = sum(rating_val == 3, na.rm = TRUE)
    ), by = genre]
    
    genre_data[, pct_pass := (pass_count / total) * 100]
    genre_data <- genre_data[total >= 5][order(-pct_pass)] 
    
    if(nrow(genre_data) == 0) return(NULL)
    
    plot_ly(genre_data, y = ~reorder(genre, pct_pass), x = ~pct_pass, type = 'bar', orientation = 'h',
            marker = list(color = '#2ecc71'), text = ~paste0(round(pct_pass, 1), "% (n=", total, ")"), 
            textfont = list(color = txt_col), hoverinfo = "text") |>
      layout(
        xaxis = list(
          title = "% de réussite", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),
          titlefont = list(color = txt_col)
        ), 
        yaxis = list(
          title = "", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col)
        ), 
        margin = list(l = 120),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = txt_col),
        hoverlabel = list(font = list(color = txt_col))
      ) |> 
      config(displayModeBar = FALSE)
    
  }) |> bindCache(input$year_range, input$ratings_filter, input$selected_decades, input$dark_mode_toggle)
  
  output$trend_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 2) return(NULL)
    
    is_dark <- identical(input$dark_mode_toggle, "dark")
    txt_col <- if(is_dark) "#ffffff" else "#2c3e50"
    grid_col <- if(is_dark) "#444444" else "#e9ecef"
    
    trend_data <- df[, .(pct_pass = (sum(rating_val == 3) / .N) * 100), by = year][order(year)]
    plot_ly(trend_data, x = ~year, y = ~pct_pass, type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#2ecc71'), marker = list(size = 4)) |>
      layout(
        xaxis = list(
          title = "Année", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),
          titlefont = list(color = txt_col)
        ),
        yaxis = list(
          title = "% Réussite", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),
          titlefont = list(color = txt_col)
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = txt_col),
        hoverlabel = list(font = list(color = txt_col))
      ) |> 
      config(displayModeBar = FALSE)
    
  }) |> bindCache(input$year_range, input$ratings_filter, input$selected_decades, input$selected_genres, input$dark_mode_toggle)
  
  # --- LOGIQUE PAYS ---
  reactive_country_data <- reactive({
    if(nrow(countries_dt) == 0) return(NULL)
    
    df_filt <- countries_dt[year >= input$year_range[1] & year <= input$year_range[2]]
    df_filt <- df_filt[rating_val %in% as.numeric(input$ratings_filter)]
    if(!is.null(input$selected_decades)) df_filt <- df_filt[decade %in% input$selected_decades]
    if(nrow(df_filt) == 0) return(NULL)
    
    res <- df_filt[, .(
      total = .N, 
      pass_count = sum(rating_val == 3, na.rm = TRUE),
      avg_score = mean(rating_val, na.rm = TRUE)
    ), by = country]
    
    res[, pct_pass := (pass_count / total) * 100]
    return(res)
  })
  
  output$map_plot <- renderPlotly({
    c_data <- reactive_country_data()
    if(is.null(c_data)) return(NULL)
    
    is_dark <- identical(input$dark_mode_toggle, "dark")
    txt_col <- if(is_dark) "#ffffff" else "#2c3e50"
    
    plot_geo(c_data[total >= 3]) |>
      add_trace(z = ~pct_pass, color = ~pct_pass, colors = "Greens", locations = ~country, locationmode = 'country names',
                text = ~paste0(country, "<br>Films: ", total, "<br>Réussite: ", round(pct_pass,1), "%")) |>
      layout(
        geo = list(showframe = FALSE, projection = list(type = 'equirectangular'), bgcolor = "transparent"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = txt_col),
        hoverlabel = list(font = list(color = txt_col))
      ) |> 
      config(displayModeBar = FALSE)
    
  }) |> bindCache(input$year_range, input$ratings_filter, input$selected_decades, input$dark_mode_toggle)
  
  output$country_bar_plot <- renderPlotly({
    c_data <- reactive_country_data()
    if(is.null(c_data)) return(NULL)
    
    is_dark <- identical(input$dark_mode_toggle, "dark")
    txt_col <- if(is_dark) "#ffffff" else "#2c3e50"
    grid_col <- if(is_dark) "#444444" else "#e9ecef"
    
    top_countries <- head(c_data[order(-total)], 20)
    plot_ly(top_countries, x = ~reorder(country, -pct_pass), y = ~pct_pass, type = 'bar', marker = list(color = '#2ecc71')) |>
      layout(
        xaxis = list(
          title = "Pays", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),
          titlefont = list(color = txt_col)
        ), 
        yaxis = list(
          title = "% de réussite", 
          gridcolor = grid_col,
          tickfont = list(color = txt_col),
          titlefont = list(color = txt_col)
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        font = list(color = txt_col),
        hoverlabel = list(font = list(color = txt_col))
      ) |> 
      config(displayModeBar = FALSE)
    
  }) |> bindCache(input$year_range, input$ratings_filter, input$selected_decades, input$dark_mode_toggle)
  
  # Recherche de titre optimisée
  search_debounced <- reactive({ input$search_query }) |> debounce(500)
  
  observeEvent(search_debounced(), {
    query <- search_debounced()
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
    content = function(file) { fwrite(filtered_dataset(), file) } 
  )
  
  # Bouton hasard
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
            a(href = paste0("https://www.imdb.com/title/tt", movie$imdbid), 
              "Voir sur IMDb", target = "_blank", class = "btn btn-primary shadow-sm")),
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
    }
  })
}
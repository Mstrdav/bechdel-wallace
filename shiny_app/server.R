function(input, output, session) {
  # Datasets
  filtered_dataset <- reactive({
    req(input$year_range, input$imdb_range)
    df_filt <- raw_data[year >= input$year_range[1] & year <= input$year_range[2]]
    
    if ("rating" %in% names(df_filt)) {
      df_filt <- df_filt[rating >= input$imdb_range[1] & rating <= input$imdb_range[2]]
    }
    
    if(!is.null(input$selected_genres) && "genre" %in% names(df_filt)) {
      df_filt <- df_filt[grepl(paste(input$selected_genres, collapse = "|"), genre, ignore.case = TRUE)]
    }
    return(df_filt)
  })
  
  filtered_genres_dataset <- reactive({
    if(nrow(genres_dt) == 0) return(NULL)
    df_filt <- genres_dt[year >= input$year_range[1] & year <= input$year_range[2]]
    if ("rating" %in% names(df_filt)) {
      df_filt <- df_filt[rating >= input$imdb_range[1] & rating <= input$imdb_range[2]]
    }
    return(df_filt)
  })
  
  filtered_countries_dataset <- reactive({
    if(nrow(countries_dt) == 0) return(NULL)
    df_filt <- countries_dt[year >= input$year_range[1] & year <= input$year_range[2]]
    if ("rating" %in% names(df_filt)) {
      df_filt <- df_filt[rating >= input$imdb_range[1] & rating <= input$imdb_range[2]]
    }
    return(df_filt)
  })
  
  # Boites à infos
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
    build_hist_plot(df, identical(input$dark_mode_toggle, "dark"))
  }) |> bindCache(input$year_range, input$imdb_range, input$selected_genres, input$dark_mode_toggle)
  
  output$genre_plot <- renderPlotly({
    df_filt <- filtered_genres_dataset()
    if(is.null(df_filt) || nrow(df_filt) == 0) return(NULL)
    build_genre_plot(df_filt, identical(input$dark_mode_toggle, "dark"))
  }) |> bindCache(input$year_range, input$imdb_range, input$dark_mode_toggle)
  
  output$trend_plot <- renderPlotly({
    df <- filtered_dataset()
    if(nrow(df) < 2) return(NULL)
    build_trend_plot(df, identical(input$dark_mode_toggle, "dark"))
  }) |> bindCache(input$year_range, input$imdb_range, input$selected_genres, input$dark_mode_toggle)
  
  output$map_plot <- renderPlotly({
    df_filt <- filtered_countries_dataset()
    if(is.null(df_filt) || nrow(df_filt) == 0) return(NULL)
    # Agrégation spécifique pour la carte géographique
    c_data <- df_filt[, .(total = .N, pass_count = sum(rating_val == 3, na.rm = TRUE)), by = country][, pct_pass := (pass_count / total) * 100]
    build_map_plot(c_data, identical(input$dark_mode_toggle, "dark"))
  }) |> bindCache(input$year_range, input$imdb_range, input$dark_mode_toggle)
  
  output$country_bar_plot <- renderPlotly({
    df_filt <- filtered_countries_dataset()
    if(is.null(df_filt) || nrow(df_filt) == 0) return(NULL)
    # On passe les données BRUTES au nouveau plot helper
    build_country_bar_plot(df_filt, identical(input$dark_mode_toggle, "dark"))
  }) |> bindCache(input$year_range, input$imdb_range, input$dark_mode_toggle)
  
  # Outils
  search_debounced <- reactive({ input$search_query }) |> debounce(500)
  
  observeEvent(search_debounced(), {
    query <- search_debounced()
    req(nchar(query) >= 2)
    results <- raw_data[grepl(query, title, ignore.case = TRUE)][1:5]
    if(nrow(results) > 0) show_search_modal(results, query)
  })
  
  observeEvent(input$random_btn, {
    df <- filtered_dataset()
    if(nrow(df) > 0) show_random_movie_modal(df[sample(.N, 1)])
  })
  
  output$table <- renderDT({ datatable(filtered_dataset(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$download_data <- downloadHandler(
    filename = function() { paste("bechdel_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { fwrite(filtered_dataset(), file) } 
  )
}
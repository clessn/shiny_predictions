# server.R - Modified for new simplified dataset
library(shiny)
library(DT)
library(ggplot2)
library(sf)
library(waiter)
library(tidyr)
library(dplyr)
library(patchwork)
library(ggiraph)
library(scales)

server <- function(input, output, session) {
  # Reduce verbosity
  options(shiny.trace = FALSE)
  
  # Initialize reactive values for data storage
  rv <- reactiveValues()
  
  # Initialize language (default to French)
  rv$lang <- reactiveVal("fr")
  
  # Toggle language when the language button is clicked
  observeEvent(input$toggleLang, {
    rv$lang(ifelse(rv$lang() == "fr", "en", "fr"))
  })
  
  # Translation helper function
  t <- function(key) {
    translations[[rv$lang()]][[key]]
  }
  
  # Load the new dataset - MODIFIED
  rv$new_data <- reactive({
    read.csv("https://raw.githubusercontent.com/clessn/agregateur_data/main/data/df.csv")
  })
  
  # Initialize an empty cache using reactiveValues
  city_data_cache <- reactiveValues()
  
  # Store current city code in a reactive value for other functions to use
  current_city_code <- reactiveVal()
  
  # Update UI choices based on language
  observe({
    updateSelectInput(session, "partyPrediction", 
                      label = t("party_prediction"),
                      choices = c(t("all_parties"), "LPC", "CPC", "BQ", "NDP", "GP", "Battlefields"),
                      selected = input$partyPrediction)
  })
  
  # New data processing function to work with the simplified dataset
  rv$processed_data <- reactive({
    # Get the data
    df <- rv$new_data()
    
    # Check column names and ensure they match our expectations
    if(!all(c("riding_id", "prediction", "probability") %in% colnames(df))) {
      stop("Dataset must contain columns: riding_id, prediction, probability")
    }
    
    # Ensure riding_id is character type to match the map_data id_riding
    df$riding_id <- as.character(df$riding_id)
    
    # Rename columns to match our expected format and add placeholder values
    # where needed for compatibility with the existing UI
    df_processed <- df %>%
      rename(
        riding = riding_id,
        party = prediction,
        first_party_percentage = probability
      ) %>%
      mutate(
        # Convert probability from 0-1 scale to 0-100 scale if needed
        first_party_percentage = if(max(first_party_percentage, na.rm = TRUE) <= 1) 
                                   first_party_percentage * 100 
                                 else 
                                   first_party_percentage,
        # Add placeholder values for second party to maintain UI compatibility
        second_party = "N/A",
        second_party_percentage = 0,
        # Use probability directly as the "closeness" metric for sorting/filtering
        closeness = first_party_percentage
      )
    
    return(df_processed)
  })
  
  # Create a more efficient reactive for filtered map data
  filtered_map_data <- reactive({
    # Get processed riding data
    df_ridings <- rv$processed_data()
    
    # Ensure id_riding is a character string in both datasets
    map_data <- map_data # access from global environment
    map_data$id_riding <- as.character(map_data$id_riding)
    df_ridings$riding <- as.character(df_ridings$riding)
    
    # Join with riding data to add party information
    map_filtered <- map_data %>%
      left_join(df_ridings, by = c("id_riding" = "riding"))
    
    # Add alpha categories based on probability (higher probability = more solid lead)
    map_filtered <- map_filtered %>%
      mutate(
        # Use probability for alpha categories
        # Higher probability = darker (more solid lead)
        # Lower probability = lighter (less certain)
        alpha_category = case_when(
          first_party_percentage <= 55 ~ 0.4,                     # Very uncertain - PALE
          first_party_percentage > 55 & first_party_percentage <= 65 ~ 0.55, # Somewhat uncertain - ASSEZ PALE
          first_party_percentage > 65 & first_party_percentage <= 75 ~ 0.7,  # Medium certainty - MOYEN
          first_party_percentage > 75 & first_party_percentage <= 85 ~ 0.85, # High certainty - FONCÉ
          first_party_percentage > 85 ~ 1.0,                      # Very high certainty - TRÈS FONCÉ
          TRUE ~ 0.6                                              # Default
        ),
        # Calculate battlefield intensity as the inverse of probability
        # Lower probability = higher battlefield intensity (more competitive)
        battlefield_intensity = pmin(100, pmax(0, 100 - first_party_percentage)),
        # Create tooltip text based on current language
        tooltip_text = paste0(
          "<strong>", name_riding_fr, "</strong><br>",
          ifelse(rv$lang() == "fr", "Parti en tête: ", "Leading party: "),
          "<span style='color:", party_colors[party], ";'>", 
          party, " (", round(first_party_percentage, 1), "%)</span><br>",
          ifelse(rv$lang() == "fr", "Probabilité de victoire: ", "Win probability: "), 
          round(first_party_percentage, 1), "%"
        )
      )
    
    # Filter by party if selected
    if (input$partyPrediction == "All parties") {
      return(map_filtered)
    } else if (input$partyPrediction == "Battlefields") {
      # Now we return ALL ridings but with the battlefield intensity calculated
      return(map_filtered)
    } else if (input$partyPrediction %in% partis_politiques) {
      return(map_filtered %>% filter(party == input$partyPrediction))
    } else {
      return(map_filtered)
    }
  })
  
  # Improved city data function with caching
  get_city_data <- function(city_code) {
    # Make sure to properly isolate and capture dependencies
    filtered_data <- isolate(filtered_map_data())
    
    # Build cache key using input values
    cache_key <- paste0(city_code, "_", input$partyPrediction)
    
    # Check if we have cached data for this combination
    if (!is.null(city_data_cache[[cache_key]])) {
      return(city_data_cache[[cache_key]])
    }
    
    # Process the data for this city
    city_data <- crop_map_for_app(filtered_data, city_code, city_mapping)
    
    # Store in cache
    city_data_cache[[cache_key]] <- city_data
    
    return(city_data)
  }
  
  # Function to generate a city map with consistent styling and hover
  generate_city_map <- function(city_data, plot_title, city_code, city_mapping) {
    # Check if we have valid data for this city
    if (!inherits(city_data, "sf") || nrow(city_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for this city", size = 6, color = "white") +
               theme_void() +
               theme(plot.background = element_rect(fill = "#faf8f2", color = NA)))
    }
    
    # Get crop coordinates for this city
    coords <- city_mapping[[city_code]]$coordinates
    
    # Create different plot depending on if we're in battlefield mode
    if (input$partyPrediction == "Battlefields") {
      # Create battlefield gradient map
      p <- ggplot() +
        geom_sf_interactive(data = city_data, 
                            aes(fill = battlefield_intensity, tooltip = tooltip_text, data_id = id_riding),
                            color = "#333333", size = 0.3) +
        scale_fill_gradientn(
          colors = c("black", "white", "#FFCC00"),
          values = c(0, 0.5, 1),
          limits = c(0, 100)
        )
    } else {
      # Standard party colors map
      p <- ggplot() +
        geom_sf_interactive(data = city_data, 
                            aes(fill = party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                            color = "#333333", size = 0.3) +
        scale_fill_manual(
          values = party_colors,
          name = "Winning Party",
          labels = function(x) paste0(party_names[[rv$lang()]][x], " (", x, ")"),
          na.value = "#777777"
        ) +
        scale_alpha_identity()
    }
    
    # Add common elements to the plot
    p <- p +
      # Set exact limits for the display area
      coord_sf(
        xlim = c(coords["xmin"], coords["xmax"]),
        ylim = c(coords["ymin"], coords["ymax"]),
        expand = FALSE
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      # Add title
      labs(
        title = plot_title
      )
    
    # Convertir en girafe pour interactivité
    girafe(ggobj = p, width_svg = 8, height_svg = 6, options = list(
      opts_tooltip(
        opacity = 0.9,
        css = "background-color: white; color: #333333; padding: 10px; border-radius: 5px; border: 1px solid #cccccc; font-family: 'Times New Roman', Times, serif;"
      ),
      opts_hover(
        css = "stroke-width: 2; stroke: #000000;"
      ),
      opts_sizing(rescale = TRUE)
    ))
  }
  
  # Render main map with hover functionality
  output$mapPlot <- renderGirafe({
    # Get filtered data
    plot_data <- filtered_map_data()
    
    # Check if plot_data is a valid sf object
    if (!inherits(plot_data, "sf") || nrow(plot_data) == 0) {
      # Return a graphical error message if no valid data
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No valid map data available", size = 6, color = "white") +
        theme_void() +
        theme(plot.background = element_rect(fill = "#faf8f2", color = NA))
      return(girafe(ggobj = p))
    }
    
    # Create a different map based on whether we're in Battlefields mode
    if (input$partyPrediction == "Battlefields") {
      # Battlefield gradient map (black -> white -> yellow)
      p <- ggplot() +
        geom_sf_interactive(data = plot_data, 
                            aes(fill = battlefield_intensity, tooltip = tooltip_text, data_id = id_riding),
                            color = "#555555", linewidth = 0.25) +
        scale_fill_gradientn(
          colors = c("black", "white", "#FFCC00"),
          values = c(0, 0.5, 1),
          limits = c(0, 100),
          name = "Competitiveness",
          guide = "none"
        )
    } else {
      # Standard party colors map
      p <- ggplot() +
        geom_sf_interactive(data = plot_data, 
                            aes(fill = party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                            color = "#777777", linewidth = 0.25) +
        scale_fill_manual(
          values = party_colors,
          name = NULL,
          labels = function(x) paste0(party_names[x], " (", x, ")"),
          na.value = "#777777"
        ) +
        scale_alpha_identity()
    }
    
    # Add common theme elements
    p <- p +
      theme_minimal() +
      theme(
        # Remove white edges
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        # Remove title and subtitle
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        
        # Remove caption
        plot.caption = element_blank(),
        
        # Remove legend
        legend.position = "none",
        
        # Remove grid and axes
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        
        # Reduce margins to maximize map space
        plot.margin = margin(0, 0, 0, 0)
      )
    
    # Pas de caption
    p <- p
    
    # Pas de légende
    p <- p
    
    # Convertir en girafe pour l'interactivité
    girafe(ggobj = p, width_svg = 10, height_svg = 7, options = list(
      opts_tooltip(
        opacity = 0.9,
        css = "background-color: white; color: #333333; padding: 10px; border-radius: 5px; border: 1px solid #cccccc; font-family: 'Times New Roman', Times, serif;"
      ),
      opts_hover(
        css = "stroke-width: 2; stroke: #000000;"
      ),
      opts_sizing(rescale = TRUE)
    ))
  })
  
  # Improved city map rendering function with hover
  renderCityMap <- function(city_code, city_name) {
    renderGirafe({
      # Create explicit dependency on these inputs
      party <- input$partyPrediction
      
      tryCatch({
        # Get filtered data directly
        filtered_data <- filtered_map_data()
        
        # Check if city mapping exists
        if (!is.null(city_mapping[[city_code]])) {
          ridings <- city_mapping[[city_code]]$ridings
          coords <- city_mapping[[city_code]]$coordinates
          
          # Filter and transform the data
          city_data <- filtered_data %>%
            filter(id_riding %in% ridings)
          
          # Only continue if we have data
          if (nrow(city_data) > 0) {
            # Convert to WGS84 projection safely
            tryCatch({
              city_data <- st_transform(city_data, 4326)
            }, error = function(e) {
              # If transformation fails, just use as-is
              message("Transform error for ", city_name, ": ", e$message)
            })
            
            # Create city map with different styling based on mode
            if (input$partyPrediction == "Battlefields") {
              # Battlefield gradient map for cities
              p <- ggplot() +
                geom_sf_interactive(data = city_data, 
                                    aes(fill = battlefield_intensity, tooltip = tooltip_text, data_id = id_riding),
                                    color = "#333333", linewidth = 0.3) +
                scale_fill_gradientn(
                  colors = c("black", "white", "#FFCC00"),
                  values = c(0, 0.5, 1),
                  limits = c(0, 100)
                )
            } else {
              # Standard party map for cities
              p <- ggplot() +
                geom_sf_interactive(data = city_data, 
                                    aes(fill = party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                                    color = "#333333", linewidth = 0.3) +
                scale_fill_manual(
                  values = party_colors,
                  labels = function(x) paste0(party_names[[rv$lang()]][x], " (", x, ")"),
                  na.value = "#777777"
                ) +
                scale_alpha_identity()
            }
            
            # Add common elements to all city maps
            p <- p +
              coord_sf(
                xlim = c(coords["xmin"], coords["xmax"]),
                ylim = c(coords["ymin"], coords["ymax"]),
                expand = FALSE
              ) +
              theme_minimal() +
              theme(
                legend.position = "none",
                plot.background = element_rect(fill = "white", color = NA, linewidth = 0),
                panel.background = element_rect(fill = "white", color = NA, linewidth = 0),
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(0, 0, 0, 0)
              )
            
            # Convertir en girafe pour l'interactivité
            return(girafe(ggobj = p, width_svg = 5, height_svg = 4, options = list(
              opts_tooltip(
                opacity = 0.9,
                css = "background-color: white; color: #333333; padding: 5px; border-radius: 3px; border: 1px solid #cccccc; font-family: 'Times New Roman', Times, serif; font-size: 12px;"
              ),
              opts_hover(
                css = "stroke-width: 2; stroke: #000000;"
              ),
              opts_sizing(rescale = TRUE)
            )))
          }
        }
        
        # Return empty plot if no data available
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("No data for", city_name), size = 4) +
          theme_void()
        girafe(ggobj = p)
      }, error = function(e) {
        # Return error message as plot
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Error loading map", size = 4) +
          theme_void()
        girafe(ggobj = p)
      })
    })
  }
  
  # Assign outputs for city maps (now including all 8 cities in 2x4 grid)
  output$montrealMap <- renderCityMap("montreal", "Montreal")
  output$torontoMap <- renderCityMap("toronto", "Toronto")
  output$vancouverMap <- renderCityMap("vancouver", "Vancouver")
  output$quebecCityMap <- renderCityMap("quebec_city", "Quebec City")
  output$ottawaMap <- renderCityMap("ottawa_gatineau", "Ottawa-Gatineau")
  output$winnipegMap <- renderCityMap("winnipeg", "Winnipeg")
  output$kitchenerMap <- renderCityMap("kitchener_waterloo", "Kitchener-Waterloo")
  output$londonMap <- renderCityMap("london", "London")
  
  # Create a reactive for table data to avoid recalculation
  table_data <- reactive({
    # Get processed data
    df_ridings <- rv$processed_data()
    
    # Apply party filter
    if (input$partyPrediction == "Battlefields") {
      # For Battlefields, show all ridings but sort by win probability (lowest probability first)
      df_ridings <- df_ridings %>% arrange(first_party_percentage)
    } else if (input$partyPrediction != "All parties" && input$partyPrediction %in% partis_politiques) {
      df_ridings <- df_ridings %>% 
        filter(party == input$partyPrediction)
    }
    
    # Format data for display
    df_ridings %>% 
      mutate(
        # Join with map_data to get riding name in French
        riding_name = map_data$name_riding_fr[match(riding, map_data$id_riding)],
        # Use ID if name not found
        riding_name = ifelse(is.na(riding_name), as.character(riding), riding_name),
        # Format percentage with % symbol
        win_probability = paste0(round(first_party_percentage, 1), "%")
      ) %>%
      # Rename columns for display
      select(
        # Use dynamic column names based on current language
        if (rv$lang() == "fr") {
          c(
            "Circonscription" = "riding_name",
            "Parti en tête" = "party",
            "Probabilité de victoire" = "win_probability"
          )
        } else {
          c(
            "Riding" = "riding_name",
            "Leading party" = "party",
            "Win probability" = "win_probability"
          )
        }
      )
  })
  
  # Render data table
<<<<<<< HEAD
  # Render data table
  output$dataTable <- renderDataTable({
    # Use the reactive table data
    display_data <- table_data()
    
    # Only create the datatable when we have data
    req(nrow(display_data) > 0)
    
    # Special color styling for Battlefields mode
    if (input$partyPrediction == "Battlefields") {
      # Get margins as numeric for battlefields coloring
      numeric_margins <- as.numeric(gsub("%", "", display_data$Marge))
      
      # Calculate color intensity for each row
      battlefield_colors <- sapply(numeric_margins, function(margin) {
        # Create gradient: yellow (100) -> white (50) -> black (0)
        intensity <- pmin(100, pmax(0, 100 - (margin * 4)))
        
        if (intensity <= 50) {
          # Black to White gradient
          val <- (intensity / 50) * 255
          r <- val
          g <- val
          b <- val
        } else {
          # White to Yellow (#FFCC00) gradient
          r <- 255
          g <- 255 - ((intensity - 50) / 50) * (255 - 204)
          b <- 255 - ((intensity - 50) / 50) * 255
        }
        
        return(rgb(r, g, b, maxColorValue = 255))
      })
      
      # Create text colors array (white for dark backgrounds, black for light backgrounds)
      text_colors <- sapply(numeric_margins, function(margin) {
        intensity <- pmin(100, pmax(0, 100 - (margin * 4)))
        # Use white text if background is dark (intensity > 65)
        if (intensity > 65) {
          return("#000000") # White text
        } else {
          return("#FFFFFF") # Black text
        }
      })
    }
    
    # Return the data with improved formatting
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 10,
        autoWidth = FALSE,
        searchHighlight = TRUE,
        dom = '<"top"f>rt<"bottom"ip>',
        language = list(
          search = t("search"),
          paginate = list(previous = t("previous"), `next` = t("next"))
        ),
        scrollX = TRUE,
        # Only define essential column defs
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          # Ajouter un columnDef pour traiter la colonne Marge numériquement
          list(
            targets = 5,  # Index de la colonne "Marge" (en supposant qu'elle est à l'index 5)
            type = 'num'  # Définir le type comme numérique
          )
        ),
        order = if(input$partyPrediction == "Battlefields") list(list(5, 'asc')) else NULL
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    ) %>%
      formatStyle(
        columns = colnames(display_data),
        backgroundColor = "white",
        color = "black"
      )
    
    # Apply specific styling based on mode
    if (input$partyPrediction == "Battlefields") {
      # For Battlefields, color the Marge column with the battlefield gradient
      # and adjust text color to ensure readability
      dt <- dt %>% formatStyle(
        columns = "Marge",
        backgroundColor = styleEqual(display_data$Marge, battlefield_colors),
        color = styleEqual(display_data$Marge, text_colors)
      )
    } else {
      # For other modes, color the party column
      dt <- dt %>% formatStyle(
        columns = "Parti en tête",
        backgroundColor = styleEqual(
          names(party_colors),
          sapply(party_colors, function(color) {
            paste0(color, "25")  # Add transparency to the color
          })
        )
      )
    }
    
    return(dt)
  })
=======
  output$dataTable <- renderDataTable({
   # Use the reactive table data
   display_data <- table_data()
   
   # Only create the datatable when we have data
   req(nrow(display_data) > 0)
   
   # Extract win probability column name based on language
   prob_col <- if(rv$lang() == "fr") "Probabilité de victoire" else "Win probability"
   
   # Special color styling for Battlefields mode
   if (input$partyPrediction == "Battlefields") {
     # Get probabilities as numeric for battlefields coloring
     numeric_probs <- as.numeric(gsub("%", "", display_data[[prob_col]]))
     
     # Calculate color intensity for each row - inverse of probability
     battlefield_colors <- sapply(numeric_probs, function(prob) {
       # Create gradient: yellow (high uncertainty) -> white (medium) -> black (high certainty)
       # Invert the probability to match the battlefield concept (lower prob = more competitive)
       intensity <- pmin(100, pmax(0, 100 - prob))
       
       if (intensity <= 50) {
         # Black to White gradient
         val <- (intensity / 50) * 255
         r <- val
         g <- val
         b <- val
       } else {
         # White to Yellow (#FFCC00) gradient
         r <- 255
         g <- 255 - ((intensity - 50) / 50) * (255 - 204)
         b <- 255 - ((intensity - 50) / 50) * 255
       }
       
       return(rgb(r, g, b, maxColorValue = 255))
     })
     
     # Create text colors array (white for dark backgrounds, black for light backgrounds)
     text_colors <- sapply(numeric_probs, function(prob) {
       intensity <- pmin(100, pmax(0, 100 - prob))
       # Use white text if background is dark
       if (intensity < 30) {
         return("#FFFFFF") # White text
       } else {
         return("#000000") # Black text
       }
     })
   }
   
   # Determine party column name based on language
   party_col <- if(rv$lang() == "fr") "Parti en tête" else "Leading party"
   
   # Return the data with improved formatting
   dt <- datatable(
     display_data,
     options = list(
       pageLength = 10,
       autoWidth = FALSE,
       searchHighlight = TRUE,
       dom = '<"top"f>rt<"bottom"ip>',
       language = list(
         search = t("search"),
         paginate = list(previous = t("previous"), `next` = t("next"))
       ),
       scrollX = TRUE,
       # Define column defs
       columnDefs = list(
         list(className = 'dt-center', targets = "_all"),
         # Add a columnDef to treat the probability column numerically
         list(
           targets = if(rv$lang() == "fr") 2 else 2,  # Probability column index
           type = 'num'
         )
       ),
       # In battlefield mode, sort by probability (ascending = most competitive first)
       order = if(input$partyPrediction == "Battlefields") list(list(2, 'asc')) else NULL
     ),
     rownames = FALSE,
     class = 'cell-border stripe compact'
   ) %>%
     formatStyle(
       columns = colnames(display_data),
       backgroundColor = "white",
       color = "black"
     )
   
   # Apply specific styling based on mode
   if (input$partyPrediction == "Battlefields") {
     # For Battlefields, color the probability column with the battlefield gradient
     # and adjust text color to ensure readability
     dt <- dt %>% formatStyle(
       columns = prob_col,
       backgroundColor = styleEqual(display_data[[prob_col]], battlefield_colors),
       color = styleEqual(display_data[[prob_col]], text_colors)
     )
   } else {
     # For other modes, color the party column
     dt <- dt %>% formatStyle(
       columns = party_col,
       backgroundColor = styleEqual(
         names(party_colors),
         sapply(party_colors, function(color) {
           paste0(color, "25")  # Add transparency to the color
         })
       )
     )
   }
   
   return(dt)
 })
>>>>>>> 3d28330b424d92572eae5ea7bf7fd6845679984a
  
  # Clear cache when major inputs change
  observeEvent(list(input$partyPrediction, rv$lang()), {
    for (key in names(city_data_cache)) {
      city_data_cache[[key]] <- NULL
    }
  }, ignoreInit = TRUE)  # Ignore initial values to avoid clearing cache at startup
  
  # Output text elements for UI based on current language
  output$modelTypeLabel <- renderText({ t("model_type") })
  output$modelTypeValue <- renderText({ t("app_title") })
  output$partyPredictionLabel <- renderText({ t("party_prediction") })
  output$leadStrengthTitle <- renderText({ t("lead_strength") })
  output$competitiveRidingLabel <- renderUI({ HTML(gsub("\n", "<br>", t("riding_competitive"))) })
  output$consolidatedLeadLabel <- renderUI({ HTML(gsub("\n", "<br>", t("consolidated_lead"))) })
  output$competitivenessTitle <- renderText({ t("competitiveness") })
  output$lessCompetitiveLabel <- renderUI({ HTML(gsub("\n", "<br>", t("less_competitive"))) })
  output$moreCompetitiveLabel <- renderUI({ HTML(gsub("\n", "<br>", t("more_competitive"))) })
  output$dataLinkLabel <- renderText({ t("data_link") })
  output$dataUpdatedText <- renderText({ t("data_updated") })
  
  # Modified to reflect data source change - no longer showing number of respondents from previous survey
  output$respondentsText <- renderText({ paste0(t("respondents"), "N/A (Aggregated data)") })
  
  output$hoverInfoText <- renderUI({ HTML(paste0("<i class='fas fa-info-circle'></i> ", t("hover_info"))) })
  output$appTitle <- renderUI({ HTML(t("app_title")) })
  
  # City names based on current language
  output$montrealTitle <- renderText({ city_names[[rv$lang()]]["montreal"] })
  output$torontoTitle <- renderText({ city_names[[rv$lang()]]["toronto"] })
  output$vancouverTitle <- renderText({ city_names[[rv$lang()]]["vancouver"] })
  output$quebecCityTitle <- renderText({ city_names[[rv$lang()]]["quebec_city"] })
  output$ottawaTitle <- renderText({ city_names[[rv$lang()]]["ottawa_gatineau"] })
  output$winnipegTitle <- renderText({ city_names[[rv$lang()]]["winnipeg"] })
  output$kitchenerTitle <- renderText({ city_names[[rv$lang()]]["kitchener_waterloo"] })
  output$londonTitle <- renderText({ city_names[[rv$lang()]]["london"] })
}
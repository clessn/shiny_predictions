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
library(lubridate)

server <- function(input, output, session) {
  # Reduce verbosity
  options(shiny.trace = FALSE)
  
  # Initialize reactive values for data storage
  rv <- reactiveValues()
  
  # Initialize language (default to French)
  rv$lang <- reactiveVal("fr")
  # Store the initial language value
  rv$initial_lang <- "fr"
  
  # Initialize button styles based on default language
  observe({
    # Run once when the app initializes
    shinyjs::runjs("
      if (document.getElementById('setLangFR') && document.getElementById('setLangEN')) {
        document.getElementById('setLangFR').style.backgroundColor = '#6c757d';
        document.getElementById('setLangFR').style.color = 'white';
        document.getElementById('setLangFR').style.border = 'none';
        
        document.getElementById('setLangEN').style.backgroundColor = 'white';
        document.getElementById('setLangEN').style.color = '#6c757d';
        document.getElementById('setLangEN').style.border = '1px solid #6c757d';
      }
    ")
  })
  
  # Set language to French when FR button is clicked
  observeEvent(input$setLangFR, {
    rv$lang("fr")
    
    # Update button styling to show active state
    shinyjs::runjs("
      document.getElementById('setLangFR').style.backgroundColor = '#6c757d';
      document.getElementById('setLangFR').style.color = 'white';
      document.getElementById('setLangFR').style.border = 'none';
      
      document.getElementById('setLangEN').style.backgroundColor = 'white';
      document.getElementById('setLangEN').style.color = '#6c757d';
      document.getElementById('setLangEN').style.border = '1px solid #6c757d';
    ")
  })
  
  # Set language to English when EN button is clicked
  observeEvent(input$setLangEN, {
    rv$lang("en")
    
    # Update button styling to show active state
    shinyjs::runjs("
      document.getElementById('setLangEN').style.backgroundColor = '#6c757d';
      document.getElementById('setLangEN').style.color = 'white';
      document.getElementById('setLangEN').style.border = 'none';
      
      document.getElementById('setLangFR').style.backgroundColor = 'white';
      document.getElementById('setLangFR').style.color = '#6c757d';
      document.getElementById('setLangFR').style.border = '1px solid #6c757d';
    ")
  })
  
  # Translation helper function
  t <- function(key) {
    translations[[rv$lang()]][[key]]
  }
  
  # Load the new dataset with proper riding name mapping
  rv$new_data <- reactive({
    # Load data 
    df <- read.csv("https://raw.githubusercontent.com/clessn/agregateur_data/main/data/df.csv")
    
    # Extract the most recent date from the data
    if ("date" %in% colnames(df) && nrow(df) > 0) {
      # Find most recent date in case there are multiple dates
      latest_date <- max(as.Date(df$date))
      rv$latest_data_date <- latest_date
    }
    
    # Create a lookup table from map_data for riding names - forcing numeric ids for consistency
    riding_names <- data.frame(
      riding_id = as.character(map_data$id_riding),
      name_riding_en = as.character(map_data$name_riding_en),
      name_riding_fr = as.character(map_data$name_riding_fr),
      stringsAsFactors = FALSE
    )
    
    # Debug output to help diagnose issues
    print("CSV riding_id sample:")
    print(head(df$riding_id, 3))
    print("Map riding_id sample:")
    print(head(riding_names$riding_id, 3))
    
    # Make sure riding_id is a character for consistent joining
    df$riding_id <- trimws(as.character(df$riding_id))
    riding_names$riding_id <- trimws(as.character(riding_names$riding_id))
    
    # Debug output to check match counts
    print(paste("CSV rows:", nrow(df)))
    print(paste("Map data rows:", nrow(riding_names)))
    print(paste("Matching IDs:", sum(df$riding_id %in% riding_names$riding_id)))
    
    # Create direct sample check to verify first few ridings
    sample_csv <- head(df$riding_id, 5)
    sample_map <- head(riding_names$riding_id, 5)
    print("Direct comparison of first 5 IDs:")
    for (i in 1:5) {
      print(paste("CSV:", sample_csv[i], "Map:", sample_map[i], "Match:", sample_csv[i] == sample_map[i]))
    }
    
    # Verify the entire dataset with explicit comparison
    df$id_match <- df$riding_id %in% riding_names$riding_id
    print(paste("Matched IDs verified:", sum(df$id_match), "out of", nrow(df)))
    
    # Join the riding names to our data
    df <- df %>%
      left_join(riding_names, by = "riding_id")
    
    # Check join result
    print(paste("Joined data rows:", nrow(df)))
    print(paste("Rows with names:", sum(!is.na(df$name_riding_en))))
    
    # Print some sample joined data to verify
    print("Sample of joined data:")
    print(head(data.frame(
      riding_id = df$riding_id,
      name_en = df$name_riding_en,
      name_fr = df$name_riding_fr
    ), 5))
    
    # If the join didn't work well, let's create a fallback
    if (sum(!is.na(df$name_riding_en)) < 50) { # If fewer than 50 ridings got names
      print("WARNING: Join failed to match names - creating fallback mapping")
      
      # Load or create a direct mapping
      riding_map <- data.frame(
        riding_id = df$riding_id,
        # Create English names directly based on province and riding number
        name_riding_en = paste0(
          ifelse(substr(df$riding_id, 1, 2) == "10", "Newfoundland: ", 
                 ifelse(substr(df$riding_id, 1, 2) == "11", "PEI: ", 
                        ifelse(substr(df$riding_id, 1, 2) == "12", "Nova Scotia: ",
                               ifelse(substr(df$riding_id, 1, 2) == "13", "New Brunswick: ",
                                      ifelse(substr(df$riding_id, 1, 2) == "24", "Quebec: ",
                                             ifelse(substr(df$riding_id, 1, 2) == "35", "Ontario: ",
                                                    ifelse(substr(df$riding_id, 1, 2) == "46", "Manitoba: ",
                                                           ifelse(substr(df$riding_id, 1, 2) == "47", "Saskatchewan: ",
                                                                  ifelse(substr(df$riding_id, 1, 2) == "48", "Alberta: ",
                                                                         ifelse(substr(df$riding_id, 1, 2) == "59", "BC: ",
                                                                                "Territory: "
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
                 )
          ),
          "Riding ", 
          substr(df$riding_id, 3, 5)
        ),
        # Create French names similarly but with French province names
        name_riding_fr = paste0(
          ifelse(substr(df$riding_id, 1, 2) == "10", "Terre-Neuve: ", 
                 ifelse(substr(df$riding_id, 1, 2) == "11", "Î.-P.-É.: ", 
                        ifelse(substr(df$riding_id, 1, 2) == "12", "Nouvelle-Écosse: ",
                               ifelse(substr(df$riding_id, 1, 2) == "13", "Nouveau-Brunswick: ",
                                      ifelse(substr(df$riding_id, 1, 2) == "24", "Québec: ",
                                             ifelse(substr(df$riding_id, 1, 2) == "35", "Ontario: ",
                                                    ifelse(substr(df$riding_id, 1, 2) == "46", "Manitoba: ",
                                                           ifelse(substr(df$riding_id, 1, 2) == "47", "Saskatchewan: ",
                                                                  ifelse(substr(df$riding_id, 1, 2) == "48", "Alberta: ",
                                                                         ifelse(substr(df$riding_id, 1, 2) == "59", "C.-B.: ",
                                                                                "Territoire: "
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
                 )
          ),
          "Circonscription ", 
          substr(df$riding_id, 3, 5)
        )
      )
      
      # Replace the names in df with our manually created ones
      df$name_riding_en <- riding_map$name_riding_en
      df$name_riding_fr <- riding_map$name_riding_fr
      
      print("Fallback mapping created")
      print(head(riding_map, 3))
    }
    
    # For any ridings that don't have names, create fallback names
    df$name_riding_en <- ifelse(is.na(df$name_riding_en), 
                                paste0("Riding ", df$riding_id),
                                df$name_riding_en)
    df$name_riding_fr <- ifelse(is.na(df$name_riding_fr), 
                                paste0("Circonscription ", df$riding_id),
                                df$name_riding_fr)
    
    return(df)
  })
  
  # Initialize an empty cache using reactiveValues
  city_data_cache <- reactiveValues()
  
  # Store current city code in a reactive value for other functions to use
  current_city_code <- reactiveVal()
  
  # Create dynamic UI for party selection based on current language
  output$partySelectionUI <- renderUI({
    selectInput(
      "partyPrediction", 
      NULL, # Remove the label here since we already have a header above
      choices = c(t("all_parties"), t("battlefields"), "LPC", "CPC", "BQ", "NDP", "GPC"),
      selected = t("all_parties")
    )
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
        # Note: name_riding_en and name_riding_fr are already added from our map_data
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
    # Get processed riding data (contains predictions, IDs)
    df_ridings <- rv$processed_data() 
    
    # Create a clean copy of the original map data (contains geometry, IDs, ORIGINAL NAMES)
    map_data_copy <- map_data %>%
      mutate(
        id_riding = as.character(id_riding),
        name_riding_en_map = as.character(name_riding_en), 
        name_riding_fr_map = as.character(name_riding_fr)
      ) %>%
      select(id_riding, name_riding_en_map, name_riding_fr_map, geometry) 
    
    # Select only prediction-related columns and the ID from df_ridings
    df_ridings_subset <- df_ridings %>%
      select(
        riding, party, first_party_percentage, 
        second_party, second_party_percentage, closeness 
      ) %>%
      mutate(riding = as.character(riding)) 
    
    # Join map data (with definitive names) to the prediction subset
    map_filtered <- map_data_copy %>%
      left_join(df_ridings_subset, by = c("id_riding" = "riding"))
    
    # Handle NAs and create fallbacks using definitive names
    map_filtered <- map_filtered %>%
      mutate(
        party = ifelse(is.na(party), "Unknown", party),
        first_party_percentage = ifelse(is.na(first_party_percentage), 50, first_party_percentage),
        name_riding_en_map = ifelse(is.na(name_riding_en_map) | name_riding_en_map == "", paste0("Riding ", id_riding), name_riding_en_map),
        name_riding_fr_map = ifelse(is.na(name_riding_fr_map) | name_riding_fr_map == "", paste0("Circonscription ", id_riding), name_riding_fr_map)
      )
    
    # !!! PRE-CALCULATE NAME BASED ON LANGUAGE !!!
    current_language <- rv$lang() # Get current language once
    map_filtered <- map_filtered %>%
      mutate(
        current_display_name = if (current_language == "fr") name_riding_fr_map else name_riding_en_map
      )
    # !!! END PRE-CALCULATION !!!
    
    # Add other display columns (alpha, categories, tooltip)
    map_filtered <- map_filtered %>%
      mutate(
        alpha_category = case_when( # ... (alpha logic) ...
          first_party_percentage <= 52 ~ 0.35,
          first_party_percentage > 52 & first_party_percentage <= 58 ~ 0.5,
          first_party_percentage > 58 & first_party_percentage <= 70 ~ 0.7,
          first_party_percentage > 70 & first_party_percentage <= 85 ~ 0.85,
          first_party_percentage > 85 ~ 1.0,
          TRUE ~ 0.6
        ),
        battlefield_intensity = pmin(100, pmax(0, 100 - first_party_percentage)),
        closeness_category = case_when( # ... (closeness logic) ...
          first_party_percentage <= 52 ~ ifelse(rv$lang() == "fr", "Trop serré pour prédire", "Too close to call"),
          first_party_percentage > 52 & first_party_percentage <= 58 ~ ifelse(rv$lang() == "fr", "Course serrée", "Toss up"),
          first_party_percentage > 58 & first_party_percentage <= 70 ~ ifelse(rv$lang() == "fr", "Avance modérée", "Competitive"),
          first_party_percentage > 70 & first_party_percentage <= 85 ~ ifelse(rv$lang() == "fr", "Avance confortable", "Likely win"),
          first_party_percentage > 85 ~ ifelse(rv$lang() == "fr", "Victoire quasi-certaine", "Safe win"),
          TRUE ~ ifelse(rv$lang() == "fr", "Inconnu", "Unknown")
        ),
        
        # Create tooltip text using the PRE-CALCULATED name
        tooltip_text = paste0(
          "<strong>", current_display_name, "</strong><br>", # Use pre-calculated name
          "<span style='font-size: 9px; color: #777;'>ID: ", id_riding, "</span><br>",
          ifelse(rv$lang() == "fr", "Parti en tête: ", "Leading party: "),
          "<span style='color:", ifelse(party %in% names(party_colors), party_colors[party], "#777777"), ";'>", 
          party, " (", round(first_party_percentage, 1), "%)</span><br>",
          ifelse(rv$lang() == "fr", "Certitude: ", "Certainty: "), 
          closeness_category
        )
        # REMOVE the old 'display_name' calculation if it existed here
      )
    
    # Filter by party (rest of the function remains the same)
    # ... (filtering logic) ...
    if (input$partyPrediction == t("all_parties")) {
      return(map_filtered)
    } else if (input$partyPrediction == t("battlefields") || input$partyPrediction == "Battlefields") {
      return(map_filtered)
    } else if (input$partyPrediction %in% partis_politiques) {
      return(map_filtered %>% 
               mutate(selected_party = party == input$partyPrediction,
                      display_party = ifelse(selected_party, party, "other")))
    } else {
      return(map_filtered)
    }
  })  
  
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
    if (input$partyPrediction == t("battlefields")) {
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
    girafe(ggobj = p, width_svg = 42, height_svg = 6, options = list(
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
    if (input$partyPrediction == t("battlefields") || input$partyPrediction == "Battlefields") {
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
      # Calculate party seats - save to a reactive value so it can be accessed in sidebar
      # Use unique riding ids to ensure each riding is counted only once
      # This solves the issue where some ridings appear in multiple shape files
      # Convert to data.frame first to avoid sf geometry issues with distinct
      unique_ridings <- plot_data %>%
        st_drop_geometry() %>%
        select(id_riding, party) %>%
        distinct(id_riding, .keep_all = TRUE)
      
      # Now count seats by party from the unique ridings
      party_seats <- table(unique_ridings$party)
      party_seats <- party_seats[partis_politiques]
      party_seats[is.na(party_seats)] <- 0
      
      # Save party seats to reactive values for use in sidebar
      rv$party_seats <- party_seats
      
      # Add total seat count to verify we have the correct number (343)
      rv$total_seats <- sum(party_seats)
      
      # Check if it's a party-specific view
      if (input$partyPrediction %in% partis_politiques) {
        # For party-specific maps, show selected party in color and others gray
        party_specific_colors <- c(party_colors, "other" = "#DDDDDD")
        
        p <- ggplot() +
          geom_sf_interactive(data = plot_data, 
                              aes(fill = display_party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                              color = "#777777", linewidth = 0.25) +
          scale_fill_manual(
            values = party_specific_colors,
            name = NULL,
            na.value = "#777777"
          ) +
          scale_alpha_identity()
      } else {
        # Standard party colors map (all parties)
        p <- ggplot() +
          geom_sf_interactive(data = plot_data, 
                              aes(fill = party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                              color = "#777777", linewidth = 0.25) +
          scale_fill_manual(
            values = party_colors,
            name = NULL,
            na.value = "#777777"
          ) +
          scale_alpha_identity()
      }
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
        
        # Remove legend from plot
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
    girafe(ggobj = p, width_svg = 18, height_svg = 12, options = list(
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
            if (input$partyPrediction == t("battlefields") || input$partyPrediction == "Battlefields") {
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
              # Check if it's a party-specific view
              if (input$partyPrediction %in% partis_politiques && "display_party" %in% colnames(city_data)) {
                # For party-specific city maps
                party_specific_colors <- c(party_colors, "other" = "#DDDDDD")
                
                p <- ggplot() +
                  geom_sf_interactive(data = city_data, 
                                      aes(fill = display_party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                                      color = "#333333", linewidth = 0.3) +
                  scale_fill_manual(
                    values = party_specific_colors,
                    na.value = "#777777"
                  ) +
                  scale_alpha_identity()
              } else {
                # Standard party map for cities
                p <- ggplot() +
                  geom_sf_interactive(data = city_data, 
                                      aes(fill = party, alpha = alpha_category, tooltip = tooltip_text, data_id = id_riding),
                                      color = "#333333", linewidth = 0.3) +
                  scale_fill_manual(
                    values = party_colors,
                    na.value = "#777777"
                  ) +
                  scale_alpha_identity()
              }
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
                legend.position = "none", # No legend for city maps as they are small
                plot.background = element_rect(fill = "white", color = NA, linewidth = 0),
                panel.background = element_rect(fill = "white", color = NA, linewidth = 0),
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = margin(0, 0, 0, 0)
              )
            
            # Convertir en girafe pour l'interactivité
            return(girafe(ggobj = p, width_svg = 6, height_svg = 5, options = list(
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
    # Get processed data - Debug output showed this HAS correct names & IDs
    df_ridings <- rv$processed_data() 
    
    # Apply party filter
    # ... (filtering logic remains same) ...
    if (input$partyPrediction == t("battlefields") || input$partyPrediction == "Battlefields") {
      df_ridings <- df_ridings %>% arrange(first_party_percentage)
    } else if (input$partyPrediction %in% partis_politiques) {
      df_ridings <- df_ridings %>% 
        filter(party == input$partyPrediction)
    }
    
    # !!! PRE-CALCULATE NAME BASED ON LANGUAGE !!!
    current_language <- rv$lang() # Get current language once
    df_ridings_named <- df_ridings %>%
      mutate(
        # Ensure source names are character and handle NAs
        name_riding_en = as.character(name_riding_en),
        name_riding_fr = as.character(name_riding_fr),
        name_riding_en = ifelse(is.na(name_riding_en) | name_riding_en == "", paste0("Riding ", riding), name_riding_en),
        name_riding_fr = ifelse(is.na(name_riding_fr) | name_riding_fr == "", paste0("Circonscription ", riding), name_riding_fr),
        # Create the single name column based on current language
        riding_name = if (current_language == "fr") name_riding_fr else name_riding_en
      )
    # !!! END PRE-CALCULATION !!!
    
    # Format data for display using the pre-calculated 'riding_name'
    df_display <- df_ridings_named %>% 
      mutate(
        # Calculate closeness category
        closeness_category = case_when( # ... (closeness logic) ...
          first_party_percentage <= 52 ~ ifelse(rv$lang() == "fr", "Trop serré pour prédire", "Too close to call"),
          first_party_percentage > 52 & first_party_percentage <= 58 ~ ifelse(rv$lang() == "fr", "Course serrée", "Toss up"),
          first_party_percentage > 58 & first_party_percentage <= 70 ~ ifelse(rv$lang() == "fr", "Avance modérée", "Competitive"),
          first_party_percentage > 70 & first_party_percentage <= 85 ~ ifelse(rv$lang() == "fr", "Avance confortable", "Likely win"),
          first_party_percentage > 85 ~ ifelse(rv$lang() == "fr", "Victoire quasi-certaine", "Safe win"),
          TRUE ~ ifelse(rv$lang() == "fr", "Inconnu", "Unknown")
        ),
        # Store original percentage for sorting
        percentage_value = first_party_percentage
      ) %>%
      # Select and rename columns for the final table output
      # Note: We already created 'riding_name' correctly above
      select(
        if (rv$lang() == "fr") {
          # Use the pre-calculated riding_name
          c("Circonscription" = "riding_name", "Parti en tête" = "party", "Certitude" = "closeness_category", "Pourcentage" = "percentage_value") 
        } else {
          # Use the pre-calculated riding_name
          c("Riding" = "riding_name", "Leading party" = "party", "Certainty" = "closeness_category", "Percentage" = "percentage_value")
        }
      )
    
    return(df_display)
  }) # Render data table
  output$dataTable <- renderDataTable({
    # Use the reactive table data
    display_data <- table_data()
    
    # Only create the datatable when we have data
    req(nrow(display_data) > 0)
    
    # Extract column names based on language
    status_col <- if(rv$lang() == "fr") "Certitude" else "Certainty"
    percentage_col <- if(rv$lang() == "fr") "Pourcentage" else "Percentage"
    party_col <- if(rv$lang() == "fr") "Parti en tête" else "Leading party"
    
    # Not using color styles for certainty column anymore, 
    # but keeping the categories for reference
    status_categories <- c(
      "Too close to call",
      "Toss up",
      "Competitive",
      "Likely win",
      "Safe win",
      # French versions
      "Trop serré pour prédire",
      "Course serrée",
      "Avance modérée",
      "Avance confortable",
      "Victoire quasi-certaine",
      # Fallbacks
      "Unknown",
      "Inconnu"
    )
    
    # Special color styling for Battlefields mode
    if (input$partyPrediction == t("battlefields")) {
      # Get percentages for battlefields coloring
      numeric_probs <- display_data[[percentage_col]]
      
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
    
    # Create the data table with its options
    dt <- datatable(
      # Keep all columns including percentage for sorting
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
          # Make the percentage column hidden but use it for sorting
          list(
            targets = 3,  # Percentage column index
            visible = FALSE
          ),
          # Make the status column sortable by the percentage column
          list(
            targets = 2,  # Status column index
            orderData = 3  # Use percentage column for sorting
          )
        ),
        # In battlefield mode, sort by percentage (ascending = most competitive first)
        order = if(input$partyPrediction == t("battlefields") || input$partyPrediction == "Battlefields") list(list(3, 'asc')) else NULL
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    ) %>%
      formatStyle(
        columns = colnames(display_data)[1:3], # Style only the visible columns
        backgroundColor = "white",
        color = "black"
      )
    
    # Apply specific styling based on mode
    if (input$partyPrediction == t("battlefields") || input$partyPrediction == "Battlefields") {
      # For Battlefields, use plain text for certainty column (no background color)
      dt <- dt %>% formatStyle(
        columns = status_col,
        backgroundColor = "white",
        color = "#333333"
      )
    } else {
      # Color the party column by party color
      dt <- dt %>% formatStyle(
        columns = party_col,
        backgroundColor = styleEqual(
          partis_politiques,  # Use the list of political parties directly
          sapply(party_colors, function(color) {
            paste0(color, "25")  # Add transparency to the color
          })
        )
      )
      
      # Plain text for certainty column (no background color)
      dt <- dt %>% formatStyle(
        columns = status_col,
        backgroundColor = "white",
        color = "#333333"
      )
    }
    
    return(dt)
  })
  
  # Clear cache when major inputs change
  observeEvent(list(input$partyPrediction, rv$lang()), {
    for (key in names(city_data_cache)) {
      city_data_cache[[key]] <- NULL
    }
    
    # Force UI refresh when language changes
    if (rv$lang() != rv$initial_lang) {
      session$sendCustomMessage(type = "refreshUI", message = list())
    }
  }, ignoreInit = TRUE)  # Ignore initial values to avoid clearing cache at startup
  
  # Output text elements for UI based on current language
  output$modelTypeLabel <- renderText({ t("model_type") })
  output$modelTypeValue <- renderText({ t("app_title") })
  output$partyPredictionLabel <- renderText({ t("party_prediction") })
  output$leadStrengthTitle <- renderText({ t("lead_strength") })
  
  # Generate new text for the closeness categories
  output$competitiveRidingLabel <- renderUI({ 
    if(rv$lang() == "fr") {
      HTML("Trop serré<br>pour prédire")
    } else {
      HTML("Too close<br>to call")
    }
  })
  
  output$consolidatedLeadLabel <- renderUI({ 
    if(rv$lang() == "fr") {
      HTML("Victoire<br>quasi-certaine")
    } else {
      HTML("Safe<br>win")
    }
  })
  
  output$competitivenessTitle <- renderText({ t("competitiveness") })
  
  output$lessCompetitiveLabel <- renderUI({ 
    if(rv$lang() == "fr") {
      HTML("Moins<br>compétitif")
    } else {
      HTML("Less<br>competitive")
    }
  })
  
  output$moreCompetitiveLabel <- renderUI({ 
    if(rv$lang() == "fr") {
      HTML("Plus<br>compétitif")
    } else {
      HTML("More<br>competitive")
    }
  })
  output$dataLinkLabel <- renderText({ t("data_link") })
  output$dataUpdatedText <- renderText({ 
    if (is.null(rv$latest_data_date)) {
      return(t("data_updated")) # Fallback to static text if date extraction fails
    }
    
    # Format the date according to the current language
    if (rv$lang() == "fr") {
      # Format date in French style (jour mois année)
      months_fr <- c("janvier", "février", "mars", "avril", "mai", "juin", 
                    "juillet", "août", "septembre", "octobre", "novembre", "décembre")
      month_num <- month(rv$latest_data_date)
      month_name <- months_fr[month_num]
      day <- day(rv$latest_data_date)
      year <- year(rv$latest_data_date)
      
      paste0("Données mises à jour: ", day, " ", month_name, " ", year)
    } else {
      # Format date in English style (Month day, Year)
      format(rv$latest_data_date, "Data updated: %B %d, %Y")
    }
  })
  
  output$hoverInfoText <- renderUI({ HTML(paste0("<i class='fas fa-info-circle'></i> ", t("hover_info"))) })
  output$appTitle <- renderUI({ HTML(t("app_title")) })
  
  output$appSubtitle <- renderUI({
    subtitle_text <- if(rv$lang() == "fr") {
      "Prédiction du nombre de sièges par parti par jour"
    } else {
      "Daily prediction of seats by party"
    }
    
    HTML(paste0("<em>", subtitle_text, "</em>"))
  })
  
  # Seat count title and legend
  output$seatCountTitle <- renderText({ t("seat_count") })
  
  output$seatCountLegend <- renderUI({
    # Update only when party prediction changes
    req(input$partyPrediction)
    
    # Get party seats from reactive values
    if (is.null(rv$party_seats)) return(HTML(""))
    
    # Create party rows with seat counts and party leader images
    seat_html <- lapply(names(rv$party_seats), function(party) {
      # Define the image path without "www/" prefix (Shiny already looks there)
      img_path <- paste0(tolower(party), ".png")
      
      count <- rv$party_seats[party]
      label <- ifelse(rv$lang() == "fr", "sièges", "seats")
      
      # Create HTML for this party row - now including the party acronym
      HTML(paste0(
        "<div style='display: flex; justify-content: space-between; align-items: center; margin: 8px 0;'>",
        "<div style='display: flex; align-items: center;'>",
        "<img src='", img_path, "' style='width: 25px; height: auto;' alt='", party, "'>",
        "<span style='margin-left: 5px; font-size: 12px; color: #777777;'>", party, "</span>",
        "</div>",
        "<span>", count, " ", label, "</span>",
        "</div>"
      ))
    })
    
    # Add a total row
    total_label <- ifelse(rv$lang() == "fr", "sièges au total", "total seats")
    total_html <- HTML(paste0(
      "<div style='display: flex; justify-content: space-between; margin: 10px 0; border-top: 1px solid #eee; padding-top: 5px;'>",
      "<span style='font-weight: bold;'>", ifelse(rv$lang() == "fr", "Total", "Total"), "</span>",
      "<span>", rv$total_seats, " ", total_label, "</span>",
      "</div>"
    ))
    
    # Join all HTML pieces together including the total
    do.call(tagList, c(seat_html, list(total_html)))
  })
  
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

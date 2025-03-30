# server.R - Modified for Battlefields visualization
library(shiny)
library(DT)
library(ggplot2)
library(sf)
library(waiter)
library(tidyr)
library(dplyr)
library(patchwork)
library(ggiraph) # Ajouté pour les fonctionnalités hover
library(scales) # For color gradient functions

server <- function(input, output, session) {
  # Reduce verbosity
  options(shiny.trace = FALSE)
  
  # Initialize reactive values for data storage
  rv <- reactiveValues()
  
  # Charger les données pondérées
  rv$donnees_sondage <- readRDS("data/donnees_sondage_ponderees.rds")
  
  # Initialize an empty cache using reactiveValues
  city_data_cache <- reactiveValues()
  
  # Store current city code in a reactive value for other functions to use
  current_city_code <- reactiveVal()
  
  # Update UI choices to include "All parties" as default
  updateSelectInput(session, "partyPrediction", 
                    choices = c("Tous les partis", "LPC", "CPC", "BQ", "NDP", "GP", "Battlefields"),
                    selected = "Tous les partis")
  
  # Calculate base data processing avec les données pondérées
  rv$processed_data <- reactive({
    # Utiliser les proportions déjà calculées dans les données pondérées
    df_ridings <- rv$donnees_sondage %>%
      select(riding, people_predict, proportion, pourcentage) %>%
      rename(party = people_predict)
    
    # Identifier le parti en tête et le second parti pour chaque circonscription
    top_parties <- df_ridings %>%
      group_by(riding) %>%
      arrange(desc(proportion)) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= 2) %>%
      ungroup()
    
    # Extraire les premiers et seconds partis
    first_parties <- top_parties %>%
      filter(rank == 1) %>%
      select(riding, party, proportion, pourcentage) %>%
      rename(
        first_party_percentage = pourcentage
      )
    
    second_parties <- top_parties %>%
      filter(rank == 2) %>%
      select(riding, party, proportion, pourcentage) %>%
      rename(
        second_party = party,
        second_party_percentage = pourcentage
      )
    
    # Joindre les informations et calculer l'écart
    df_ridings <- first_parties %>%
      left_join(second_parties, by = "riding") %>%
      mutate(
        closeness = first_party_percentage - second_party_percentage
      ) %>%
      select(
        riding, 
        party,
        first_party_percentage,
        second_party,
        second_party_percentage,
        closeness
      )
    
    return(df_ridings)
  })
  
  # Create a more efficient reactive for filtered map data
  filtered_map_data <- reactive({
    # Get processed riding data
    df_ridings <- rv$processed_data()
    
    # Ensure id_riding is a character string
    map_data <- map_data # access from global environment
    map_data$id_riding <- as.character(map_data$id_riding)
    
    # Join with riding data to add party information
    map_filtered <- map_data %>%
      left_join(df_ridings, by = c("id_riding" = "riding"))
    
    # Add alpha categories based on closeness
    map_filtered <- map_filtered %>%
      mutate(
        # INVERSER LE GRADIENT: Maintenant foncé = non compétitif (avance solide), pale = compétitif (serré)
        alpha_category = case_when(
          closeness <= 5 ~ 0.4,                   # Très compétitif (0-5%) - PALE
          closeness > 5 & closeness <= 15 ~ 0.55, # Compétitif (5-15%) - ASSEZ PALE
          closeness > 15 & closeness <= 30 ~ 0.7, # Moyennement compétitif (15-30%) - MOYEN
          closeness > 30 & closeness <= 50 ~ 0.85, # Peu compétitif (30-50%) - FONCÉ
          closeness > 50 ~ 1.0,                   # Non compétitif (50%+) - TRÈS FONCÉ
          TRUE ~ 0.6                              # Default
        ),
        # Calculate battlefield intensity for the gradient (0-100 scale)
        battlefield_intensity = pmin(100, pmax(0, 100 - (closeness * 4))),
        # Utiliser directement name_riding_fr pour le hover
        tooltip_text = paste0(
          "<strong>", name_riding_fr, "</strong><br>",
          "Parti en tête: <span style='color:", party_colors[party], ";'>", 
          party, " (", round(first_party_percentage, 1), "%)</span><br>",
          "Second parti: <span style='color:", party_colors[second_party], ";'>", 
          second_party, " (", round(second_party_percentage, 1), "%)</span><br>",
          "Écart: ", round(closeness, 1), "%"
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
          labels = function(x) paste0(party_names[x], " (", x, ")"),
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
      # For Battlefields, show all ridings but sort by competitiveness (lowest closeness first)
      df_ridings <- df_ridings %>% arrange(closeness)
    } else if (input$partyPrediction != "All parties" && input$partyPrediction %in% partis_politiques) {
      df_ridings <- df_ridings %>% 
        filter(party == input$partyPrediction)
    }
    
    # Format data for display
    df_ridings %>% 
      mutate(
        # Joindre avec map_data pour obtenir name_riding_fr
        riding_name = map_data$name_riding_fr[match(riding, map_data$id_riding)],
        # Utiliser l'ID si nom non trouvé
        riding_name = ifelse(is.na(riding_name), as.character(riding), riding_name),
        # Format percentages with % symbol
        first_party_pct = paste0(round(first_party_percentage, 1), "%"),
        second_party_pct = paste0(round(second_party_percentage, 1), "%"),
        margin = paste0(round(closeness, 1), "%")
      ) %>%
      # Rename columns for display
      select(
        # Change this line to use riding_name instead of riding
        "Circonscription" = riding_name,
        "Parti en tête" = party,
        "Pourcentage" = first_party_pct,
        "Second parti" = second_party,
        "Pourcentage (2e)" = second_party_pct,
        "Marge" = margin
      )
  })
  
  # Render data table
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
        search = "Rechercher:",
        paginate = list(previous = "Précédent", `next` = "Suivant")
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
  
  # Clear cache when major inputs change
  observeEvent(list(input$partyPrediction), {
    for (key in names(city_data_cache)) {
      city_data_cache[[key]] <- NULL
    }
  }, ignoreInit = TRUE)  # Ignore initial values to avoid clearing cache at startup
}

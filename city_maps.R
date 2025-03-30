library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(ggiraph)
library(tidyr)

# Load necessary data
map_data <- readRDS("data/map_data.rds")
map_data$id_riding <- as.character(map_data$id_riding)
donnees_sondage <- readRDS("data/donnees_sondage_ponderees.rds")

# Define party colors for consistent visualization
party_colors <- c(
  "LPC" = "#D91920",  # Liberal - Red
  "CPC" = "#0E4C92",  # Conservative - Blue
  "NDP" = "#FF8000",  # NDP - Orange
  "BQ" = "#00B2FF",   # Bloc Québécois - Light blue
  "GP" = "#39D353"    # Green Party - Green
)

# Define party names for labels
party_names <- c(
  "LPC" = "Liberal", 
  "CPC" = "Conservative", 
  "NDP" = "New Democratic",
  "BQ" = "Bloc Québécois",
  "GP" = "Green"
)

# Define Canadian political parties
partis_politiques <- c("LPC", "CPC", "BQ", "NDP", "GP")

# City mapping list
city_mapping <- list(
  "quebec_city" = list(
    "ridings" = c(
      "24016", # Charlesbourg—Haute-Saint-Charles
      "24042", # Louis-Hébert
      "24059", # Québec Centre
      "24043", # Louis-Saint-Laurent—Akiawenhrahk
      "24008", # Beauport—Limoilou
      "24051", # Montmorency—Charlevoix
      "24058", # Portneuf—Jacques-Cartier
      "24010", # Bellechasse—Les Etchemins—Lévis
      "24039"
    ),
    "coordinates" = c("xmin" = -71.6, "xmax" = -71.1, "ymin" = 46.7, "ymax" = 47)
  ),
  "montreal" = list(
    "ridings" = c(
      "24003", "24004", "24013", "24015", "24017", "24021", "24025", 
      "24026", "24030", "24033", "24034", "24036", "24037", "24040", 
      "24041", "24044", "24046", "24047", "24048", "24052", "24053", 
      "24054", "24055", "24056", "24063", "24064", "24065", "24068", 
      "24069", "24074", "24076", "24077", "24073", "24060", "24031", "24078"
    ),
    "coordinates" = c("xmin" = -74.05, "xmax" = -73.45, "ymin" = 45.40, "ymax" = 45.70)
  ),
  "toronto" = list(
    "ridings" = c(
      "35105", "35108", "35109", "35110", "35111", "35112", "35113", 
      "35117", "35120", "35122", "35022", "35023", "35024", "35026", 
      "35029", "35030", "35031", "35041", "35047", "35057", "35058", 
      "35059", "35062", "35063", "35064", "35065", "35066", "35067", 
      "35076", "35077", "35090", "35093", "35094", "35095", "35096", 
      "35097", "35098", "35101", "35001", "35003", "35007", "35008", 
      "35009", "35010", "35011", "35012", "35013", "35014", "35017", 
      "35018", "35069", "35070", "35079", "35088", "35116", "35121"
    ),
    "coordinates" = c("xmin" = -79.67, "xmax" = -79.1, "ymin" = 43.55, "ymax" = 43.9)
  ),
  "ottawa_gatineau" = list(
    "ridings" = c(
      "35020", "35043", "35068", "35078", "35080", "35081", "35082", "35083", "35089",
      "24024", "24027", "24057", "24005"
    ),
    "coordinates" = c("xmin" = -76.0, "xmax" = -75.5, "ymin" = 45.2, "ymax" = 45.5)
  ),
  "vancouver" = list(
    "ridings" = c(
      "59001", "59002", "59003", "59004", "59006", "59007", "59009", 
      "59012", "59013", "59014", "59015", "59019", "59020", "59021", 
      "59022", "59025", "59026", "59028", "59029", "59030", "59033", 
      "59034", "59035", "59036", "59037", "59038", "59039", "59040", 
      "59041", "59043"
    ),
    "coordinates" = c("xmin" = -123.5, "xmax" = -122.1, "ymin" = 48.8, "ymax" = 49.6)
  ),
  "winnipeg" = list(
    "ridings" = c(
      "46001", "46002", "46003", "46004", "46005", "46006", "46007", 
      "46008", "46009", "46010", "46011", "46012", "46013", "46014"
    ),
    "coordinates" = c("xmin" = -97.5, "xmax" = -96.5, "ymin" = 49.6, "ymax" = 50.2)
  ),
  "kitchener_waterloo" = list(
    "ridings" = c(
      "35048", "35049", "35050", "35114", "35019", "35115", "35033", 
      "35084", "35086", "35015", "35061", "35018", "35032", "35039"
    ),
    "coordinates" = c("xmin" = -80.8, "xmax" = -80.2, "ymin" = 43.3, "ymax" = 43.7)
  ),
  "london" = list(
    "ridings" = c(
      "35053", "35054", "35055", "35027", "35060"
    ),
    "coordinates" = c("xmin" = -81.46, "xmax" = -81.07, "ymin" = 42.8, "ymax" = 43.1)
  )
)

# Process data function
process_data <- function() {
  # Utiliser les proportions déjà calculées dans les données pondérées
  df_ridings <- donnees_sondage %>%
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
}

# Filter map data function
filter_map_data <- function(processed_data, party_filter = "All parties") {
  # Ensure id_riding is a character string
  map_data$id_riding <- as.character(map_data$id_riding)
  
  # Join with riding data to add party information
  map_filtered <- map_data %>%
    left_join(processed_data, by = c("id_riding" = "riding"))
  
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
  if (party_filter == "All parties") {
    return(map_filtered)
  } else if (party_filter == "Battlefields") {
    # Now we return ALL ridings but with the battlefield intensity calculated
    return(map_filtered)
  } else if (party_filter %in% partis_politiques) {
    return(map_filtered %>% filter(party == party_filter))
  } else {
    return(map_filtered)
  }
}

# Function to crop map for a specific city
crop_map_for_app <- function(spatial_df, city_code, city_mapping) {
  # Verify arguments are valid
  if (!inherits(spatial_df, "sf")) {
    warning("spatial_df must be an sf object")
    return(spatial_df[0,])  # Return empty dataframe with same structure
  }
  
  if (!(city_code %in% names(city_mapping))) {
    warning(paste("City", city_code, "not found in city_mapping"))
    return(spatial_df[0,])  # Return empty dataframe with same structure
  }
  
  # Ensure id_riding is a character string
  spatial_df$id_riding <- as.character(spatial_df$id_riding)
  
  # Get ridings for the city
  city_ridings <- city_mapping[[city_code]]$ridings
  
  # Filter the spatial dataframe by city ridings
  spatial_df_filtered <- tryCatch({
    spatial_df %>% filter(id_riding %in% city_ridings)
  }, error = function(e) {
    warning("Error filtering ridings: ", e$message)
    return(spatial_df[0,])
  })
  
  # Check if there are matches
  if (nrow(spatial_df_filtered) == 0) {
    warning(paste("No matching ridings found for city:", city_code))
    return(spatial_df[0,])  # Return empty dataframe with same structure
  }
  
  # Transform and crop
  tryCatch({
    # Transform to WGS84 more robustly
    # Check if data has a valid CRS first
    if (is.na(sf::st_crs(spatial_df_filtered))) {
      warning("No CRS in filtered data, setting to WGS84")
      sf::st_crs(spatial_df_filtered) <- 4326  # Assign WGS84 if no CRS
    } else {
      # Transform to WGS84
      spatial_df_filtered <- sf::st_transform(spatial_df_filtered, 4326)
    }
    
    # Define crop area with city coordinates
    coords <- city_mapping[[city_code]]$coordinates
    
    # Verify all coordinates exist
    required_coords <- c("xmin", "xmax", "ymin", "ymax")
    if (!all(required_coords %in% names(coords))) {
      warning("Missing coordinates for city: ", city_code)
      return(spatial_df_filtered)
    }
    
    # Create bounding box
    crop_bbox <- sf::st_bbox(
      c(xmin = coords["xmin"], 
        xmax = coords["xmax"], 
        ymin = coords["ymin"],
        ymax = coords["ymax"]
      ),
      crs = sf::st_crs(4326)
    )
    
    # Apply spatial crop with careful error handling
    spatial_df_cropped <- tryCatch({
      suppressWarnings(sf::st_crop(spatial_df_filtered, crop_bbox))
    }, error = function(e) {
      warning("Error in st_crop: ", e$message)
      return(spatial_df_filtered)
    })
    
    # If there's no data after cropping, return filtered data without cropping
    if (nrow(spatial_df_cropped) == 0) {
      return(spatial_df_filtered)
    }
    
    return(spatial_df_cropped)
  }, error = function(e) {
    warning("Error in processing city data: ", e$message)
    return(spatial_df_filtered)
  })
}

# Function to generate a city map
generate_city_map <- function(city_code, city_name, party_filter = "All parties") {
  # Process data
  processed_data <- process_data()
  
  # Filter map data
  filtered_data <- filter_map_data(processed_data, party_filter)
  
  # Get city data
  city_data <- crop_map_for_app(filtered_data, city_code, city_mapping)
  
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
  if (party_filter == "Battlefields") {
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
      plot.title = element_text(hjust = 0.5, size = 12),
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
      title = city_name
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

# Function to generate all city maps
generate_all_city_maps <- function(party_filter = "All parties") {
  city_list <- list(
    "montreal" = "Montreal",
    "toronto" = "Toronto",
    "vancouver" = "Vancouver",
    "quebec_city" = "Quebec City",
    "ottawa_gatineau" = "Ottawa-Gatineau",
    "winnipeg" = "Winnipeg",
    "kitchener_waterloo" = "Kitchener-Waterloo",
    "london" = "London"
  )
  
  maps <- list()
  for (city_code in names(city_list)) {
    maps[[city_code]] <- generate_city_map(city_code, city_list[[city_code]], party_filter)
  }
  
  return(maps)
}

# Run standalone to generate a specific city map
if (interactive()) {
  # Choose a city to display
  city_code <- "montreal"
  city_name <- "Montreal"
  
  # Generate the city map
  city_map <- generate_city_map(city_code, city_name)
  
  # Print in the viewer
  print(city_map)
}
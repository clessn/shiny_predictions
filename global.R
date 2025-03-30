# global.R - Central configuration and data loading
library(shiny)
library(ggplot2)
library(DT)
library(sf)
library(shinydashboard)
library(shinyjs)
library(waiter)
library(shinycssloaders)
library(fresh)
library(bslib)
library(fontawesome)
library(dplyr)
library(tidyr)
library(ggiraph) # Pour les fonctionnalités hover

# Load map data
map_data <- readRDS("data/map_data_simplified.rds")
map_data$id_riding <- as.character(map_data$id_riding)

# City mapping list - centralized definition to avoid redundancy
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

# Define party colors for consistent visualization
party_colors <- c(
  "LPC" = "#D91920",  # Liberal - Red
  "CPC" = "#0E4C92",  # Conservative - Blue
  "NDP" = "#FF8000",  # NDP - Orange
  "BQ" = "#00B2FF",   # Bloc Québécois - Light blue
  "GP" = "#39D353"   # Green Party - Green
)

# Define party names for labels (English and French versions)
party_names <- list(
  "en" = c(
    "LPC" = "Liberal", 
    "CPC" = "Conservative", 
    "NDP" = "New Democratic",
    "BQ" = "Bloc Québécois",
    "GP" = "Green"
  ),
  "fr" = c(
    "LPC" = "Libéral", 
    "CPC" = "Conservateur", 
    "NDP" = "Nouveau Démocratique",
    "BQ" = "Bloc Québécois",
    "GP" = "Vert"
  )
)

# Define Canadian political parties
partis_politiques <- c("LPC", "CPC", "BQ", "NDP", "GP")

# Dictionary for translations
translations <- list(
  "fr" = list(
    "loading" = "Chargement des données...",
    "app_title" = "Agrégateur d'agrégateurs",
    "model_type" = "Type de modèle:",
    "party_prediction" = "Prédiction par parti:",
    "all_parties" = "Tous les partis",
    "lead_strength" = "Niveau de certitude",
    "seat_count" = "Décompte des sièges",
    "riding_competitive" = "CIRCONSCRIPTION\nCOMPÉTITIVE",
    "consolidated_lead" = "AVANCE\nCONSOLIDÉE",
    "competitiveness" = "Compétitivité des circonscriptions",
    "less_competitive" = "MOINS\nCOMPÉTITIF",
    "more_competitive" = "PLUS\nCOMPÉTITIF",
    "data_link" = "Lien vers les données",
    "data_updated" = "Données mises à jour: Mars 2025",
    "respondents" = "Nombre de répondants : ",
    "hover_info" = "Survolez sur les cartes pour plus de détails",
    "riding" = "Circonscription",
    "leading_party" = "Parti en tête",
    "percentage" = "Pourcentage",
    "second_party" = "Second parti",
    "second_percentage" = "Pourcentage (2e)",
    "margin" = "Marge",
    "search" = "Rechercher:",
    "previous" = "Précédent",
    "next" = "Suivant"
  ),
  "en" = list(
    "loading" = "Loading data...",
    "app_title" = "Aggregator of Aggregators",
    "model_type" = "Model type:",
    "party_prediction" = "Prediction by party:",
    "all_parties" = "All parties",
    "lead_strength" = "Certainty level",
    "seat_count" = "Seat count",
    "riding_competitive" = "COMPETITIVE\nRIDING",
    "consolidated_lead" = "CONSOLIDATED\nLEAD",
    "competitiveness" = "Riding competitiveness",
    "less_competitive" = "LESS\nCOMPETITIVE",
    "more_competitive" = "MORE\nCOMPETITIVE",
    "data_link" = "Data links",
    "data_updated" = "Data updated: March 2025",
    "respondents" = "Number of respondents: ",
    "hover_info" = "Hover over maps for more details",
    "riding" = "Riding",
    "leading_party" = "Leading party",
    "percentage" = "Percentage",
    "second_party" = "Second party",
    "second_percentage" = "Second percentage",
    "margin" = "Margin",
    "search" = "Search:",
    "previous" = "Previous",
    "next" = "Next"
  )
)

# List of bilingual city names (both English and French)
city_names <- list(
  "fr" = c(
    "montreal" = "Montréal",
    "toronto" = "Toronto",
    "vancouver" = "Vancouver",
    "quebec_city" = "Ville de Québec",
    "ottawa_gatineau" = "Ottawa-Gatineau",
    "winnipeg" = "Winnipeg",
    "kitchener_waterloo" = "Kitchener-Waterloo",
    "london" = "London"
  ),
  "en" = c(
    "montreal" = "Montreal",
    "toronto" = "Toronto",
    "vancouver" = "Vancouver",
    "quebec_city" = "Quebec City",
    "ottawa_gatineau" = "Ottawa-Gatineau",
    "winnipeg" = "Winnipeg",
    "kitchener_waterloo" = "Kitchener-Waterloo",
    "london" = "London"
  )
)

# Function to crop map for a specific city
crop_map_for_app <- function(spatial_df, city, city_mapping) {
  # Verify arguments are valid
  if (!inherits(spatial_df, "sf")) {
    warning("spatial_df must be an sf object")
    return(spatial_df[0,])  # Return empty dataframe with same structure
  }
  
  if (!(city %in% names(city_mapping))) {
    warning(paste("City", city, "not found in city_mapping"))
    return(spatial_df[0,])  # Return empty dataframe with same structure
  }
  
  # Ensure id_riding is a character string
  spatial_df$id_riding <- as.character(spatial_df$id_riding)
  
  # Get ridings for the city
  city_ridings <- city_mapping[[city]]$ridings
  
  # Filter the spatial dataframe by city ridings
  spatial_df_filtered <- tryCatch({
    spatial_df %>% filter(id_riding %in% city_ridings)
  }, error = function(e) {
    warning("Error filtering ridings: ", e$message)
    return(spatial_df[0,])
  })
  
  # Check if there are matches
  if (nrow(spatial_df_filtered) == 0) {
    warning(paste("No matching ridings found for city:", city))
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
    coords <- city_mapping[[city]]$coordinates
    
    # Verify all coordinates exist
    required_coords <- c("xmin", "xmax", "ymin", "ymax")
    if (!all(required_coords %in% names(coords))) {
      warning("Missing coordinates for city: ", city)
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

# Custom theme for Bootstrap
my_theme <- bs_theme(
  version = 5,
  bootswatch = "default",
  primary = "#18BC9C",
  secondary = "#95A5A6",
  success = "#18BC9C",
  info = "#3498DB",
  warning = "#F39C12",
  danger = "#E74C3C",
  font_scale = 1,
  spacer = "1rem"
)

# Custom font for the app - define here so it's accessible globally
font_file <- "www/PixelOperatorSC.ttf"

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
map_data <- readRDS("data/map_statcan.rds")
map_data$id_riding <- as.character(map_data$id_riding)

# City mapping list - centralized definition to avoid redundancy
city_mapping <- list(
  "quebec_city" = list(
    "ridings" = c(
      "24016", # Charlesbourg—Haute-Saint-Charles
      "24043", # Louis-Hébert
      "24059", # Québec-Centre
      "24044", # Louis-Saint-Laurent—Akiawenhrahk
      "24008", # Beauport—Limoilou
      "24051", # Montmorency—Charlevoix
      "24058", # Portneuf—Jacques-Cartier
      "24010", # Bellechasse—Les Etchemins—Lévis
      "24040"  # Lévis—Lotbinière
    ),
    "coordinates" = c("xmin" = -71.6, "xmax" = -71.1, "ymin" = 46.7, "ymax" = 47)
  ),
  "montreal" = list(
    "ridings" = c(
      "24003", # Ahuntsic-Cartierville
      "24004", # Alfred-Pellan
      "24013", # Bourassa
      "24015", # Brossard—Saint-Lambert
      "24017", # Châteauguay—Les Jardins-de-Napierville
      "24022", # Dorval—Lachine—LaSalle
      "24026", # Hochelaga—Rosemont-Est
      "24027", # Honoré-Mercier
      "24031", # La Pointe-de-l'Île
      "24034", # Lac-Saint-Louis
      "24035", # LaSalle—Émard—Verdun
      "24037", # Laurier—Sainte-Marie
      "24038", # Laval—Les Îles
      "24041", # Longueuil—Charles-LeMoyne
      "24042", # Longueuil—Saint-Hubert
      "24045", # Marc-Aurèle-Fortin
      "24047", # Mirabel
      "24048", # Mont-Royal
      "24052", # Notre-Dame-de-Grâce—Westmount
      "24053", # Outremont
      "24054", # Papineau
      "24055", # Pierre-Boucher—Les Patriotes—Verchères
      "24056", # Pierrefonds—Dollard
      "24060", # Repentigny
      "24063", # Rivière-des-Mille-Îles
      "24065", # Rosemont—La Petite-Patrie
      "24068", # Saint-Laurent
      "24069", # Saint-Léonard—Saint-Michel
      "24073", # Terrebonne
      "24074", # Thérèse-De Blainville
      "24076", # Vaudreuil
      "24077", # Ville-Marie—Le Sud-Ouest—Île-des-Soeurs
      "24078", # Vimy
      "24032", # La Prairie—Atateken
      "24049"  # Mont-Saint-Bruno—L'Acadie
    ),
    "coordinates" = c("xmin" = -74.05, "xmax" = -73.45, "ymin" = 45.40, "ymax" = 45.70)
  ),
  "toronto" = list(
    "ridings" = c(
      "35105", # Taiaiako'n—Parkdale—High Park
      "35109", # Toronto-Centre
      "35110", # Toronto—Danforth
      "35111", # Toronto—St. Paul's
      "35112", # University—Rosedale
      "35117", # Willowdale
      "35120", # York-Centre
      "35122", # York-Sud—Weston—Etobicoke
      "35022", # Davenport
      "35023", # Don Valley-Nord
      "35024", # Don Valley-Ouest
      "35026", # Eglinton—Lawrence
      "35029", # Etobicoke-Centre
      "35030", # Etobicoke—Lakeshore
      "35031", # Etobicoke-Nord
      "35041", # Humber River—Black Creek
      "35047", # King—Vaughan
      "35056", # Markham—Stouffville
      "35057", # Markham—Thornhill
      "35058", # Markham—Unionville
      "35061", # Mississauga-Centre
      "35062", # Mississauga-Est—Cooksville
      "35063", # Mississauga—Erin Mills
      "35064", # Mississauga—Lakeshore
      "35065", # Mississauga—Malton
      "35066", # Mississauga—Streetsville
      "35075", # Oakville-Est
      "35076", # Oakville-Ouest
      "35089", # Richmond Hill-Sud
      "35092", # Scarborough—Agincourt
      "35093", # Scarborough-Centre—Don Valley-Est
      "35094", # Scarborough—Guildwood—Rouge Park
      "35095", # Scarborough-Nord
      "35096", # Scarborough-Sud-Ouest
      "35097", # Scarborough—Woburn
      "35100", # Spadina—Harbourfront
      "35106", # Thornhill
      "35113", # Vaughan—Woodbridge
      "35001", # Ajax
      "35003", # Aurora—Oak Ridges—Richmond Hill
      "35007", # Beaches—East York
      "35008", # Bowmanville—Oshawa-Nord
      "35009", # Brampton-Centre
      "35010", # Brampton—Chinguacousy Park
      "35011", # Brampton-Est
      "35012", # Brampton-Nord—Caledon
      "35013", # Brampton-Sud
      "35014", # Brampton-Ouest
      "35017", # Burlington
      "35018", # Burlington-Nord—Milton-Ouest
      "35068", # Newmarket—Aurora
      "35069", # New Tecumseth—Gwillimbury
      "35087", # Pickering—Brooklin
      "35116", # Whitby
      "35121", # York—Durham
      "35025"  # Dufferin—Caledon
    ),
    "coordinates" = c("xmin" = -79.67, "xmax" = -79.1, "ymin" = 43.55, "ymax" = 43.9)
  ),
  "ottawa_gatineau" = list(
    "ridings" = c(
      "35020", # Carleton
      "35043", # Kanata
      "35067", # Nepean
      "35077", # Orléans
      "35079", # Ottawa-Centre
      "35080", # Ottawa-Sud
      "35081", # Ottawa—Vanier—Gloucester
      "35082", # Ottawa-Ouest—Nepean
      "35088", # Prescott—Russell—Cumberland
      "24025", # Gatineau
      "24028", # Hull—Aylmer
      "24057", # Pontiac—Kitigan Zibi
      "24005"  # Argenteuil—La Petite-Nation
    ),
    "coordinates" = c("xmin" = -76.0, "xmax" = -75.5, "ymin" = 45.2, "ymax" = 45.5)
  ),
  "vancouver" = list(
    "ridings" = c(
      "59001", # Abbotsford—Langley-Sud
      "59002", # Burnaby Central
      "59003", # Burnaby-Nord—Seymour
      "59006", # Cloverdale—Langley City
      "59008", # Coquitlam—Port Coquitlam
      "59011", # Delta
      "59013", # Fleetwood—Port Kells
      "59017", # Langley Township—Fraser Heights
      "59018", # Mission—Matsqui—Abbotsford
      "59019", # Nanaimo—Ladysmith
      "59020", # New Westminster—Burnaby—Maillardville
      "59022", # North Vancouver—Capilano
      "59024", # Pitt Meadows—Maple Ridge
      "59025", # Port Moody—Coquitlam
      "59027", # Richmond-Centre—Marpole
      "59028", # Richmond-Est—Steveston
      "59032", # Surrey-Sud—White Rock
      "59033", # Surrey-Centre
      "59034", # Surrey Newton
      "59035", # Vancouver-Centre
      "59036", # Vancouver-Est
      "59037", # Vancouver Fraserview—Burnaby-Sud
      "59038", # Vancouver Granville
      "59039", # Vancouver Kingsway
      "59040", # Vancouver Quadra
      "59042", # Victoria
      "59043"  # West Vancouver—Sunshine Coast—Sea to Sky Country
    ),
    "coordinates" = c("xmin" = -123.5, "xmax" = -122.1, "ymin" = 48.8, "ymax" = 49.6)
  ),
  "winnipeg" = list(
    "ridings" = c(
      "46003", # Elmwood—Transcona
      "46004", # Kildonan—St. Paul
      "46008", # Saint-Boniface—Saint-Vital
      "46010", # Winnipeg-Centre
      "46011", # Winnipeg-Nord
      "46012", # Winnipeg-Sud
      "46013", # Winnipeg-Centre-Sud
      "46014", # Winnipeg-Ouest
      "46001", # Brandon—Souris
      "46002", # Churchill—Keewatinook Aski
      "46005", # Portage—Lisgar
      "46006", # Provencher
      "46007", # Mont-Riding
      "46009"  # Selkirk—Interlake—Eastman
    ),
    "coordinates" = c("xmin" = -97.5, "xmax" = -96.5, "ymin" = 49.6, "ymax" = 50.2)
  ),
  "kitchener_waterloo" = list(
    "ridings" = c(
      "35048", # Kitchener-Centre
      "35049", # Kitchener—Conestoga
      "35050", # Kitchener-Sud—Hespeler
      "35114", # Waterloo
      "35019", # Cambridge
      "35115", # Wellington—Halton Hills-Nord
      "35033", # Guelph
      "35083", # Oxford
      "35085", # Perth—Wellington
      "35015", # Brantford—Brant-Sud—Six Nations
      "35060", # Milton-Est—Halton Hills-Sud
      "35018", # Burlington-Nord—Milton-Ouest
      "35032", # Flamborough—Glanbrook—Brant-Nord
      "35039"  # Hamilton-Ouest—Ancaster—Dundas
    ),
    "coordinates" = c("xmin" = -80.8, "xmax" = -80.2, "ymin" = 43.3, "ymax" = 43.7)
  ),
  "london" = list(
    "ridings" = c(
      "35053", # London-Centre
      "35054", # London—Fanshawe
      "35055", # London-Ouest
      "35027", # Elgin—St. Thomas—London-Sud
      "35059"  # Middlesex—London
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
  "GPC" = "#39D353"   # Green Party - Green
)

# Define party names for labels (English and French versions)
party_names <- list(
  "en" = c(
    "LPC" = "Liberal", 
    "CPC" = "Conservative", 
    "NDP" = "New Democratic",
    "BQ" = "Bloc Québécois",
    "GPC" = "Green"
  ),
  "fr" = c(
    "LPC" = "Libéral", 
    "CPC" = "Conservateur", 
    "NDP" = "Nouveau Démocratique",
    "BQ" = "Bloc Québécois",
    "GPC" = "Parti Vert"
  )
)

# Define Canadian political parties
partis_politiques <- c("LPC", "CPC", "BQ", "NDP", "GPC")

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
    "next" = "Suivant",
    "battlefields" = "Champs de bataille"
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
    "next" = "Next",
    "battlefields" = "Battlefields"
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
  primary = "#6c757d",      # Changed to a smooth slate gray
  secondary = "#95A5A6",
  success = "#6c757d",      # Also updated success to match primary
  info = "#3498DB",
  warning = "#F39C12",
  danger = "#E74C3C",
  font_scale = 1,
  spacer = "1rem"
)

# Custom font for the app - define here so it's accessible globally
font_file <- "www/PixelOperatorSC.ttf"

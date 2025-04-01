library(dplyr)
library(sf)

# Test script to debug the map_df.rds issue

# Load the map data
map_data <- readRDS("data/map_df.rds")
map_data$id_riding <- as.character(map_data$id_riding)

# Print the number of ridings in the map
cat("Number of rows in map_data:", nrow(map_data), "\n")
cat("Number of unique riding IDs:", length(unique(map_data$id_riding)), "\n")

# Check Quebec City ridings
quebec_city_ridings <- c(
  "24016", # Charlesbourg—Haute-Saint-Charles
  "24042", # Louis-Hébert (name changed in new map)
  "24059", # Québec Centre
  "24043", # Louis-Saint-Laurent—Akiawenhrahk (name changed in new map)
  "24008", # Beauport—Limoilou
  "24051", # Montmorency—Charlevoix
  "24058", # Portneuf—Jacques-Cartier
  "24010", # Bellechasse—Les Etchemins—Lévis
  "24039"  # Lévis—Lotbinière (name changed in new map)
)

cat("\nQuebec City ridings check:\n")
for (rid in quebec_city_ridings) {
  riding <- map_data %>% filter(id_riding == rid)
  if(nrow(riding) > 0) {
    cat(paste(rid, "found as", riding$name_riding_en), "\n")
  } else {
    cat(paste(rid, "NOT FOUND"), "\n")
  }
}

# Test crop_map_for_app function
city_mapping <- list(
  "quebec_city" = list(
    "ridings" = quebec_city_ridings,
    "coordinates" = c("xmin" = -71.6, "xmax" = -71.1, "ymin" = 46.7, "ymax" = 47)
  )
)

# Define a simplified version of the crop function for testing
crop_map_for_app <- function(spatial_df, city, city_mapping) {
  # Ensure id_riding is a character string
  spatial_df$id_riding <- as.character(spatial_df$id_riding)
  
  # Get ridings for the city
  city_ridings <- city_mapping[[city]]$ridings
  
  # Filter the spatial dataframe by city ridings
  spatial_df_filtered <- spatial_df %>% filter(id_riding %in% city_ridings)
  
  # Return the filtered data
  return(spatial_df_filtered)
}

# Test the crop function
quebec_map <- crop_map_for_app(map_data, "quebec_city", city_mapping)
cat("\nRidings found for Quebec City:", nrow(quebec_map), "\n")
if(nrow(quebec_map) > 0) {
  print(quebec_map[, c("id_riding", "name_riding_en")])
}

# Print the actual riding names in the map data
cat("\nActual riding names in map_df.rds for the Quebec City IDs:\n")
for (rid in quebec_city_ridings) {
  riding_name <- map_data$name_riding_en[map_data$id_riding == rid]
  if(length(riding_name) > 0) {
    cat(paste(rid, ":", riding_name), "\n")
  } else {
    cat(paste(rid, ": NOT FOUND"), "\n")
  }
}

library(dplyr)
library(sf)
test <- readRDS("data/map_statcan.rds") %>% st_drop_geometry() %>% select(id_riding, name_riding_en)

# Load the mapping data
map_data <- readRDS("data/map_statcan.rds")
map_ids <- as.character(map_data$id_riding)
cat("Map contains", length(map_ids), "unique riding IDs\n")

# Load the city mapping list from global.R
source("global.R")

# Define cities to check
cities <- c("quebec_city", "montreal", "toronto", "ottawa_gatineau", "vancouver", "winnipeg", "kitchener_waterloo", "london")

for (city in cities) {
  cat("\nChecking", city, ":\n")
  city_ridings <- city_mapping[[city]]$ridings
  cat("  Total ridings defined:", length(city_ridings), "\n")
  
  # Check missing ridings
  missing <- city_ridings[\!city_ridings %in% map_ids]
  if (length(missing) > 0) {
    cat("  MISSING RIDINGS:", length(missing), "\n")
    cat("    IDs:", paste(missing, collapse=", "), "\n")
  } else {
    cat("  All ridings exist in the map data\n")
  }
}

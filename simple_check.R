# Load the mapping data
map_data <- readRDS("data/map_statcan.rds")
map_ids <- as.character(map_data$id_riding)
cat("Map contains", length(map_ids), "unique riding IDs\n")

# Display the first 10 riding IDs for reference
cat("First 10 riding IDs in map_statcan.rds:\n")
cat(paste(head(map_ids, 10), collapse=", "), "\n\n")

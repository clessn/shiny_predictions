##########################################
# PART 1: SIMPLIFY AND SAVE MAP DATA
##########################################

# Load essential libraries for simplification
library(sf)

# Load map data first
map_data <- readRDS("data/map_data.rds")
map_data$id_riding <- as.character(map_data$id_riding)

# Transform to WGS84 for better web rendering
map_data <- st_transform(map_data, 4326)

# Safe geometry simplification function
simplify_geometry <- function(g, tolerance = 0.01) {
  tryCatch({
    st_simplify(g, dTolerance = tolerance)
  }, error = function(e) {
    # If error, try with smaller tolerance
    tryCatch({
      st_simplify(g, dTolerance = tolerance/10)
    }, error = function(e) {
      # If still error, return original
      g
    })
  })
}

# Apply simplification to each geometry individually
geom_list <- st_geometry(map_data)
simplified_geoms <- lapply(geom_list, function(g) simplify_geometry(g, tolerance = 0.01))

# Create simplified map data
map_data_simplified <- map_data
map_data_simplified$geometry <- st_sfc(simplified_geoms, crs = st_crs(map_data))

# Save the simplified map data
saveRDS(map_data_simplified, "data/map_data_simplified.rds")

##########################################
# PART 2: USE SIMPLIFIED MAP FOR ANALYSIS
##########################################

# Essential libraries
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)

# Load pre-simplified map data
map_data_simplified <- readRDS("data/map_data_simplified.rds")

# Load the electoral data
donnees_sondage <- readRDS("data/donnees_sondage_ponderees.rds")

# Define party colors
party_colors <- c(
  "LPC" = "#D91920",  # Liberal - Red
  "CPC" = "#0E4C92",  # Conservative - Blue
  "NDP" = "#FF8000",  # NDP - Orange
  "BQ" = "#00B2FF",   # Bloc Québécois - Light blue
  "GP" = "#39D353"    # Green Party - Green
)

# Process electoral data
df_ridings <- donnees_sondage %>%
  select(riding, people_predict, proportion, pourcentage) %>%
  rename(party = people_predict)

# Get top two parties for each riding
top_parties <- df_ridings %>%
  group_by(riding) %>%
  arrange(desc(proportion)) %>%
  slice_head(n = 2) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Extract first parties
first_parties <- top_parties %>%
  filter(rank == 1) %>%
  select(riding, party, pourcentage) %>%
  rename(first_party_percentage = pourcentage)

# Extract second parties
second_parties <- top_parties %>%
  filter(rank == 2) %>%
  select(riding, party, pourcentage) %>%
  rename(
    second_party = party,
    second_party_percentage = pourcentage
  )

# Join and calculate closeness
riding_data <- first_parties %>%
  left_join(second_parties, by = "riding") %>%
  mutate(closeness = first_party_percentage - second_party_percentage)

# Join with SIMPLIFIED map data and prepare for plotting
plot_data <- map_data_simplified %>%
  left_join(riding_data, by = c("id_riding" = "riding")) %>%
  mutate(
    alpha_category = case_when(
      closeness <= 5 ~ 0.4,
      closeness > 5 & closeness <= 15 ~ 0.55,
      closeness > 15 & closeness <= 30 ~ 0.7,
      closeness > 30 & closeness <= 50 ~ 0.85,
      closeness > 50 ~ 1.0,
      TRUE ~ 0.6
    ),
    tooltip_text = paste0(
      "<b>", name_riding_fr, "</b><br>",
      "Parti en tête: ", party, " (", round(first_party_percentage, 1), "%)<br>",
      "Second parti: ", second_party, " (", round(second_party_percentage, 1), "%)<br>",
      "Écart: ", round(closeness, 1), "%"
    )
  )

##########################################
# GGIRAPH INTERACTIVE MAP
##########################################

# Create interactive ggplot with ggiraph
ggiraph_map <- ggplot() +
  geom_sf_interactive(
    data = plot_data,
    aes(fill = party, 
        alpha = alpha_category,
        tooltip = tooltip_text, 
        data_id = id_riding),
    color = "white", 
    size = 0.1
  ) +
  scale_fill_manual(values = party_colors, name = "Parti politique") +
  scale_alpha_identity() +
  theme_minimal() +
  labs(title = "Carte des prévisions électorales au Canada") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Convert to interactive graphic
girafe_map <- girafe(
  ggobj = ggiraph_map,
  width_svg = 10, 
  height_svg = 8,
  options = list(
    opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px;"),
    opts_hover(css = "stroke-width: 1.5; stroke: #black;"),
    opts_zoom(max = 5)
  )
)

# Display ggiraph map
print(girafe_map)

library(shiny)
library(DT)
library(dplyr)
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

# Define Canadian political parties
partis_politiques <- c("LPC", "CPC", "BQ", "NDP", "GP")

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

# Function to generate the table data
generate_table_data <- function(party_filter = "All parties") {
  # Get processed data
  df_ridings <- process_data()
  
  # Apply party filter
  if (party_filter == "Battlefields") {
    # For Battlefields, show all ridings but sort by competitiveness (lowest closeness first)
    df_ridings <- df_ridings %>% arrange(closeness)
  } else if (party_filter != "All parties" && party_filter %in% partis_politiques) {
    df_ridings <- df_ridings %>% 
      filter(party == party_filter)
  }
  
  # Format data for display
  display_data <- df_ridings %>% 
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
  
  return(display_data)
}

# Function to format the data table
format_data_table <- function(display_data, party_filter = "All parties") {
  # Only create the datatable when we have data
  if (nrow(display_data) == 0) {
    return(NULL)
  }
  
  # Special color styling for Battlefields mode
  if (party_filter == "Battlefields") {
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
      order = if(party_filter == "Battlefields") list(list(5, 'asc')) else NULL
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
  if (party_filter == "Battlefields") {
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
}

# Function to generate and display the data table
generate_data_table <- function(party_filter = "All parties") {
  # Generate table data
  display_data <- generate_table_data(party_filter)
  
  # Format the data table
  dt <- format_data_table(display_data, party_filter)
  
  return(dt)
}

# Run standalone to generate the table
if (interactive()) {
  # Generate table with default settings
  data_table <- generate_data_table()
  
  # Print in the viewer
  print(data_table)
}
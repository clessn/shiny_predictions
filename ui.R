# ui.R - Avec légende pour Battlefields
library(shiny)
library(waiter)
library(shinyjs)
library(shinydashboard)
library(bslib)
library(fontawesome)
library(ggiraph) # Ajouté pour les fonctionnalités hover

# Create a refined loading screen
loading_screen <- tagList(
  div(
    style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; background-color: #f9f9f9; z-index: 9999; display: flex; align-items: center; justify-content: center; flex-direction: column;",
    div(class = "text-center",
        tags$div(style = "color: #222222; font-size: 28px; margin-bottom: 20px; font-family: 'Libre Baskerville', Georgia, serif;"),
#                 HTML("<i class='fas fa-map-marked-alt'></i> Electoral Map")),
        tags$p(style = "color: #555555; font-size: 16px; font-family: 'Times New Roman', Times, serif;", "Chargement des données..."),
        tags$div(style = "width: 120px; height: 2px; background-color: #f0f0f0; border-radius: 0; margin-top: 20px; position: relative; overflow: hidden;",
                 tags$div(class = "loading-bar", style = "position: absolute; top: 0; left: 0; height: 100%; width: 30%; background-color: #444444; border-radius: 0; animation: loading 1.5s infinite;"))
    )
  )
)

ui <- fluidPage(
  # Use optimized loading screen
  useWaiter(loading_screen),
  useShinyjs(),
  tags$head(
    # Load CSS via CDN for better performance
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    # Import Google Fonts for the sophisticated look - only Libre Baskerville for headings
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Libre+Baskerville:wght@400;700&display=swap"),
    
    # Directly embed CSS in the UI instead of linking to external file
    tags$style(HTML("
      /* Base styles */
      body {
        font-family: 'Times New Roman', Times, serif;
        background-color: #f6f7f9;
        color: #333333;
        margin: 0;
        padding: 0;
        line-height: 1.6;
        font-weight: 400;
      }
      
      /* Standard sidebar panel styling */
      .well {
        background-color: #ffffff;
        border: 1px solid #e6e6e6;
        border-radius: 2px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
        padding: 20px;
      }
      
      /* Standard container styling */
      .container-fluid {
        padding: 20px;
      }
      
      /* Headings */
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Libre Baskerville', Georgia, serif;
        font-weight: 700;
        margin-top: 0;
        color: #222222;
        letter-spacing: -0.02em;
      }
      
      .main-title {
        font-size: 28px;
        border-bottom: 2px solid #e6e6e6;
        padding-bottom: 12px;
        margin-bottom: 24px;
        letter-spacing: -0.03em;
      }
      
      .section-header {
        font-size: 20px;
        margin-bottom: 15px;
        border-bottom: none;
        padding-bottom: 0;
        color: #222222;
      }
      
      .sidebar-title {
        font-size: 22px;
        color: #222222;
        margin-bottom: 25px;
      }
      
      /* Form control styling */
      .form-control {
        border: 1px solid #d9d9d9;
        border-radius: 2px;
        box-shadow: none;
        padding: 10px 12px;
        color: #333333;
        background-color: #ffffff;
        transition: border-color 0.2s ease;
        font-family: 'Times New Roman', Times, serif;
      }
      
      .form-control:focus {
        border-color: #18BC9C;
        box-shadow: 0 0 0 3px rgba(24, 188, 156, 0.15);
        outline: none;
      }
      
      /* Select input styling */
      .selectize-input {
        border: 1px solid #d9d9d9 !important;
        box-shadow: none !important;
        border-radius: 2px !important;
        padding: 10px 12px !important;
        font-family: 'Times New Roman', Times, serif !important;
      }
      
      .selectize-input.focus {
        border-color: #18BC9C !important;
        box-shadow: 0 0 0 3px rgba(24, 188, 156, 0.15) !important;
      }
      
      .selectize-dropdown {
        border: 1px solid #d9d9d9 !important;
        border-radius: 0 0 2px 2px !important;
        box-shadow: 0 3px 8px rgba(0, 0, 0, 0.1) !important;
        font-family: 'Times New Roman', Times, serif !important;
      }
      
      .selectize-dropdown-content .option {
        padding: 10px 12px !important;
      }
      
      .selectize-dropdown-content .option.active {
        background-color: #18BC9C !important;
        color: #ffffff !important;
      }
      
      /* Checkbox styling */
      .checkbox label {
        display: inline-block;
        cursor: pointer;
        font-weight: normal;
        margin-bottom: 0;
        font-family: 'Times New Roman', Times, serif;
      }
      
      .checkbox input[type='checkbox'] {
        margin-right: 8px;
        vertical-align: middle;
      }
      
      /* Map container styling */
      .map-container {
        background-color: #ffffff;
        border-radius: 2px;
        border: 1px solid #e6e6e6;
        overflow: hidden;
        margin-bottom: 24px;
      }
      
      .map-container .shiny-plot-output {
        min-height: 500px;
      }
      
      /* Tooltip styling */
      .tooltip-riding {
        background-color: white;
        border: 1px solid #ccc;
        padding: 10px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        font-family: 'Times New Roman', Times, serif;
        font-size: 14px;
      }
      
      /* City maps container */
      .city-maps-container {
        background-color: #ffffff;
        border-radius: 2px;
        border: 1px solid #e6e6e6;
        padding: 20px;
        margin-bottom: 24px;
      }
      
      /* City map boxes */
      .city-map-box {
        background-color: #ffffff;
        border: 1px solid #e9e9e9;
        border-radius: 2px;
        overflow: hidden;
        position: relative;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      
      .city-map-box:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      
      .map-title-overlay {
        position: absolute;
        bottom: 0;
        left: 0;
        right: 0;
        background-color: rgba(255, 255, 255, 0.9);
        color: #222222;
        padding: 8px;
        font-family: 'Libre Baskerville', Georgia, serif;
        font-size: 14px;
        text-align: center;
        border-top: 1px solid #f0f0f0;
      }
      
      /* Table container styling */
      .table-container {
        background-color: #ffffff;
        border-radius: 2px;
        border: 1px solid #e6e6e6;
        padding: 20px;
        margin-bottom: 24px;
      }
      
      /* DataTable styling */
      .dataTables_wrapper {
        font-size: 14px;
        font-family: 'Times New Roman', Times, serif;
      }
      
      .dataTable {
        width: 100% !important;
        border-collapse: collapse;
        margin-bottom: 0;
      }
      
      .dataTable thead th {
        background-color: #f4f4f4 !important;
        color: #222222 !important;
        border-bottom: 2px solid #e6e6e6 !important;
        font-weight: 600 !important;
        padding: 12px 10px !important;
        text-align: left;
        font-family: 'Libre Baskerville', Georgia, serif;
        font-size: 14px;
      }
      
      .dataTable tbody td {
        padding: 10px !important;
        border-top: 1px solid #f0f0f0 !important;
        vertical-align: middle !important;
      }
      
      .dataTable tbody tr:nth-child(even) {
        background-color: #f9f9f9 !important;
      }
      
      .dataTable tbody tr:hover {
        background-color: #f0f0f0 !important;
      }
      
      /* DataTable controls */
      .dataTables_filter input {
        padding: 8px 12px !important;
        border: 1px solid #d9d9d9 !important;
        border-radius: 2px !important;
        margin-left: 8px !important;
        font-family: 'Times New Roman', Times, serif !important;
      }
      
      .dataTables_length select {
        padding: 8px 30px 8px 12px !important;
        border: 1px solid #d9d9d9 !important;
        border-radius: 2px !important;
        margin: 0 6px !important;
        font-family: 'Times New Roman', Times, serif !important;
      }
      
      .dataTables_paginate .paginate_button {
        padding: 0.5em 1em !important;
        border: 1px solid #d9d9d9 !important;
        border-radius: 2px !important;
        margin: 0 2px !important;
        background-color: #ffffff !important;
        color: #333333 !important;
        font-family: 'Times New Roman', Times, serif !important;
      }
      
      .dataTables_paginate .paginate_button:hover {
        background-color: #f0f0f0 !important;
        color: #333333 !important;
        border-color: #d9d9d9 !important;
      }
      
      .dataTables_paginate .paginate_button.current,
      .dataTables_paginate .paginate_button.current:hover {
       background-color: #f0f0f0 !important; 
       color: #333333 !important; 
       border-color: #d9d9d9 !important;
       }
      
      /* Spinner styling */
      .shiny-spinner-output-container .load-container {
        background-color: rgba(255, 255, 255, 0.8);
        border-radius: 2px;
      }
      
      .shiny-spinner-output-container .loader {
        color: #d9d9d9 !important;
      }
      
      /* Loading screen animation */
      @keyframes loading {
        0% { left: -30%; }
        100% { left: 100%; }
      }
      
      /* Sidebar footer */
      .sidebar-footer {
        margin-top: 40px;
        border-top: 1px solid #eeeeee;
        padding-top: 20px;
        font-size: 12px;
        color: #777777;
        text-align: center;
        font-family: 'Times New Roman', Times, serif;
        font-weight: 300;
      }
      
      /* Font Awesome icons */
      .fa, .fas, .far, .fab {
        font-family: 'Font Awesome 5 Free', 'Font Awesome 5 Brands' !important;
        font-weight: 900; /* Ensure correct weight for icons */
      }
      
      /* Label styling */
      label {
        font-weight: 500;
        font-size: 15px;
        margin-bottom: 8px;
        display: block;
        color: #444444;
      }
      
      /* Animation for fade in */
      .animate-fade-in {
        animation: fadeIn 0.3s ease-in-out;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }

      /* Masquer les boutons de contrôle ggiraph */
.ggiraph-toolbar {
  display: none !important;
}
      
      /* Responsive layout adjustments */
      @media (max-width: 991px) {
        .city-maps-grid {
          grid-template-columns: repeat(2, 1fr) !important;
        }
      }
      
      @media (max-width: 767px) {
        .city-maps-grid {
          grid-template-columns: 1fr !important;
        }
      }
    "))
  ),
  
  # Main layout with sidebar and main panel
  sidebarLayout(
    # Sidebar with controls
    sidebarPanel(
      width = 3,
      h3(HTML(""), 
         class = "sidebar-title"),
      
      # Model type dropdown
      h4("Type de modèle:", class = "input-title"),
      selectInput("modelType", NULL,
                  choices = c("People predict", "Agrégateur d'agrégateurs", "Combiné"),
                  selected = "People predict"),
      
      # Party prediction dropdown
      h4("Prédiction par parti:", class = "input-title"),
      selectInput("partyPrediction", NULL,
                  choices = c("All parties", "LPC", "CPC", "BQ", "NDP", "GP", "Battlefields"),
                  selected = "All parties"),
      
                  # Modification de la légende - Partie de ui.R
# Cette partie remplace le code de la légende des partis

conditionalPanel(
  condition = "input.partyPrediction != 'Battlefields'",
  div(class = "party-legend-container",
    h4("Solidité des avances", class = "party-legend-title"),
    
    # Légende textuelle
    div(class = "party-labels",
      div(class = "party-label left", "CIRCONSCRIPTION", br(), "COMPÉTITIVE"),
      div(class = "party-label right", "AVANCE", br(), "CONSOLIDÉE")
    ),
    
    # Légende minimaliste avec gradients pour chaque parti - GP retiré
    div(class = "party-gradients-container",
      lapply(c("LPC", "CPC", "NDP", "BQ"), function(party) {
        div(class = "party-gradient-row",
          span(class = "party-name", party),
          div(class = "party-gradient-bar", 
              style = paste0("background: linear-gradient(to right, ", 
                            ifelse(party == "LPC", "#d71920", 
                              ifelse(party == "CPC", "#0e2c68", 
                                ifelse(party == "NDP", "#f58220", 
                                  "#29b2e6"
                                )
                              )
                            ), "20 0%, ", 
                            ifelse(party == "LPC", "#d71920", 
                              ifelse(party == "CPC", "#0e2c68", 
                                ifelse(party == "NDP", "#f58220", 
                                  "#29b2e6"
                                )
                              )
                            ), "60 50%, ", 
                            ifelse(party == "LPC", "#d71920", 
                              ifelse(party == "CPC", "#0e2c68", 
                                ifelse(party == "NDP", "#f58220", 
                                  "#29b2e6"
                                )
                              )
                            ), " 100%);"))
        )
      })
    )
  )
),

# Légende spécifique au mode Battlefields - réintégrée
conditionalPanel(
  condition = "input.partyPrediction == 'Battlefields'",
  div(class = "battlefield-legend-container",
    h4("Compétitivité des circonscriptions", class = "bf-legend-title"),
    div(class = "battlefield-gradient-bar"),
    div(class = "battlefield-labels",
      div(class = "bf-label left", "MOINS", br(), "COMPÉTITIF"),
      div(class = "bf-label right", "PLUS", br(), "COMPÉTITIF")
    )
  )
),

# Style CSS amélioré pour les légendes
tags$style(HTML("
/* Styles pour la légende des partis - Version améliorée */
.party-legend-container {
margin-top: 25px;
padding: 15px 0;
border-top: 1px solid #eeeeee;
}

.party-legend-title {
font-size: 15px;
margin-bottom: 15px;
font-family: 'Libre Baskerville', Georgia, serif;
text-align: center;
color: #333;
}

.party-labels {
display: flex;
justify-content: space-between;
margin: 0 0 15px 0;
}

.party-label {
font-size: 10px;
text-transform: uppercase;
font-weight: 500;
color: #666666;
line-height: 1.3;
letter-spacing: 0.02em;
}

.party-label.left {
text-align: left;
}

.party-label.right {
text-align: right;
}

/* Styles améliorés pour les gradients des partis */
.party-gradients-container {
margin-top: 15px;
}

.party-gradient-row {
display: flex;
align-items: center;
margin-bottom: 8px; /* Réduit l'espacement entre les gradients */
}

.party-name {
font-size: 12px;
font-weight: 500;
margin-right: 15px;
width: 40px;
color: #444;
}

.party-gradient-bar {
height: 6px; /* Plus fin pour plus d'élégance */
flex-grow: 1;
border-radius: 3px;
box-shadow: 0 1px 2px rgba(0,0,0,0.05);
position: relative;
overflow: hidden;
}

/* Reflet pour effet glossy sur les barres */
.party-gradient-bar:after {
content: '';
position: absolute;
top: 0;
left: 0;
right: 0;
height: 50%;
background: linear-gradient(to bottom, rgba(255,255,255,0.2), rgba(255,255,255,0));
border-radius: 3px 3px 0 0;
}

/* Styles pour la légende Battlefields */
.battlefield-legend-container {
margin-top: 25px;
padding: 15px 0;
border-top: 1px solid #eeeeee;
}

.bf-legend-title {
font-size: 15px;
margin-bottom: 15px;
font-family: 'Libre Baskerville', Georgia, serif;
text-align: center;
color: #333;
}

.battlefield-gradient-bar {
height: 6px; /* Plus fin */
width: 100%;
margin: 0 auto 5px auto;
background: linear-gradient(to right, black 0%, #333333 20%, white 50%, #FFDD55 80%, #FFCC00 100%);
position: relative;
border-radius: 3px;
box-shadow: 0 1px 2px rgba(0,0,0,0.05);
}

.battlefield-gradient-bar:after {
content: '';
position: absolute;
top: -3px;
left: 50%;
height: 12px;
width: 1px;
background-color: #777777;
}

.battlefield-labels {
display: flex;
justify-content: space-between;
margin-top: 10px;
}

.bf-label {
font-size: 10px;
text-transform: uppercase;
font-weight: 500;
color: #666666;
line-height: 1.3;
letter-spacing: 0.02em;
}

.bf-label.left {
text-align: left;
}

.bf-label.right {
text-align: right;
}

/* Styles pour la section Sources de données */
.data-sources-container {
  margin-top: 25px;
  padding: 15px 0;
  border-top: 1px solid #eeeeee;
}

.sources-title {
  font-size: 15px;
  margin-bottom: 15px;
  font-family: 'Libre Baskerville', Georgia, serif;
  text-align: center;
  color: #333;
}

.sources-list {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.source-item {
  text-align: center;
}

.source-item a {
  color: #444;
  text-decoration: none;
  font-size: 14px;
  transition: color 0.2s;
}

.source-item a:hover {
  color: #0e2c68;
  text-decoration: underline;
}

.title-container {
  background-color: #ffffff;
  padding: 1.5rem 2rem;
  margin-bottom: 1.5rem;
  border-radius: 2px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
  border: 1px solid #e6e6e6;
  text-align: center;
}

.main-title {
  font-family: 'Libre Baskerville', Georgia, serif;
  font-size: 32px;
  font-weight: 700;
  color: #222222;
  margin: 0;
  line-height: 1.2;
  letter-spacing: -0.02em;
}

.shrug-icon {
  font-family: sans-serif;
  font-weight: 700; /* Bold */
  font-size: 1.1em; /* Plus grand que le texte normal */
  display: inline-block;
  margin-left: 8px;
}

.subtitle {
  font-family: 'Libre Baskerville', Georgia, serif;
  font-size: 22px;
  font-weight: 400;
  color: #444444;
  margin: 10px 0 0 0;
  line-height: 1.4;
}

@media (max-width: 768px) {
  .main-title {
    font-size: 26px;
  }
  
  .subtitle {
    font-size: 18px;
  }
}

")),
      
# Section "Source de données"
div(class = "data-sources-container",
    h4("Source de données", class = "sources-title"),
    div(class = "sources-list",
        div(class = "source-item",
            a(href = "https://www.voxpoplabs.com/thesignal", target = "_blank", "The Signal")
        ),
        div(class = "source-item",
            a(href = "https://338canada.com", target = "_blank", "Canada 338")
        ),
        div(class = "source-item",
            a(href = "https://www.poliwave.com", target = "_blank", "Poliwave")
        ),
        div(class = "source-item",
            a(href = "https://canada.datagotchi.com", target = "_blank", "Léger-Datagotchi")
        )
    )
),
      # Information section at bottom of sidebar
      div(class = "sidebar-footer",
          p("Données mises à jour: Mars 2025"),
          p(paste0("Nombre de répondants : ", nrow(readRDS("data/datagotchi2025_canada_app_20250318.rds")))),
          p(HTML("<i class='fas fa-info-circle'></i> Survolez sur les cartes pour plus de détails"))
      )
    ),
    
    # Main panel with maps and data table
    mainPanel(
      width = 9,
      
      # Title
      HTML('
<div class="title-container">
  <h1 class="main-title">Agrégateur d\'agrégateurs <span class="shrug-icon">¯\\_(ツ)_/¯</span></h1>
  <h2 class="subtitle">+ Prédiction citoyenne Léger-Datagotchi</h2>
</div>
'),
      
      # Canada map at the top with container and section header
      div(class = "map-container",
          style = "margin-bottom: 20px; background: white; padding: 15px; border: 1px solid #e6e6e6; border-radius: 2px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
#          h4("Carte du Canada", class = "section-header"),
          withSpinner(
            girafeOutput("mapPlot", height = "500px"),
            color = "#555555", type = 8, size = 0.8
          )
      ),
      
      # City maps grid (2x4) - always visible now
      div(id = "city-maps-container", class = "city-maps-container",
          style = "margin-bottom: 20px; background: white; padding: 15px; border: 1px solid #e6e6e6; border-radius: 2px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
#          h4("Cartes électorales des grandes villes", class = "section-header"),
          # 2x4 grid layout for city maps
          div(class = "city-maps-grid",
              style = "display: grid; grid-template-columns: repeat(4, 1fr); grid-template-rows: repeat(2, 1fr); gap: 20px;",
                    
              # Row 1 cities
              div(class = "city-map-box",
                  girafeOutput("montrealMap", height = "180px"),
                  div(class = "map-title-overlay", "Montreal")
              ),
              div(class = "city-map-box",
                  girafeOutput("torontoMap", height = "180px"),
                  div(class = "map-title-overlay", "Toronto")
              ),
              div(class = "city-map-box",
                  girafeOutput("vancouverMap", height = "180px"),
                  div(class = "map-title-overlay", "Vancouver")
              ),
              div(class = "city-map-box",
                  girafeOutput("quebecCityMap", height = "180px"),
                  div(class = "map-title-overlay", "Quebec City")
              ),
              
              # Row 2 cities
              div(class = "city-map-box",
                  girafeOutput("ottawaMap", height = "180px"),
                  div(class = "map-title-overlay", "Ottawa-Gatineau")
              ),
              div(class = "city-map-box",
                  girafeOutput("winnipegMap", height = "180px"),
                  div(class = "map-title-overlay", "Winnipeg")
              ),
              div(class = "city-map-box",
                  girafeOutput("kitchenerMap", height = "180px"),
                  div(class = "map-title-overlay", "Kitchener-Waterloo")
              ),
              div(class = "city-map-box",
                  girafeOutput("londonMap", height = "180px"),
                  div(class = "map-title-overlay", "London")
              )
          )
      ),
      
      # Data table at the bottom
      div(class = "table-container",
          style = "margin-bottom: 20px; background: white; padding: 15px; border: 1px solid #e6e6e6; border-radius: 2px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
#          h4("Résultats par circonscription", class = "section-header"),
          withSpinner(
            dataTableOutput("dataTable"),
            color = "#555555", type = 8, size = 0.8
          )
      )
    ),
    
    # Spécifier explicitement la position en tant que caractère
    position = "left"
  )
)
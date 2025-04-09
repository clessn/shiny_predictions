# Script pour créer un jeu de données hybride combinant les géométries
# de spatial_canada_2022_electoral_ridings_aligned et map_statcan.rds
library(sf)
library(dplyr)
library(cartessn)  # Package source

# Définir le mapping des villes
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
  "toronto" = list("ridings" = c(
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

# Chargement des données
message("Chargement des données originales...")
map_data_original <- cartessn::spatial_canada_2022_electoral_ridings_aligned
map_statcan <- readRDS("data/map_statcan.rds")

# Liste des ridings spécifiques à conserver avec la géométrie originale
ridings_a_conserver <- c(
  "60001", # Yukon
  "61001", # Northwest Territories
  "62001", # Nunavut
  "10004"  # Labrador
)

# Extraire tous les ridings des villes
all_city_ridings <- unique(unlist(lapply(city_mapping, function(x) x$ridings)))

# Combiner avec les ridings à conserver
ridings_a_conserver_tous <- c(ridings_a_conserver, all_city_ridings)

message(paste("Nombre total de circonscriptions à conserver de l'original:", 
              length(ridings_a_conserver_tous)))

# 1. Extraction des géométries originales pour les ridings spécifiques
message("Extraction des géométries pour les ridings spécifiques...")
map_data_original$id_riding <- as.character(map_data_original$id_riding)

ridings_original <- map_data_original %>%
  filter(id_riding %in% ridings_a_conserver_tous)

# 2. Extraction des géométries de map_statcan pour les autres ridings
message("Extraction des géométries de map_statcan pour les autres ridings...")
map_statcan$id_riding <- as.character(map_statcan$id_riding)

ridings_statcan <- map_statcan %>%
  filter(!id_riding %in% ridings_a_conserver_tous)

# Vérifier que les deux jeux de données ont les mêmes colonnes
cols_original <- names(ridings_original)
cols_statcan <- names(ridings_statcan)

# Identifier les colonnes communes
common_cols <- intersect(cols_original, cols_statcan)

message("Colonnes communes aux deux jeux de données:")
message(paste(common_cols, collapse = ", "))

# Utiliser seulement les colonnes communes pour le rbind
ridings_original <- ridings_original %>% select(all_of(common_cols))
ridings_statcan <- ridings_statcan %>% select(all_of(common_cols))

# S'assurer que les deux jeux de données ont le même CRS
target_crs <- st_crs(ridings_original)
ridings_statcan <- st_transform(ridings_statcan, target_crs)

# 3. Combiner les deux jeux de données
message("Combinaison des deux jeux de données...")
map_hybrid <- rbind(ridings_original, ridings_statcan)

# Vérification
message(paste("Nombre de ridings dans le jeu hybride:", nrow(map_hybrid)))
message(paste("Nombre de ridings originaux conservés:", nrow(ridings_original)))
message(paste("Nombre de ridings de StatCan utilisés:", nrow(ridings_statcan)))

# Mesurer les tailles
original_size <- object.size(map_data_original)
statcan_size <- object.size(map_statcan)
hybrid_size <- object.size(map_hybrid)

message("Statistiques de taille:")
message(paste("Taille originale (aligned):", format(original_size, units = "MB")))
message(paste("Taille StatCan:", format(statcan_size, units = "MB")))
message(paste("Solution hybride:", format(hybrid_size, units = "MB")))
message(paste("Réduction par rapport à l'original:", 
             round((1 - as.numeric(hybrid_size)/as.numeric(original_size)) * 100, 1), "%"))

# Sauvegarder le jeu de données hybride
message("Sauvegarde de map_hybrid...")
saveRDS(map_hybrid, "data/map_hybrid.rds")

message("Terminé! Le fichier map_hybrid.rds a été créé dans le répertoire 'data'.")
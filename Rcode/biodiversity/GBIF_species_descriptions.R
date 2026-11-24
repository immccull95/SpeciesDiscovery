# 9-22-26

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/Biodiversity/RecentDescriptions")

library(dplyr)
#library(pak)
library(rgbif)
library(stringr)
library(purrr)
library(leaflet)

# apparently now we use pak instead of devtools
#pak::pak("ropensci-archive/zbank")
#library(zbank)

## GBIF keys for select classes
amphibia_key <- name_backbone(name = "Amphibia", rank = "class")$classKey
aves_key <- name_backbone(name = "Aves", rank = "class")$classKey
mammalia_key <- name_backbone(name = "Mammalia", rank = "class")$classKey
insecta_key <- name_backbone(name = "Insecta", rank = "class")$classKey

## First approach: gets year, but not location
# scientificName field has records like:
# Nasikabatrachus sahyadrensis Biju & Bossuyt, 2003

# # Lookup names
# amphibian_query <- name_lookup(
#   higherTaxonKey = amphibia_key,
#   status = "ACCEPTED",
#   rank = "SPECIES",
#   limit = 10000
# )
# 
# # Pull data
# amphibian_df <- amphibian_query$data
# amphibian_df$year <- as.numeric(str_extract(amphibian_df$scientificName, "\\d{4}$"))
# 
# # Filter the output data locally for your research timeline (2025–2026)
# recent_amphibians <- subset(amphibian_df, year %in% seq(2000,2026,1))
# 
# summary(recent_amphibians$year)
# length(unique(recent_amphibians$scientificName))
# hist(recent_amphibians$year, main='Recent amphibian descriptions (2000-present)',
#      xlab='Year', breaks=seq(2000,2026,1), xlim=c(2000,2026))


## Another approach using preserved specimen records
# some risk that these would be museum locations
# but then wouldn't they all be in cities?
user <- "immccull95"
pwd <- "z!J)>:w'*5LW5JU"
email <- "immccull@gmail.com"

## Amphibians
download_amphibians <- occ_download(
  pred("taxonKey", amphibia_key),
  pred_in("year", seq(2000,2026,1)),
  #pred("country", "PE"),
  pred_in("country", c("PE", "EC", "BO", "CO")),
  pred("hasGeospatialIssue", FALSE), #no issues with coordinates
  pred("hasCoordinate", TRUE),  #has coordinates
  pred("occurrenceStatus", "PRESENT"), #recorded as presence
  user = user, pwd = pwd, email = email,
  format = "SIMPLE_CSV"
)

occ_download_wait(download_amphibians)

amphibian_locations <- occ_download_get(download_amphibians) %>%
  occ_download_import()

amphibian_specimens <- amphibian_locations %>%
  select(scientificName, countryCode, decimalLatitude, decimalLongitude, basisOfRecord, year) %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN") %>%
  distinct(scientificName, .keep_all = TRUE)

summary(amphibian_specimens$year)
hist(amphibian_specimens$year, main='Recent amphibian descriptions (2000-present)',
     xlab='Year', breaks=seq(2000,2026,1), xlim=c(2000,2026))

map_amphibians <- amphibian_specimens %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# View the points in leaflet
leaflet_amphibians <- leaflet(data = map_amphibians) %>%
  # Add default OpenStreetMap map background tiles
  addTiles() %>% 
  # Add circles for each species discovery location
  addCircleMarkers(
    lng = ~decimalLongitude, 
    lat = ~decimalLatitude,
    radius = 6,
    color = "#1b9e77",      # Clean teal boundary line
    fillColor = "#d95f02",  # Contrasting orange interior fill
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    # This creates a dynamic HTML popup when you click a marker
    popup = ~paste0(
      "<strong>Species:</strong> <em>", scientificName, "</em><br>",
      "<strong>Record Type:</strong> ", basisOfRecord, "<br>",
      "<strong>Country:</strong> ", countryCode, "<br>",
      "<strong>Coordinates:</strong> ", decimalLatitude, ", ", decimalLongitude
    )
  )

leaflet_amphibians

## Mammals
download_mammals <- occ_download(
  pred("taxonKey", mammalia_key),
  pred_in("year", seq(2000,2026,1)),
  #pred("country", "PE"),
  pred_in("country", c("PE", "EC", "BO", "CO")),
  pred("hasGeospatialIssue", FALSE), #no issues with coordinates
  pred("hasCoordinate", TRUE),  #has coordinates
  pred("occurrenceStatus", "PRESENT"), #recorded as presence
  user = user, pwd = pwd, email = email,
  format = "SIMPLE_CSV"
)

occ_download_wait(download_mammals)

mammal_locations <- occ_download_get(download_mammals) %>%
  occ_download_import()

mammal_specimens <- mammal_locations %>%
  select(scientificName, countryCode, decimalLatitude, decimalLongitude, basisOfRecord, year) %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN") %>%
  distinct(scientificName, .keep_all = TRUE)

summary(mammal_specimens$year)
hist(mammal_specimens$year, main='Recent mammal descriptions (2000-present)',
     xlab='Year', breaks=seq(2000,2026,1), xlim=c(2000,2026))

map_mammals <- mammal_specimens %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# View the points in leaflet
leaflet_mammals <- leaflet(data = map_mammals) %>%
  # Add default OpenStreetMap map background tiles
  addTiles() %>% 
  # Add circles for each species discovery location
  addCircleMarkers(
    lng = ~decimalLongitude, 
    lat = ~decimalLatitude,
    radius = 6,
    color = "#1b9e77",      # Clean teal boundary line
    fillColor = "#d95f02",  # Contrasting orange interior fill
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    # This creates a dynamic HTML popup when you click a marker
    popup = ~paste0(
      "<strong>Species:</strong> <em>", scientificName, "</em><br>",
      "<strong>Record Type:</strong> ", basisOfRecord, "<br>",
      "<strong>Country:</strong> ", countryCode, "<br>",
      "<strong>Coordinates:</strong> ", decimalLatitude, ", ", decimalLongitude
    )
  )

leaflet_mammals

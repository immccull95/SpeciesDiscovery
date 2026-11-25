# 9-28-26

# Analysis of Mammal Diversity Database (v2.5)
# https://www.mammaldiversity.org/releases/

library(terra)
library(dplyr)
library(leaflet)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/Biodiversity/RecentDescriptions/MDD")

mdd_species <- read.csv("MDD/MDD_v2.5_6904species.csv")
worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")

# create subset with location data
mdd_species_loc <- mdd_species %>%
  filter(!is.na(typeLocalityLatitude),
    !is.na(typeLocalityLongitude),
    typeLocalityLatitude != "",
    typeLocalityLongitude != "")


# this will convert to numeric, but records with ranges instead of
# specific coordinate will be NA
mdd_species_loc$latitude <- as.numeric(mdd_species_loc$typeLocalityLatitude)
summary(mdd_species_loc$latitude)

mdd_species_loc$longitude <- as.numeric(mdd_species_loc$typeLocalityLongitude)
summary(mdd_species_loc$longitude)

mdd_species_loc <- mdd_species_loc %>%
  filter(!is.na(latitude),
    !is.na(longitude))

leaflet(mdd_species_loc) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>", sciName, "</b><br>",
      authoritySpeciesYear)
  )

hist(mdd_species_loc$authoritySpeciesYear, main='Mammal discoveries',
     breaks=seq(1750,2030,10), xlim=c(1750,2050), xlab='Decade')

# only neotropics
mdd_species_loc_neotropic <- subset(mdd_species_loc, biogeographicRealm=='Neotropic')

leaflet(mdd_species_loc_neotropic) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>", sciName, "</b><br>",
      authoritySpeciesYear)
  )

hist(mdd_species_loc_neotropic$authoritySpeciesYear, main='Mammal discoveries: neotropics',
     breaks=seq(1750,2030,10), xlim=c(1750,2050), xlab='Decade')
length(subset(mdd_species_loc_neotropic, authoritySpeciesYear >= 2000))

# only those post 2000
mdd_species_loc_post2000 <- subset(mdd_species_loc, authoritySpeciesYear >= 2000)

mdd_species_loc_neotropic_post2000 <- subset(mdd_species_loc_neotropic, authoritySpeciesYear >=2000)

leaflet(mdd_species_loc_post2000) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Scientific name:</b> ", sciName, "<br>",
      "<b>Year</b> ", authoritySpeciesYear, "<br>",
      "<b>Common name:</b> ", mainCommonName)
  )


# multi-layer map
leaflet() %>%
  addTiles() %>%
  
  # All descriptions
  addCircleMarkers(
    data = mdd_species_loc_neotropic,
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    stroke = FALSE,
    fillColor = "blue",
    fillOpacity = 0.5,
    popup = ~paste0(
      "<b>", sciName, "</b><br>",
      authoritySpeciesYear, "<br>",
      mainCommonName
    ),
    group = "All descriptions"
  ) %>%
  
  # Descriptions from 2000 onward
  addCircleMarkers(
    data = mdd_species_loc_neotropic_post2000,
    lng = ~longitude,
    lat = ~latitude,
    radius = 4,
    stroke = FALSE,
    fillColor = "lightgreen",
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>", sciName, "</b><br>",
      authoritySpeciesYear, "<br>",
      mainCommonName
    ),
    group = "2000 and later"
  ) %>%
  
  addLayersControl(
    overlayGroups = c("All descriptions", "2000 and later"),
    options = layersControlOptions(collapsed = FALSE)
  )

# save as shapefile
mdd_species_loc_neotropic_shp <- vect(mdd_species_loc_neotropic, 
                                      geom = c("longitude", "latitude"), 
                                      crs = "EPSG:4326")

writeVector(mdd_species_loc_neotropic_shp,
            filename="MDD/MDD_species_loc_neotropic.shp", overwrite=T)

## Clip to countries of interest
worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")

# subset desired countries and only keep primary land (mainland)
countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")

mdd_species_loc_neotropic_shp_aoi <- terra::intersect(mdd_species_loc_neotropic_shp,
                                                      countries_aoi)
plot(countries_aoi)
plot(mdd_species_loc_neotropic_shp_aoi, add=T, col='royalblue')

writeVector(mdd_species_loc_neotropic_shp_aoi, "MDD_species_loc_neotropic_pilot/MDD_species_loc_neotropic_pilot.shp",
            overwrite=T)

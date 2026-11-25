## Wrangle KBA data
# 11-24-26

library(terra)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery")

# Identify countries of interest
worldcountries <- vect("Data/WorldCountries/World_Countries.shp")
countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

# KBA
KBA <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/KBA/KBA_Data/KBA_Data/KBAsGlobal_2026_September_02_POL.shp")

## Clip to study area and reproject
KBA_clip <- terra::intersect(KBA, countries_aoi)
KBA_clip_GLANCE <- project(KBA_clip, "EPSG:10603")
writeVector(KBA_clip_GLANCE, filename="Data/KBA/KBA_Data/KBA_Data/pilot/KBA_pilot.shp",
            overwrite=T)

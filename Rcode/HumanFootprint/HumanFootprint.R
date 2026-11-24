# 11-11-26

library(terra)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/HumanFootprint")

# Human Footprint 2020 from Sun et al. (2026)
# https://zenodo.org/records/21831881
HF_2020 <- rast("HF_2020.tif") 

# Identify country of interest
worldcountries <- vect("Data/WorldCountries/World_Countries.shp")
countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

## Clip human footprint to study area
# match CRS to countries first
countries_aoi_match <- project(countries_aoi, HF_2020)

HF_2020_clip <- crop(HF_2020, countries_aoi_match, mask=T)

plot(countries_aoi_match)
plot(HF_2020_clip, add=T)

# Reproject to GLANCE and export
HF_2020_clip_GLANCE <- project(HF_2020_clip, "EPSG:10603",
                               res=c(1000,1000), method='bilinear',
                               filename='HF_2020_pilot.tif',
                               overwrite=T)

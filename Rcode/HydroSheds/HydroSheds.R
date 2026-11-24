# 11-11-26

library(terra)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/HydroSheds")

# HydroSheds: HydroRivers
# https://www.hydrosheds.org/products/hydrorivers
rivers <- vect("HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa.shp") 

# Identify country of interest
worldcountries <- vect("Data/WorldCountries/World_Countries.shp")
countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

## Clip rivers to study area
rivers_aoi <- terra::intersect(rivers, countries_aoi)
#plot(countries_aoi)
#plot(rivers_aoi, add=T, col='blue')

# reproject and export
rivers_aoi_GLANCE <- project(rivers_aoi, "EPSG:10603")

#writeVector(rivers_aoi_GLANCE,
#            filename="HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa_shp/pilot/HydroRivers_pilot.shp",
#            overwrite=T)

## Calculate river density
raster_template <- rast(countries_aoi_GLANCE, resolution = 1000) # 1km resolution
line_lengths <- rasterizeGeom(rivers_aoi_GLANCE, raster_template, fun="length", unit="km")

# create moving window
search_radius <- 5000 #m
weight_matrix <- focalMat(line_lengths, d=search_radius, type="circle")

# Smooth lengths across the circle neighborhood (may take ~20–30 seconds)
focal_lengths <- focal(line_lengths, w=weight_matrix, fun=sum, na.rm=T)

# Divide by the moving window circle area to calculate km/km2
circle_area_sqkm <- (pi * (search_radius / 1000)^2) 
river_density <- focal_lengths / circle_area_sqkm

# clip to study area and export
river_density_clip <- crop(river_density, countries_aoi_GLANCE, mask=T)
plot(river_density_clip)
plot(countries_aoi_GLANCE, add=T)
#writeRaster(river_density_clip, filename='pilot/river_density_pilot.tif',
#            overwrite=T)

## 8-14-26

library(terra)

## Data and setup
setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/roads")

# GRIP road data
GRIP <- vect("GRIP4_Region2_vector_shp/GRIP4_region2.shp")

# Countries
countries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")
#peru <- subset(countries, countries$COUNTRY=='Peru')
#peru <- project(peru, "EPSG:10603") #GLANCE S America: https://epsg.io/10603

countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(countries, countries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

# Road subset for focal country (may be slow)
# reproject to same as peru above
# but first need focal country in WGS84 (EPSG 4326) to clip GRIP
#peruWGS84 <- project(peru, "EPSG:4326") #for clipping purposes only
#GRIP_peru <- terra::intersect(GRIP, peruWGS84)
#GRIP_peru_proj <- project(GRIP_peru, "EPSG:10603")

GRIP_aoi <- terra::intersect(GRIP, countries_aoi)
GRIP_aoi_proj <- project(GRIP_aoi, "EPSG:10603")

## Line density calculation
#raster_template <- rast(peru, resolution = 1000) # 1km resolution
#line_lengths <- rasterizeGeom(GRIP_peru_proj, raster_template, fun="length", unit="km")

raster_template <- rast(countries_aoi_GLANCE, resolution = 1000) # 1km resolution
line_lengths <- rasterizeGeom(GRIP_aoi_proj, raster_template, fun="length", unit="km")

# create moving window
search_radius <- 5000 #m
weight_matrix <- focalMat(line_lengths, d=search_radius, type="circle")

# Smooth lengths across the circle neighborhood (may take ~20–30 seconds)
focal_lengths <- focal(line_lengths, w=weight_matrix, fun=sum, na.rm=T)

# Divide by the moving window circle area to calculate km/km2
circle_area_sqkm <- (pi * (search_radius / 1000)^2) 
road_density <- focal_lengths / circle_area_sqkm

# Clip to study area and export
#road_density_peru <- terra::crop(road_density, peru, mask=T)
#plot(road_density_peru, main = "Road Density of Peru (km / km²)")
#summary(road_density_peru)
#hist(road_density_peru)
#writeRaster(road_density_peru, filename="GRIP4_Region2_vector_shp/peru_GRIP_road_density.tif",
#            overwrite=T)

road_density_aoi <- terra::crop(road_density, countries_aoi_GLANCE, mask=T)
plot(road_density_aoi, main = "Road Density (km / km²)")
summary(road_density_aoi)
hist(road_density_aoi)
writeRaster(road_density_aoi, filename="GRIP4_Region2_vector_shp/pilot_GRIP_road_density.tif",
            overwrite=T)

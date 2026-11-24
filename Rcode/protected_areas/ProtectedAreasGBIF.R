## 10-26-26

library(terra)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data")

# countries
worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")
peru <- subset(worldcountries, worldcountries$COUNTRY=='Peru')
peru <- project(peru, "EPSG:4326")
peru_GLANCE <- project(peru, "EPSG:10603")

# protected areas
WDPA_peru <- vect("protected_areas/peru/peru_WDPA.shp")
WDPA_peru <- project(WDPA_peru, "EPSG:10603")
WDPA_peru <- terra::intersect(WDPA_peru, peru_GLANCE) #terrestrial only

# unprotected areas
nonPA_peru <- erase(peru_GLANCE, WDPA_peru)
#plot(peru_GLANCE)
#plot(nonPA_peru, add=T, col='red')

# GBIF points
# reextracted on 10-26-26 then exported pts as shp
amphibians_peru_pts <- vect('Biodiversity/GBIF/peru/amphibian_peru_pts.shp')
amphibians_peru_pts <- project(amphibians_peru_pts, "EPSG:10603")

mammals_peru_pts <- vect('Biodiversity/GBIF/peru/mammals_peru_pts.shp')
mammals_peru_pts <- project(mammals_peru_pts, "EPSG:10603")

insects_peru_pts <- vect('Biodiversity/GBIF/peru/insects_peru_pts.shp')
insects_peru_pts <- project(insects_peru_pts, "EPSG:10603")

# GBIF rasters
#mammal_records <- rast("Biodiversity/GBIF/peru/mammal_records_peru.tif")
#mammal_richness <- rast("Biodiversity/GBIF/peru/mammal_richness_peru.tif")

#### Calculate number of observation points in each PA polygon ####
amphibians_matrix <- terra::relate(amphibians_peru_pts, WDPA_peru, "intersects")
amphibians_matrix_nonPA <- terra::relate(amphibians_peru_pts, nonPA_peru, "intersects")

amphibians_nonPA <- rowSums(amphibians_matrix_nonPA) == 0
amphibians_nonPA <- sum(amphibians_nonPA)

# Sum the columns to get a count of points inside each individual PA polygon
WDPA_peru$amphibian_count <- colSums(amphibians_matrix)
WDPA_peru_df <- as.data.frame(WDPA_peru)

plot(peru_GLANCE)
plot(WDPA_peru, add=T,
     y = "amphibian_count", 
     type = "continuous",           # Continuous color gradient for counts
     mar = c(3, 3, 3, 5),           # Extra right margin space for the legend
     col = rev(terrain.colors(50)), # Clean green-to-brown ecological palette
     main = "Amphibians")

## mammals
mammals_matrix <- terra::relate(mammals_peru_pts, WDPA_peru, "intersects")
mammals_matrix_nonPA <- terra::relate(mammals_peru_pts, nonPA_peru, "intersects")

mammals_nonPA <- rowSums(mammals_matrix_nonPA) == 0
mammals_nonPA <- sum(mammals_nonPA)

# Sum the columns to get a count of points inside each individual PA polygon
WDPA_peru$mammals_count <- colSums(mammals_matrix)
WDPA_peru_df$mammals_count <- colSums(mammals_matrix)

plot(peru_GLANCE)
plot(WDPA_peru, add=T,
     y = "mammals_count", 
     type = "continuous",           # Continuous color gradient for counts
     mar = c(3, 3, 3, 5),           # Extra right margin space for the legend
     col = rev(terrain.colors(50)), # Clean green-to-brown ecological palette
     main = "Mammals")

hist(WDPA_peru_df$amphibian_count, main='GBIF amphibian observations per protected area',
     xlab='Observation count', xlim=c(0,3500), breaks=seq(0,3500,100))
hist(WDPA_peru_df$mammals_count, main='GBIF mammal observations per protected area',
     xlab='Observation count', xlim=c(0,2000), breaks=seq(0,2000,50))


## insects
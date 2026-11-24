## 8-17-26
# 9-29-26: add "below median" species grids

library(terra)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/Biodiversity")

# Identify country of interest
worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")
#peru <- subset(worldcountries, worldcountries$COUNTRY=='Peru')
#peru <- project(peru, "EPSG:4326")

countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

#### Biodiversity data ####
# from biodiversitymapping.org

# From Dec 2017 IUCN
amphibians <- rast("BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/Amphibians/Richness_10km_AMPHIBIANS_dec2017_EckertIV.tif")
amphibians_rare <- rast("BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/Amphibians/Richness_10km_AMPHIBIANS_dec2017_EckertIV_less_than_median.tif")

# From BirdLife International 2018
birds <- rast("BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/Birds/Richness_10km_Birds_v7_EckertIV_no_seabirds.tif")
birds_rare <- rast("BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/Birds/Richness_10km_Birds_v7_EckertIV_less_than_median_no_seabirds2.tif")

# From Mar 2018 IUCN 
mammals <- rast("BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/Mammals/Richness_10km_MAMMALS_mar2018_EckertIV.tif")
mammals_rare <- rast("BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/Mammals/Richness_10km_MAMMALS_mar2018_EckertIV_less_than_median.tif")

## Extract biodiversitymapping data for country
# reproject country
#peru_eckert <- project(peru, birds)
aoi_eckert <- project(countries_aoi, birds)

# extract to country and reproject to GLANCE S America
birds_aoi <- crop(birds, aoi_eckert, mask=T)
birds_aoi <- project(birds_aoi, "EPSG:10603", method="bilinear",
                      res=c(10000,10000))
plot(birds_aoi)
writeRaster(birds_aoi, filename="BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/birds_pilot_BM.tif",
            overwrite=T)

birds_rare_aoi <- crop(birds_rare, aoi_eckert, mask=T)
birds_rare_aoi <- project(birds_rare_aoi, "EPSG:10603", method="bilinear",
                      res=c(10000,10000))
plot(birds_rare_aoi)
writeRaster(birds_rare_aoi, filename="BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/birds_rare_pilot_BM.tif",
            overwrite=T)


amphibians_aoi <- crop(amphibians, aoi_eckert, mask=T)
amphibians_aoi <- project(amphibians_aoi, "EPSG:10603", method="bilinear",
                           res=c(10000,10000))
plot(amphibians_aoi)
writeRaster(amphibians_aoi, filename="BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/amphibians_pilot_BM.tif",
            overwrite=T)

amphibians_rare_aoi <- crop(amphibians_rare, aoi_eckert, mask=T)
amphibians_rare_aoi <- project(amphibians_rare_aoi, "EPSG:10603", method="bilinear",
                           res=c(10000,10000))
plot(amphibians_rare_aoi)
writeRaster(amphibians_rare_aoi, filename="BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/amphibians_rare_pilot_BM.tif",
            overwrite=T)

mammals_aoi <- crop(mammals, aoi_eckert, mask=T)
mammals_aoi <- project(mammals_aoi, "EPSG:10603", method="bilinear",
                        res=c(10000,10000))
plot(mammals_aoi)
writeRaster(mammals_aoi, filename="BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/mammals_pilot_BM.tif",
            overwrite=T)

mammals_rare_aoi <- crop(mammals_rare, aoi_eckert, mask=T)
mammals_rare_aoi <- project(mammals_rare_aoi, "EPSG:10603", method="bilinear",
                        res=c(10000,10000))
plot(mammals_rare_aoi)
writeRaster(mammals_rare_aoi, filename="BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/mammals_rare_pilot_BM.tif",
            overwrite=T)

#### IUCN Red List data ####
# values and method don't matter; just need cell locations
template_raster <- disagg(mammals_aoi, fact=10, method="near")

## Amphibians
amphibians1 <- vect("IUCNredlist/AMPHIBIANS/AMPHIBIANS_PART1.shp")
amphibians2 <- vect("IUCNredlist/AMPHIBIANS/AMPHIBIANS_PART2.shp")
amphibians_merged <- rbind(amphibians1, amphibians2)
amphibians_merged_aoi <- terra::intersect(amphibians_merged, countries_aoi)
amphibians_merged_aoi <- project(amphibians_merged_aoi, "EPSG:10603")

# fix and simplify geometry
amphibians_merged_aoi <- simplifyGeom(makeValid(amphibians_merged_aoi))

# count species
# need to account for multi-part polygons first, so aggregate
amphibian_richness_aggregated <- terra::aggregate(amphibians_merged_aoi, by="sci_name")

# rasterize to count number of species per sq km
# background = 0 assigns 0 to cells with no range maps in that cell
amphibians_IUCN_raster <- rasterize(amphibian_richness_aggregated, template_raster,
                                   field=1, fun="sum", na.rm=T, background=0)
#peru_GLANCE <- project(peru, "EPSG:10603")
amphibians_IUCN_raster <- crop(amphibians_IUCN_raster, countries_aoi_GLANCE, mask=T,
                                   filename="IUCNredlist/AMPHIBIANS/amphibians_IUCNredlist_richness.tif",
                                   overwrite=T)
plot(amphibians_IUCN_raster)

## Mammals
mammals1 <- vect("IUCNredlist/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
mammals_merged_aoi <- terra::intersect(mammals1, countries_aoi)
mammals_merged_aoi <- project(mammals_merged_aoi, "EPSG:10603")

# fix and simplify geometry
mammals_merged_aoi <- simplifyGeom(makeValid(mammals_merged_aoi))

# count species
# need to account for multi-part polygons first, so aggregate
mammal_richness_aggregated <- terra::aggregate(mammals_merged_aoi, by="sci_name")

# rasterize to count number of species per sq km
mammals_IUCN_raster <- rasterize(mammal_richness_aggregated, template_raster,
                                     fun="sum", field=1, na.rm=T, background=0)
mammals_IUCN_raster <- crop(mammals_IUCN_raster, countries_aoi_GLANCE, mask=T,
                            filename="IUCNredlist/MAMMALS_TERRESTRIAL_ONLY/mammals_IUCNredlist_richness.tif",
                            overwrite=T)
plot(mammals_IUCN_raster)

## Trees
trees1 <- vect("IUCNredlist/TREES/TREES_PART1.shp")
trees2 <- vect("IUCNredlist/TREES/TREES_PART2.shp")
trees_merged <- rbind(trees1, trees2)
trees_merged_aoi <- terra::intersect(trees_merged, countries_aoi)
trees_merged_aoi <- project(trees_merged_aoi, "EPSG:10603")

# count species
# need to account for multi-part polygons first, so aggregate
tree_richness_aggregated <- terra::aggregate(trees_merged_aoi, by="sci_name")

# rasterize to count number of species per sq km
# background = 0 assigns 0 to cells with no range maps in that cell
trees_IUCN_raster <- rasterize(tree_richness_aggregated, template_raster,
                               field=1, fun="sum", na.rm=T, background=0)
trees_IUCN_raster <- crop(trees_IUCN_raster, countries_aoi_GLANCE, mask=T,
                          filename="IUCNredlist/trees/trees_IUCNredlist_richness.tif",
                          overwrite=T)
plot(trees_IUCN_raster)

## Reptiles
reptiles1 <- vect("IUCNredlist/REPTILES/REPTILES_PART1.shp")
reptiles2 <- vect("IUCNredlist/REPTILES/REPTILES_PART2.shp")
reptiles_merged <- rbind(reptiles1, reptiles2)
reptiles_merged_aoi <- terra::intersect(reptiles_merged, countries_aoi)
reptiles_merged_aoi <- project(reptiles_merged_aoi, "EPSG:10603")

# fix and simplify geometry
reptiles_merged_aoi <- simplifyGeom(makeValid(reptiles_merged_aoi))

# count species
# need to account for multi-part polygons first, so aggregate
reptile_richness_aggregated <- terra::aggregate(reptiles_merged_aoi, by="sci_name")

# rasterize to count number of species per sq km
# background = 0 assigns 0 to cells with no range maps in that cell
reptiles_IUCN_raster <- rasterize(reptile_richness_aggregated, template_raster,
                                  field=1, fun="sum", na.rm=T, background=0)
reptiles_IUCN_raster <- crop(reptiles_IUCN_raster, countries_aoi_GLANCE, mask=T,
                             filename="IUCNredlist/reptiles/reptiles_IUCNredlist_richness.tif",
                             overwrite=T)
plot(reptiles_IUCN_raster)



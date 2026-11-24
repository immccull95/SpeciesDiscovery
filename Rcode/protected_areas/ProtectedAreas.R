## 8-17-26

library(wdpar)
library(terra)
library(sf)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/protected_areas")

# Specify country of interest
country <- "Peru"

# Create new folder to store output (throws warning if already run, but OK)
dir.create(paste0(getwd(), "/", tolower(country)))
save_dir <- paste0(getwd(), "/", tolower(country))

# Download protected areas
protected_areas <- wdpa_fetch(x = country, wait = T, verbose = T)

# see basic info about data
protected_areas

# it is likely there will be a combination of points, lines and polygons
# we want just the polygons
table(st_geometry_type(protected_areas))
protected_areas_polygons <- st_collection_extract(protected_areas, "POLYGON")

# Convert to spatvector and save output
protected_areas_polygons_vect <- vect(protected_areas_polygons)
plot(protected_areas_polygons_vect)
writeVector(protected_areas_polygons_vect, 
            filename = paste0(save_dir, "/", tolower(country), "_WDPA.shp"))


### 11-7-26
# Because of some issues with wdpar updates and my lack of R updates (thanks CMC)
# I had to download data for other countries from the protected planet
# website, which come in multiple shapefiles that need to be merged

## Colombia
COL_0 <- vect("colombia/WDPA_WDOECM_Nov2026_Public_COL_shp_0/WDPA_WDOECM_Nov2026_Public_COL_shp-polygons.shp")
COL_1 <- vect("colombia/WDPA_WDOECM_Nov2026_Public_COL_shp_1/WDPA_WDOECM_Nov2026_Public_COL_shp-polygons.shp")
COL_2 <- vect("colombia/WDPA_WDOECM_Nov2026_Public_COL_shp_2/WDPA_WDOECM_Nov2026_Public_COL_shp-polygons.shp")
COL_merged <- vect(list(COL_0, COL_1, COL_2)) #merge
COL_dissolved <- terra::aggregate(COL_merged, by='SITE_ID')
writeVector(COL_dissolved, filename="colombia/colombia_WDPA.shp", overwrite=T)

## Ecuador
ECU_0 <- vect("ecuador/WDPA_WDOECM_Nov2026_Public_ECU_shp_0/WDPA_WDOECM_Nov2026_Public_ECU_shp-polygons.shp")
ECU_1 <- vect("ecuador/WDPA_WDOECM_Nov2026_Public_ECU_shp_1/WDPA_WDOECM_Nov2026_Public_ECU_shp-polygons.shp")
ECU_2 <- vect("ecuador/WDPA_WDOECM_Nov2026_Public_ECU_shp_2/WDPA_WDOECM_Nov2026_Public_ECU_shp-polygons.shp")
ECU_merged <- vect(list(ECU_0, ECU_1, ECU_2)) #merge
ECU_dissolved <- terra::aggregate(ECU_merged, by='SITE_ID')
writeVector(ECU_dissolved, filename="ecuador/ecuador_WDPA.shp", overwrite=T)

## Bolivia
BOL_0 <- vect("bolivia/WDPA_WDOECM_Nov2026_Public_BOL_shp_0/WDPA_WDOECM_Nov2026_Public_BOL_shp-polygons.shp")
BOL_1 <- vect("bolivia/WDPA_WDOECM_Nov2026_Public_BOL_shp_1/WDPA_WDOECM_Nov2026_Public_BOL_shp-polygons.shp")
BOL_2 <- vect("bolivia/WDPA_WDOECM_Nov2026_Public_BOL_shp_2/WDPA_WDOECM_Nov2026_Public_BOL_shp-polygons.shp")
BOL_merged <- vect(list(BOL_0, BOL_1, BOL_2)) #merge
BOL_dissolved <- terra::aggregate(BOL_merged, by='SITE_ID')
writeVector(BOL_dissolved, filename="bolivia/bolivia_WDPA.shp", overwrite=T)

## Merge across countries
worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")
countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

peruPA <- vect("peru/peru_WDPA.shp")

merged_PA <- vect(list(peruPA, BOL_dissolved, 
                       COL_dissolved, ECU_dissolved))

merged_PA_inland <- terra::intersect(merged_PA, countries_aoi)
merged_PA_inland <- project(merged_PA_inland, "EPSG:10603")
#plot(countries_aoi)
#plot(merged_PA_inland, add=T, col='darkgreen')
writeVector(merged_PA_inland, overwrite=T,
            filename='pilot/COL_ECU_PER_BOL_wdpa.shp')

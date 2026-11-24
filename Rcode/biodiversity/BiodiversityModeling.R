## 9-17-26

library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery")

# Identify country of interest
worldcountries <- vect("Data/WorldCountries/World_Countries.shp")
#peru <- subset(worldcountries, worldcountries$COUNTRY=='Peru')
#peru <- project(peru, "EPSG:4326")

countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

## Protected areas
# also create subset of indigenous protected areas
wdpa <- vect("Data/protected_areas/pilot/COL_ECU_PER_BOL_wdpa.shp")
wdpa_indig <- subset(wdpa, wdpa$GOV_TYPE=='Indigenous Peoples')

#plot(countries_aoi_GLANCE)
#plot(wdpa_indig, add=T, col='green')

## Road density
road_dens <- rast("Data/roads/GRIP4_Region2_vector_shp/pilot_GRIP_road_density.tif")

## Human footprint
HF_2020 <- rast("Data/HumanFootprint/HF_2020_pilot.tif")

## River density
river_density <- rast("Data/HydroSheds/pilot/river_density_pilot.tif")

## World ecosystems
world_ecosystems <- rast("Data/ecoregions/WorldEcosystems/WorldEcosystems_pilot.tif")

# patch metrics from landscapemetrics (ecoregions: Temp_Moist attribute)
p_area <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_area.tif")
p_core <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_core.tif")
p_cai <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_cai.tif")
p_ncore <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_ncore.tif")
p_contig <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_contig.tif")
p_shape <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_shape.tif")
p_enn <- rast("Data/ecoregions/WorldEcosystems/ecoregion_patches/pilot_p_enn.tif")

## Terrain data
# Need to mosaic across pilot countries
# Resample to 1km grid first using road density (1km as template)

# Peru
TRI_3x3_PER <- rast("Data/DEM/peru/90m/peru_TRI_90m.tif")
TRI_11x11_PER <- rast("Data/DEM/peru/90m/peru_TRI_90m_11x11.tif")
DEM_PER <- rast("Data/DEM/peru/90m/peru_DEM_90m_mosaic.tif")

TRI_3x3_PER <- resample(TRI_3x3_PER, road_dens, method='bilinear')
TRI_11x11_PER <- resample(TRI_11x11_PER, road_dens, method='bilinear')
DEM_PER <- resample(DEM_PER, road_dens, method='bilinear')

# Bolivia
TRI_3x3_BOL <- rast("Data/DEM/bolivia/bolivia_TRI_90m.tif")
TRI_11x11_BOL <- rast("Data/DEM/bolivia/bolivia_TRI_90m_11x11.tif")
DEM_BOL <- rast("Data/DEM/bolivia/bolivia_DEM_90m_mosaic.tif")

TRI_3x3_BOL <- resample(TRI_3x3_BOL, road_dens, method='bilinear')
TRI_11x11_BOL <- resample(TRI_11x11_BOL, road_dens, method='bilinear')
DEM_BOL <- resample(DEM_BOL, road_dens, method='bilinear')

# Colombia  
TRI_3x3_COL <- rast("Data/DEM/colombia/colombia_TRI_90m.tif")
TRI_11x11_COL <- rast("Data/DEM/colombia/colombia_TRI_90m_11x11.tif")
DEM_COL <- rast("Data/DEM/colombia/colombia_DEM_90m_mosaic.tif")

TRI_3x3_COL <- resample(TRI_3x3_COL, road_dens, method='bilinear')
TRI_11x11_COL <- resample(TRI_11x11_COL, road_dens, method='bilinear')
DEM_COL <- resample(DEM_COL, road_dens, method='bilinear')

# Ecuador
TRI_3x3_ECU <- rast("Data/DEM/ecuador/ecuador_TRI_90m.tif")
TRI_11x11_ECU <- rast("Data/DEM/ecuador/ecuador_TRI_90m_11x11.tif")
DEM_ECU <- rast("Data/DEM/ecuador/ecuador_DEM_90m_mosaic.tif")

TRI_3x3_ECU <- resample(TRI_3x3_ECU, road_dens, method='bilinear')
TRI_11x11_ECU <- resample(TRI_11x11_ECU, road_dens, method='bilinear')
DEM_ECU <- resample(DEM_ECU, road_dens, method='bilinear')

TRI_3x3_list <- sprc(list(TRI_3x3_PER, TRI_3x3_BOL, TRI_3x3_COL, TRI_3x3_ECU))
TRI_11x11_list <- sprc(list(TRI_11x11_PER, TRI_11x11_BOL, TRI_11x11_COL, TRI_11x11_ECU))
DEM_list <- sprc(list(DEM_PER, DEM_BOL, DEM_COL, DEM_ECU))


TRI_3x3 <- mosaic(TRI_3x3_list, fun='mean')#,
                  #filename='Data/DEM/pilot/TRI_3x3_pilot.tif', overwrite=T)

TRI_11x11 <- mosaic(TRI_11x11_list, fun='mean')#,
                  #filename='Data/DEM/pilot/TRI_11x11_pilot.tif', overwrite=T)

DEM <- mosaic(DEM_list, fun='mean')#,
                  #filename='Data/DEM/pilot/DEM_pilot.tif', overwrite=T)


# GBIF-derived richness data in 1x1 km grid from GBIF
richness_amphib <- rast("Data/Biodiversity/GBIF/pilot/amphibian_richness_pilot.tif")
richness_mammal <- rast("Data/Biodiversity/GBIF/pilot/mammal_richness_pilot.tif")
#richness_bird <- rast("Data/Biodiversity/GBIF/pilot/bird_richness_pilot.tif")
richness_insect <- rast("Data/Biodiversity/GBIF/pilot/insect_richness_pilot.tif")

# IUCN Red List-derived richness data in 1x1 km grid
richness_amphib_IUCN <- rast("Data/Biodiversity/IUCNredlist/AMPHIBIANS/amphibians_IUCNredlist_richness.tif")
richness_mammals_IUCN <- rast("Data/Biodiversity/IUCNredlist/MAMMALS_TERRESTRIAL_ONLY/mammals_IUCNredlist_richness.tif")
richness_reptiles_IUCN <- rast("Data/Biodiversity/IUCNredlist/REPTILES/reptiles_IUCNredlist_richness.tif")
richness_trees_IUCN <- rast("Data/Biodiversity/IUCNredlist/TREES/trees_IUCNredlist_richness.tif")

# Biodiversity mapping richness (Jenkins) in 10x10 km grid
richness_amphib_BM <- rast("Data/Biodiversity/BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/amphibians_pilot_BM.tif")
richness_birds_BM <- rast("Data/Biodiversity/BiodiversityMapping_TIFFs_2019_03d14_revised/BiodiversityMapping_TIFFs_2019_03d14/pilot/birds_pilot_BM.tif")


# worldclim data
MAT <- rast("Data/climate/pilot/climate/wc2.1_30s//wc2.1_30s_bio_1.tif")
MAP <- rast("Data/climate/pilot/climate/wc2.1_30s//wc2.1_30s_bio_12.tif")

MAT_GLANCE <- project(MAT, richness_amphib, method="bilinear")
MAT_GLANCE <- crop(MAT_GLANCE, countries_aoi_GLANCE, mask=T)
MAP_GLANCE <- project(MAP, richness_amphib, method="bilinear")
MAP_GLANCE <- crop(MAP_GLANCE, countries_aoi_GLANCE, mask=T)

## Standardize rasters to common resolution and extent
# matching GBIF richness raster
#road_dens_1km <- resample(road_dens, richness_amphib, method="bilinear") 
#TRI_3x3_1km <- resample(TRI_3x3, richness_amphib, method="bilinear") 
#TRI_11x11_1km <- resample(TRI_11x11, richness_amphib, method="bilinear") 
#DEM_1km <- resample(DEM, richness_amphib, method="bilinear") 
MAT_1km <- resample(MAT_GLANCE, road_dens, method="bilinear")
MAP_1km <- resample(MAP_GLANCE, road_dens, method="bilinear")

richness_amphib_IUCN <- resample(richness_amphib_IUCN, road_dens, method="bilinear")
richness_mammals_IUCN <- resample(richness_mammals_IUCN, road_dens, method="bilinear")
richness_reptiles_IUCN <- resample(richness_reptiles_IUCN, road_dens, method="bilinear")
richness_trees_IUCN <- resample(richness_trees_IUCN, road_dens, method="bilinear")

## Convert GBIF richness data to points
#richness_amphib_pts <- as.points(richness_amphib, values=T, na.rm=T)
#richness_mammal_pts <- as.points(richness_mammal, values=T, na.rm=T)
#richness_bird_pts <- as.points(richness_bird, values=T, na.rm=T)
#richness_insect_pts <- as.points(richness_insect, values=T, na.rm=T)

#plot(peru)
#plot(richness_amphib_pts, add=T, col='royalblue')

## Extract predictor and richness data for random pts
# using sample for now because there are so many points
set.seed(95)
random_pts <- spatSample(countries_aoi, size=25000)
#plot(peru)
#plot(random_pts, add=T)
random_pts <- project(random_pts, "EPSG:10603")

road_dens_1km_ext <- terra::extract(road_dens, random_pts, na.rm=T)
TRI_3x3_1km_ext <- terra::extract(TRI_3x3, random_pts, na.rm=T)
TRI_11x11_1km_ext <- terra::extract(TRI_11x11, random_pts, na.rm=T)
DEM_1km_ext <- terra::extract(DEM, random_pts, na.rm=T)
MAT_1km_ext <- terra::extract(MAT_1km, random_pts, na.rm=T)
MAP_1km_ext <- terra::extract(MAP_1km, random_pts, na.rm=T)

amphib_richness_1km_ext <- terra::extract(richness_amphib, random_pts, na.rm=T)
mammal_richness_1km_ext <- terra::extract(richness_mammal, random_pts, na.rm=T)
#bird_richness_1km_ext <- terra::extract(richness_bird, random_pts, na.rm=T)
insect_richness_1km_ext <- terra::extract(richness_insect, random_pts, na.rm=T)

amphib_richness_IUCN_1km_ext <- terra::extract(richness_amphib_IUCN, random_pts, na.rm=T)
mammal_richness_IUCN_1km_ext <- terra::extract(richness_mammals_IUCN, random_pts, na.rm=T)
reptile_richness_IUCN_1km_ext <- terra::extract(richness_reptiles_IUCN, random_pts, na.rm=T)
tree_richness_IUCN_1km_ext <- terra::extract(richness_trees_IUCN, random_pts, na.rm=T)

# Combine into single data frame
#random_pts_df <- as.data.frame(random_pts)
#random_pts_df$ID <- road_dens_1km_ext$ID

combined_sample <- road_dens_1km_ext %>%
  left_join(TRI_3x3_1km_ext, by = "ID") %>%
  left_join(TRI_11x11_1km_ext, by = "ID") %>%
  left_join(DEM_1km_ext, by = "ID") %>%
  left_join(MAT_1km_ext, by = "ID") %>%
  left_join(MAP_1km_ext, by = "ID") %>%
  left_join(amphib_richness_1km_ext, by = "ID") %>%
  left_join(mammal_richness_1km_ext, by = "ID") %>%
  #left_join(bird_richness_1km_ext, by = "ID") %>%
  left_join(insect_richness_1km_ext, by = "ID") %>%
  left_join(amphib_richness_IUCN_1km_ext, by = "ID") %>%
  left_join(mammal_richness_IUCN_1km_ext, by = "ID") %>%
  left_join(reptile_richness_IUCN_1km_ext, by = "ID") %>%
  left_join(tree_richness_IUCN_1km_ext, by = "ID")

names(combined_sample) <- c('ID','road_dens_kmsqkm','TRI_3x3_m',
                            'TRI_11x11_m','DEM_m','MAT_C','MAP_mm',
                            'amphib_richness_GBIF', 'mammal_richness_GBIF',
                            #'bird_richness_GBIF',
                            'insect_richness_GBIF',
                            'amphib_richness_IUCN','mammal_richness_IUCN',
                            'reptile_richness_IUCN','tree_richness_IUCN')

## Exploratory analysis
# get into long format
combined_sample_long <- combined_sample %>%
  select(-ID) %>% # Drops ID column
  pivot_longer(
    cols = c('road_dens_kmsqkm', 'TRI_3x3_m', 'TRI_11x11_m', 
             'DEM_m', 'MAT_C', 'MAP_mm'),
    names_to = "var",          
    values_to = "value")

# Amphibians
ggplot(combined_sample_long, aes(x = value, y = amphib_richness_GBIF)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free_x") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Amphibian richness",
    title = "Amphibian GBIF Richness Predictors"
  )

## correlation matrix
amphib_cor_table <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -amphib_richness_GBIF, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(amphib_richness_GBIF, value, use = "complete.obs"))
amphib_cor_table

# Mammals
ggplot(combined_sample_long, aes(x = value, y = mammal_richness_GBIF)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free_x") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Mammal richness",
    title = "Mammal GBIF Richness Predictors"
  )

## correlation matrix
mammal_cor_table <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -mammal_richness_GBIF, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(mammal_richness_GBIF, value, use = "complete.obs"))
mammal_cor_table

# Birds
ggplot(combined_sample_long, aes(x = value, y = bird_richness_GBIF)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free_x") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Bird richness",
    title = "Bird GBIF Richness Predictors"
  )

## correlation matrix
bird_cor_table <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -bird_richness_GBIF, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(bird_richness_GBIF, value, use = "complete.obs"))
bird_cor_table

# Insects
ggplot(combined_sample_long, aes(x = value, y = insect_richness_GBIF)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free_x") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Insect richness",
    title = "Insect GBIF Richness Predictors"
  )

## correlation matrix
insect_cor_table <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -insect_richness_GBIF, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(insect_richness_GBIF, value, use = "complete.obs"))
insect_cor_table

## Now try with IUCN Red List-derived richness
# Amphibians
ggplot(combined_sample_long, aes(x = value, y = amphib_richness_IUCN)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Amphibian richness",
    title = "Amphibian IUCN Richness Predictors"
  )

## correlation matrix
amphib_cor_table2 <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -amphib_richness_IUCN, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(amphib_richness_IUCN, value, use = "complete.obs"))
amphib_cor_table2

# Mammals
ggplot(combined_sample_long, aes(x = value, y = mammal_richness_IUCN)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Mammal richness",
    title = "Mammal IUCN Richness Predictors"
  )

## correlation matrix
mammal_cor_table2 <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -mammal_richness_IUCN, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(mammal_richness_IUCN, value, use = "complete.obs"))
mammal_cor_table2

library(scales)
ggplot(combined_sample_long, aes(x = value, y = mammal_richness_IUCN)) +
  geom_point(color = "dodgerblue") +
  # Using GLM Poisson prevents the line from going negative
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = F, color = "black") + 
  facet_wrap(~ var, scales = "free") + 
  scale_x_continuous(trans = "pseudo_log") + # Transforms the x-axis while preserving 0s
  theme_classic() +
  labs(
    x = "Predictor (Pseudo-Log Scale)",
    y = "Mammal richness",
    title = "Mammal IUCN Richness Predictors"
  )


# Reptiles
ggplot(combined_sample_long, aes(x = value, y = reptile_richness_IUCN)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Reptile richness",
    title = "Reptile IUCN Richness Predictors"
  )

## correlation matrix
reptile_cor_table2 <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -reptile_richness_IUCN, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(reptile_richness_IUCN, value, use = "complete.obs"))
reptile_cor_table2

# Trees
ggplot(combined_sample_long, aes(x = value, y = tree_richness_IUCN)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", se = F, color = "black") + 
  facet_wrap(~ var, scales = "free") + 
  theme_classic() +
  labs(
    x = "Predictor",
    y = "Tree richness",
    title = "Tree IUCN Richness Predictors"
  )

## correlation matrix
tree_cor_table2 <- combined_sample %>%
  # omit ID column from calculation
  select(-ID) %>%
  # Pivot into long format
  pivot_longer(cols = -tree_richness_IUCN, names_to = "var", values_to = "value") %>%
  # Group by var for correlation calculation
  group_by(var) %>%
  summarize(
    r_val = cor(tree_richness_IUCN, value, use = "complete.obs"))
tree_cor_table2

#### Exploratory analysis of mammal diversity database data ####
MDD <- vect("Data/Biodiversity/RecentDescriptions/MDD/MDD_species_loc_neotropic.shp")

# Reproject and clip to study area
MDD_GLANCE <- project(MDD, "EPSG:10603")
MDD_GLANCE_pilot <- terra::intersect(MDD_GLANCE, countries_aoi_GLANCE)
plot(countries_aoi_GLANCE)
plot(MDD_GLANCE_pilot, add=T, col='red')

MDD_GLANCE_pilot_post2000 <- subset(MDD_GLANCE_pilot, MDD_GLANCE_pilot$authority1 >=2000)
plot(MDD_GLANCE_pilot_post2000, add=T, col='blue')

# Add country attribute to points
extracted_data <- terra::extract(countries_aoi_GLANCE, MDD_GLANCE_pilot)
MDD_GLANCE_pilot$Country <- extracted_data$COUNTRY
table(MDD_GLANCE_pilot$Country) #summary of number in each country

MAT_MDD <- terra::extract(MAT_GLANCE, MDD_GLANCE_pilot, na.rm=T)
names(MAT_MDD) <- c('ID','MAT_C')

MAP_MDD <- terra::extract(MAP_GLANCE, MDD_GLANCE_pilot, na.rm=T)
names(MAP_MDD) <- c('ID','MAP_mm')

DEM_MDD <- terra::extract(DEM, MDD_GLANCE_pilot, na.rm=T)
names(DEM_MDD) <- c('ID','DEM_m')

road_dens_MDD <- terra::extract(road_dens, MDD_GLANCE_pilot, na.rm=T)
names(road_dens_MDD) <- c('ID','road_dens_kmsqkm')

TRI_3x3_MDD <- terra::extract(TRI_3x3, MDD_GLANCE_pilot, na.rm=T)
names(TRI_3x3_MDD) <- c('ID','TRI_3x3_m')

TRI_11x11_MDD <- terra::extract(TRI_11x11, MDD_GLANCE_pilot, na.rm=T)
names(TRI_11x11_MDD) <- c('ID','TRI_11x11_m')

HF_2020_MDD <- terra::extract(HF_2020, MDD_GLANCE_pilot, na.rm=T)
names(HF_2020_MDD) <- c('ID','HF_2020')

river_dens_MDD <- terra::extract(river_density, MDD_GLANCE_pilot, na.rm=T)
names(river_dens_MDD) <- c('ID','river_dens_kmsqkm')

## Identify those in protected areas
PA_MDD <- terra::intersect(MDD_GLANCE_pilot, wdpa)
plot(countries_aoi_GLANCE)
plot(PA_MDD, add=T, col='red')
table(PA_MDD$Country)

PA_indig_MDD <- terra::intersect(MDD_GLANCE_pilot, wdpa_indig)
table(PA_indig_MDD$Country)
dim(PA_indig_MDD)

## Count descriptions in various ecoregions
activeCat(world_ecosystems) <- "Temp_Moist" 

ecoregion_MDD <- terra::extract(world_ecosystems, MDD_GLANCE_pilot)
names(ecoregion_MDD) <- c('ID','ecoregion')
ecoregion_MDD$ecoregion_fac <- as.numeric(ecoregion_MDD$ecoregion)

## extract patch metrics
# patch area
p_area_MDD <- terra::extract(p_area, MDD_GLANCE_pilot)
p_area_MDD$value <- p_area_MDD$value * 0.01 #convert ha to sq km
names(p_area_MDD) <- c('ID','p_areasqkm')

# patch core area
p_core_MDD <- terra::extract(p_core, MDD_GLANCE_pilot)
p_core_MDD$value <- p_core_MDD$value * 0.01 #convert ha to sq km
names(p_core_MDD) <- c('ID','p_coresqkm')

# patch core area index
p_cai_MDD <- terra::extract(p_cai, MDD_GLANCE_pilot)
names(p_cai_MDD) <- c('ID','p_cai')

# number of core areas per patch
p_ncore_MDD <- terra::extract(p_ncore, MDD_GLANCE_pilot)
names(p_ncore_MDD) <- c('ID','p_ncore')

# patch shape index (1 = perfect square, higher values = more complex shapes)
p_shape_MDD <- terra::extract(p_shape, MDD_GLANCE_pilot)
names(p_shape_MDD) <- c('ID','p_shape')

# patch contiguity index (0=low; single pixel, 1=high; fully contiguous)
# diagonally connected pixels are considered less contiguous/connected
p_contig_MDD <- terra::extract(p_contig, MDD_GLANCE_pilot)
names(p_contig_MDD) <- c('ID','p_contig')

# patch nearest neighbor distance (to patch of same class)
p_enn_MDD <- terra::extract(p_enn, MDD_GLANCE_pilot)
names(p_enn_MDD) <- c('ID','p_enn_m')

MDD_combined <- MAT_MDD %>%
  left_join(MAP_MDD, by = "ID") %>%
  left_join(TRI_3x3_MDD, by = "ID") %>%
  left_join(TRI_11x11_MDD, by = "ID") %>%
  left_join(DEM_MDD, by = "ID")%>%
  left_join(road_dens_MDD, by = "ID") %>%
  left_join(HF_2020_MDD, by = "ID") %>%
  left_join(river_dens_MDD, by = "ID") %>%
  left_join(ecoregion_MDD, by = "ID") %>%
  left_join(p_area_MDD, by = 'ID') %>%
  left_join(p_core_MDD, by = 'ID') %>%
  left_join(p_cai_MDD, by = 'ID') %>%
  left_join(p_ncore_MDD, by = 'ID') %>%
  left_join(p_shape_MDD, by = 'ID') %>%
  left_join(p_contig_MDD, by = 'ID') %>%
  left_join(p_enn_MDD, by = 'ID')

# Rejoin attributes from description location points
# order of rows is the same
MDD_GLANCE_pilot$ID <- seq(1, nrow(MDD_GLANCE_pilot), 1)
MDD_combined <- merge(MDD_combined, as.data.frame(MDD_GLANCE_pilot), by="ID")
table(MDD_combined$Country) #summary by country
summary(MDD_combined$authority1)

## Export
#write.csv(MDD_combined, file='Data/Biodiversity/RecentDescriptions/MDD/mammal_description_location_attributes.csv',
#          row.names=F)

# it's as if they purposefully make base R bad to force you to use ggplot
hist(MDD_combined$authority1, main='Mammal species descriptions',
     xlab='Year', breaks=seq(1750,2030,10), xlim=c(1750,2030))

ggplot(MDD_combined, aes(x = authority1)) +
  # closed = "left" ensures 1750-1760 falls into the same bin format as base R
  geom_histogram(
    breaks = seq(1750, 2030, 10), 
    fill = "gray80", 
    color = "black", 
    alpha = 0.8,
    closed = "left") +
  scale_x_continuous(
    limits = c(1750, 2030),
    breaks = seq(1750, 2030, 20)) +
  theme_classic() +
  theme(plot.title=element_text(face='bold'))+
  labs(title = "Mammal species descriptions by decade",
       subtitle= "Bolivia, Colombia, Ecuador, Peru",
    x = "Year", y = "Frequency")

plot_colors <- c(
  "Bolivia"  = "pink", 
  "Colombia" = "dodgerblue", 
  "Ecuador"  = "khaki", 
  "Peru"     = "lightgreen")

# stacked histogram
ggplot(MDD_combined, aes(x = authority1, fill = Country)) +
  geom_histogram(
    breaks = seq(1750, 2030, 10), 
    color = "black", 
    linewidth = 0.3,
    alpha = 0.9,
    closed = "left",
    position = "stack") +
  scale_x_continuous(
    limits = c(1750, 2030),
    breaks = seq(1750, 2030, 20)) +
  scale_fill_manual(values = plot_colors) +
  theme_classic() +
  labs(title = "Mammal species descriptions by decade",
    #subtitle = "Bolivia, Colombia, Ecuador, Peru",
    x = "Year", y = "Frequency") +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    #plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    axis.title = element_text(size = 11),
    #axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "right", 
    legend.title = element_text(face = "bold"))

# Create subset of post 2000
# checked metadata to use right field
MDD_GLANCE_pilot_post2000 <- subset(MDD_GLANCE_pilot, MDD_GLANCE_pilot$authority1 >=2000)
plot(countries_aoi_GLANCE)
plot(MDD_GLANCE_pilot_post2000, add=T, col='blue')

MDD_combined_post2000 <- subset(MDD_combined, authority1 >= 2000)
table(MDD_combined_post2000$Country)

MDD_combined_post2000_long <- MDD_combined_post2000 %>%
  #select(-ID) %>% # Drops ID column
  pivot_longer(
    cols = c('MAT_C', 'MAP_mm', 'TRI_3x3_m', 
             'TRI_11x11_m', 'DEM_m', 'road_dens_kmsqkm','HF_2020',
             'river_dens_kmsqkm','ecoregion_fac','p_areasqkm','p_coresqkm',
             'p_ncore','p_cai','p_shape','p_contig','p_enn_m'),
    names_to = "var",          
    values_to = "value")

ggplot(MDD_combined_post2000_long, aes(x = value)) +
  geom_histogram(fill = "royalblue", color = "white") +
  facet_wrap(~ var, scales='free') +
  theme_classic() +
  labs(title = "Mammal descriptions (2000-present)", 
       x = "Values", y = "Frequency")

# Duplicate the data to create combined dataset
dupe <- MDD_combined_post2000_long %>%
  mutate(Country = "Combined")

# Merge combined country data and individual country data
plot_data <- bind_rows(dupe, MDD_combined_post2000_long) %>%
  mutate(Country = factor(Country, levels = c("Combined", "Bolivia", "Colombia", "Ecuador", "Peru")))

# select variable to plot and execute
plot_colors2 <- c("Combined" = "gray", plot_colors)

plot_data_sub <- plot_data %>% 
  filter(var == "MAT_C")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Mean Annual Temperature (°C)",
    x = "Temperature (°C)",
    y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
    legend.position = "none")

# MAP
plot_data_sub <- plot_data %>% 
    filter(var == "MAP_mm")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
    geom_histogram(bins = 5, color = "white", alpha = 0.8) +
    facet_wrap(~ Country, nrow = 1) +
    theme_classic() +
    labs(title = "Mammals: Mean Annual Precipitation (mm)",
         x = "Precipitation (mm)",
         y = "Count") +
    scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Elevation
plot_data_sub <- plot_data %>% 
  filter(var == "DEM_m")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Elevation (m)",
       x = "Elevation (m)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Road density
plot_data_sub <- plot_data %>% 
  filter(var == "road_dens_kmsqkm")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 10, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Road density (km/km²)",
       x = "Road density (km/km²)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# River density
plot_data_sub <- plot_data %>% 
  filter(var == "river_dens_kmsqkm")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 10, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: River density (km/km²)",
       x = "River density (km/km²)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Terrain ruggedness index with standard 3x3 cell window
plot_data_sub <- plot_data %>% 
  filter(var == "TRI_3x3_m")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Terrain ruggedness index",
       x = "TRI (m; 3x3 cell neighborhood)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Terrain ruggedness index with larger 11x11 cell window
# may make less sense now that we resampled to 1km
plot_data_sub <- plot_data %>% 
  filter(var == "TRI_11x11_m")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Terrain ruggedness index",
       x = "TRI (m; 11x11 cell neighborhood)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Human footprint index
# higher value = more human influence
plot_data_sub <- plot_data %>% 
  filter(var == "HF_2020")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Human footprint index",
       x = "Human footprint",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Ecoregion

# Get ordered unique pairs from original data
eco_lookup <- ecoregion_MDD %>%
  filter(!is.na(ecoregion_fac), !is.na(ecoregion)) %>%
  distinct(ecoregion_fac, .keep_all = TRUE) %>%
  arrange(ecoregion_fac)

# Convert to factor using the text names as labels
plot_data_sub <- plot_data %>% 
  filter(var == "ecoregion_fac") %>%
  mutate(value_label = factor(value, 
                              levels = eco_lookup$ecoregion_fac, 
                              labels = eco_lookup$ecoregion)) %>%
  filter(!is.na(value_label))

# bar plot by country
ggplot(plot_data_sub, aes(x = value_label, fill = Country)) +
  geom_bar(color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  scale_fill_manual(values = plot_colors2) +
  theme_classic() +
  labs(title = "Mammals: Ecoregion",
       #x = "Ecoregion",
       y = "Count") +
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.title.x= element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size=8,
                                   hjust = 1, color = "black"))

# Nearest neighbor distance
plot_data_sub <- plot_data %>% 
  filter(var == "p_enn_m")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Nearest neighbor distance",
       x = "Nearest neighbor distance (m)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))


# Patch area
plot_data_sub <- plot_data %>% 
  filter(var == "p_areasqkm")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Patch area",
       x = "Patch area (sq km)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Patch core area
plot_data_sub <- plot_data %>% 
  filter(var == "p_coresqkm")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Patch core area (sq km)",
       x = "Patch core area (sq km)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Patch core area index
plot_data_sub <- plot_data %>% 
  filter(var == "p_cai")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Patch core area index",
       x = "Patch core area index (%)",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Patch shape index
plot_data_sub <- plot_data %>% 
  filter(var == "p_shape")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Patch shape index",
       x = "Patch shape index",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Patch contiguity index
plot_data_sub <- plot_data %>% 
  filter(var == "p_contig")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Patch contiguity index",
       x = "Patch contiguity index",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))

# Number of core areas per patch
plot_data_sub <- plot_data %>% 
  filter(var == "p_ncore")
ggplot(plot_data_sub, aes(x = value, fill = Country)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.8) +
  facet_wrap(~ Country, nrow = 1) +
  theme_classic() +
  labs(title = "Mammals: Core areas per patch",
       x = "Core areas per patch",
       y = "Count") +
  scale_fill_manual(values=plot_colors2)+
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"))


#### Similar exploratory analysis for Amphibian Species of the World (ASW) ####
ASW <- read.csv("Data/Biodiversity/ASW/ASW_Peru_type_localities_preliminary.csv")
hist(ASW$descriptionYear, main='Amphibian descriptions',
     xlab='Description year',
     breaks=seq(2000,2030,1), xlim=c(2000,2030))
mtext(side=3, "Source: Amphibian Species of the World")


ASW_vect <- vect(ASW, 
                 geom = c("longitude", "latitude"), 
                 crs = "EPSG:4326")
ASW_GLANCE <- project(ASW_vect, "EPSG:10603")

plot(peru)
plot(ASW_GLANCE, add=T, col='lightgreen')


MAT_ASW <- terra::extract(MAT_GLANCE, ASW_GLANCE, na.rm=T)
names(MAT_ASW) <- c('ID','MAT_C')

MAP_ASW <- terra::extract(MAP_GLANCE, ASW_GLANCE, na.rm=T)
names(MAP_ASW) <- c('ID','MAP_mm')

DEM_ASW <- terra::extract(DEM_1km, ASW_GLANCE, na.rm=T)
names(DEM_ASW) <- c('ID','DEM_m')

road_dens_ASW <- terra::extract(road_dens_1km, ASW_GLANCE, na.rm=T)
names(road_dens_ASW) <- c('ID','road_dens_kmsqkm')

TRI_3x3_ASW <- terra::extract(TRI_3x3_1km, ASW_GLANCE, na.rm=T)
names(TRI_3x3_ASW) <- c('ID','TRI_3x3_m')

TRI_11x11_ASW <- terra::extract(TRI_11x11_1km, ASW_GLANCE, na.rm=T)
names(TRI_11x11_ASW) <- c('ID','TRI_11x11_m')

ASW_combined <- MAT_ASW %>%
  left_join(MAP_ASW, by = "ID") %>%
  left_join(TRI_3x3_ASW, by = "ID") %>%
  left_join(TRI_11x11_ASW, by = "ID") %>%
  left_join(DEM_ASW, by = "ID")%>%
  left_join(road_dens_ASW, by = "ID")

# remove NAs (non Peru)
ASW_combined <- ASW_combined[complete.cases(ASW_combined),]
summary(ASW_combined)

ASW_combined_long <- ASW_combined %>%
  select(-ID) %>% # Drops ID column
  pivot_longer(
    cols = c('MAT_C', 'MAP_mm', 'TRI_3x3_m', 
             'TRI_11x11_m', 'DEM_m', 'road_dens_kmsqkm'),
    names_to = "var",          
    values_to = "value")

ggplot(ASW_combined_long, aes(x = value)) +
  geom_histogram(fill = "royalblue", color = "white") +
  facet_wrap(~ var, scales='free') +
  theme_minimal() +
  labs(title = "Amphibian descriptions (2000-present), Peru", 
       x = "Values", y = "Frequency")
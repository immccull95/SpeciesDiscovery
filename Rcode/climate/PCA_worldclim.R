# 9-29-26

library(terra)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(landscapemetrics)
library(tidyr)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/climate/peru/climate/wc2.1_country")

# bioclimatic raster descriptions:
# https://www.worldclim.org/data/bioclim.html

bioclim <- rast("PER_wc2.1_30s_bio.tif")

worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")
peru <- subset(worldcountries, worldcountries$COUNTRY=='Peru')
peru <- project(peru, "EPSG:4326")

# Temperature variables
bio1 <- bioclim$wc2.1_30s_bio_1 #MAT
bio4 <- bioclim$wc2.1_30s_bio_4 #temp seasonality
bio5 <- bioclim$wc2.1_30s_bio_5 #max temp of warmest month
bio6 <- bioclim$wc2.1_30s_bio_6 #min temp of coldest month

# Precipitation variables
bio12 <- bioclim$wc2.1_30s_bio_12 #MAP
bio15 <- bioclim$wc2.1_30s_bio_15 #precip seasonality
bio16 <- bioclim$wc2.1_30s_bio_16 #precip of wettest quarter
bio17 <- bioclim$wc2.1_30s_bio_17 #precip of driest quarter

## crop to peru boundary
bio1 <- crop(bio1, peru, mask=T)
bio4 <- crop(bio4, peru, mask=T)
bio5 <- crop(bio5, peru, mask=T)
bio6 <- crop(bio6, peru, mask=T)

bio12 <- crop(bio12, peru, mask=T)
bio15 <- crop(bio15, peru, mask=T)
bio16 <- crop(bio16, peru, mask=T)
bio17 <- crop(bio17, peru, mask=T)

## random sample of points for climate grids
set.seed(95)
sample_pts <- spatSample(peru, 1000)
plot(peru)
plot(sample_pts, add=T, col='red')

bio1_sample <- terra::extract(bio1, sample_pts)
bio4_sample <- terra::extract(bio4, sample_pts)
bio5_sample <- terra::extract(bio5, sample_pts)
bio6_sample <- terra::extract(bio6, sample_pts)

bio12_sample <- terra::extract(bio12, sample_pts)
bio15_sample <- terra::extract(bio15, sample_pts)
bio16_sample <- terra::extract(bio16, sample_pts)
bio17_sample <- terra::extract(bio17, sample_pts)

combined_sample <- bio1_sample %>%
  left_join(bio4_sample, by='ID') %>%
  left_join(bio5_sample, by='ID') %>%
  left_join(bio6_sample, by='ID') %>%
  left_join(bio12_sample, by='ID') %>%
  left_join(bio15_sample, by='ID') %>%
  left_join(bio16_sample, by='ID') %>%
  left_join(bio17_sample, by='ID')

names(combined_sample) <- c('ID','bio1','bio4','bio5','bio6',
                            'bio12','bio15','bio16','bio17')

## execute PCA
# 1. Run the PCA (excluding column 1 'ID', and scaling/centering the data)
pca_result <- prcomp(combined_sample[, -1], center=T, scale.=T)

# 2. Inspect how much variance each Principal Component captures
summary(pca_result)

# 3. View the 'loadings' (how much each bio variable contributes to each PC)
pca_result$rotation

# 4. Extract the PC scores for your grid cells/samples
# This creates a new matrix of coordinates in multivariate climate space
pc_scores <- pca_result$x

# 5. Bind the PC scores back to your original dataset for downstream isolation metrics
final_data <- cbind(combined_sample, pc_scores)
head(final_data)

# biplot
autoplot(pca_result, 
         data = combined_sample, 
         loadings = T, 
         loadings.colour = 'blue',
         loadings.label = T, 
         loadings.label.size = 4, 
         loadings.label.colour = 'black') +
  theme_classic() +
  labs(title = "WorldClim Peru")

#### Apply PCA to calculate climate space ####
pca_rotation <- pca_result$rotation[, 1:2] # Loadings for PC1 and PC2
pca_center   <- pca_result$center           # Mean of each variable
pca_scale    <- pca_result$scale            # SD of each variable

bioclim_stack <- c(bio1, bio4, bio5, bio6, bio12, bio15, bio16, bio17)
names(bioclim_stack) <- c('bio1', 'bio4', 'bio5', 'bio6', 'bio12', 'bio15', 'bio16', 'bio17')

# Project the entire raster stack into PCA space
# terra::predict function automatically handles prcomp objects,
# applying the scaling, centering and rotation matrix pixel-by-pixel.
pca_rasters <- terra::predict(bioclim_stack, pca_result, index = 1:2)

# Extract individual layers for your landscape analysis
pc1_raster <- pca_rasters[[1]]
pc2_raster <- pca_rasters[[2]]

# Verify spatial output
plot(pc1_raster, main = "PC1")
plot(pc2_raster, main = "PC2")

## Combine PC1 and PC2 and convert to discrete bins
bin_width <- 0.5

# Round the continuous PC scores to the nearest bin center
pc1_binned <- round(pc1_raster / bin_width) * bin_width
pc2_binned <- round(pc2_raster / bin_width) * bin_width

# Create a unique integer ID for each distinct 2D climate bin
# Multiplying PC1 by 1000 ensures the codes never overlap or collide
climate_raster <- (pc1_binned * 1000) + pc2_binned

# Force integer format (strict requirement for landscapemetrics)
climate_raster <- as.int(climate_raster)

# project to GLANCE (so distances make sense)
climate_raster <- project(climate_raster, "EPSG:10603", method="near",
                          res=c(1000,1000))
plot(climate_raster)

## Class-level landscape metrics
print(list_lsm(level='class'), n=55)

# Isolation metrics
climate_metrics <- calculate_lsm(climate_raster, 
                                  level = "class", 
                                  metric = c("np", "ca", "area_mn", "cai_mn",
                                             "tca", "enn_mn", "cohesion", "ai",
                                             "ndca"),
                                 directions=8, #default=8 (queen)
                                 edge_depth=1) #default is 1 pixel, but is 1000m in this case

# Convert metrics table to wide format so each metric is its own column
metrics_wide <- climate_metrics %>%
  select(class, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

# Extract spatial grid coordinates and climate IDs
raster_df <- as.data.frame(climate_raster, xy = T)
colnames(raster_df) <- c("x", "y", "class")

# Join the landscape metrics back to their physical locations
spatial_metrics_df <- left_join(raster_df, metrics_wide, by = "class")

# Turn the dataframe back into a multi-layer SpatRaster
isolation_maps <- rast(spatial_metrics_df, type = "xyz", crs = crs(climate_raster))

# Crop and mask to Peru's exact borders to clean up the edges
peru_GLANCE <- project(peru, "EPSG:10603")
isolation_maps <- crop(isolation_maps, peru_GLANCE, mask=T)

plot(isolation_maps$class)
plot(isolation_maps$ai, main='Aggregation index')
plot(isolation_maps$cohesion, main='Cohesion')
plot(isolation_maps$tca, main='Total core area')

plot(isolation_maps[["cohesion"]], 
     main = "Climate Cohesion in Peru\n(Low values = Highly Isolated)", 
     col = terrain.colors(100))

plot(isolation_maps[["np"]], 
     main = "Number of Climate Fragments per Pixel Niche", 
     col = rev(topo.colors(100)))

writeRaster(isolation_maps$class, "climate_classes.tif",
            overwrite=T)
writeRaster(isolation_maps$cohesion, "cohesion.tif",
            overwrite=T)
writeRaster(isolation_maps$ai, "aggregation.tif",
            overwrite=T)


# Basic histogram of climate class area
ggplot(metrics_wide, aes(x = ca)) +
  geom_histogram(fill = "dodgerblue", color = "white") +
  labs(
    title = "Climate class total area",
    x = "Hectares",
    y = "Frequency"
  ) +
  theme_classic()


### PCA with all bioclim vars?
# worldclim_test <- terra::extract(bioclim, sample_pts)
# 
# pca_all <- terra::princomp(bioclim, cor=T, maxcell=1000)
# summary(pca_all)
# loadings(pca_all)
# pc_maps <- predict(bioclim, pca_all)
# plot(pc_maps[[c("Comp.1", "Comp.2")]])
# 
# # biplot
# # 1. Extract the sample data matrix cleanly
# sampled_data <- terra::spatSample(bioclim, size = 1000, method = "regular", na.rm = TRUE)
# scaled_data <- scale(as.matrix(sampled_data))
# 
# # 2. Re-calculate PC matrix positions (selecting first 2 comps)
# calculated_scores <- scaled_data %*% pca_all$loadings
# scores_pc1_pc2 <- calculated_scores[, c(1,2)]
# 
# # 3. Extract the clean variable loadings matrix for the first 2 comps
# # (Explicitly converting the loadings object to a standard numeric matrix)
# loadings_matrix <- as.matrix(pca_all$loadings)[, c(1,2)]
# 
# # 4. Use the default biplot engine directly by explicitly providing x and y matrices
# biplot(x = scores_pc1_pc2, 
#        y = loadings_matrix,
#        cex = c(0.5, 0.9), 
#        col = c("darkgray", "red"),
#        xlab = "Comp 1", 
#        ylab = "Comp 2",
#        main = "Worldclim Peru: all bioclim vars")

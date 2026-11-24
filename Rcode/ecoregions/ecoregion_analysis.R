## 10-19-26

library(terra)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/ecoregions")

worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")
peru <- subset(worldcountries, worldcountries$COUNTRY=='Peru')
peru <- project(peru, "EPSG:4326")
peru_GLANCE <- project(peru, "EPSG:10603")

countries_sub <- c('Ecuador','Peru','Bolivia','Colombia')
countries_aoi <- subset(worldcountries, worldcountries$COUNTRY %in% countries_sub)
plot(countries_aoi)
countries_aoi <- subset(countries_aoi, countries_aoi$LAND_TYPE=='Primary land')
plot(countries_aoi)
countries_aoi <- project(countries_aoi, "EPSG:4326")
countries_aoi_GLANCE <- project(countries_aoi, "EPSG:10603")

# caution: large dataset
# Sayre et al. (2020): https://data.usgs.gov/datacatalog/data/USGS:6296791ed34ec53d276bb293
world_ecosystems <- rast("WorldEcosystems/WorldEcosystems.tif")

# "activate" world ecosystem attribute (finest detail)
activeCat(world_ecosystems) <- "W_Ecosystm" 

# clip to area of interest
#world_ecosystems_peru <- crop(world_ecosystems, peru, mask=T)
#plot(world_ecosystems_peru)

world_ecosystems_aoi <- crop(world_ecosystems, countries_aoi, mask=T)
plot(world_ecosystems_aoi)

# reproject to GLANCE
#world_ecosystems_peru <- project(world_ecosystems_peru, "EPSG:10603", method="near",
#                                 res=c(250,250))#,
                                 #filename="WorldEcosystems/WorldEcosystems_Peru.tif",
                                 #overwrite=T)

world_ecosystems_aoi <- project(world_ecosystems_aoi, "EPSG:10603", method="near",
                                 res=c(250,250),
          filename="WorldEcosystems/WorldEcosystems_pilot.tif",
          overwrite=T)
# get raster attribute table
world_ecosystems_aoi_df <- as.data.frame(cats(world_ecosystems_aoi))

# However, the count column is for the whole world and we need just our
# country subset
freq_table <- as.data.frame(freq(world_ecosystems_aoi))

world_ecosystems_aoi_df <- merge(world_ecosystems_aoi_df[,c(1:11)], freq_table,
              by.x='W_Ecosystm', by.y='value', all=F)

# Calculate relative abundance of different ecosystem classes
world_ecosystems_aoi_df$rarity_rank <- rank(world_ecosystems_aoi_df$count,
                                      ties.method="average")
world_ecosystems_aoi_df$pct_coverage <- round(world_ecosystems_aoi_df$count/sum(world_ecosystems_aoi_df$count), 4)*100
world_ecosystems_aoi_df$areasqkm_coverage <- world_ecosystems_aoi_df$count * 0.0625
world_ecosystems_aoi_df$ID <- world_ecosystems_aoi_df$Value

# rare ecosystems < 1 km2
world_ecosystems_aoi_1km <- subset(world_ecosystems_aoi_df, areasqkm_coverage < 1)
#write.csv(world_ecosystems_peru_1km, "world_ecosystems_peru_1km.csv", row.names=F)

# Identify individual ecosystems that take up less than 1% of area of interest
world_ecosystems_aoi_1pct <- subset(world_ecosystems_aoi_df, pct_coverage <=1)$ID

# Force extract the attribute table, preserving categorical data
rat <- as.data.frame(cats(world_ecosystems_aoi)[1])

# Match strings using position-based indexing (Column 1 ID, Column 2 is text)
matching_ids <- rat[rat[, 2] %in% world_ecosystems_aoi_1pct, 1]

# Create binary T/F mask
tf_mask <- world_ecosystems_aoi %in% matching_ids

# Mask the original raster (keeps original values, turns FALSE into NA)
world_ecosystems_aoi_1pct <- mask(world_ecosystems_aoi, tf_mask, maskvalue = F)
plot(world_ecosystems_aoi_1pct)

#hist(world_ecosystems_aoi_df$areasqkm_coverage, main='World ecosystems',
#     xlab='sq km', breaks=seq(0,1500000,1000))

hist(world_ecosystems_aoi_df$pct_coverage, main='World ecosystems',
     xlab='Percent coverage', xlim=c(0,50),
     breaks=seq(0,50,0.5))

#### Extract rare ecosystems for landscape metrics analysis ####
# test_eco <- 203 #ecosystem ID
# 
# rat <- as.data.frame(cats(world_ecosystems_peru))
# 
# # Find the spatial ID where Value column equals desired ecosystem ID
# # Column 1 is spatial ID, Column 3 is 'Value')
# spatial_id <- rat[rat[, 1] == test_eco, 1]
# 
# # Create mask and extract original raster values
# single_mask <- world_ecosystems_peru %in% spatial_id
# single_ecosystem_raster <- mask(world_ecosystems_peru, single_mask, maskvalue = FALSE)
# 
# plot(peru_GLANCE)
# plot(single_ecosystem_raster, add=T, col='red')
# 
# patches <- patches(single_ecosystem_raster, directions=8, allowGaps=F)
# patches
# lsm_l_np(single_ecosystem_raster, directions=8)
# 
# lsm_data <- calculate_lsm(single_ecosystem_raster, level="patch",
#                           directions=8, edge_depth=1,
#                           metric=c('ncore','area','core','cai','enn'))

#### Work with climate region (Temp_Moist) instead ####
activeCat(world_ecosystems) <- "Temp_Moist" 

# clip to AOI
#world_ecosystems_aoiTM <- crop(world_ecosystems, countries_aoi, mask=T)
#plot(world_ecosystems_aoiTM)

# update: buffered area to allow "patches" to bleed over country borders
countries_aoi_50kmbuff <- terra::buffer(countries_aoi, width=50000)
countries_aoi_50kmbuff_dissolved <- terra::aggregate(countries_aoi_50kmbuff, dissolve=T)

world_ecosystems_aoiTM <- crop(world_ecosystems, countries_aoi_50kmbuff_dissolved, mask=T)
plot(world_ecosystems_aoiTM)

# reproject to GLANCE
world_ecosystems_aoiTM <- project(world_ecosystems_aoiTM, "EPSG:10603", method="near",
                                 res=c(1000,1000))#,
            #filename="WorldEcosystems/WorldEcosystems_aoiTM_1km.tif",
            #overwrite=T)

# frequency table
aoiTM_df <- as.data.frame(freq(world_ecosystems_aoiTM$Temp_Moist))

# need to rerun with summarize to aggregate Temp_Moist field
aoiTM_df <- aoiTM_df %>%
  group_by(value) %>%
  summarize(sum=sum(count, na.rm=T)) %>%
  as.data.frame()
names(aoiTM_df) <- c('Temp_Moist', 'Sum')

aoi_GLANCE_sqkm <- sum(terra::expanse(countries_aoi_GLANCE, "km"))
aoiTM_df$areasqkm <- aoiTM_df$Sum * 0.0625
#aoiTM_df$pct <- (aoiTM_df$areasqkm/aoi_GLANCE_sqkm)*100
aoiTM_df$pct <- (aoiTM_df$Sum/sum(aoiTM_df$Sum, na.rm=T))*100
aoiTM_df$rarity_rank <- rank(aoiTM_df$Sum, ties.method="average")

rat_df <- as.data.frame(cats(world_ecosystems_aoiTM)[])

# 2. Extract every single unique climate zone present in your column
# (Using the literal name we verified from your unique check earlier)
unique_labels <- unique(rat_df$Temp_Moist)

# 3. Create a reclassification table matching every raw ID to a clean class number (1 to 18)
mapping_df <- data.frame(
  from = rat_df$Value,
  to = match(rat_df$Temp_Moist, unique_labels)
)

# 4. Reclassify the raster cell values into these new, sequential class IDs
lsm_ready_raster <- classify(world_ecosystems_aoiTM, mapping_df)
names(lsm_ready_raster) <- "layer"
lsm_ready_raster <- terra::trim(lsm_ready_raster)

check_landscape(lsm_ready_raster)

#global_df <- cats(world_ecosystems)[[1]]

# 2. Extract the actual original attribute table from your world_ecosystems metadata
global_rat <- cats(world_ecosystems)[[1]]

# 3. Group by the unique character categories of Temp_Moist to see how 
# they match up sequentially in your environment
true_mapping <- global_rat %>%
  distinct(Temp_Moist) %>%
  mutate(class_id = row_number()) # Or match this with your unique ID logic

class_metrics <- calculate_lsm(lsm_ready_raster, 
                      level = "class", 
                      metric = c("np", "ca", "area", "cai", 
                                 "tca", "enn", "cohesion", "ai", "ndca"),
                      directions=8, #default=8 (queen)
                      edge_depth=4) #default is 1 pixel

metrics_wide <- class_metrics %>%
  select(class, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

metrics_wide <- merge(metrics_wide, true_mapping, by.x='class', by.y='class_id')

ggplot(metrics_wide, aes(x = reorder(Temp_Moist, ai), y = ai, fill = Temp_Moist)) +
  geom_col(color = "black", alpha = 0.8, width = 0.7) +
  coord_flip() +  # Flips the axes so long climate names are easy to read
  scale_fill_viridis_d(option = "mako", guide = "none") + # Clean color palette, hides redundant legend
  theme_classic(base_size = 13) +
  labs(
    title = "Aggregation Index",
    subtitle = "World Ecosystems",
    x = "Climate Region (Temp Moist)",
    y = "Aggregation Index (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.text.y = element_text(color = "black")
  )

ggplot(metrics_wide, aes(x = reorder(Temp_Moist, enn_mn), y = ai, fill = Temp_Moist)) +
  geom_col(color = "black", alpha = 0.8, width = 0.7) +
  coord_flip() +  # Flips the axes so long climate names are easy to read
  scale_fill_viridis_d(option = "mako", guide = "none") + # Clean color palette, hides redundant legend
  theme_classic(base_size = 13) +
  labs(
    title = "Average nearest neighbor distance",
    subtitle = "World Ecosystems",
    x = "Climate Region (Temp Moist)",
    y = "Distance (m)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.text.y = element_text(color = "black")
  )

## patch level analysis
patch_metrics <- calculate_lsm(lsm_ready_raster,
                               level = "patch",
                               metric = c("shape", "area", "cai", "core",
                                          "ncore", "contig", "enn"),
                               directions=8, #default=8 (queen)
                               edge_depth=1) #default is 1 pixel

patch_metrics_wide <- patch_metrics %>%
  select(id, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

# enn_p <- lsm_p_enn(lsm_ready_raster, directions=8)
# enn_p <- merge(enn_p, true_mapping, by.x='class', by.y='class_id')
# 
# 
# ggplot(enn_p, aes(x = reorder(Temp_Moist, value, FUN = median, na.rm = TRUE), 
#                   y = value, 
#                   fill = Temp_Moist)) +
#   geom_boxplot(color = "black", alpha = 0.7, outlier.alpha = 0.3, outlier.size = 1) +
#   scale_fill_viridis_d(option = "mako", guide = "none") + 
#   theme_classic(base_size = 13) +
#   labs(
#     title = "Nearest Neighbor Distance by Climate Zone",
#     x = "Climate Zone (Temp_Moist)",
#     y = "Distance (m)"
#   ) +
#   coord_cartesian(ylim = c(0, 15000)) + 
#   # Rotate x-axis text 45 degrees and align it to the ticks
#   theme(
#     plot.title = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1, color = "black")
#   )
# 
# area_p <- lsm_p_area(lsm_ready_raster, directions=8)
# area_p <- merge(area_p, true_mapping, by.x='class', by.y='class_id')
# 
# 
# ggplot(area_p, aes(x = reorder(Temp_Moist, value, FUN = median, na.rm = TRUE), 
#                   y = value, 
#                   fill = Temp_Moist)) +
#   geom_boxplot(color = "black", alpha = 0.7, outlier.alpha = 0.3, outlier.size = 1) +
#   scale_fill_viridis_d(option = "mako", guide = "none") + 
#   theme_classic(base_size = 13) +
#   labs(
#     title = "Patch area by Climate Zone",
#     x = "Climate Zone (Temp_Moist)",
#     y = "Area (sqm)"
#   ) +
#   coord_cartesian(ylim = c(0, 15000)) + 
#   # Rotate x-axis text 45 degrees and align it to the ticks
#   theme(
#     plot.title = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1, color = "black")
#   )

patch_rasters <- spatialize_lsm(lsm_ready_raster, 
                       level = "patch", 
                       metric = c("shape", "area", "cai", "core",
                                  "ncore", "contig", "enn"),
                       directions=8, #default=8 (queen)
                       edge_depth=1)

p_area <- patch_rasters$layer_1$lsm_p_area
#writeRaster(p_area, filename="WorldEcosystems/ecoregion_patches/pilot_p_area.tif",
#            overwrite=T)

p_shape <- patch_rasters$layer_1$lsm_p_shape
#writeRaster(p_shape, filename="WorldEcosystems/ecoregion_patches/pilot_p_shape.tif",
#            overwrite=T)

p_ncore <- patch_rasters$layer_1$lsm_p_ncore
#writeRaster(p_ncore, filename="WorldEcosystems/ecoregion_patches/pilot_p_ncore.tif",
#            overwrite=T)

p_p_core <- patch_rasters$layer_1$lsm_p_core
#writeRaster(p_p_core, filename="WorldEcosystems/ecoregion_patches/pilot_p_core.tif",
#            overwrite=T)

p_cai <- patch_rasters$layer_1$lsm_p_cai
#writeRaster(p_cai, filename="WorldEcosystems/ecoregion_patches/pilot_p_cai.tif",
#            overwrite=T)

p_contig <- patch_rasters$layer_1$lsm_p_contig
#writeRaster(p_contig, filename="WorldEcosystems/ecoregion_patches/pilot_p_contig.tif",
#            overwrite=T)

p_enn <- patch_rasters$layer_1$lsm_p_enn
#writeRaster(p_enn, filename="WorldEcosystems/ecoregion_patches/pilot_p_enn.tif",
#            overwrite=T)

#patches_polygons <- as.polygons(p_area, aggregate=F, values=T)



ecoregion_patches <- get_patches(lsm_ready_raster, class="all", directions=8)

class_rasters <- ecoregion_patches$layer_1
class_names <- names(class_rasters)

# 2. Offset patch IDs across classes so every patch in the raster has a unique ID
id_offset <- 0
patch_rasters <- list()

for (i in seq_along(class_rasters)) {
  r_patch <- class_rasters[[i]] + id_offset
  max_id <- as.numeric(global(r_patch, "max", na.rm = TRUE))
  
  if (!is.na(max_id)) {
    id_offset <- max_id
    patch_rasters[[i]] <- r_patch
  }
}

# 3. Merge into a single SpatRaster with unique patch IDs as cell values
p_id_raster <- cover(rast(patch_rasters))

# Verify cell values are unique patch IDs (1 to total patches)
plot(p_id_raster)

# Write directly to disk for ArcGIS Pro or downstream analysis
writeRaster(p_id_raster, "unique_patch_ids.tif", overwrite = TRUE)





writeRaster(ecoregion_patches, filename="WorldEcosystems/ecoregion_patches/ecoregion_patches_pilot.tif",
            overwrite=T)
ecoregion_patches_polygons <- as.polygons(ecoregion_patches, values=T)
ecoregion_patches_polygons$patchID <- seq(1, nrow(ecoregion_patches_polygons), 1)

ecoregion_patches_polygons <- merge(ecoregion_patches_polygons, patch_metrics_wide,
              by.x='patchID', by.y='id')
#writeVector(ecoregion_patches_polygons, filename='WorldEcosystems/ecoregion_patches/ecoregion_patches_polygons_pilot.shp',
#            overwrite=T)

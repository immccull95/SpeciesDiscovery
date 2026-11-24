## 8-14-26

library(CopernicusDEM)
library(terra)
library(sf)
library(aws.s3)

worldcountries <- vect("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/WorldCountries/World_Countries.shp")

# identify country of interest
country <- subset(worldcountries, worldcountries$COUNTRY=='Colombia')
country <- project(country, "EPSG:4326") #for compatibility with DEM download
plot(country)

# mainland only
country <- subset(country, country$LAND_TYPE=='Primary land')
plot(country)

country_sf <- st_as_sf(country)
country_sf <- st_make_valid(country_sf) #avoid topology errors below

# AWS shenanigans
#my_data <- s3read_using(FUN = read.csv, 
#                        bucket = "bucket-name",
#                        object = "path/to/file.csv", 
#                        region = "eu-central-1",
#                        anonymous = T)

my_data <- read.csv("https://amazonaws.com")

# may still run into topology errors related to the bounding box
# 1. Grab existing bounding box coordinates
bbox <- sf::st_bbox(country_sf)

# 2. Rebuild the matrix coordinates manually, ensuring the path closes (starts and ends at the same point)
# Path: Bottom-Left -> Bottom-Right -> Top-Right -> Top-Left -> Bottom-Left
lon_coords <- c(bbox["xmin"], bbox["xmax"], bbox["xmax"], bbox["xmin"], bbox["xmin"])
lat_coords <- c(bbox["ymin"], bbox["ymin"], bbox["ymax"], bbox["ymax"], bbox["ymin"])

# 3. Create a structurally perfect polygon object
country_polygon <- sf::st_polygon(list(matrix(c(lon_coords, lat_coords), ncol = 2)))

# 4. Turn it into a spatial geometry column and explicitly assign the WGS84 CRS string
country_aoi_clean <- sf::st_sfc(country_polygon, crs = 4326)

# 5. Disable s2 temporarily just to ensure the package handles the data layout smoothly
sf::sf_use_s2(FALSE)

# Create directory to save downloaded DEM tiles
download_dir <- file.path(getwd(), "colombia")
if (!dir.exists(download_dir)) dir.create(download_dir)

# Re-confirm your custom AWS path and anonymous login
Sys.setenv(PATH = paste(Sys.getenv("PATH"), paste0(Sys.getenv("LOCALAPPDATA"), "\\Programs\\Amazon\\AWSCLIV2"), sep = ";"))
Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")

# Get the exact list of required S3 URLs for AOI without downloading them yet
tile_matches <- CopernicusDEM::aoi_geom_save_tif_matches(
  sf_or_file = country_aoi_clean,
  dir_save_tifs = download_dir,
  resolution = 90,
  crs_value = 4326,
  threads = parallel::detectCores(),
  verbose = T
)

# Create a clean list of AWS CLI commands
# The package returns a vector of the matching S3 links
s3_paths <- unlist(tile_matches)
total_files <- length(s3_paths)

# 3. Create a live tracking bar
pb <- txtProgressBar(min = 0, max = total_files, style = 3)
cat(sprintf("\nStarting download of %d tiles directly to: %s\n", total_files, download_dir))

# 4. Download cleanly one-by-one
for(i in 1:total_files) {
  current_path <- s3_paths[i]
  
  # Extract just the folder name to build the file target (e.g., "Copernicus_DSM_COG_10_S01_00_W069_00_DEM")
  folder_name <- basename(current_path)
  
  # Construct the precise path to the underlying .tif file inside that S3 folder
  s3_file_url <- sprintf("s3://copernicus-dem-90m/%s/%s.tif", folder_name, folder_name)
  
  # Build a direct copy command targeting your local download directory
  cmd <- sprintf("aws s3 cp %s \"%s/\" --no-sign-request", s3_file_url, download_dir)
  
  # Run the command silently
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

close(pb)
cat("\nDownload Complete!\n")

# Download the intersecting DEM tiles from AWS Open Data
# Resolution: 90 m (for GLO-90)
# downloaded_files <- CopernicusDEM::aoi_geom_save_tif_matches(
#   sf_or_file = peru_aoi_clean,
#   dir_save_tifs = download_dir,
#   resolution = 90,
#   crs_value = 4326,
#   threads = parallel::detectCores(),
#   verbose = T
# )

# Re-enable spherical checking
#sf::sf_use_s2(TRUE)

## mosaic rasters
raster_dir <- download_dir
#raster_dir <- paste0(download_dir, "/90m")
raster_files <- list.files(path = raster_dir, pattern = "\\.tif$", full.names = T)

# Read all rasters into a SpatRaster collection
raster_list <- lapply(raster_files, rast)

# Convert the list into a SpatRasterCollection
raster_src <- sprc(raster_list)

# Mosaic the rasters together and reproject to GLANCE South America
# cropped to country boundary
# https://epsg.io/10603
country_proj <- project(country, "EPSG:10603")
mosaic_output <- mosaic(raster_src, fun = "mean")
mosaic_output_proj <- project(x=mosaic_output, y="EPSG:10603", res=c(90,90),
                              method="bilinear")
mosaic_output_cropped <- crop(mosaic_output_proj, country_proj, mask=T,
                              filename="colombia/colombia_DEM_90m_mosaic.tif",
                              overwrite=T)

## Terrain ruggedness index
# might actually make sense to calculate TRI before cropping
TRI <- terrain(mosaic_output, "TRI")
TRI_proj <- project(x=TRI, y="EPSG:10603", res=c(90,90),
                    method="bilinear")
TRI_cropped <- crop(TRI_proj, country_proj, mask=T,
                    filename="colombia/colombia_TRI_90m.tif",
                    overwrite=T)

# Try a larger 11x11 cell neighborhood; default above is 3x3 cells
# 11x90 = 990m, approximately 1km 
medium_matrix <- matrix(1, nrow=11, ncol=11)
TRI_medium <- focal(mosaic_output, w = medium_matrix, fun = function(x) {
  center_pixel <- x[61] #this is the center pixel
  mean(abs(x - center_pixel), na.rm = T)
})
TRI_medium_proj <- project(x=TRI_medium, y="EPSG:10603", res=c(90,90),
                           method="bilinear")
TRI_medium_cropped <- crop(TRI_medium_proj, country_proj, mask=T,
                           filename="colombia/colombia_TRI_90m_11x11.tif",
                           overwrite=T)

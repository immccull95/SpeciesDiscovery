#9-21-26

library(terra)
library(geodata)

setwd("C:/Users/imccullough/Dropbox/SpeciesDiscovery/Data/climate")


# Download bioclimatic variables for country at ~ 1km resolution
# will save to working directory by default
# info: https://www.worldclim.org/data/bioclim.html
country <- 'peru'
dir.create(country)
outpath <- paste0(getwd(), "/", country)
country_code <- "PER"
peru_clim <- worldclim_country(country=country_code, var="bio", res=0.5,
                               path=outpath)

## For multiple countries at once
# PER = Peru, ECU = Ecuador, BOL = Bolivia, COL = Colombia
country_codes <- c("PER", "ECU", "BOL", "COL")
path_dir <- paste0(getwd(), "/pilot")

# 2. Download bioclim variables
# Other var options: "tmin", "tmax", "tavg", "prec", "vapr", "wind", "srad"
global_climate <- worldclim_global(
  var = "bio", 
  res = 0.5,       # Resolution choices: 10, 5, 2.5, or 0.5 (minutes of a degree)
  path = path_dir, 
  version = "2.1"
)

# 3. Download and combine the spatial boundaries for all four countries
region_boundaries <- gadm(
  country = country_codes, 
  level = 0, 
  path = path_dir
)

# 4. Crop to the bounding box of the region, then mask to the exact borders
climate_region <- crop(global_climate, region_boundaries, mask=T)

# Plot the result to verify
plot(climate_region[[1]], main = "Bio1 (Annual Mean Temperature)")
plot(region_boundaries, add = T, border = "black")

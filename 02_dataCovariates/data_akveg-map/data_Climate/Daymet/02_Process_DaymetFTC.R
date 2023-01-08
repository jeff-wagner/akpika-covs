# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Process Daymet Data
# Author: Jeff Wagner
# Last Updated: 2022-12-30  
# Usage: Must be executed in R 4.0.0+.
# Description: "Process Daymet Data" processess Daymet daily minimum temperature data to create a 1-km raster of 20-year normalized freeze-thaw cycles (FTCs)
# ---------------------------------------------------------------------------

library(terra)
library(ggplot2)
library(dplyr)
library(data.table)

# Define paths
data.folder = 'C:/Users/jeffw/iCloudDrive/Projects/Pika/Data/Daymet'
files <- list.files(data.folder, full.names = TRUE)[3:22]

# Create a raster stack
r.tmin <- terra::rast(files, drivers = "NETCDF")

# Set the correct projection
crs(r.tmin) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"

tmin <- terra::as.array(r.tmin)

# Calculate number of FTC at each cell
# Define FTC function
FTC <- function(x){
  df <- x %>%
    as_tibble() %>%
    mutate(a1 = . > 0,
           a2 = lag(.) < 0,
           cross = a1 == a2)
  FTC <- sum(df$cross, na.rm = T)/2
  return(FTC)
}

# Initialize variables
raster <- list()
n <- list()
tmin <- list()

# Loop through each year & calculate the FTC for each cell
for( year in 1:length(files)){
  #raster[[year]] <- terra::rast(files[year])
  #n[[year]] <- matrix(data = NA, nrow = 854, ncol = 802)
  #tmin[[year]] <- as.array(raster[[year]])
  
  for(i in 1:nrow(tmin[[year]])){
    for(j in 1:ncol(tmin[[year]])){
      n[[year]][i,j] <- FTC(tmin[[year]][i,j,])
    }
  }
  
}

# Calculate mean for each cell across years
n.mean <- apply(simplify2array(n), 1:2, mean)

# Create raster object
ftc <- rast(n.mean, crs = crs(r.tmin), extent = ext(r.tmin))

# Export
terra::writeRaster(ftc, '/Users/jeff/GitHub/akpika-density/data/ftc_2000_2019.tif')

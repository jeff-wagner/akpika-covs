# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Process ABoVE Snow Depth Data
# Author: Jeff Wagner
# Last Updated: 2022-12-30  
# Usage: Must be executed in R 4.0.0+.
# Description: "Process Daymet Data" processess Daymet daily minimum temperature data to create a 1-km raster of 20-year normalized freeze-thaw cycles (FTCs)
# ---------------------------------------------------------------------------

library(terra)
library(processNC)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

# Define paths
root_folder = 'C:/Users/jeffw/iCloudDrive/Projects/Pika'
data_folder = paste(root_folder,
                    'Data/ABoVE',
                    sep = '/')

files <- list.files(data_folder, full.names = TRUE)

# Create SpatRaster layers for snow extent and snow depth
snow <- rast(files)
snow.depth <- subset(snow, 783:1564)
snow.extent <- subset(snow, 1:782)

# Create vector of dates
dates <- time(snow.depth)

# Retrieve positions of winter dates (Dec - Apr)
winter <- which(month(dates) %in% c(1:4,12))

# Create subset for winter snow depths only
winter.sd <- subset(snow.depth, winter)

mean.winter.sd <- mean(winter.sd, na.rm = TRUE)

terra::writeRaster(mean.winter.sd, './data/ABoVE_meanSnowDepth_2001_2017.tif')


# Convert to arrays
snow.depth.arr <- terra::as.array(snow.depth)
snow.extent.arr <- terra::as.array(snow.extent)

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

# Read in site data
sites <- vect('./data/Transect_Export.gdb/')

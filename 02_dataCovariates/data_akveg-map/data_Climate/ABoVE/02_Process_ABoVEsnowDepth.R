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
root_folder = 'C:/Users/jeffw/OneDrive/Desktop/GISdata'
data_folder = paste(root_folder,
                    'ABoVE',
                    sep = '/')

files <- list.files(data_folder, full.names = TRUE)

# Create SpatRaster layers for snow extent and snow depth
snow <- rast(files)
snow.depth <- subset(snow, 783:1564)
snow.extent <- subset(snow, 1:782)

# Create vector of dates
dates <- time(snow.depth)

# Retrieve positions of winter dates (Dec - Mar)
winter <- which(month(dates) %in% c(1:3,12))

# Create subset for winter snow depths only
winter.sd <- subset(snow.depth, winter)
winter.se <- subset(snow.extent, winter)

# Calculate mean across years
mean.winter.sd <- mean(winter.sd, na.rm = TRUE)

# Reproject using study area template
temp <- rast('C:/Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp/data/AlaskaPika_TotalArea_1.tif')
mean.sd.proj <- project(mean.winter.sd, crs(temp))

terra::writeRaster(mean.sd.proj, paste(root_folder,
                                         'climatology/snowdepth/unprocessed/1km/ABoVE_meanSnowDepth_2001_2017.tif',
                                         sep = '/'), overwrite = TRUE)


# Convert to arrays
winter.sd.arr <- terra::as.array(winter.sd)
winter.se.arr <- terra::as.array(winter.se)

# Calculate number of zeros in each cell
# Define count zero function
count.zero <- function(x){
  df <- x %>%
    as_tibble() %>%
    mutate(a1 = . == 0)
  zeros <- sum(df$a1, na.rm = FALSE)
  return(zeros)
}

# Define count snow-free function
count_snowFree <- function(x){
  df <- x %>%
    as_tibble() %>%
    mutate(a1 = . == 25)
  snowFree <- sum(df$a1, na.rm = FALSE)
  return(snowFree)
}

# Initialize variables
n.sd <- matrix(data = NA, nrow = 1650, ncol = 1776)
n.se <- matrix(data = NA, nrow = 1650, ncol = 1776)

# Loop through each layer & calculate total zeros for each cell
  for(i in 1:nrow(winter.sd.arr)){
    for(j in 1:ncol(winter.sd.arr)){
      n.sd[i,j] <- count.zero(winter.sd.arr[i,j,])
    }
  }

for(i in 1:nrow(snow.extent.arr)){
  for(j in 1:ncol(snow.extent.arr)){
    n.se[i,j] <- count_snowFree(snow.extent.arr[i,j,])
  }
}

# Create raster object
sd.cycles <- rast(n.sd, crs = crs(winter.sd), extent = ext(winter.sd))
se.cycles <- rast(n.se, crs = crs(winter.se), extent = ext(winter.se))

# Reproject using study area template
sd.cycles.proj <- project(sd.cycles, crs(temp))
se.cycles.proj <- project(se.cycles, crs(temp))

# Export
terra::writeRaster(sd.cycles.proj, 'C:/Users/jeffw/OneDrive/Desktop/GISdata/climatology/snowmelt/unprocessed/1km/ABoVE_SnowMeltCycles_2001_2017.tif')
terra::writeRaster(se.cycles.proj, 'C:/Users/jeffw/OneDrive/Desktop/GISdata/climatology/snowextent/unprocessed/1km/ABoVE_SnowExtentCycles_2001_2017.tif')

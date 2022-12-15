# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Distance Class Probabilities
# Author: Jeff Wagner
# Last Updated: 2022-07-21
# Usage: Must be executed in R 4.0.0+.
# Description: "Distance Class Probabilities" imports multiple ring buffers around each replicate survey track and calculates the proportional area of each site covered by each distance bin.
# ---------------------------------------------------------------------------

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source( "00_init/initscript.r")

# Define data folder
data_folder = './data'

# Define input folders
sp_folder = paste(data_folder,
                    'Transect_Export.gdb',
                    sep = '/')

# List input files
sp_data <- st_layers(sp_folder)
files <- sp_data$name[c(233:261,263:348)]

# Read in multiple ring buffer feature classes
buffers <- lapply(files, st_read, dsn=sp_folder)

# Define names
names(buffers) <- sapply(strsplit(files, "_"), function(x) paste(x[2],x[3], sep = "_"))
buffers <- buffers[order(names(buffers))] 
tracks <- readRDS("./data/tracks.RData") # Read in track data to find matches
names(buffers) == tracks$t.match # Check that everything matches
names(buffers) <- tracks$transect  # Redefine names to match tracks

# Calculate proportion of total area for each bin
area_sum <- NA
for(i in 1:length(buffers)){
  area_sum[i] <- sum(buffers[[i]]$SHAPE_Area)
  buffers[[i]]$p.area <- buffers[[i]]$SHAPE_Area/area_sum[i]
  print(paste(names(buffers[i]), "areas sum to", sum(buffers[[i]]$p.area), sep = " ")) # everything should sum to 1
}


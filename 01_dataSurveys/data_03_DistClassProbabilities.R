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
  buffers[[i]] <- filter(buffers[[i]], distance < 70)
  area_sum[i] <- sum(buffers[[i]]$SHAPE_Area)
  buffers[[i]]$p.area <- buffers[[i]]$SHAPE_Area/area_sum[i]
  print(paste(names(buffers[i]), "areas sum to", sum(buffers[[i]]$p.area), sep = " ")) # everything should sum to 1
  buffers[[i]]$site <- gsub("[.][1-4]",'', names(buffers[i]))
  buffers[[i]]$replicate <- gsub(".*[.]",'',names(buffers[i]))
  buffers[[i]]$site_area <- sum(buffers[[i]]$SHAPE_Area)
}

df <- plyr::ldply(buffers) %>% 
  rename("dclass"="OBJECTID") %>% 
  mutate("Site" = as.numeric(as.factor(site)))


testarr <- table(factor(df$site, levels = levels(as.factor(df$site))),
                 df$dclass~, df$replicate)

nsites <- 47
nD <- 13
K <- 4

pi3d<-array(data=NA, dim=c(nsites, nD, K))

for(k in 1:K){
  for(d in 1:nD){
    for(s in 1:nsites){
      pi3d[s,d,k] <- ifelse(!is.na(subset(df, Site == s & dclass == d & replicate == k)$p.area), subset(df, Site == s & dclass == d & replicate == k)$p.area, NA)
    }
  }
}

for(s in 1:nsites){
  for(d in 1:nD){
    for(k in 1:K){
      pi3d[s,d,k] <- ifelse(testarr[s,d,k]==1, subset(df, Site == s & dclass == d & replicate == k)$p.area, 0)
    }
  }
}


sa_meta <- table(factor(df$site, levels = levels(as.factor(df$site))),
                 df$replicate)
  
site_area <- matrix(data = NA, nrow = 47, ncol = 4, dimnames = list(unique(df$Site),
                                                                    unique(df$replicate)))

for(s in 1:nsites){
  for(k in 1:K){
    site_area[s,k] <- ifelse(sa_meta[s,k]>1, subset(df, Site == s & replicate == k)$site_area, 0)
  }
}

save(pi3d, site_area, file = './data/spData.RData')

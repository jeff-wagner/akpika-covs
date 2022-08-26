# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Vegetation survey data
# Author: Jeff Wagner
# Last Updated: 2022-07-21
# Usage: Must be executed in R 4.0.0+.
# Description: "Vegetation survey data" wrangles the data from the site info datasheet and the 10x10m vegetation plots that were conducted at each site.
# ---------------------------------------------------------------------------

# We are interested in categorizing the dominant vegetation types around the talus
# at each site, the mean height of the dominant vegetation type from the vegetation plots, and the % low shrub
# from the vegetation plots. 

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source("initscript.r")

# Part 1: Dominant vegetation type around the talus  ------------------------------------------------------------
# Read in the site description datasheet
siteDesc <- read_excel(path = "./data/2018_and_2019_SiteInfo.11.8.2019.xlsx", 
                       sheet = "50mViereck", na = 'NA')

# Remove rows with NA for PercentCover
siteDesc <- siteDesc %>% 
              filter(!is.na(PercentCover) == TRUE) # removed one row


# We are only interested in the dominant vegetation type, so we will just keep the row with the maximum PercentCover
# for each site.

sites <- unique(siteDesc$Site) # store unique sites for loop

domVeg <- data.frame(Site = sites, Viereck = NA, PercentCover = NA) # create empty dataframe for loop

for(i in 1:length(sites)){
  a <- subset(siteDesc, Site==sites[i])
  max <- max(a$PercentCover)
  domVeg[i, "PercentCover"] <- max
  domVeg[i, "Viereck"] <- siteDesc %>% 
                            filter(Site == sites[i]) %>% 
                            select(Viereck, PercentCover) %>% 
                            filter(PercentCover==max(PercentCover)) %>% 
                            select(Viereck)
}

unique(domVeg$Viereck)
# Some sites had two 'dominant' vegetation types (equal % cover). We will just keep the first one for now, so we can
# ignore the warnings.

# Simplify the veg type names using abbreviations. For example, "ericaceous dwarf shrub" will become "eds".
# We will also condense 'closed' and 'open' shrubs into the same category
domVeg <- domVeg %>% 
              mutate(vegclass = recode(Viereck, 'ericaceous dwarf shrub' = 'eds', 'closed low shrub' = 'ls', 
                                       'closed tall shrub' = 'ts', 'dryas dwarf shrub' = 'dds', 'tall shrub' = 'ts',
                                       'lichen' = 'lic', 'dry graminoid herbaceous' = 'dgh', 'low shrub' = 'ls')) %>% 
              select(-Viereck)

# Sites with missing data -----
# We need to decide what to do with sites that are missing data... for now I will fill them with the most
# common type of vegetation we saw in each region.
# Load in the site summary sheet to retrieve site locations
site.summary <- read_excel(path=path, sheet="Site_Summary")
site.summary <- site.summary %>% 
  select(Site, Location) %>% 
  filter(!duplicated(Site))

# Match sites from domVeg with locations from site.summary
domVeg <- left_join(site.summary, domVeg, by = "Site")
domVeg$Location <- as.factor(domVeg$Location)

# Fill in missing values with most frequent veg type by location
table(domVeg$vegclass, domVeg$Location) 

# The missing sites are in Hatcher Pass and Paxson. 'eds' is the most common veg type in both regions, so we will fill
# in all of the missing values with 'eds'

domVeg$vegclass <- ifelse(is.na(domVeg$vegclass) == TRUE, "eds", domVeg$vegclass)

# We are good to go with this covariate, we just have to clean up the dataframe for loading into the transect covariates
domVeg <- domVeg %>% 
  select(Site, vegclass) %>% 
  mutate(vegclass = as.factor(vegclass))

# Part 2: Mean height of dominant vegetation type from 10x10m veg plots  -----------------------------------------------
veg2018 <- read_excel(path = paste(getwd(), "/data/raw_data/2018_vegPlots.xlsx", sep = ""), 
                      sheet = "General_Cover", na = "NA")
veg2018$Date <- as.POSIXct(veg2018$Date)

veg2019 <- read_excel(path = paste(getwd(), "/data/raw_data/2019_vegPlots.xlsx", sep = ""), 
                      sheet = "vegPlot_generalCover", na = "NA")

# Add column for year and merge two years together
veg2018$year <- 2018
veg2019$year <- 2019

veg <- merge(veg2018, veg2019,all = TRUE)

# Select columns of interest (only vegetation types for which there is a height measurment). We will also fill the NAs with 0s since we can assume 
# the percent cover was 0 if not recorded.
veg <- veg %>% 
  select(Site, `Needleleaf_LF_%`, Needleleaf_H_m, `Broadleaf_LF_%`, Broadleaf_H_m, `TS_LF_%`, TS_H_m, 
         `LS_LF_%`, LS_H_m, `DS_LF_%`, DS_H_m)

veg[, 2:11] <- veg[, 2:11] %>% 
  mutate_all(~replace(., is.na(.), 0))

# Figure out which column contains the maximum value for percent cover (e.g. the dominant veg type)
veg$dom.class <- substr(colnames(veg[, 2:11])[apply(veg[, 2:11], 1, which.max)], 1, 2)
unique(veg$dom.class)

# Dominant veg is always LS or DS, select only those columns
veg.height <- veg %>% 
  select(Site, `LS_LF_%`, LS_H_m, `DS_LF_%`, DS_H_m, dom.class)

# We need to fix some of the DS height values, which appear to be in cm instead of m. Values that are 8 & 4 need to be 
# changed to 0.08 and 0.04
veg.height$DS_H_m <- ifelse(veg.height$DS_H_m > 1, veg.height$DS_H_m/100, veg.height$DS_H_m)

# Fill new column with height values for dominant veg class
veg.height$dom.height <- ifelse(veg.height$dom.class == 'DS', veg.height$DS_H_m, NA)
veg.height$dom.height <- ifelse(veg.height$dom.class == 'LS', veg.height$LS_H_m, veg.height$dom.height)

# Clean up
veg.height <- veg.height %>% 
  select(Site, dom.class, dom.height)

# Need to deal with sites that had multiple plots... take mean of the all plots
library(data.table)
keys <- "Site"
X <- as.data.table(veg.height)
veg.height <-  X[, list(veg.height = mean(dom.height)), keys]

# Fill in missing values with mean from location --------
veg.height <- left_join(site.summary, veg.height, by = "Site")

tapply(veg.height$veg.height, veg.height$Location, mean, na.rm = TRUE)

veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'Denali', 
                                 0.114, veg.height$veg.height)
veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'Hatcher Pass', 
                                 0.195, veg.height$veg.height)
veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'JBER', 
                                 0.01, veg.height$veg.height)
veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'Paxson', 
                                 0.099, veg.height$veg.height)
veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'Southcentral', 
                                 0.045, veg.height$veg.height)
veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'Steese', 
                                 0.0462, veg.height$veg.height)
veg.height$veg.height <- ifelse(is.na(veg.height$veg.height) & veg.height$Location == 'Thompson Pass', 
                                 0.075, veg.height$veg.height)

# Clean up
veg.height <- veg.height %>% 
  select(-Location)

# Part 3: Percent cover of low shrubs from 10x10m veg plots  -----------------------------------------------
lowshrub <- veg %>% 
  select(Site, `LS_LF_%`)

# Take mean value at sites with multiple plots
X <- as.data.table(lowshrub)
lowshrub <-  X[, list(lowshrub.cover = mean(`LS_LF_%`)), keys]

# Fill in missing values with mean from location
lowshrub <- left_join(site.summary, lowshrub, by = "Site")

tapply(lowshrub$lowshrub.cover, lowshrub$Location, mean, na.rm = TRUE)

lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'Denali', 
                                 14.8, lowshrub$lowshrub.cover)
lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'Hatcher Pass', 
                                 19.7, lowshrub$lowshrub.cover)
lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'JBER', 
                                 0, lowshrub$lowshrub.cover)
lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'Paxson', 
                                 11.5, lowshrub$lowshrub.cover)
lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'Southcentral', 
                                 10.5, lowshrub$lowshrub.cover)
lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'Steese', 
                                 7.33, lowshrub$lowshrub.cover)
lowshrub$lowshrub.cover <- ifelse(is.na(lowshrub$lowshrub.cover) & lowshrub$Location == 'Thompson Pass', 
                                 13.5, lowshrub$lowshrub.cover)

# Clean up
lowshrub <- lowshrub %>% 
  select(-Location)

# Part 4: Clean up environment  -----------------------------------------------------------------------------
rm(a, site.summary, siteDesc, veg, veg2018, veg2019, X, i, keys, max, path, sites)

# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Historical Site Extirpation
# Author: Jeff Wagner
# Last Updated: 2022-08-22
# Usage: Must be executed in R 4.0.0+.
# Description: "Historical Site Extirpation" conducts a simple logistic regression of occupied vs. unoccupied historical sites.
# ---------------------------------------------------------------------------

# Read in the initialization script (reads in required packages, etc)
source("initscript.r")

## Prepare data
# Read in the data
path <- 'data/pika_site_occupation.csv'
siteOcc <- read.csv(path)

# We are only interested in historically occupied sites for the purposes of this analysis:
siteOcc <- siteOcc %>% 
  filter(random_site == 0)

# Read in site (transect) covariates from the distance sampling analysis
source("02_dataCovariates/covs_02_CompileCovariates.R")

# Filter to keep only one row per site & join to siteOcc
transect.covs <- transect.covs %>% 
  select(-(obs1.4:search.speed), -latitude, -longitude) %>% # get rid of transect-specific columns
  distinct() %>% # filter out duplicate rows
  filter(Site %in% unique(siteOcc$Site)) # keep only historical sites

siteOcc <- siteOcc %>% 
  left_join(., transect.covs, by = c("Location", "Site", "Year"))

# Correct column data types
siteOcc$Org <- as.factor(siteOcc$Org)
siteOcc$Location <- as.factor(siteOcc$Location)
siteOcc$occ.status <- as.factor(siteOcc$occ.status)
siteOcc$Date <- as.Date(siteOcc$Date, format = '%m/%d/%Y')

# Convert occupied site status to binary variable
siteOcc$occ.status <- ifelse(siteOcc$occ_status=="occupied", 1, 0)

# Write out site-level data with covariates
save(siteOcc, file = "./data/siteOcc.RDS")
write.csv(siteOcc, file = "./data/siteOcc.csv")

## Logistic regression models
climate <- glm(occ.status ~ precip + summerWarmth + januaryMinTemp, data = siteOcc, family = "binomial")
summary(climate)
confint(climate)

productivity <- glm(occ.status ~ logs + ndvi + wetness, data = siteOcc, family = "binomial")
summary(productivity)
confint(productivity)

topography <- glm(occ.status ~ elevation + roughness + northness, data = siteOcc, family = "binomial")
summary(topography)
confint(topography)

climateProductivity <- glm(occ.status ~ precip + summerWarmth + januaryMinTemp + ndvi, data = siteOcc, family = "binomial")
summary(climateProductivity)

latitude <- glm(occ.status ~ latitude, data = siteOcc, family = "binomial")
summary(latitude)

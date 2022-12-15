# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Compile model covariates
# Author: Jeff Wagner
# Last Updated: 2022-07-21
# Usage: Must be executed in R 4.0.0+.
# Description: "Compile model covariates" compiles covariates from various sources and prepares them for use in the distance sampling analysis.
# ---------------------------------------------------------------------------

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source( "00_init/initscript.r")

# Define data folder
data_folder = paste(getwd(),
                    'data',
                    sep ='/')

# Part 1: Read in extracted topographic and climate data -----------------------
sites_extracted = read.csv(paste(data_folder, "sites_extracted.csv", sep = "/"))

# Select columns of interest
sites_extracted <- sites_extracted %>% 
  select(Site, random_sit, occ_status, aspect, wetness, elevation, slope, roughness, exposure,
         heatload, relief, position, radiation, evi2, nbr, ndmi, ndsi, ndvi, ndwi, precip, 
         summerWarmth, januaryMinTemp, logs)

# Part 2: Load and clean up the transect-level covariates  --------------------------------------------------

# There are 2 places where we can find transect-level covariates recorded at each site. The time, temp, 
# wind speed, latitude, elevation, slope, aspect, and roughness can be found in the same excel workbook
# as the observation and track data (pika.distance.sampling.11.4.2019.xlsx) in the 'Site_Summary' sheet.

# The other file is in a different excel workbook called '2018_and_2019_SiteInfo.11.6.2019.xlsx'.
# We will need to load these, merge them, and then clean them up so we have a unique transect.observer 
# combination to pair with the observation and track length data.

# Load the site info from the same Excel workbook as the observation and transect length data.
# Since from the same Excel file ('pika.distance.sampling.11.4.2019.xlsx') we have been working with for 
# the observation data and track data, we can keep the same 'path' to the file of interest.
site.summary <- read_excel(path=path, sheet="Site_Summary")
site.summary <- as.data.frame(site.summary)

# Select the columns of interest.
site.summary <- site.summary %>% 
  select(-Org, -random_site, -No_groups, -Veg_survey, -Site_info, -Notes)

# Need to standardize all temperatures to Celsius and all wind speed to m/s.
site.summary <- site.summary %>% 
  mutate(tempc = case_when(Temp_Unit == "F" ~  ( (Temp-32) * 5/9), 
                           Temp_Unit == "C" ~ Temp )) %>% 
  mutate(windms = case_when(Wind_Unit == "m/s" ~ Wind,
                            Wind_Unit == "mph" ~ Wind/2.237))

# Subset out revisits too, being sure to keep the JBER sites which were labeled as revisits.
site.summary <- site.summary %>% 
  mutate(visit_type = ifelse(Location == "JBER", "i", visit_type))

site.summary <- site.summary %>% 
  filter(visit_type=="i")

# Need to consolidate the P002 site which is listed twice since it was surveyed over two days.
# The rows are identical, so we will just keep the first row for each site.
site.summary <- site.summary %>% 
  distinct(Site, .keep_all = TRUE)

# Clean up the columns further
site.summary <- site.summary %>% 
  select(Location, Site, latitude, longitude, Year, Date, Observer1, Observer2, Observer3, Observer4, tempc, windms)

# Part 3: Add new covariates for day of year and distance to road  -------------------------------------------

# Include day of year as a covariate using lubridate pkg.
site.summary$Date <- as.Date(site.summary$Date)
site.summary <- site.summary %>% 
  mutate(day.of.year = yday(Date))

# Load and add the distance to roads data
dist.road <- read.csv('./data/allSites_distToRoad.csv')

# Select columns of interest: site and distance to road (NEAR_DIST)
dist.road <- dist.road %>% 
  select(-OBJECTID, -Org, -Location, -latitude, - longitude, -Year, -Date, -visit_type,-NEAR_FID) %>% 
  rename(dist.road = NEAR_DIST)

# Join dist.road with other covariates by site
site.summary <- left_join(site.summary, dist.road, by = 'Site')

# Part 4: Load the site-level (250m radius) data from the Site Info workbook   -------------------------------
# The variables in this worksheet are at the site level (250m radius). The worksheet is called '250mCover' in 
# the '2018_and_2019_SiteInfo.xlsx' workbook.

# Define path to new workbook
path1 <- "./data/2018_and_2019_SiteInfo.11.8.2019.xlsx"

# Load the '250mCover' sheet 
site.250 <- read_excel(path=path1, sheet="250mCover")
site.250 <- as.data.frame(site.250)

# Primary interests are % cover of shrub and % cover of talus
site.250 <- site.250 %>% 
  select(Site, `LS_%`, `TS_%`, `Talus_%`)

# Rename these columns to something simpler
site.250 <- site.250 %>% 
  rename(lowshrub = `LS_%`, tallshrub = `TS_%`, talus = `Talus_%`)

# There are NA's for LS_% that will either need values of 0 or something else. Will leave as NA's for now.
unique(site.250$Site)   # 47 sites and all are unique

# Part 5: Load the 50m buffer data from the Site Info workbook   --------------------------------------------
# The variables in this worksheet describe % cover of vegetation types within an approximately 50m buffer 
# around talus patches. The worksheet is called '50mViereck' in the '2018_and_2019_SiteInfo.xlsx' workbook.

# Load the '50mViereck' sheet
site.50 <- read_excel(path=path1, sheet="50mViereck")
site.50 <- as.data.frame(site.50)

# Select the columns of interest
site.50 <- site.50 %>% 
  select(-Viereck_ID, -Surveyed, -Dist_talus_m, -Notes)

# Make PercentCover numeric
site.50$PercentCover <- as.numeric(site.50$PercentCover)
summary(site.50$PercentCover) #There is 1 NA.

# How many unique Viereck levels were recorded?
unique(site.50$Viereck)  #15 Viereck types

# I want to use spread to create a column for each Viereck level and 1 row per site so that we can use some 
# of these as transect-level covariaates. I think it will be useful to have %cover of ericaceus dwarf shrub 
# and maybe others potentially.
site.50 <- site.50 %>% 
  spread(Viereck, PercentCover)    
site.50   #41 transects represented. Will need to make sure we have consistent number of transect-level covariates.

# Let's look at the # of NA's to decide which viereck levels are well represented across sites.
summary(site.50)   

# Ericaceous dwarf shrub is at all but 3 sites, ranging from 5% to 90%. All of the other Viereck levels are not 
# represented at 20+ sites so it's not worth using unless consolidating into more generic groups, such as 
# 'low shrub'. 

# Alternatively, we could introduce 0s for all missing values, assuming that if a group was not recorded
# for a particular site, it's percent cover was ~0 or low.

# For now, let's just keep % cover of ericaceous dwarf shrub as a transect-level covariate. Subset and 
# rename the column to something simpler. 
site.50 <- site.50 %>%
  select(Site, `ericaceous dwarf shrub`) %>% 
  rename(eds = `ericaceous dwarf shrub`)

# Need to replace all NAs with 0's since the percent cover was presumably 0 or low.
site.50 <- site.50 %>% 
  mutate(`eds` = ifelse(is.na(`eds`), 0, `eds`))
site.50

unique(site.50$Site)  # 41 sites and all of them are unique

# Part 6: Join the three different sets of transect-level covariates   ---------------------------------------

# First, let's see how many transect-level covariates are represented in each set of covariates.
summary(site.250)  #47 transect-level covariates
summary(site.50)   #41 transect-level covariates
summary(site.summary)  #47 transect-level covariates; check Date of '0201-07-24'

# Which transects are missing when comparing covariates from site.250, site.50, and site.summary
(compare.250.50 <- anti_join(site.250, site.50, by="Site"))  #P003, P005, P007, P008, P128, P250
(compare.250.sitesummary <- anti_join(site.250, site.summary, by="Site"))  #no mismatches
(compare.50.sitesummary <- anti_join(site.50, site.summary, by="Site"))  #no mismatches
(compare.sitesummary.50 <- anti_join(site.summary, site.50, by="Site")) #P003, P005, P007, P008, P128, P250
(compare.sitesummary.250 <- anti_join(site.summary, site.250, by="Site")) #no mismatches

# The mis-match in transect-level covariates should rectified to match up with observation data.

# Left join: Start with the data frame that has the most info, site.250, at 47 sites.
trans.covs <- site.summary %>% 
  left_join(site.250, by = "Site")
trans.covs

# Now add the site.50 covariates
transect.covs <- trans.covs %>% 
  left_join(site.50, by = "Site")
transect.covs   #numerous NA's for low shrub, eds

# Now add the sites_extracted data
transect.covs <- transect.covs %>% 
  left_join(sites_extracted, by = "Site")

# Returning to the comment above about mis-matches in transect-level covariates... We need to decide which NAs
# for lowshrub, talus, and eds are NA's because they are 0 or because these details were not recorded at the site. 
# If so, perhaps add a mean value or leave as NA. I think the distsamp model will just not include those transects
# in consideration of that covariate.

# Part 6: Recode observers to match observation and track data   ----------------------------------------------

# Remove the columns not of interest
transect.covs <- transect.covs %>% 
  select(-Date)
dim(transect.covs)  #transect covs is 47 rows.

# Put observers in a single column. This will quadruple the number of rows in the dataframe from 47 to 184 rows
# because there are 4 columns for observers. The obs1.4 column will represent the Observer 1, Observer 2, 
# Observer 3, Observer 4 columns. Observer will represent the initials of the field observer. obs1.4 is a placeholder
# for identifying Observers 1, 2, 3, 4. 
transect.covs <- transect.covs %>% 
  gather(obs1.4, Observer, -Location, -Site, -latitude, -longitude, -Year, - tempc, -windms, -day.of.year, -dist.road,
         -lowshrub, - tallshrub, -talus, -eds, -random_sit, -occ_status,  -aspect, -wetness, -elevation, -slope,
         -roughness, -exposure, -heatload, -relief, -position, -radiation, -evi2, -nbr, -ndmi, -ndsi, -ndvi, -ndwi,
         -precip, -summerWarmth, -januaryMinTemp, -logs) #Yep, 188 rows.
head(transect.covs)
dim(transect.covs)

# Identify the unique observer names so we can begin pairing with the observation data.
unique(transect.covs$Observer)

# Recode to match observation and track data
transect.covs <- transect.covs %>% 
  mutate(observer = Observer) %>%  
  mutate(observer = recode(observer, 'JW' = 'JW', 'AD' = 'AD', 'AD.JW' = 'JW', 'AD.RK' = 'AD', 
                           'LT' = 'T2', 'LS' = 'T1', 'CB' = 'T1', 'PS.RK' = 'ACCS',
                           'CB.JW.KC.PS.SG' = 'ACCS', 'RK' = 'ACCS', 'LS.LT' = 'T1', 'PS' = 'ACCS',
                           'NA' = 'notsurveyed'))   #to easily identify where there was not a 3rd or 4th observer.

# Make sure that we have successfully created the observer field with 5 levels to match the same levels used in 
# the observation data. We have 6 here because we want to keep track of the site.observer pairs that don't exist.
unique(transect.covs$observer)

##### REMOVE JBER REPLICATES (CB & PS) UNTIL SPATIAL DATA IS FOUND) ######

transect.covs <- transect.covs %>% 
  filter(!(((Site == 'JB01' & Observer == 'CB') |
              (Site == 'JB01' & Observer == 'PS') |
              (Site == 'JB02' & Observer == 'CB') |
              (Site == 'JB02' & Observer == 'PS'))))

##### RECODE SITES TO MATCH TRACKS #####

transect.covs <- transect.covs %>% 
  mutate(Site = gsub('A','', Site)) %>% 
  mutate(Site = recode(Site, "Polychrome" = "PC"))

# Now, let's create the 'transect' field in the same manner that we did for the observation data and
# the transect lenghts.
transect.covs <- transect.covs %>% 
  mutate(transect = str_c(Site, ".", observer))
head(transect.covs)

# Arrange by transect so they are alphabetized.
transect.covs %>% 
  arrange(transect)  
# P002 and possibly other sites were surveyed over two days (I think this is the site), but we only want 1 row per transect,
# so will need to consolidate.
unique(transect.covs$transect)  #there are 159 unique transects since there will be replicates of '...notsurveyed' for 
# sites where there wasn't an observer 3 or 4.

# Part 7: Remove "notsurveyed" transects and transect with missing GPS track   ----------------------------------------------
# Keep only 1 distinct entry for each transect.
transect.covs <- transect.covs %>% 
  distinct(transect, .keep_all = TRUE) %>% 
  arrange(transect)

# Remove P138.AD
transect.covs <- transect.covs %>% 
  filter(!transect %in% 'P138.AD') 


# Subset to remove the transects that were not surveyed (i.e. since most sites had two observers, we will remove
# most of the options for the 3rd and 4th observers at each site) Remove all the transects that were not surveyed. 
# Keep in mind a 'transect' is a site and observer combination. 
transect.covs <- transect.covs %>% 
  filter(!observer %in% 'notsurveyed')  #now there are 119 rows representing each transect, i.e. combinations of sites and observers

# View the transect-level covariates. There are 119 transects rather than 188 (when you view the data below) 
# because we removed the redundant '...notsurveyed' sites where there were not obs 3 and 4.
head(transect.covs)
unique(transect.covs$transect)  #there are 119 unique transects so no redundancies


# Part 8: Add in the transect lengths and search effort  ----------------------------------------------------------------
# First, read in the cleaned pika tracks from the data management script
pika.tracks <- readRDS("./data/tracks.RData")

# Let's arrange them alphabetically for quick inspection
(pika.tracks <- pika.tracks %>% 
    arrange(transect))   # Note that we removed the transect 'P138.AD' in both the transect.covs and pika.tracks because
# the GPS track info was missing for that site/observer.

# Compare the transect length and search speed info with the transect - level covariates and see if anything missing.

(compare.transect.covs.pika.tracks <- anti_join(transect.covs, pika.tracks, by= c("transect" = "Transect")))  # no issues
(compare.transect.covs.pika.tracks <- anti_join(pika.tracks, transect.covs, by= c("Transect" = "transect")))   # no issues

# Add the covariates from the pika.tracks dataframe to the transect.covs dataframe
transect.covs <- transect.covs %>% 
  left_join(pika.tracks, by = c("transect" = "Transect"))

# View the transect-level covariates with transect length and search speed
transect.covs
summary(transect.covs)  #looking pretty good. Just need to clarify some of these mismatches and NA's.

# # Part 9: Fill in NAs in '% EDS'   ----------------------------------------------------------------------------------------
# 
# # Fill in NA's with mean value for EDS
# 
# eds.mean <- mean(transect.covs$eds, na.rm = TRUE)
# 
# transect.covs$eds <- replace(transect.covs$eds, is.na(transect.covs$eds), eds.mean)
# 
# # Part 10: Add in other vegetation covariates -------------------------------------------------------------
# source("02_dataCovariates/covs_01_VegSurveys.r")
# 
# # Add in dominant vegetation within 50m of the talus
# transect.covs <- left_join(transect.covs, domVeg, by = "Site")
# 
# # Add in mean vegetation height from 10x10m veg plots
# transect.covs <- left_join(transect.covs, veg.height, by = "Site")
# 
# # Add in percent cover of lowshrub from 10x10m veg plots
# transect.covs <- left_join(transect.covs, lowshrub, by = "Site")
# 
# # Add in total shrub cover within plot
# shrub <- read.csv("./data/percentShrub.csv") %>% 
#   select(Site, PERCENTAGE) %>% 
#   rename(shrubCover=PERCENTAGE)
# transect.covs <- left_join(transect.covs, shrub, by = "Site")

# Part 11: Convert aspect to meaningful variables ----------------------------------

# Degrees to radians
transect.covs$aspectRad <- transect.covs$aspect*pi/180

# Calculate northness & eastness
transect.covs <- transect.covs %>% 
  mutate(northness = cos(aspectRad), eastness = sin(aspectRad))


# Lastly, cleanup the environment, keeping only the objects that we will use in the final
# analysis. When we read in this script later, we will only have the objects that we need. 
rm(compare.250.50, compare.250.sitesummary, compare.50.sitesummary, compare.sitesummary.250, 
   compare.sitesummary.50, compare.transect.covs.pika.tracks, dist.road, pika.tracks, site.250, 
   site.50, trans.covs, eds.mean, path1, shrub, veg.height, domVeg, lowshrub, sites_extracted, data_folder)




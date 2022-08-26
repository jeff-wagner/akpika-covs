# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Observation data
# Author: Jeff Wagner
# Last Updated: 2022-07-21
# Usage: Must be executed in R 4.0.0+.
# Description: "Observation data" imports, explores, and processes distance sampling observations of pika for use in abundance analysis with unmarked.
# ---------------------------------------------------------------------------

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source( "initscript.r")

# Part 1: Load the distance sampling observation data  ------------------------------------------------------

# Load the observation data
pika.obs <- read_excel(path=path, sheet="Observations")
head(pika.obs)

pika.obs <- as.data.frame(pika.obs)
head(pika.obs)

# Count is being treated as a character Need to make numeric.
str(pika.obs$Count)

unique(pika.obs$Count)  

# There are a few NA's that were entered into Excel as text rather than missing values, but as there was 
# distance sampling data entered for these lines, we will assume the number of animals was 1.
pika.obs <- pika.obs %>% 
  mutate(Count = ifelse(Count == "NA", 1, Count),
         Count = as.numeric(Count))
head(pika.obs)
unique(pika.obs$Count)

# Perpendicular distance is also getting treated as a character. 
str(pika.obs$perp.dist)

# Convert to numeric value and then remove the NA's since distance data was not recorded on the data sheet.
pika.obs <- pika.obs %>% 
  mutate(perp.dist = ifelse(perp.dist == "NA", NA, perp.dist),
         perp.dist = as.numeric(perp.dist)) %>% 
  filter(is.na(perp.dist) == FALSE)

hist(pika.obs$perp.dist)

# Part 2: Filter for initial visits (no revisits) and pika only  --------------------------------------------

unique(pika.obs$Species)  # Make sure PIKA doesn't have any misspellings and see which species we have data

# Arrange by Site name: want to view the JBER sites
pika.obs <- pika.obs %>% 
  arrange(Site)

# We only have JBER data collected by JW, PS, and Colette Brandt (CB) in here so want to treat this as 
# an 'initial visit' with our complete methods. We will remove the 'revisit' data to other sites for now
# and want to make sure we include our 2019 JBER data, so we will recode JBER sites as "inital". 
pika.obs <- pika.obs %>% 
  mutate(visit_type = ifelse(Location == "JBER", "i", visit_type))

# Sort by visit_type to verify the other sites listed as revisits are to be excluded.
# Side note: Changes in density between initial visit and revisits are of interest, but we only did 
# this at a few sites so it's not the primary interest in this analysis. Year to year changes in 
# density or seasonal changes in density would be really interesting to explore in the future.

pika.obs %>% 
  arrange(desc(visit_type))  # yes, only P007 and P008 were revisited so we will exclude those next.

# Simplify the data frame by selecting the columns of interest and
# selecting the pika data, using only the initial surveys (exclude P007 and P008 revisits).
pika.obs <- pika.obs %>%
  select(Location, Site, visit_type, Observer, Species, Obs_Type, Count, perp.dist, Audio) %>% 
  filter(Species == 'PIKA' & visit_type == 'i')

head(pika.obs)

# Truncate observations to remove the small number of observations where the pika was >150m
# away (which may be unreliable). 
pika.obs <- pika.obs %>% 
  filter(perp.dist <= 150)
hist(pika.obs$perp.dist)
mean(pika.obs$perp.dist)  #mean perpendicular distance of pika sightings is 15.2m

# Part 3: Recode observers into observer "teams"  ---------------------------------------------------------
# We want to test for differences in detection probability based on the number of observers [2,3, or 4 (rare)]
# surveying each site.

unique(pika.obs$Observer)

# Will reduce all observers to 5 options based on experience level and number of sites visited.
# 5 levels will include: AD (Amanda Droghini), JW (Jeff Wagner), ACCS (Paul Schuette or Rachel Kelty), 
# T1 (Logan Suiter, also includes 3 sites by Colette Brandt), and T2 (Lindsey Taylor, also includes 1
# site by Molly Garner). I wanted to keep this to a max of 4-5 levels while also avoiding
# observer codes that would result in redundancies since each transect - observer pair has to be unique.

# For sites where any of these 5 options worked together, I will use the first initial recorded 
# in the data sheet. We want to avoid too many levels or this won't be a useful covariate in our models. 

# Add a new field for 'observer' which is a copy of 'Observer', but then recoded.
pika.obs.t <- pika.obs %>% 
  mutate(observer = Observer) %>%  
  mutate(observer = recode(observer, 'CB' = 'T1', 'PS' = 'ACCS', 'JW'= 'JW', 'LT' = 'T2',
                           'LS' = 'T1', 'JW.AD' = 'JW', 'PS.RK' = 'ACCS',
                           'KC.SG.CB.JW.PS' = 'ACCS', 'AD.RK' = 'AD', 'RK' = 'ACCS',
                           'AD' = 'AD', 'LS.LT' = 'T1'))

head(pika.obs.t)

# Check to make sure we only have AD, JW, T1, T2, and ACCS as the 5 options.
unique(pika.obs.t$observer)

# Part 4: Join each observation with the observer  ---------------------------------------------------------

# For clarity, we are trying to estimate density at the transect level at each site. With two or more observers
# at each site recording distance sampling data along their respective transects, we can get two density
# estimates per site. Or, we can at least test if there different density estimates (and/or detection probability
# estimates) per site based on data collected from multiple observers.

# We will do this by combining the site name and observer into a new field that will be our 'transect'. 
# For example, site P060, if it was surveyed by AD and JW would result in two transects at that site: P060.AD
# and P060.JW. 

# We will need to create a matching 'transect' field in the
# TracksCombined the transect-level covariate sheets so they can be joined together prior to analyses. 

# Create a new field called 'transect':
pika.obs.t <- pika.obs.t %>% 
  mutate(transect = str_c(Site, ".", observer))
head(pika.obs.t)

# Part 5: Final cleanup  ------------------------------------------------------------------------------------
# Exclude P138.AD because we don't have a GPS track for that site/observer.
pika.obs.t <- pika.obs.t %>% 
  filter(!transect %in% 'P138.AD')

# Clean up the environment, keeping only the object that we need.
rm(pika.obs)




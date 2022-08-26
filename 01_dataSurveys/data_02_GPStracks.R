# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# GPS tracks
# Author: Jeff Wagner
# Last Updated: 2022-07-21
# Usage: Must be executed in R 4.0.0+.
# Description: "GPS tracks" imports, explores, and processes survey GPS tracks for use in abundance analysis with unmarked.
# ---------------------------------------------------------------------------

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source( "00_init/initscript.r")

# Part 1: Load the GPS tracks  -----------------------------------------------------------------------------

# Load the track data from each observer's GPS unit. Each observer traced their search path during the survey 
# with the GPS. These tracks had to be cleaned up in ArcGIS so that the track was limited to the 250m radius site. 
# For example, sometimes GPS units were left on when people took a break, left the site at the end of the day, 
# and/or got in the truck and drove off. We need to limit the tracks to the actual survey effort. We are primarily
# interested in transect length, which we need for estimating density (e.g. # of pika/km2) and search effort, 
# which we can incorporate as a covariate on detection probability (e.g. was detection rate higher when individuals 
# conducted their survey more slowly?)

# Load the track data
pika.tracks <- read_excel(path=path, sheet="TracksCombined")
pika.tracks <- as.data.frame(pika.tracks)
head(pika.tracks)

# Select the columns of interest
pika.tracks <- pika.tracks %>% 
  select(-Track_ID, -Multiple_tracks_to_combine, -GPS_EndTIme, -Notes, -`SearchEffort_min/m`)

# View the track data to see if there are any issues of numeric values being treated as text.
summary(pika.tracks)   #looks like we're in good shape.

# Part 2: Recode observers into observer "teams"  ---------------------------------------------------------
# Same as with the observation data, we need to convert Observer to the 5 observers of interest:
# "T1" "PS" "JW" "T2" "AD"
unique(pika.tracks$Observer)

# Add a field for observer and then recode for the 5 levels of interest: AD, JF, LS, LT, Other
pika.tracks <- pika.tracks %>% 
  mutate(observer = Observer) %>%  
  mutate(observer = recode(observer, 'AD' = 'AD', 'AD,RK' = 'AD', 'AD.RK' = 'AD',
                           'CB' = 'T1', 'JW' = 'JW', 'JW.AD' = 'JW', 
                           'KC.SG.CB.JW.PS' = 'ACCS', 'LS' = 'T1', 'LS.LT' = 'T1',
                           'LT' = 'T2', 'MG' = 'T2', 'PS' = 'ACCS', 'PS.RK' = 'ACCS',
                           'RK' = 'ACCS'))

# Make sure that we have successfully created the observer field with 5 levels.
unique(pika.tracks$observer)  

# Part 3: Join each observation with the observer  ---------------------------------------------------------
# Now, let's create the 'transect' field in the same manner that we did for the observation data.
# This will allow us to match the observations to the track and search effort.
pika.tracks <- pika.tracks %>% 
  mutate(transect = str_c(Site, ".", observer))
head(pika.tracks)

# Part 4: Remove revisits and NAs & clean up formatting  ---------------------------------------------------
# Remove P007 and P008 revisits and remove the row where there is no track details (P138.AD)

# Check when P007 & P008 were revisited
subset(pika.tracks, Site %in% c("P007", "P008"), select = c(Site, Date))

# Remove surveys from 2018-09-28
pika.tracks <- pika.tracks[!(pika.tracks$Date == "2018-09-28"),]

# Remove rows with missing values
pika.tracks <- pika.tracks[complete.cases(pika.tracks) ==TRUE,] # Removed 1 row (P138.AD)

# Also need to remove the Toklat track since it wasn't an official site
pika.tracks <- pika.tracks[!(pika.tracks$Site == "TOK"),]  

head(pika.tracks)

# Clean up some of these field names and select the columns of interest.
pika.tracks <- pika.tracks %>% 
  rename(t.length = TrackLengt, start.time = GPS_StartTime)

# Strip the dates from start time
pika.tracks <- pika.tracks %>% 
  mutate(start.hr = hour(start.time))

head(pika.tracks)

# Part 5: Consolidate tracks: only one line per transect  ---------------------------------------------------
#Consolidate tracks so that each transect is only represented once. There might be multiple entries
# still for sites, such as P002 which was surveyed over 2 days since it was a big site and people
# were training.

#Sum up the elapsed time and t.length, grouped by transect. This code will find all locations where
# transect has multiple rows and sum those columns. 
pika.tracks <- pika.tracks %>% 
  group_by(transect) %>% 
  summarise(search.time = sum(Elapsed_Time_min), trans.length = sum(t.length), start.hr = first(start.hr))
(pika.tracks <- as.data.frame(pika.tracks))

summary(pika.tracks)

# Now need to add back in the search effort. We'll express this as "search.speed" (m/min), which is a little
# bit more intuitive than the original min/m.
pika.tracks <- pika.tracks %>% 
  mutate(search.speed = trans.length/search.time)
pika.tracks
summary(pika.tracks)   #119 unique transects with a mean search time of 165.9 min per transect (Doesn't account for area surveyed)
# a mean transect length of 2107.7 m and search speed of 13.0 m/min.







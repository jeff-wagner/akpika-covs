# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# GPS tracks cleanup
# Author: Jeff Wagner
# Last Updated: 2022-07-21
# Usage: Must be executed in R 4.0.0+.
# Description: "GPS tracks cleanup" imports and cleans GPS track data from two sources.
# ---------------------------------------------------------------------------

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source( "00_init/initscript.r")

# Part 1: Load the GPS tracks  -----------------------------------------------------------------------------

tracks <- read.csv('./data/PikaTracks.csv')
  
  # Clean up
tracks <- tracks %>% 
  filter(!(Site == 'P080')) %>% 
  mutate(GPS_unit = gsub('U','', GPS_unit))

pika.tracks <- read_excel(path=path, sheet="TracksCombined")
pika.tracks <- as.data.frame(pika.tracks)

  # Clean up
pika.tracks <- pika.tracks %>%
  filter(!((Site == "TOK") | 
             (Site == 'P138' & Observer == 'AD') |
             (Site == 'JB01' & Observer == 'CB') |
             (Site == 'JB01' & Observer == 'PS') |
             (Site == 'JB02' & Observer == 'CB') |
             (Site == 'JB02' & Observer == 'PS') |
             (Date == "2018-09-28")))


# Part 2: Recode observers into observer "teams"  ---------------------------------------------------------
# Same as with the observation data, we need to convert Observer to the 5 observers of interest:
# "T1" "PS" "JW" "T2" "AD"
unique(pika.tracks$Observer)

# Add a field for observer and then recode for the 5 levels of interest: AD, JW, LS, LT, Other
pika.tracks <- pika.tracks %>% 
  mutate(observer = Observer) %>%  
  mutate(observer = recode(observer, 'AD' = 'AD', 'AD,RK' = 'AD', 'AD.RK' = 'AD',
                           'CB' = 'T1', 'JW' = 'JW', 'JW.AD' = 'JW', 
                           'KC.SG.CB.JW.PS' = 'ACCS', 'LS' = 'T1', 'LS.LT' = 'T1',
                           'LT' = 'T2', 'MG' = 'T2', 'PS' = 'ACCS', 'PS.RK' = 'ACCS',
                           'RK' = 'ACCS'))
unique(pika.tracks$observer)


unique(tracks$Observer)

# Add a field for observer and then recode for the 5 levels of interest: AD, JW, LS, LT, Other
tracks <- tracks %>% 
  mutate(observer = Observer) %>%  
  mutate(observer = recode(observer, 'AD' = 'AD', 'AD,RK' = 'AD', 'JW' = 'JW', 'JW,AD' = 'JW', 
                           'KC,SG,CB,JW,PS' = 'ACCS', 'LS' = 'T1', 'LS,LT' = 'T1', 'LT' = 'T2', 
                           'PS' = 'ACCS', 'PS,RK' = 'ACCS','RK' = 'ACCS'))
unique(tracks$observer)

# Part 3: Join each observation with the observer  ---------------------------------------------------------
# Now, let's create the 'transect' field in the same manner that we did for the observation data.
# This will allow us to match the observations to the track and search effort.
pika.tracks <- pika.tracks %>% 
  mutate(transect = str_c(Site, ".", observer))

tracks <- tracks %>% 
  mutate(Transect = str_c(Site,".",observer))

# Part 4: Remove revisits and NAs & clean up formatting  ---------------------------------------------------
# Clean up some of these field names and select the columns of interest.
pika.tracks <- pika.tracks %>% 
  rename(t.length = TrackLengt, start.time = GPS_StartTime)

# Strip the dates from start time
pika.tracks <- pika.tracks %>% 
  mutate(start.hr = hour(start.time))

# Part 5: Consolidate tracks: only one line per transect  ---------------------------------------------------
#Consolidate tracks so that each transect is only represented once. There might be multiple entries
# still for sites, such as P002 which was surveyed over 2 days since it was a big site and people
# were training.

#Sum up the elapsed time and t.length, grouped by transect. This code will find all locations where
# transect has multiple rows and sum those columns. 
pika.tracks <- pika.tracks %>% 
  group_by(transect) %>% 
  summarise(search.time = sum(Elapsed_Time_min), trans.length = sum(t.length), start.hr = first(start.hr))

# Now need to add back in the search effort. We'll express this as "search.speed" (m/min), which is a little
# bit more intuitive than the original min/m.
pika.tracks <- pika.tracks %>% 
  mutate(search.speed = trans.length/search.time)

summary(pika.tracks)

(pika.tracks <- as.data.frame(pika.tracks) %>% 
    arrange(transect))

# Add "replicate" number for unique transects at each site
sites <- unique(tracks$Site)
tracks$replicate <- NA
for(i in 1:length(sites)){
  tracks[tracks$Site==sites[i],]$replicate <- as.numeric(as.factor(tracks[tracks$Site==sites[i],]$Transect))
}

(tracks <- tracks %>%
    arrange(Transect) %>% 
    rename(t.length = Shape_Length) %>%
    mutate(search.time = pika.tracks$search.time, 
           start.hr = pika.tracks$start.hr, 
           search.speed = pika.tracks$search.speed,
           t.match = paste(Site, "_U", GPS_unit, sep = ""),
           transect = paste(Site, replicate, sep = ".")) %>% 
    select(Site, Transect, transect, t.match, replicate, t.length, search.time, search.speed, start.hr) %>% 
    arrange(t.match))


saveRDS(tracks, file = "./data/tracks.RData")



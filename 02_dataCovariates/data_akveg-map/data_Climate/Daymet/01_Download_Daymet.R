# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Daymet Data Download
# Author: Jeff Wagner
# Last Updated: 2022-12-30  
# Usage: Must be executed in R 4.0.0+.
# Description: "Daymet Data Download" programatically downloads Daymet daily weather data for Collared Pika distance sampling analysis.
# ---------------------------------------------------------------------------

library(daymetr)

# Define paths
data.folder = '/Users/jeff/Documents/Projects/Pika/Data/Daymet'

# Download gridded daily minimum temperature data for the study area for 2000-2019
download_daymet_ncss(location = c(66, -152, 60, -142),
                     start = 2000,
                     end = 2019,
                     param = 'tmin',
                     path = data.folder)


# Pika distance sampling initialization script

# To be sourced in at the beginning of every script

# Load packages -----------------------------------------------------------------

library(dplyr)
library(data.table)
library(tidyverse)
library(unmarked)
library(lubridate)
library(ggplot2)
library(readxl)
library(psych)
library(snotelr)
library(sf)
library(sp)
library(mapview)
library(lwgeom)
library(AICcmodavg)
library(spatialEco)


# Set path to Excel workbook that contains the data -----------------------------
path <- "./data/pika.distance.sampling.11.8.2019.xlsx"
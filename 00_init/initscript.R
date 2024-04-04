# Pika distance sampling initialization script

# To be sourced in at the beginning of every script

# Load packages -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(psych)
library(sf)
library(sp)
library(mapview)
library(lwgeom)


# Set path to Excel workbook that contains the data -----------------------------
path <- "./data/pika.distance.sampling.11.8.2019.xlsx"

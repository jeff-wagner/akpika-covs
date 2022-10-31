# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# GBIF Download
# Author: Jeff Wagner
# Last Updated: 2022-10-31
# Usage: Must be executed in R 4.0.0+.
# Description: "GBIF Download" dowloads and filters occurrence records of collared pika from the Global Biodiversity Information Facility (GBIF).
# ---------------------------------------------------------------------------
library(rgbif)
library(dplyr)

# Download collared pika occurrence records from GBIF
name_backbone("Ochotona collaris")$usageKey
gbif <- occ_download(pred("taxonKey",2437020))
occ_download_wait(gbif)
gbifData <- occ_download_get(gbif) %>% 
  occ_download_import() %>% 
  select_if(~ !any(is.na(.)))

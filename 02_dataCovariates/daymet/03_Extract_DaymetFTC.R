# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Extract Daymet FTC Data to Sites
# Author: Jeff Wagner
# Last Updated: 2023-01-02
# Usage: Must be executed in R 4.0.0+.
# Description: "Extract Daymet FTC Data to Sites" reads in processed Daymet FTC data and extracts site means.
# ---------------------------------------------------------------------------

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source( "00_init/initscript.r")

# Define data folder
data_folder = './data'

# Define input folders
ftc_folder = paste(data_folder,
                   'ftc_2000_2019.tif',
                   sep = '/')
sp_folder = paste(data_folder,
                  'Transect_Export.gdb',
                  sep = '/')

# List input files
sp_data <- st_layers(sp_folder)
files <- sp_data$name[grep("Talus.",sp_data$name)]
site_file <- files[grep(".50m", files)]

# Import required libraries for geospatial processing: terra and stringr.
library(stringr)

# Read site metadata into dataframe
site_metadata = st_read(site_file, dsn=sp_folder)

# Create an empty list to store data frames
data_list = list()

# Loop through all grids with site data and extract features to sites
count = 1
for (grid in grid_list) {
  print(grid)
  print(paste('Extracting predictor data to site points ', count, ' of ', grid_length, '...', sep=''))
  count = count + 1
  
  # Create full path to points shapefile
  grid_sites = paste(parsed_folder, grid, sep = '/')
  
  # Identify grid name and predictor folders
  grid_name = str_remove(grid, '.shp')
  grid_folder = paste(grid_name)
  topography_grid = paste(topography_folder, grid_folder, sep = '/')
  sentinel2_grid = paste(sentinel2_folder, grid_folder, sep = '/')
  temperature_grid = paste(temperature_folder, grid_folder, sep = '/')
  precipitation_grid = paste(precipitation_folder, grid_folder, sep = '/')
  growingSeason_grid = paste(growingSeason_folder, grid_folder, sep = '/')
  
  
  # Create a list of all predictor rasters
  predictors_topography = list.files(topography_grid, pattern = 'tif$', full.names = TRUE)
  predictors_sentinel2 = list.files(sentinel2_grid, pattern = 'tif$', full.names = TRUE)
  predictors_temperature = list.files(temperature_grid, pattern = 'tif$', full.names = TRUE)
  predictors_precipitation = list.files(precipitation_grid, pattern = 'tif$', full.names = TRUE)
  predictors_growingSeason = list.files(growingSeason_grid, pattern = 'tif$', full.names = TRUE)
  predictors_all = c(predictors_topography,
                     predictors_sentinel2,
                     predictors_temperature,
                     predictors_precipitation,
                     predictors_growingSeason)
  print("Number of predictor rasters:")
  print(length(predictors_all))
  
  # Generate a stack of all predictor rasters
  predictor_stack = stack(predictors_all)
  
  # Read site data and extract features
  site_data = readOGR(dsn = grid_sites)
  extracted = extract(predictor_stack, site_data, fun = "mean")
  
  # Ensure all elements in the extracted list are the same length
  # for(i in 1:length(extracted)){
  #   if(nrow(extracted[[i]]) < max(unlist(lapply(extracted, nrow)))){
  #     nrow = max(unlist(lapply(extracted, nrow)))-nrow(extracted[[i]])
  #     extracted[[i]] <- rbind(extracted[[i]], matrix(data= NA, 
  #                                                    nrow = nrow,
  #                                                    ncol = 13))
  #     print(paste(nrow, "rows added to element", i, sep = " "))
  #   }
  # }
  sites_extracted = data.frame(site_data@data, extracted)
  
  # Convert field names to standard
  sites_extracted = sites_extracted %>%
    rename(aspect = paste('Aspect_', grid_name, sep = '')) %>%
    rename(wetness = paste('Wetness_', grid_name, sep = '')) %>%
    rename(elevation = paste('Elevation_', grid_name, sep = '')) %>%
    rename(slope = paste('Slope_', grid_name, sep = '')) %>%
    rename(roughness = paste('Roughness_', grid_name, sep = '')) %>%
    rename(exposure = paste('Exposure_', grid_name, sep = '')) %>%
    rename(heatload = paste('HeatLoad_', grid_name, sep = '')) %>%
    rename(relief = paste('Relief_', grid_name, sep = '')) %>%
    rename(position = paste('Position_', grid_name, sep = '')) %>%
    rename(radiation = paste('Radiation_', grid_name, sep = '')) %>%
    rename(evi2_06 = paste('Sent2_06_evi2_', grid_name, sep = '')) %>%
    rename(nbr_06 = paste('Sent2_06_nbr_', grid_name, sep = '')) %>%
    rename(ndmi_06 = paste('Sent2_06_ndmi_', grid_name, sep = '')) %>%
    rename(ndsi_06 = paste('Sent2_06_ndsi_', grid_name, sep = '')) %>%
    rename(ndvi_06 = paste('Sent2_06_ndvi_', grid_name, sep = '')) %>%
    rename(ndwi_06 = paste('Sent2_06_ndwi_', grid_name, sep = '')) %>%
    rename(evi2_07 = paste('Sent2_07_evi2_', grid_name, sep = '')) %>%
    rename(nbr_07 = paste('Sent2_07_nbr_', grid_name, sep = '')) %>%
    rename(ndmi_07 = paste('Sent2_07_ndmi_', grid_name, sep = '')) %>%
    rename(ndsi_07 = paste('Sent2_07_ndsi_', grid_name, sep = '')) %>%
    rename(ndvi_07 = paste('Sent2_07_ndvi_', grid_name, sep = '')) %>%
    rename(ndwi_07 = paste('Sent2_07_ndwi_', grid_name, sep = '')) %>%
    rename(evi2_08 = paste('Sent2_08_evi2_', grid_name, sep = '')) %>%
    rename(nbr_08 = paste('Sent2_08_nbr_', grid_name, sep = '')) %>%
    rename(ndmi_08 = paste('Sent2_08_ndmi_', grid_name, sep = '')) %>%
    rename(ndsi_08 = paste('Sent2_08_ndsi_', grid_name, sep = '')) %>%
    rename(ndvi_08 = paste('Sent2_08_ndvi_', grid_name, sep = '')) %>%
    rename(ndwi_08 = paste('Sent2_08_ndwi_', grid_name, sep = '')) %>%
    rename(evi2_09 = paste('Sent2_09_evi2_', grid_name, sep = '')) %>%
    rename(nbr_09 = paste('Sent2_09_nbr_', grid_name, sep = '')) %>%
    rename(ndmi_09 = paste('Sent2_09_ndmi_', grid_name, sep = '')) %>%
    rename(ndsi_09 = paste('Sent2_09_ndsi_', grid_name, sep = '')) %>%
    rename(ndvi_09 = paste('Sent2_09_ndvi_', grid_name, sep = '')) %>%
    rename(ndwi_09 = paste('Sent2_09_ndwi_', grid_name, sep = '')) %>%
    rename(summerWarmth = paste('SummerWarmth_MeanAnnual_AKALB_', grid_name, sep = '')) %>%
    rename(januaryMinTemp = paste("January_MinimumTemperature_AKALB_", grid_name, sep = '')) %>% 
    rename(precip = paste('Precipitation_MeanAnnual_AKALB_', grid_name, sep = '')) %>% 
    rename(logs = paste('LengthOfGrowingSeason_AKALB_', grid_name, sep = ''))
  
  # Summarize data by site
  sites_mean = sites_extracted %>%
    group_by(SiteID) %>%
    summarize(aspect = round(mean(aspect), digits = 0),
              wetness = round(mean(wetness), digits = 0),
              elevation = round(mean(elevation), digits = 0),
              slope = round(mean(slope), digits = 0),
              roughness = round(mean(roughness), digits = 0),
              exposure = round(mean(exposure), digits = 0),
              heatload = round(mean(heatload), digits = 0),
              relief = round(mean(relief), digits = 0),
              position = round(mean(position), digits = 0),
              radiation = round(mean(radiation), digits = 0),
              evi2 = round(mean(evi2_06+evi2_07+evi2_08+evi2_09), digits = 0),
              nbr = round(mean(nbr_06+nbr_07+nbr_08+nbr_09), digits = 0),
              ndmi = round(mean(ndmi_06+ndmi_07+ndmi_08+ndmi_09), digits = 0),
              ndsi = round(mean(ndsi_06+ndsi_07+ndsi_08+ndsi_09), digits = 0),
              ndvi = round(mean(ndvi_06+ndvi_07+ndvi_08+ndvi_09), digits = 0),
              ndwi = round(mean(ndwi_06+ndwi_07+ndwi_08+ndwi_09), digits = 0),
              precip = round(mean(precip), digits = 0),
              summerWarmth = round(mean(summerWarmth), digits = 0),
              januaryMinTemp = round(mean(januaryMinTemp), digits = 0),
              logs = round(mean(logs), digits = 0))
  
  # Add sites mean to list of data frames
  data_list = c(data_list, list(sites_mean))
}
# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Create length of growing season (logs) for 2000-2009
# Author: Jeff Wagner
# Last Updated: 2022-07-22
# Usage: Must be executed in an ArcGIS Pro Python 3.6 installation.
# Description: "Create length of growing season for 2000-2015" calculates the length of growing season mean for years 2000-2009. The primary data are the SNAP Alaska-Yukon 2km data.
# ---------------------------------------------------------------------------

# Import packages
import arcpy
import os
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import calculate_climate_mean
from package_GeospatialProcessing import format_climate_grids
from package_GeospatialProcessing import interpolate_raster

# Set root directory
drive = 'C:/'
root_folder = 'Users/jeffw/Dropbox/Pika/'

# Define data folder
data_folder = os.path.join(drive, root_folder, 'GISdata/climatology/logs')
project_folder = os.path.join(drive, 'Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp')
grid_folder = os.path.join(drive, root_folder, 'GISdata/analyses/grid_major/studyarea/')
unprocessed_folder = os.path.join(data_folder, 'unprocessed/2km/decadal_mean')
processed_folder = os.path.join(data_folder, 'processed')
output_folder = os.path.join(data_folder, 'gridded')

# Define geodatabases
work_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')

# Define input datasets
nab_raster = os.path.join(project_folder, 'data/AlaskaPika_TotalArea_1.tif')

# Define output datasets
mean_raw = os.path.join(processed_folder, 'full/LengthOfGrowingSeason_Raw_2km_2000-2009.tif')
mean_interpolated = os.path.join(processed_folder, 'sa/LengthOfGrowingSeason_2km_2000-2009.tif')

# Define grids
grid_list = ['A1', 'A2', 'B1', 'B2']

# Define month and property values
climate_property = 'logs_cru_TS31_historical'
decades = ['2000_2009']
denominator = 1

# Create a list of all climate raster data
raster_list = []
for decade in decades:
        raster = os.path.join(unprocessed_folder, climate_property + '_' + decade + '.tif')
        raster_list.append(raster)

mean_interpolated = os.path.join(unprocessed_folder, climate_property + '_' + decade + '.tif')
#### PARSE DATA TO GRIDS

# Set initial count
count = 1

# For each grid, process the climate metric
for grid in grid_list:
    # Define folder structure
    output_path = os.path.join(output_folder, grid)
    output_raster = os.path.join(output_path, 'LengthOfGrowingSeason_AKALB_' + grid + '.tif')

    # Make grid folder if it does not already exist
    if os.path.exists(output_path) == 0:
        os.mkdir(output_path)

    # Define the grid raster
    grid_raster = os.path.join(grid_folder, grid + '.tif')

    # If output raster does not exist then create output raster
    if arcpy.Exists(output_raster) == 0:
        # Create key word arguments
        kwargs_grid = {'input_array': [nab_raster, grid_raster, mean_interpolated],
                       'output_array': [output_raster]
                       }

        # Extract climate data to grid
        print(f'Processing grid {count} of {len(grid_list)}...')
        arcpy_geoprocessing(format_climate_grids, **kwargs_grid)
        print('----------')
    else:
        print(f'Climate grid {count} of {len(grid_list)} already exists.')
        print('----------')

    # Increase counter
    count += 1

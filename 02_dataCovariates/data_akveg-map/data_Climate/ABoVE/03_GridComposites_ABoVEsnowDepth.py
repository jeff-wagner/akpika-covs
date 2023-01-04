# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Create mean annual summer warmth index composite for 2000-2015
# Author: Timm Nawrocki (modified by Jeff Wagner)
# Last Updated: 2021-11-20 (2022-07-15) 
# Usage: Must be executed in an ArcGIS Pro Python 3.6 installation.
# Description: "Create mean annual summer warmth index composite for 2000-2015" calculates the mean annual summer warmth index from May-September for years 2000-2015. The primary data are the SNAP Alaska-Yukon 2km data with the included portion of the Northwest Territories interpolated by geographic nearest neighbors.
# ---------------------------------------------------------------------------

# Import packages
import arcpy
import os
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import format_climate_grids
arcpy.CheckOutExtension("Spatial")

# Set root directory
drive = 'C:/'
root_folder = 'Users/jeffw/OneDrive/Desktop'

# Define data folder
data_folder = os.path.join(drive, root_folder, 'GISdata/climatology/snowdepth')
project_folder = os.path.join(drive, 'Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp')
grid_folder = os.path.join(drive, root_folder, 'GISdata/analyses/grid_major/studyarea/')
processed_folder = os.path.join(data_folder, 'processed')
output_folder = os.path.join(data_folder, 'gridded')

# Define geodatabases
work_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')

# Define input datasets
nab_raster = os.path.join(project_folder, 'data/AlaskaPika_TotalArea_1.tif')
mean_interpolated = os.path.join(processed_folder, 'sa/ABoVE_SnowDepth_MeanWinter_Interpolated_1km_2001-2017.tif')

# Define grids
grid_list = ['A1', 'A2', 'B1', 'B2']

#### PARSE DATA TO GRIDS

# Set initial count
count = 1

# For each grid, process the climate metric
for grid in grid_list:
    # Define folder structure
    output_path = os.path.join(output_folder, grid)
    output_raster = os.path.join(output_path, 'ABoVE_SnowDepth_MeanWinter_AKALB_' + grid + '.tif')

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
        print(f'Grid {count} of {len(grid_list)} already exists.')
        print('----------')

    # Increase counter
    count += 1

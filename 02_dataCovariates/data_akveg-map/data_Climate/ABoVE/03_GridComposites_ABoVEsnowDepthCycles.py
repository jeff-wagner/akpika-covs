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
import datetime
import time
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import calculate_climate_mean
from package_GeospatialProcessing.formatClimateGrids_32bit import format_climate_grids_32bit
from package_GeospatialProcessing.interpolateRaster_32bit import interpolate_raster_32bit
arcpy.CheckOutExtension("Spatial")

# Set root directory
drive = 'C:/'
root_folder = 'Users/jeffw/OneDrive/Desktop'

# Define data folder
data_folder = os.path.join(drive, root_folder, 'GISdata/climatology/snowmelt')
project_folder = os.path.join(drive, 'Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp')
grid_folder = os.path.join(drive, root_folder, 'GISdata/analyses/grid_major/studyarea/')
unprocessed_folder = os.path.join(data_folder, 'unprocessed/1km')
processed_folder = os.path.join(data_folder, 'processed')
output_folder = os.path.join(data_folder, 'gridded')

# Define geodatabases
work_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')

# Define input datasets
nab_raster = os.path.join(project_folder, 'data/AlaskaPika_TotalArea_1.tif')

# Define output datasets
mean_raw = os.path.join(processed_folder, 'full/ABoVE_SnowMeltCycles_MeanWinter_Raw_1km_2001-2017.tif')
mean_interpolated = os.path.join(processed_folder, 'sa/ABoVE_SnowMeltCycles_MeanWinter_Interpolated_1km_2001-2017.tif')

# Define grids
grid_list = ['A1', 'A2', 'B1', 'B2']

# Define month and property values
climate_property = 'ABoVE_SnowMeltCycles'
years = ['2001_2017']
denominator = len(years)

# Create a list of all climate raster data
raster_list = []
for year in years:
        raster = os.path.join(unprocessed_folder, climate_property + '_' + year + '.tif')
        raster_list.append(raster)

#### CALCULATE CLIMATE MEAN

# Create a composite raster of the climate mean
if arcpy.Exists(mean_raw) == 0:
    iteration_start = time.time()
    print(f'\tExporting climate mean to output raster...')
    arcpy.management.CopyRaster(raster,
                                mean_raw,
                                '',
                                '',
                                '-32768',
                                'NONE',
                                'NONE',
                                '32_BIT_FLOAT',
                                'NONE',
                                'NONE',
                                'TIFF',
                                'NONE')
    print('----------')
    # End timing
    iteration_end = time.time()
    iteration_elapsed = int(iteration_end - iteration_start)
    iteration_success_time = datetime.datetime.now()
    # Report success
    print(f'\tCompleted at {iteration_success_time.strftime("%Y-%m-%d %H:%M")} (Elapsed time: {datetime.timedelta(seconds=iteration_elapsed)})')
else:
    print('Mean snow depth already exists.')
    print('----------')

#### FILL MISSING DATA

# Create key word arguments to interpolate raster
kwargs_interpolate = {'input_array': [nab_raster, mean_raw],
                      'output_array': [mean_interpolated]
                      }

# Interpolate climate raster
if arcpy.Exists(mean_interpolated) == 0:
    print('Filling missing data...')
    arcpy_geoprocessing(interpolate_raster_32bit, **kwargs_interpolate)
    print('----------')
else:
    print('Filled data already exists.')
    print('----------')

#### PARSE DATA TO GRIDS

# Set initial count
count = 1

# For each grid, process the climate metric
for grid in grid_list:
    # Define folder structure
    output_path = os.path.join(output_folder, grid)
    output_raster = os.path.join(output_path, 'ABoVE_SnowMelt_MeanWinter_AKALB_' + grid + '.tif')

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
        arcpy_geoprocessing(format_climate_grids_32bit, **kwargs_grid)
        print('----------')
    else:
        print(f'Grid {count} of {len(grid_list)} already exists.')
        print('----------')

    # Increase counter
    count += 1

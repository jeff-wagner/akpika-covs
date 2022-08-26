# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Process Sentinel-2 tiles
# Author: Timm Nawrocki (modified by Jeff Wagner)
# Last Updated: 2021-12-08 (2022-07-27)
# Usage: Must be executed in an ArcGIS Pro Python 3.6 installation.
# Description: "Process Sentinel-2 tiles" reprojects tiles and converts to integer.
# ---------------------------------------------------------------------------

# Import packages
import arcpy
import datetime
import fnmatch
import os
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import reproject_integer
import time

arcpy.CheckOutExtension("Spatial")

# Set root directory
drive = 'C:/'
root_folder = 'Users/jeffw/Dropbox/Pika'

# Define folder structure
data_folder = os.path.join(drive, root_folder, 'GISdata/imagery/sentinel-2')
project_folder = os.path.join(drive, 'Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp')
unprocessed_folder = os.path.join(data_folder, 'unprocessed/sa')
processed_folder = os.path.join(data_folder, 'processed/sa')

# Define geodatabases
work_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')

# Define input datasets
nab_raster = os.path.join(project_folder, 'data/AlaskaPika_TotalArea_1.tif')

# List imagery tiles
print('Searching for imagery tiles...')
# Start timing function
iteration_start = time.time()
# Set environment workspace to the folder containing the grid rasters
arcpy.env.workspace = unprocessed_folder
# Create a raster list using the Arcpy List Rasters function
unprocessed_list = arcpy.ListRasters('*', 'TIF')
# Append file names to rasters in list
unprocessed_tiles = []
for raster in unprocessed_list:
    raster_path = os.path.join(unprocessed_folder, raster)
    unprocessed_tiles.append(raster_path)
tiles_length = len(unprocessed_tiles)
print(f'Spectral composites will be created from {tiles_length} imagery tiles...')
# End timing
iteration_end = time.time()
iteration_elapsed = int(iteration_end - iteration_start)
iteration_success_time = datetime.datetime.now()
# Report success
print(f'Completed at {iteration_success_time.strftime("%Y-%m-%d %H:%M")} (Elapsed time: {datetime.timedelta(seconds=iteration_elapsed)})')
print('----------')

# Reset environment workspace
arcpy.env.workspace = work_geodatabase

#### PROCESS IMAGERY TILES

# Set initial tile count
count = 1

# Reproject all imagery tiles
for tile in unprocessed_tiles:
    # Define processed raster tile
    processed_tile = os.path.join(processed_folder, os.path.split(tile)[1])

    # Reproject and convert tile if processed tile does not already exist
    if arcpy.Exists(processed_tile) == 0:
        # Determine conversion factor
        if (fnmatch.fnmatch(processed_tile, '*evi2*')
                or fnmatch.fnmatch(processed_tile, '*nbr*')
                or fnmatch.fnmatch(processed_tile, '*ndmi*')
                or fnmatch.fnmatch(processed_tile, '*ndsi*')
                or fnmatch.fnmatch(processed_tile, '*ndvi*')
                or fnmatch.fnmatch(processed_tile, '*ndwi*')):
            conversion_factor = 1000000
        else:
            conversion_factor = 10

        # Create key word arguments
        kwargs_reproject = {'cell_size': 10,
                            'input_projection': 4326,
                            'output_projection': 3338,
                            'geographic_transformation': 'WGS_1984_(ITRF00)_To_NAD_1983',
                            'conversion_factor': conversion_factor,
                            'input_array': [nab_raster, tile],
                            'output_array': [processed_tile]
                            }

        # Process the reproject integer function
        print(f'Processing tile {count} of {tiles_length} using conversion factor {conversion_factor}...')
        arcpy_geoprocessing(reproject_integer, **kwargs_reproject)
        print('----------')
    else:
        print(f'Tile {count} of {tiles_length} already exists.')
        print('----------')

    # Increase counter
    count += 1

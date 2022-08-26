# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Create elevation composite
# Author: Timm Nawrocki (modified by Jeff Wagner)
# Last Updated: 2021-11-22 (2022-07-18)
# Usage: Must be executed in an ArcGIS Pro Python 3.6 installation.
# Description: "Create elevation composite" creates a composite from multiple source DEMs based on order of priority. All sources must be in the same projection with the same cell size and grid. The grid tiles must be predefined as rasters.
# ---------------------------------------------------------------------------

# Import packages
import os
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import create_composite_dem

# Set root directory
drive = 'C:/'
root_folder = 'Users/jeffw/Dropbox/Pika/'

# Define data folder
data_folder = os.path.join(drive, root_folder, 'GISdata/topography')
project_folder = os.path.join(drive, 'Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp')
grid_folder = os.path.join(drive, root_folder, 'GISdata/analyses/grid_major/full/')
output_folder = os.path.join(data_folder, 'Composite_10m')

# Define geodatabases
work_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')

# Define input datasets
source_USGS_5m = os.path.join(data_folder, 'USGS_3DEP_5m/Elevation_5m_Alaska_AKALB.tif')
source_USGS_10m = os.path.join(data_folder, 'USGS_3DEP_10m/Elevation_10m_Alaska_AKALB.tif')
source_USGS_30m = os.path.join(data_folder, 'USGS_3DEP_30m/Elevation_30m_Alaska_AKALB.tif')
source_USGS_60m = os.path.join(data_folder, 'USGS_3DEP_60m/Elevation_60m_Alaska_AKALB.tif')

# Define grids
grid_list = ['A1', 'A2', 'B1', 'B2']

#### CREATE COMPOSITE DEM

# Define prioritized list of elevation inputs
elevation_inputs = [source_USGS_5m, source_USGS_10m, source_USGS_30m, source_USGS_60m]

# Set initial count
count = 1

# Iterate through each buffered grid and create elevation composite
for grid in grid_list:
    # Define folder structure
    output_path = os.path.join(output_folder, grid)
    output_raster = os.path.join(output_path, 'Elevation_' + grid + '.tif')

    # Make grid folder if it does not already exist
    if os.path.exists(output_path) == 0:
        os.mkdir(output_path)

    # Define the grid raster
    grid_raster = os.path.join(grid_folder, grid + '.tif')

    # If output raster does not exist then create output raster
    if os.path.exists(output_raster) == 0:
        # Create key word arguments
        kwargs_composite = {'cell_size': 10,
                            'output_projection': 3338,
                            'input_array': [grid_raster] + elevation_inputs,
                            'output_array': [output_raster]
                            }

        # Process the elevation composite
        print(f'Processing grid {count} of {len(grid_list)}...')
        arcpy_geoprocessing(create_composite_dem, **kwargs_composite, check_output=False)
        print('----------')
    else:
        print(f'Elevation grid {count} of {len(grid_list)} already exists.')
        print('----------')

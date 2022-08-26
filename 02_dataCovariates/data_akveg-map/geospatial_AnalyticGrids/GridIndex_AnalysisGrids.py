# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Create Analysis Grids
# Author: Timm Nawrocki (modified by Jeff Wagner)
# Last Updated: 2021-11-20 (2022-07-12)
# Usage: Must be executed in an ArcGIS Pro Python 3.6 installation.
# Description: "Create Analysis Grids" creates major and minor grid indices and overlapping grid tiles from a manually-generated study area polygon.
# ---------------------------------------------------------------------------

# Import packages
import arcpy
from package_GeospatialProcessing import arcpy_geoprocessing
from package_GeospatialProcessing import create_buffered_tiles
from package_GeospatialProcessing import create_grid_index
from package_GeospatialProcessing import convert_validation_grid
from package_GeospatialProcessing import select_location
import os

# Set root directory
drive = 'C:/'
root_folder = 'Users/jeffw/Dropbox/Pika/'

# Set data folder
data_folder = os.path.join(drive, root_folder, 'GISdata/analyses')
project_folder = os.path.join(drive, 'Users/jeffw/OneDrive/Documents/Projects/Pika/Pika_distSamp')

# Define geodatabases
work_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')
regions_geodatabase = os.path.join(project_folder, 'Pika_distSamp.gdb')

# Define input raster datasets
total_feature = os.path.join(regions_geodatabase, 'studyArea')
total_raster = os.path.join(project_folder, 'data/AlaskaPika_TotalArea_1.tif')
sa_feature = os.path.join(regions_geodatabase, 'studyArea')
sa_raster = os.path.join(project_folder, 'data/AlaskaPika_TotalArea_1.tif')

# Define output grid datasets
major_grid = os.path.join(regions_geodatabase, 'AlaskaPika_GridIndex_Major_400km')
major_sa = os.path.join(regions_geodatabase, 'StudyArea_GridIndex_Major_400km')
minor_grid = os.path.join(regions_geodatabase, 'AlaskaPika_GridIndex_Minor_10km')
minor_sa = os.path.join(regions_geodatabase, 'StudyArea_GridIndex_Minor_10km')
validation_grid = os.path.join(regions_geodatabase, 'AlaskaPika_GridIndex_Validation_100km')
validation_sa = os.path.join(regions_geodatabase, 'StudyArea_GridIndex_Validation_100km')


#### GENERATE MAJOR GRID INDEX

# Create key word arguments for the major grid index
major_kwargs = {'distance': '400 Kilometers',
                'grid_field': 'grid_major',
                'work_geodatabase': work_geodatabase,
                'input_array': [total_feature],
                'output_array': [major_grid]
                }
major_sa_kwargs = {'work_geodatabase': work_geodatabase,
                    'input_array': [sa_feature, major_grid],
                    'output_array': [major_sa]
                    }

# Create the major grid index
if arcpy.Exists(major_grid) == 0:
    print('Creating major grid index...')
    arcpy_geoprocessing(create_grid_index, **major_kwargs)
    print('----------')
else:
    print('Major grid index already exists.')
    print('----------')

# Subset major grids for North American Beringia
if arcpy.Exists(major_sa) == 0:
    print('Subsetting major grids for North American Beringia...')
    arcpy_geoprocessing(select_location, **major_sa_kwargs)
    print('----------')
else:
    print('Major grids for North American Beringia already exist.')
    print('----------')

#### GENERATE MINOR GRID INDEX

# Create key word arguments for the minor grid index
minor_kwargs = {'distance': '10 Kilometers',
                'grid_field': 'grid_minor',
                'work_geodatabase': work_geodatabase,
                'input_array': [major_grid, major_grid],
                'output_array': [minor_grid]
                }
minor_sa_kwargs = {'work_geodatabase': work_geodatabase,
                    'input_array': [sa_feature, minor_grid],
                    'output_array': [minor_sa]
                    }

# Create the minor grid index
if arcpy.Exists(minor_grid) == 0:
    print('Creating minor grid index...')
    arcpy_geoprocessing(create_grid_index, **minor_kwargs)
    print('----------')
else:
    print('Minor grid index already exists.')
    print('----------')

# Subset minor grids for North American Beringia
if arcpy.Exists(minor_sa) == 0:
    print('Subsetting minor grids for North American Beringia...')
    arcpy_geoprocessing(select_location, **minor_sa_kwargs)
    print('----------')
else:
    print('Minor grids for North American Beringia already exist.')
    print('----------')

#### GENERATE VALIDATION GRID INDEX

# Create key word arguments for the validation grid index
validation_kwargs = {'distance': '100 Kilometers',
                     'grid_field': 'grid_validation',
                     'work_geodatabase': work_geodatabase,
                     'input_array': [major_grid, major_grid],
                     'output_array': [validation_grid]
                     }
validation_sa_kwargs = {'work_geodatabase': work_geodatabase,
                         'input_array': [sa_feature, validation_grid],
                         'output_array': [validation_sa]
                         }

# Create the validation grid index
if arcpy.Exists(validation_grid) == 0:
    print('Creating validation grid index...')
    arcpy_geoprocessing(create_grid_index, **validation_kwargs)
    print('----------')
else:
    print('Validation grid index already exists.')
    print('----------')

# Subset validation grids for North American Beringia
if arcpy.Exists(validation_sa) == 0:
    print('Subsetting validation grids for North American Beringia...')
    arcpy_geoprocessing(select_location, **validation_sa_kwargs)
    print('----------')
else:
    print('Validation grids for North American Beringia already exist.')
    print('----------')



#### CREATE GRID BLOCKS FOR MAJOR GRIDS

block_kwargs = {'tile_name': 'grid_major',
                'distance': '5000 Meters',
                'work_geodatabase': work_geodatabase,
                'input_array': [major_sa, total_raster],
                'output_folder': os.path.join(data_folder, 'grid_major/full')
                }

# Create buffered tiles for the major grid
arcpy_geoprocessing(create_buffered_tiles, check_output=False, **block_kwargs)
print('----------')

#### CREATE GRID BLOCKS FOR MAJOR GRIDS IN NORTH AMERICAN BERINGIA

block_sa_kwargs = {'tile_name': 'grid_major',
                    'distance': '5000 Meters',
                    'work_geodatabase': work_geodatabase,
                    'input_array': [major_sa, sa_raster],
                    'output_folder': os.path.join(data_folder, 'grid_major/studyarea')
                    }

# Create buffered tiles for the major grid
arcpy_geoprocessing(create_buffered_tiles, check_output=False, **block_sa_kwargs)
print('----------')

#### CREATE GRID TILES FOR MINOR GRIDS IN NORTH AMERICAN BERINGIA

tile_sa_kwargs = {'tile_name': 'grid_minor',
                   'distance': '10 Meters',
                   'work_geodatabase': work_geodatabase,
                   'input_array': [minor_sa, sa_raster],
                   'output_folder': os.path.join(data_folder, 'grid_minor/studyarea')
                   }

# Create buffered tiles for the minor grid
arcpy_geoprocessing(create_buffered_tiles, check_output=False, **tile_sa_kwargs)
print('----------')

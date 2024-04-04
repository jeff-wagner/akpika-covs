# akpika-covs
Compilation of normalized spatial covariates for use in analysis of collared pika (*Ochotona collaris*) population densities.

*Author*: Jeff Wagner, University of Wyoming

*Created On*: 2022-08-26

*Last Updated*: 2023-10-23

*Description*: Scripts for data acquisition, data manipulation, and post-processing of climatological and spectral covariates. Code for statistical modeling and visualization of collared pika (*Ochotona collaris*) densities in Alaska are performed in a separate repository. 

## Getting Started

These instructions will enable you to run scripts to estimate and evaluate drivers of density of collared pika at sites across Alaska. The scripts integrate multiple systems: Google Earth Engine, Python-based ArcGIS Pro, Python, and R. The scripts that have dependencies on ArcGIS Pro must be run on Windows machines, all other scripts are platform independent. Reproducing the results will require proper execution of all scripts. Scripts can be modified to apply to other data or study regions.

### Prerequisites

1. ArcGIS Pro 2.5.1+
   1. Python 3.6.9+
   3. numpy 1.16.5+
   4. pandas 0.25.1+
2. Access to Google Earth Engine
3. Access to Google Cloud Compute (or create virtual machines by other means or run all scripts locally)
4. Ubuntu 18.04 LTS (if provisioning virtual machines)
5. Anaconda 3.7 Build 2020.02+
   1. Python 3.7.6+
   3. numpy 1.18.1+
   4. pandas 1.0.1+
   5. seaborn 0.10.0+
   6. matplotlib 3.1.3+
   7. scikit-learn 0.22.1+
   8. lightgbm 2.3.1+
   8. google-api-python-client 1.8.3+
   9. google-auth-oauthlib 0.4.1+
   10. GPy 1.9.9+
   11. GPyOpt 1.2.6+
   12. PyDrive 1.3.1+
6. R 4.0.0+
   1. dplyr 0.8.5+
   2. raster 3.1-5+
   3. rgdal 1.4-8+
   4. sp 1.4-1+
   5. stringr 1.4.0+
   6. tidyr 1.0.2+
   9. spatialEco 1.3-7+
   10. parallel
   11. nimble
   12. mcmc
   13. coda
   14. MCMCvis
7. RStudio Server 1.2.5042+

## Usage
* The analysis is organized in the following manner:
  +  **00_init:** Initialize R environment: set base directory (or use optional R project directory), file paths, and load required packages.
  +  **01_dataSurveys:** Data management of survey observations & GPS tracks.
  +  **02_dataCovariates:** Data requisition and management of remote-sensed covariates; data management of survey covariates. See README in data_akveg-map folder for more details on usage, adapted from [work by colleagues](https://github.com/accs-uaa/akveg-map) at the [Alaska Center for Conservation Science](https://accs.uaa.alaska.edu/).


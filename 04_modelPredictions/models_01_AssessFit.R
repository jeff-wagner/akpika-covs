# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Top Models: Evaluate Fit
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Top Models: Evaluate Fit" fits the top models and tests goodness of fit.
# ---------------------------------------------------------------------------

# Read in the model set
load("03_distSamp_Models/modelSet.RData")

# Fit a model: use your best-supported model (Lowest AICc)
expMod.Selection

## Top model - Climate & Productivity
      # Define model covariates
      covs.climateProductivity <- data.frame(precip=transect.covs$precip,
                                             summerWarmth=transect.covs$summerWarmth,
                                             januaryMinTemp=transect.covs$januaryMinTemp,
                                             ndvi=transect.covs$ndvi)
      # Assess multicolinearity
      vif(climateProductivity, type = "state")
      
      # Goodness of fit of the best performing models: the model is a good fit if results of some or all of these tests show p > 0.05
      fitstats <- function(climateProductivity) {
         observed <- getY(climateProductivity@data)
         expected <- fitted(climateProductivity)
         resids <- residuals(climateProductivity)
         sse <- sum(resids^2)
         chisq <- sum((observed - expected)^2 / expected)
         freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
         out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
         return(out)
       }
      (pb.climateProductivity <- parboot(climateProductivity, fitstats, nsim=500, report=1))

## Second best model - Climate
      # Define model covariates
      covs.climate <- data.frame(precip=transect.covs$precip,
                                 summerWarmth=transect.covs$summerWarmth,
                                 januaryMinTemp=transect.covs$januaryMinTemp)
      # Assess multicolinearity
      vif(climate, type = "state")
      
      # Goodness of fit of the best performing models: the model is a good fit if results of some or all of these tests show p > 0.05
      fitstats <- function(climate) {
         observed <- getY(climate@data)
         expected <- fitted(climate)
         resids <- residuals(climate)
         sse <- sum(resids^2)
         chisq <- sum((observed - expected)^2 / expected)
         freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
         out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
         return(out)
       }
      (pb.climate <- parboot(climate, fitstats, nsim=500, report=1))
      
## Third best model - Productivity
      # Define model covariates
      covs.productivity <- data.frame(logs=transect.covs$logs,
                                      ndvi=transect.covs$ndvi,
                                      wetness=transect.covs$wetness)
      # Assess multicolinearity
      vif(productivity, type = "state")
      
      # Goodness of fit of the best performing models: the model is a good fit if results of some or all of these tests show p > 0.05
      fitstats <- function(productivity) {
        observed <- getY(productivity@data)
        expected <- fitted(productivity)
        resids <- residuals(productivity)
        sse <- sum(resids^2)
        chisq <- sum((observed - expected)^2 / expected)
        freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
        out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
        return(out)
      }
      (pb.productivity <- parboot(productivity, fitstats, nsim=500, report=1))
      
## Fourth best model - Climate & Topography
      # Define model covariates
      covs.climateTopo <- data.frame(precip=transect.covs$precip,
                                     summerWarmth=transect.covs$summerWarmth,
                                     januaryMinTemp=transect.covs$januaryMinTemp,
                                     roughness=transect.covs$roughness,
                                     elevation=transect.covs$elevation,
                                     northness=transect.covs$northness)
      # Assess multicolinearity
      vif(climateTopo, type = "state")
      
      # Goodness of fit of the best performing models: the model is a good fit if results of some or all of these tests show p > 0.05
      fitstats <- function(climateTopo) {
        observed <- getY(climateTopo@data)
        expected <- fitted(climateTopo)
        resids <- residuals(climateTopo)
        sse <- sum(resids^2)
        chisq <- sum((observed - expected)^2 / expected)
        freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
        out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
        return(out)
      }
      (pb.climateTopo <- parboot(climateTopo, fitstats, nsim=500, report=1))

save.image(file = "./04_modelPredictions/ws_modelFit.RData")

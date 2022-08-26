# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Top Models: Make Predictions
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Top Models: Make Predictions" uses the distance sampling model set to make predictions.
# ---------------------------------------------------------------------------

# Read in the model set
load("04_modelPredictions/ws_modelFit.RData")

# Predict transect-level density estimates -----------------------------------
# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients from the best-supported models, combined with the covariate values for each transect.

## Top model - Climate & Productivity
pred.climateProductivity <- predict(climateProductivity, type='state', newdata=covs.climateProductivity, appendData=TRUE)
summary(pred.climateProductivity)

pred.climateProductivity <- pred.climateProductivity %>% 
  arrange(Predicted)

hist(pred.climateProductivity$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(pred.climateProductivity, file="output/density_climateProductivity.csv")

## Next best model - Climate
pred.climate <- predict(climate, type='state', newdata=covs.climate, appendData=TRUE)
summary(pred.climate)

pred.climate <- pred.climate %>% 
  arrange(Predicted)

hist(pred.climate$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(pred.climate, file="output/density_climate.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------

## Top model - Climate & Productivity
      # Define parameter means
      mean.precip <- mean(covs.climateProductivity$precip)
      mean.summerWarmth <- mean(covs.climateProductivity$summerWarmth)
      mean.januaryMinTemp <- mean(covs.climateProductivity$januaryMinTemp)
      mean.ndvi <- mean(covs.climateProductivity$ndvi)
      
      # Create a sequence for each variable
      precip <- seq(min(covs.climateProductivity$precip), max(covs.climateProductivity$precip), length = 100)
      summerWarmth <- seq(min(covs.climateProductivity$summerWarmth), max(covs.climateProductivity$summerWarmth), length = 100)
      januaryMinTemp <- seq(min(covs.climateProductivity$januaryMinTemp), max(covs.climateProductivity$januaryMinTemp), length = 100)
      ndvi <- seq(min(covs.climateProductivity$ndvi), max(covs.climateProductivity$ndvi), length = 100)
      
      df.climateProductivityPrecip <- data.frame(precip = precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp, ndvi = mean.ndvi)
      df.climateProductivitySummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, januaryMinTemp = mean.januaryMinTemp, ndvi = mean.ndvi)
      df.climateProductivityJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = januaryMinTemp, ndvi = mean.ndvi)
      df.climateProductivityNDVI <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp, ndvi = ndvi)
      
      predict.climateProductivityPrecip <- predict(climateProductivity, type="state", newdata=df.climateProductivityPrecip, appendData=TRUE)
      predict.climateProductivitySummerWarmth <- predict(climateProductivity, type="state", newdata=df.climateProductivitySummerWarmth, appendData=TRUE)
      predict.climateProductivityJanuaryMinTemp <- predict(climateProductivity, type="state", newdata=df.climateProductivityJanuaryMinTemp, appendData=TRUE)
      predict.climateProductivityNDVI <- predict(climateProductivity, type="state", newdata=df.climateProductivityNDVI, appendData=TRUE)

## Second best model - Climate
      # Define parameter means
      mean.precip <- mean(covs.climate$precip)
      mean.summerWarmth <- mean(covs.climate$summerWarmth)
      mean.januaryMinTemp <- mean(covs.climate$januaryMinTemp)
      
      # Create a sequence for each variable
      precip <- seq(min(covs.climate$precip), max(covs.climate$precip), length = 100)
      summerWarmth <- seq(min(covs.climate$summerWarmth), max(covs.climate$summerWarmth), length = 100)
      januaryMinTemp <- seq(min(covs.climate$januaryMinTemp), max(covs.climate$januaryMinTemp), length = 100)
      
      df.climatePrecip <- data.frame(precip = precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp)
      df.climateSummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, januaryMinTemp = mean.januaryMinTemp)
      df.climateJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = januaryMinTemp)
      
      
      predict.climatePrecip <- predict(climate, type="state", newdata=df.climatePrecip, appendData=TRUE)
      predict.climateSummerWarmth <- predict(climate, type="state", newdata=df.climateSummerWarmth, appendData=TRUE)
      predict.climateJanuaryMinTemp <- predict(climate, type="state", newdata=df.climateJanuaryMinTemp, appendData=TRUE)

## Third best model - Productivity
      # Define parameter means
      mean.logs <- mean(covs.productivity$logs)
      mean.ndvi <- mean(covs.productivity$ndvi)
      mean.wetness <- mean(covs.productivity$wetness)
      
      # Create a sequence for each variable
      logs <- seq(min(covs.productivity$logs), max(covs.productivity$logs), length = 100)
      ndvi <- seq(min(covs.productivity$ndvi), max(covs.productivity$ndvi), length = 100)
      wetness <- seq(min(covs.productivity$wetness), max(covs.productivity$wetness), length = 100)
      
      df.productivityLogs <- data.frame(logs = logs, ndvi = mean.ndvi, wetness = mean.wetness)
      df.productivityNDVI <- data.frame(logs = mean.logs, ndvi = ndvi, wetness = mean.wetness)
      df.productivityWetness <- data.frame(logs = mean.logs, ndvi = mean.ndvi, wetness = wetness)
      
      
      predict.productivityLogs <- predict(productivity, type="state", newdata=df.productivityLogs, appendData=TRUE)
      predict.productivityNDVI <- predict(productivity, type="state", newdata=df.productivityNDVI, appendData=TRUE)
      predict.productivityWetness <- predict(productivity, type="state", newdata=df.productivityWetness, appendData=TRUE)            
      
## Fourth best model - Climate & Topography
      # Define parameter means
      mean.precip <- mean(covs.climateTopo$precip)
      mean.summerWarmth <- mean(covs.climateTopo$summerWarmth)
      mean.januaryMinTemp <- mean(covs.climateTopo$januaryMinTemp)
      mean.roughness <- mean(covs.climateTopo$roughness)
      mean.elevation <- mean(covs.climateTopo$elevation)
      mean.northness <- mean(covs.climateTopo$northness)
      
      # Create a sequence for each variable
      precip <- seq(min(covs.climateTopo$precip), max(covs.climateTopo$precip), length = 100)
      summerWarmth <- seq(min(covs.climateTopo$summerWarmth), max(covs.climateTopo$summerWarmth), length = 100)
      januaryMinTemp <- seq(min(covs.climateTopo$januaryMinTemp), max(covs.climateTopo$januaryMinTemp), length = 100)
      roughness <- seq(min(covs.climateTopo$roughness), max(covs.climateTopo$roughness), length = 100)
      elevation <- seq(min(covs.climateTopo$elevation), max(covs.climateTopo$elevation), length = 100)
      northness <- seq(min(covs.climateTopo$northness), max(covs.climateTopo$northness), length = 100)
      
      df.climateTopoPrecip <- data.frame(precip = precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp,
                                         roughness = mean.roughness, elevation = mean.elevation, northness = mean.northness)
      df.climateTopoSummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, januaryMinTemp = mean.januaryMinTemp,
                                               roughness = mean.roughness, elevation = mean.elevation, northness = mean.northness)
      df.climateTopoJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = januaryMinTemp,
                                                 roughness = mean.roughness, elevation = mean.elevation, northness = mean.northness)
      df.climateTopoRoughness <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp,
                                                 roughness = roughness, elevation = mean.elevation, northness = mean.northness)
      df.climateTopoElevation <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp,
                                                 roughness = mean.roughness, elevation = elevation, northness = mean.northness)
      df.climateTopoNorthness <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp,
                                                 roughness = mean.roughness, elevation = mean.elevation, northness = northness)
      
      
      predict.climateTopoPrecip <- predict(climateTopo, type="state", newdata=df.climateTopoPrecip, appendData=TRUE)
      predict.climateTopoSummerWarmth <- predict(climateTopo, type="state", newdata=df.climateTopoSummerWarmth, appendData=TRUE)
      predict.climateTopoJanuaryMinTemp <- predict(climateTopo, type="state", newdata=df.climateTopoJanuaryMinTemp, appendData=TRUE)
      predict.climateTopoRoughness <- predict(climateTopo, type="state", newdata=df.climateTopoRoughness, appendData=TRUE)
      predict.climateTopoElevation <- predict(climateTopo, type="state", newdata=df.climateTopoElevation, appendData=TRUE)
      predict.climateTopoNorthness <- predict(climateTopo, type="state", newdata=df.climateTopoNorthness, appendData=TRUE)
      
save.image(file = "./04_modelPredictions/ws_modelPredictions.RData")

# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Top Models: Plots
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Top Models: Plots" makes plots of predicted pika densities for each model parameter.
# ---------------------------------------------------------------------------

library(gridExtra)
library(grid)

# Read in the model predictions
load("./04_modelPredictions/ws_modelPredictions.RData")

# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------

## Top Model - Climate & Productivity ---
      # Precip
      plot.climateProductivityPrecip <- ggplot(predict.climateProductivityPrecip, aes(x=precip, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual precipitation (mm)")+
        theme(axis.title.y = element_blank())
      
      plot.climateProductivityPrecip
      
      # Summer warmth
      plot.climateProductivitySummerWarmth <- ggplot(predict.climateProductivitySummerWarmth, aes(x=summerWarmth, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual summer warmth index (°C)")+
        theme(axis.title.y = element_blank())
      
      plot.climateProductivitySummerWarmth
      
      # January Minimum Temperature
      plot.climateProductivityJanuaryMinTemp <- ggplot(predict.climateProductivityJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual January minimum temperature (°C)")+
        theme(axis.title.y = element_blank())
      
      plot.climateProductivityJanuaryMinTemp
      
      # NDVI
      plot.climateProductivityNDVI <- ggplot(predict.climateProductivityNDVI, aes(x=ndvi, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("NDVI")+
        theme(axis.title.y = element_blank())
      
      plot.climateProductivityNDVI
      
      # Stitch plots together
      png("./output/figures/climateProductivity.png", units = "in", width = 8, height = 8, res = 300)
      plot.climateProductivity <- grid.arrange(arrangeGrob(plot.climateProductivityPrecip, plot.climateProductivitySummerWarmth,
                                                           plot.climateProductivityJanuaryMinTemp, plot.climateProductivityNDVI,
                                                           layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2),
                                                           left = textGrob(expression(
                                                             paste("Denisty (Pika / km" ^ "2"*")")),
                                                             rot = 90,  gp = gpar(fontsize = 10))))
      dev.off()

## Second best model - Climate ---
      # Precip
      plot.climatePrecip <- ggplot(predict.climatePrecip, aes(x=precip, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual precipitation (mm)")+
        theme(axis.title.y = element_blank())
      
      plot.climatePrecip
      
      # Summer warmth
      plot.climateSummerWarmth <- ggplot(predict.climateSummerWarmth, aes(x=summerWarmth, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual summer warmth index (°C)")+
        theme(axis.title.y = element_blank())
      
      plot.climateSummerWarmth
      
      # January minimum temperature
      plot.climateJanuaryMinTemp <- ggplot(predict.climateJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual January minimum temperature (°C)")+
        theme(axis.title.y = element_blank())
      
      plot.climateJanuaryMinTemp
      
      # Stitch plots together
      png("./output/figures/climate.png", units = "in", width = 8, height = 8, res = 300)
      plot.climate <- grid.arrange(arrangeGrob(plot.climatePrecip, plot.climateSummerWarmth,
                                               grid::nullGrob(), plot.climateJanuaryMinTemp, grid::nullGrob(),
                                               layout_matrix = matrix(c(1,1,2,2,3,4,4,5), byrow = TRUE, ncol = 4),
                                               left = textGrob(expression(
                                                 paste("Denisty (Pika / km" ^ "2"*")")),
                                                 rot = 90,  gp = gpar(fontsize = 10))))
      dev.off()

## Third best model - Productivity ---
      # Length of growing season (logs)
      plot.productivityLOGS <- ggplot(predict.productivityLogs, aes(x=logs, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Length of growing season (days)")+
        theme(axis.title.y = element_blank())
      plot.productivityLOGS
      
      # NDVI
      plot.productivityNDVI <- ggplot(predict.productivityNDVI, aes(x=ndvi, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("NDVI")+
        theme(axis.title.y = element_blank())
      plot.productivityNDVI
      
      # Wetness
      plot.productivityWetness <- ggplot(predict.productivityWetness, aes(x=wetness, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Topographic wetness index")+
        theme(axis.title.y = element_blank())
      plot.productivityWetness
      
      # Stitch plots together
      png("./output/figures/productivity.png", units = "in", width = 8, height = 8, res = 300)
      plot.productivity <- grid.arrange(arrangeGrob(plot.productivityLOGS, plot.productivityNDVI, 
                                                    grid::nullGrob(), plot.productivityWetness, grid::nullGrob(),
                                                           layout_matrix = matrix(c(1,1,2,2,3,4,4,5), byrow = TRUE, ncol = 4),
                                                           left = textGrob(expression(
                                                             paste("Denisty (Pika / km" ^ "2"*")")),
                                                             rot = 90,  gp = gpar(fontsize = 10))))
      dev.off()

## Fourth best model - Climate & Topography ---
      # Precip
      plot.climateTopoPrecip <- ggplot(predict.climateTopoPrecip, aes(x=precip, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual precipitation (mm)")+
        theme(axis.title.y = element_blank())
      
      plot.climateTopoPrecip
      
      # Summer warmth
      plot.climateTopoSummerWarmth <- ggplot(predict.climateTopoSummerWarmth, aes(x=summerWarmth, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual summer warmth index (°C)")+
        theme(axis.title.y = element_blank())
      
      plot.climateTopoSummerWarmth
      
      # January Minimum Temperature
      plot.climateTopoJanuaryMinTemp <- ggplot(predict.climateTopoJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Mean annual January minimum temperature (°C)")+
        theme(axis.title.y = element_blank())
      
      plot.climateTopoJanuaryMinTemp
      
      # Roughness
      plot.climateTopoRoughness <- ggplot(predict.climateTopoRoughness, aes(x=roughness, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Topographic roughness index")+
        theme(axis.title.y = element_blank())
      
      plot.climateTopoRoughness
      
      # Elevation
      plot.climateTopoElevation <- ggplot(predict.climateTopoElevation, aes(x=elevation, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Elevation (m)")+
        theme(axis.title.y = element_blank())
      
      plot.climateTopoElevation
      
      # Northness
      plot.climateTopoNorthness <- ggplot(predict.climateTopoNorthness, aes(x=northness, y=Predicted)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
        theme_classic()+
        xlab("Aspect (northness)")+
        theme(axis.title.y = element_blank())
      
      plot.climateTopoNorthness
      
      # Stitch plots together
      png("./output/figures/climateTopo.png", units = "in", width = 8, height = 12, res = 300)
      plot.climateTopo <- grid.arrange(arrangeGrob(plot.climateTopoPrecip, plot.climateTopoSummerWarmth, plot.climateTopoJanuaryMinTemp, 
                                                   plot.climateTopoRoughness, plot.climateTopoElevation, plot.climateTopoNorthness,
                                                           layout_matrix = matrix(c(1,2,3,4,5,6), byrow = TRUE, ncol = 2),
                                                           left = textGrob(expression(
                                                             paste("Denisty (Pika / km" ^ "2"*")")),
                                                             rot = 90,  gp = gpar(fontsize = 10))))
      dev.off()
      
## Model tables
library(gplots)
pdf("./output/figures/explanatoryModels.pdf", width = 10, height = 4)
grid.table(expMod.Selection)
dev.off()

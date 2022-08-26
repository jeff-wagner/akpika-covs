# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Model Averaged Plots
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Model Averaged Plots" creates plots for model-averaged estimates of pika density using the top model set.
# ---------------------------------------------------------------------------

load("05_modelAveraging/ws_modAvg.RData")

# Plot -----------------------------
# Precip
plot.avgPrecip <- ggplot(df.predict.avgPrecip, aes(x=precip, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Mean annual precipitation (mm)")+
  theme(axis.title.y = element_blank())

plot.avgPrecip

# Summer warmth
plot.avgSummerWarmth <- ggplot(df.predict.avgSummerWarmth, aes(x=summerWarmth, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Mean annual summer\nwarmth index (°C)")+
  theme(axis.title.y = element_blank())

plot.avgSummerWarmth

# January minimum temperature
plot.avgJanuaryMinTemp <- ggplot(df.predict.avgJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Mean annual January\nminimum temperature (°C)")+
  theme(axis.title.y = element_blank())

plot.avgJanuaryMinTemp

# Length of growing season
plot.avgLogs <- ggplot(df.predict.avgLogs, aes(x=logs, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Length of growing season (days)")+
  theme(axis.title.y = element_blank())

plot.avgLogs

# NDVI
plot.avgNDVI <- ggplot(df.predict.avgNDVI, aes(x=ndvi, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("NDVI")+
  theme(axis.title.y = element_blank())

plot.avgNDVI

# Wetness
plot.avgWetness <- ggplot(df.predict.avgWetness, aes(x=wetness, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Topographic wetness index")+
  theme(axis.title.y = element_blank())

plot.avgWetness

# Roughness
plot.avgRoughness <- ggplot(df.predict.avgRoughness, aes(x=roughness, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Topographic roughness index")+
  theme(axis.title.y = element_blank())

plot.avgRoughness

# Elevation
plot.avgElevation <- ggplot(df.predict.avgElevation, aes(x=elevation, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Elevation (m)")+
  theme(axis.title.y = element_blank())

plot.avgElevation

# Northness
plot.avgNorthness <- ggplot(df.predict.avgNorthness, aes(x=northness, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Aspect (northness)")+
  theme(axis.title.y = element_blank())

plot.avgNorthness

# Predictions by location
predByLoc <- df.modavg.Pred %>%
  group_by(location) %>% 
  summarize(predicted = mean(Predicted), 
            lower = mean(lower),
            upper = mean(upper))
plot.Location <- ggplot(predByLoc, aes(x=location, y = predicted)) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  theme_classic()+
  xlab("Site Location")+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot.Location

# Stitch plots together -----------------------------------
png("./output/figures/averaged.png", units = "in", width = 11, height = 15, res = 300)
plot.average <- grid.arrange(arrangeGrob(plot.avgPrecip, plot.avgSummerWarmth, plot.avgJanuaryMinTemp, 
                                         plot.avgLogs, plot.avgNDVI, plot.avgWetness,
                                         plot.avgRoughness, plot.avgElevation, plot.avgNorthness,
                                         grid::nullGrob(), plot.Location, grid::nullGrob(),
                                         layout_matrix = matrix(c(1,2,3,
                                                                  4,5,6,
                                                                  7,8,9,
                                                                  10,11,12), byrow = TRUE, ncol = 3),
                                         left = textGrob(expression(
                                           paste("Denisty (Pika / km" ^ "2"*")")),
                                           rot = 90,  gp = gpar(fontsize = 10))))
dev.off()

## Model-averaged coefficient table
library(gplots)
pdf("./output/figures/modavgCoefficients.pdf", width = 8, height = 4)
grid.table(df.modavgEstimates)
dev.off()

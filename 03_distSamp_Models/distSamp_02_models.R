# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Distance Sampling Models
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Distance Sampling Models" builds distance sampling models in unmarked and performs model selection using AICc.
# ---------------------------------------------------------------------------

# We will use a simple set of models with the hazard detection function, constant detection probability, 
# and sets of covariates on density, each representing an a priori hypothesis we are testing.
# umf is the unmarked data frame, and we want the output as density in square km.

# Read in the observation and covariate data from our data management scripts.
source("03_distSamp_Models/distSamp_01_formatting.r")

# View the data one more time to verify that each row contains the transect observation data and matching 
# transect-level covariates. ** Make sure the first column matches the Location, Site, and transect columns. **
umf

# Part 1: Detection Function Models  -----------------------------------------------------------------------
# Explore the best detection function option: halfnorm, hazard, or uniform. Select via AICc.
halfnorm <- distsamp(~1 ~1, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")
hazard <- distsamp(~1 ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
uniform <- distsamp(~1 ~1, umf, keyfun="uniform", output="density", unitsOut="kmsq")

# AICc model selection from AICcmodavg
detFun.List <- list("halfnormal" = halfnorm, "hazard" = hazard, "uniform" = uniform)

detFun.Selection <- aictab(detFun.List)
detFun.Selection

# Part 2: Detection Probability Models  --------------------------------------------------------------------
# Explore covariates that hypothesize might influence detection probability.
# Use the 'scale' command for continuous variables to improve model fit and to allow comparisons of coefficients on a standardized scale.
det1 <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
det2 <- distsamp(~observer ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
det3 <- distsamp(~Observer ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
det4 <- distsamp(~scale(tempc) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
det5 <- distsamp(~scale(windms) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
det6 <- distsamp(~scale(start.hr) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
det7 <- distsamp(~scale(day.of.year) ~1, umf, keyfun ="hazard", output="density", unitsOut = "kmsq")

# AICc model selection from AICcmodavg
detMod.List <- list("searchEffort" = det1, "observerGroup" = det2, "observerInd" = det3,
                    "surveyTemp" = det4, "surveyWind" = det5, "startHr" = det6, "dayOfYear" = det7)

detMod.Selection <- aictab(detMod.List)
detMod.Selection

# Part 3: Explanatory  ---------------------------------------------------------------------------
null <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
climate <- distsamp(~scale(search.speed) ~scale(precip) + scale(summerWarmth) + scale(januaryMinTemp), 
                    umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
topography <- distsamp(~scale(search.speed) ~scale(roughness) + scale(elevation) + scale(northness), 
                       umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
climateTopo <- distsamp(~scale(search.speed) ~scale(precip) + scale(summerWarmth) + scale(januaryMinTemp) +
                          scale(roughness) + scale(elevation) + scale(northness), 
                        umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
productivity <- distsamp(~scale(search.speed) ~scale(logs) + scale(ndvi) + scale(wetness),
                         umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
climateProductivity <- distsamp(~scale(search.speed) ~scale(precip) + scale(summerWarmth) +
                                  scale(januaryMinTemp) + scale(ndvi), 
                                umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")

# AICc model selection from AICcmodavg
expMod.List <- list("null"=null, 
                    "climate"=climate, 
                    "topography"=topography,
                    "climate & topography"=climateTopo,
                    "productivity"=productivity,
                    "climate & productivity"=climateProductivity)

expMod.Selection <- aictab(expMod.List)
expMod.Selection

# Part 4: Parameter Estimates – look for significance ---------------------
summary(climateProductivity)
confint(climateProductivity, type = "state")

summary(climate)
confint(climate, type = "state")

summary(productivity)
confint(productivity, type = "state")

summary(climateTopo)
confint(climateTopo, type = "state")

save.image(file = "./03_distSamp_Models/modelSet.RData")
#rm(list=setdiff(ls(), c("umf", "modelList", "modelList.sub", "models", "transect.covs")))
   
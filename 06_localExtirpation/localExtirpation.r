# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Historical Site Extirpation
# Author: Jeff Wagner
# Last Updated: 2022-08-22
# Usage: Must be executed in R 4.0.0+.
# Description: "Historical Site Extirpation" conducts a simple logistic regression of occupied vs. unoccupied historical sites.
# ---------------------------------------------------------------------------

# Read in the initialization script (reads in required packages, etc)
source("00_init/initscript.r")

## Prepare data
# Read in the data
path <- 'data/pika_site_occupation.csv'
siteOcc <- read.csv(path)

# We are only interested in historically occupied sites for the purposes of this analysis:
siteOcc <- siteOcc %>% 
  filter(random_site == 0)

# Read in site (transect) covariates from the distance sampling analysis
source("02_dataCovariates/covs_02_CompileCovariates.R")

# Filter to keep only one row per site & join to siteOcc
transect.covs <- transect.covs %>% 
  select(-(obs1.4:search.speed), -latitude, -longitude) %>% # get rid of transect-specific columns
  distinct() %>% # filter out duplicate rows
  filter(Site %in% unique(siteOcc$Site)) # keep only historical sites

siteOcc <- siteOcc %>% 
  left_join(., transect.covs, by = c("Location", "Site", "Year"))

# Correct column data types
siteOcc$Org <- as.factor(siteOcc$Org)
siteOcc$Location <- as.factor(siteOcc$Location)
siteOcc$occ.status <- as.factor(siteOcc$occ.status)
siteOcc$Date <- as.Date(siteOcc$Date, format = '%m/%d/%Y')

# Convert occupied site status to binary variable
siteOcc$occ.status <- ifelse(siteOcc$occ_status=="occupied", 0, 1)

# Standardize variables
siteOcc$st.precip <- (siteOcc$precip-mean(siteOcc$precip))/sd(siteOcc$precip)
siteOcc$st.summerWarmth <- (siteOcc$summerWarmth-mean(siteOcc$summerWarmth))/sd(siteOcc$summerWarmth)
siteOcc$st.januaryMinTemp <- (siteOcc$januaryMinTemp-mean(siteOcc$januaryMinTemp))/sd(siteOcc$januaryMinTemp)

# Write out site-level data with covariates
# save(siteOcc, file = "./data/siteOcc.RDS")
# write.csv(siteOcc, file = "./data/siteOcc.csv")

## Logistic regression models
climate <- glm(occ.status ~ precip + summerWarmth + januaryMinTemp, data = siteOcc, family = "binomial")
summary(climate)
confint(climate)

productivity <- glm(occ.status ~ logs + ndvi + wetness, data = siteOcc, family = "binomial")
summary(productivity)
confint(productivity)

topography <- glm(occ.status ~ elevation + roughness + northness, data = siteOcc, family = "binomial")
summary(topography)
confint(topography)

climateProductivity <- glm(occ.status ~ precip + summerWarmth + januaryMinTemp + ndvi, data = siteOcc, family = "binomial")
summary(climateProductivity)

latitude <- glm(occ.status ~ latitude, data = siteOcc, family = "binomial")
summary(latitude)

## Logistic regression by region
# Define 3 different regions along latitudinal gradient
siteOcc$region <- ifelse(siteOcc$Location == "Hatcher Pass" | siteOcc$Location == "Southcentral" | siteOcc$Location == "JBER",
                         "Southcentral", siteOcc$Location)
siteOcc$region <- ifelse(siteOcc$Location == "Denali" | siteOcc$Location == "Paxson",
                         "Interior", siteOcc$region)
siteOcc$region <- ifelse(siteOcc$Location == "Steese",
                         "Subarctic", siteOcc$region)
siteOcc$region <- factor(siteOcc$region, levels = c("Southcentral", "Interior", "Subarctic"))
siteOcc$region.num <- as.numeric(siteOcc$region)

# libraries
library(rjags)
library(jagsUI)
library(ggmcmc)

nregions <- nlevels(siteOcc$region)

# Bayesian model, this is JAGS code
mod <- "
model{
  for(i in 1:length(y)){
        y[i] ~ dbinom(p[i], 1)  # Each observation follows a binomial distribution with 1 trial and probability of extirpation (p)
        logit(p[i]) = b0[r[i]] + b1[r[i]]*x1[i] + b2[r[i]]*x2[i] + b3[r[i]]*x3[i]  # this probability has an inverse logit relationship with the 
  }
    for(r in 1:nregions){
    b0[r] ~ dnorm(sigma0, tau0)
    b1[r] ~ dnorm(sigma1, tau1)
    b2[r] ~ dnorm(sigma2, tau2)
    b3[r] ~ dnorm(sigma3, tau3)
    }
    
    sigma0 ~ dnorm(0, 0.0001)
    sigma1 ~ dnorm(0, 0.0001)
    sigma2 ~ dnorm(0, 0.0001)
    sigma3 ~ dnorm(0, 0.0001)
    tau0 ~ dgamma(0.001, 0.001)
    tau1 ~ dgamma(0.001, 0.001)
    tau2 ~ dgamma(0.001, 0.001)
    tau3 ~ dgamma(0.001, 0.001)
}
"
dataList <- list(region=siteOcc$region.num,
                 y=siteOcc$occ.status,
                 x1=siteOcc$st.precip,
                 x2=siteOcc$st.summerWarmth,
                 x3=siteOcc$st.januaryMinTemp)

reprex({
  library(rjags)
  library(jagsUI)
  
  dataList <- list(region= c(1,1,1,1,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,1,1,2,2,2,2,1,1,2,2,1,1),
                   y= c(0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0),
                   x1= runif(31,-1,1),
                   x2= runif(31,-1,1),
                   x3= runif(31,-1,1))
  
  mod <- "
model{
  for(i in 1:length(y)){
        y[i] ~ dbern(p[region[i]]) 
  }
  for(r in 1:max(region)){
          logit(p[region[i]] = b0[r] + b1[r]*x1[r[i]] + b2[r]*x2[r[i]] + b3[r]*x3[r[i]]
          b0[r] ~ dnorm(sigma, tau)
          b1[r] ~ dnorm(sigma, tau)
          b2[r] ~ dnorm(sigma, tau)
          b3[r] ~ dnorm(sigma, tau)
  }
    
    sigma ~ dnorm(0, 0.0001)
    tau ~ dgamma(0.001, 0.001)

}
"
})

params <- c("b0", "b1", "b2", "b3", "p")


# Bayesian model, this is JAGS code
mod1 <- "
model{
  for(i in 1:length(y)){
        y[i] ~ dbern(p[region[i]]) 
  }
  for(r in 1:max(region)){
          # logit(p[r[i]]) = b0[r] + b1[r]*x1[r[i]] + b2[r]*x2[r[i]] + b3[r]*x3[r[i]]
          logit(p[r[i]]) = b0 + b1*x1[r[i]] + b2*x2[r[i]] + b3*x3[r[i]]
          # b0[r] ~ dnorm(sigma, tau)
          # b1[r] ~ dnorm(sigma, tau)
          # b2[r] ~ dnorm(sigma, tau)
          # b3[r] ~ dnorm(sigma, tau)
  }
    b0 ~ dnorm(0, 0.0001)
    b1 ~ dnorm(0, 0.0001)
    b2 ~ dnorm(0, 0.0001)
    b3 ~ dnorm(0, 0.0001)
    # sigma ~ dnorm(0, 0.0001)
    # tau ~ dgamma(0.001, 0.001)

}
"

dataList1 <- list(region=siteOcc$region.num,
                 y=siteOcc$occ.status,
                 x1=siteOcc$precip,
                 x2=siteOcc$summerWarmth,
                 x3=siteOcc$januaryMinTemp)
params <- c("b0", "b1", "b2", "b3", "p")
inits <- NULL

output <- jags(data = dataList1,
               inits = inits,
               parameters.to.save = params,
               model.file = textConnection(mod1),
               n.chains = 3,
               n.adapt = 10000,
               n.iter = 1000000,
               n.burnin = 1000,
               n.thin = 100,
               parallel = FALSE,
               DIC = TRUE)
output

## diagnostic plots
traceplot(output, parameters=c("b0", "b1", "b2", "b3", "p"))
autocorr.plot(output$samples[,1:7],lag.max=40,auto.layout=TRUE,ask=TRUE)

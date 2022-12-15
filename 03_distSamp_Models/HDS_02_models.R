# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Bayesian HDS models
# Author: Jeff Wagner
# Last Updated: 2022-11-18
# Usage: Must be executed in R 4.0.0+.
# Description: "Bayesian HDS models" builds and fits Bayesian Hierarchical Distance Sampling (HDS) models for the collared pika data.
# ---------------------------------------------------------------------------

library(rjags)
library(jagsUI)

load("./data/DSdata.RData")

# Subset for columns of interest
data <- win.data[c(1:11, 18, 21:23)]

# Code from AHM Vol 1 Ch 8.5 (pg 453)
# BUGS model specification for line-transect HDS
cat("
model{
# Priors
  alpha0 ~ dunif(-10,10)
  alpha1 ~ dunif(-10,10)
  beta0 ~ dunif(-10,10)
  beta1 ~ dunif(-10,10)
  sigma.site ~ dunif(0,10)
  tau <- 1/(sigma.site*sigma.site)

  for(i in 1:nind){ # Loop through all individuals
    dclass[i] ~ dcat(fc[transect[i],]) # Part 1 of HM, distance class of each ind. ~ cat(cell prob. vector)
  }

  for(s in 1:ntransects){     # loop through transects
    # Construct cell probabilities for nD multinomial cells
    for(k in 1:nreps[s]) # reps per site
    for(g in 1:nD){                 # loop through each distance class
      log(p[s,g,k]) <- -midpt[g] * midpt[g] / (2*sigma[s]*sigma[s])   # midpt = mid-point of each cell
      pi[s,g,k] <- delta / B          # probability per interval, feed 3D array of expected proportions of pika/nD per site, per rep, per distance class
      f[s,g,k] <- p[s,g,k] * pi[s,g,k]
      fc[s,g,k] <- f[,k] / pcap[s,k]
    }
    pcap[s,k] <- sum(f[s,1:nD,k])           # Pr(capture): sum of rectangular areas

    ncap[s,k] ~ dbin(pcap[s], N[s])   # Part 2 of HM
    
    # OFFSET TERM - This is where I need help
    N[s,k] ~ dpois(lambda.abs[s])         # Part 3 of HM
  
    # Do I need to use site area here instead of transect length (which is in m, so I convert to km)?
    lambda.abs[s] <- lambda[s]*(transectLength[s]/1000) # lambda is a site density in indiviudals per sq km, lambda.abs is the number of expected per km surveyed?
    log(lambda[s]) <- beta0 + beta1 * summerWarmth[s] + site.eff[s] # linear model abundance w/ random effect of site
    log(sigma[s])<- alpha0 + alpha1*searchSpeed[s,k]      # linear model detection, ADD k loop for SS per replicate
    
    site.eff[s] ~ dnorm(0, tau) # random effect of site
    
  }
  
  # Derived parameters
  
  # for(j in 1:nsites)){
  #         Nsite[j] <- N[]
  # }

  Ntotal <- sum(N[])
}
",fill=TRUE, file = "./models/SW_model.txt")

# Inits
Nst <- win.data$ncap + 1

inits <- function(){
  list(
    alpha0=rnorm(1,1.7,0.5), alpha1=0, beta0=rnorm(1,0,2), beta1=rnorm(1,0,2), N=Nst
)}

# Params to save
params <- c("alpha0", "alpha1", "beta0", "beta1", "lambda.abs", "Ntotal", "N")

# MCMC settings
ni <- 12000   ;   nb <- 2000   ;   nt <- 1   ;   nc <- 3

# Run JAGS (ART 1 min) and summarize posteriors
out <- jags(data = data, 
            inits = inits, 
            parameters.to.save = params,
            model.file = "./models/SW_model.txt", 
            n.thin = nt,
            n.chains = nc, 
            n.burnin = nb, 
            n.iter = ni, 
            parallel = FALSE)
print(out, 2)

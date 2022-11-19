# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Bayesian HDS models
# Author: Jeff Wagner
# Last Updated: 2022-11-18
# Usage: Must be executed in R 4.0.0+.
# Description: "Bayesian HDS models" builds and fits Bayesian Hierarchica Distance Sampling (HDS) models for the collared pika data.
# ---------------------------------------------------------------------------

library(rjags)
library(jagsUI)

load("./data/DSdata.RData")

data <- win.data[c(1:9, 19:20)]
data$nind <- 41
data$dclass <- data$dclass[1:41]
data$site <- data$site[1:41]

# Code from AHM Vol 1 Ch 8.5 (pg 453)
# BUGS model specification for line-transect HDS
cat("
model{
# Priors
  alpha0 ~ dunif(-10,10)
  alpha1 ~ dunif(-10,10)
  beta0 ~ dunif(-10,10)
  beta1 ~ dunif(-10,10)

  for(i in 1:nind){
    dclass[i] ~ dcat(fc[site[i],]) # Part 1 of HM
  }

  for(s in 1:nsites){
    # Construct cell probabilities for nD multinomial cells
    for(g in 1:nD){                 # midpt = mid-point of each cell
      log(p[s,g]) <- -midpt[g] * midpt[g] / (2*sigma[s]*sigma[s])
      pi[s,g] <- delta / B          # probability per interval
      f[s,g] <- p[s,g] * pi[s,g]
      fc[s,g] <- f[s,g] / pcap[s]
    }
    pcap[s] <- sum(f[s,])           # Pr(capture): sum of rectangular areas

    ncap[s] ~ dbin(pcap[s], N[s])   # Part 2 of HM
    N[s] ~ dpois(lambda[s])         # Part 3 of HM
    log(lambda[s]) <- beta0 + beta1 * summerWarmth[s] # linear model abundance
    log(sigma[s])<- alpha0 + alpha1*searchSpeed[s]      # linear model detection
  }
  # Derived parameters
  Ntotal <- sum(N[])
  area<- nsites*1*2*B  # Unit length == 1, half-width = B
  D<- Ntotal/area
}
",fill=TRUE, file = "./models/SW_model.txt")

# Inits
Nst <- data$ncap + 1
inits <- function(){list(alpha0=0, alpha1=0, beta0=0, beta1=0, N=Nst)}

# Params to save
params <- c("alpha0", "alpha1", "beta0", "beta1", "Ntotal","D", "N")

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
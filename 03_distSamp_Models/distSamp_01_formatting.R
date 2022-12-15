# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Bayesian HDS formatting
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Unmarked formatting" imports and explores the cleaned distance sampling and covariate data and formats it for analysis with unmarked.
# ---------------------------------------------------------------------------

# Read in the observation and covariate data from our data management scripts.
source("01_dataSurveys/data_01_Observations.r")
source("02_dataCovariates/covs_02_CompileCovariates.r")

# Part 1: Explore and cleanup the observation data  ------------------------------------------------------------

# The distance sampling observation data
head(pika.obs.t)

# Initial histogram of the data
hist(pika.obs.t$perp.dist)

# There are 71 transects where pika were observed and distance sampling recorded.
unique(pika.obs.t$transect)

# View the number of observations
dim(pika.obs.t)           #185 pika observations


#Truncate data at 70m since there are few observations beyond that
pika.obs.tr <- pika.obs.t %>% 
  filter(perp.dist < 70)

dim(pika.obs.tr)        #Now have 176 observations; removed 9 observations (4.9% of the data) 
hist(pika.obs.tr$perp.dist)

summary(pika.obs.tr)

# Convert character columns to factors
colType <- sapply(pika.obs.tr, typeof)

for(i in 1:length(colType)){
  if(class(pika.obs.tr[[i]]) == "character"){
    pika.obs.tr[[i]] <- as.factor(pika.obs.tr[[i]])
    print(paste(colnames(pika.obs.tr[i]), "converted to factor", sep = " "))
  }
}


# Part 2: Explore and cleanup the covariate data  -------------------------------------------------------------
# Transect-level covariates
head(transect.covs)
summary(transect.covs)

transect.covs <- transect.covs %>% 
  rename("Site"="Site.x") %>% 
  mutate("site"=as.numeric(as.factor(Site)))

# Convert character columns to factors

colType <- sapply(transect.covs, typeof)

for(i in 1:length(colType)){
  if(class(transect.covs[[i]]) == "character"){
    transect.covs[[i]] <- as.factor(transect.covs[[i]])
    print(paste(colnames(transect.covs[i]), "converted to factor", sep = " "))
  }
}

# Some numeric columns with NA need to be told to be numeric.
transect.covs$lowshrub <- as.numeric(transect.covs$lowshrub)
transect.covs$tallshrub <- as.numeric(transect.covs$tallshrub)

# Check for correlations ------------------------------------------------------------------------
# Covariates that are highly correlated should not be included in the same model
covs.cor <- transect.covs %>% 
  select(latitude, longitude, tempc, windms, day.of.year, dist.road, lowshrub, tallshrub, talus, 
         eds, aspect, wetness, elevation, slope, roughness, exposure, heatload, relief, position,
         radiation, evi2, nbr, ndmi, ndsi, ndvi, ndwi, precip, summerWarmth, januaryMinTemp, logs, 
         search.time, t.length, start.hr, 
         #veg.height, lowshrub.cover, shrubCover, 
         northness, eastness)

cor <- cor(covs.cor, use="pairwise")

# Just topographic & climate
topoCovs <- transect.covs %>% 
  select(latitude, longitude, wetness, elevation, slope, roughness, exposure, heatload, relief, position, radiation, 
         evi2, nbr, ndmi, ndsi, ndvi, ndwi, precip, summerWarmth, januaryMinTemp, logs, northness, eastness)
topoCor <- cor(topoCovs, use = "pairwise")

# Visualize correlations: only slope and roughness are highly correlated (r=0.85)
# Anything > 0.60 or < -0.60 we considered correlated and won't consider in same model
# library(psych)
# pairs.panels(covs.cor,ellipses = F)  
# pairs.panels(topoCor, ellipses = F)

cor[which(cor > 0.6)]
which(cor > 0.6 | cor < -0.6)
cor[which(cor < 0.6 & cor > -0.6)] <- NA

topoCor60 <- topoCor
topoCor60[which(topoCor60 < 0.6 & topoCor60 > -0.6)] <- NA


# Include transects which were surveyed, but where no individuals were detected -----------------
# Check which transects didn't produce any observations
compare.transcovs.obs <- anti_join(transect.covs, pika.obs.tr, by='transect')
compare.transcovs.obs

# Create a dataframe for the transects where pika were not detected
nopika <- as.data.frame(compare.transcovs.obs$transect)
nopika

# Add a column named 'transect' that will allow us to match with the pika observation data using bind_rows
nopika$transect <- nopika$`compare.transcovs.obs$transect`
nopika     #48 transects without pika observations

# Use the bind_rows function to add the missing transects (in missingtrans.df) to the pika distance sampling 
# observation data (pika.obs.tr), identified by 'transect' in each data frame.
pika.obs.alltrans <- bind_rows(pika.obs.tr, nopika)
head(pika.obs.alltrans)  #view a table to see that the 50 transects were tacked on to the observation data. NAs indicate no data.
dim(pika.obs.alltrans)   #we've gone from 182 observations to 230 indicating we've added 48 rows.
tail(pika.obs.alltrans)  #yes, looks like we've tacked on the transects where no pika observed.

# Site Covariates

siteCovs <- transect.covs[!duplicated(transect.covs["Site"]),]

# Part 3: Format the observation data for JAGS  ------------------------------------------------------------


obsCovData <- left_join(transect.covs, pika.obs.alltrans, by = c("Site"="Site", "transect")) %>% 
  select(-'compare.transcovs.obs$transect', -'Location.y', -'Observer.y', -'observer.y', -'Site.y') %>% 
  rename("Location" = "Location.x", "Observer" = "Observer.x", "observer" = "observer.x",
         "Transect" = "transect.y")

# saveRDS(obsCovData, "./data/obsCovData.RData")

obsCovData <- obsCovData %>%
                filter(Count == 3) %>%
                slice(rep(1:n(), each = 2)) %>%
                bind_rows(obsCovData) %>%
                mutate(Count = replace(Count, Count == 3, 1))

DSdata <- data.frame(Site = as.character(obsCovData$Site),
                     site = as.numeric(obsCovData$Site),
                     lat = obsCovData$latitude,
                     lon = obsCovData$longitude,
                     Transect = as.character(obsCovData$Transect),
                     replicate = obsCovData$replicate,
                     t.length = obsCovData$t.length,
                     search.speed = obsCovData$search.speed,
                     y = obsCovData$Count,
                     perp.dist = obsCovData$perp.dist)


# # Order DS data and transect covariates to ensure correct order in analysis
# DSdata <- DSdata %>% 
#   arrange(site)
# transect.covs <- transect.covs %>%
#   mutate(site = as.numeric(Site)) %>% 
#   arrange(site)

# Get number of individuals detected per site
# ncap = 1 plus number of detected individuals per site
# ncap <- table(DSdata[,c(2,5)])            # ncap = 1 if no individuals captured
# sites0 <- DSdata[is.na(DSdata[,3]),][,2] # sites where nothing detected
# ncap[as.character(sites0)] <- 0    # Fill in 0 for sites with no detections
# ncap <- as.vector(ncap)
# 
# # Prepare other data
# B = round(max(DSdata$perp.dist, na.rm = TRUE))+2 # rounded max detection distance
# transect <- as.numeric(as.factor(DSdata[!is.na(DSdata[,3]),2]))   # site ID of each observation
# site <- as.numeric(as.factor(DSdata[!is.na(DSdata[,3]),1]))
# delta <- 5                         # distance bin width for rect. approx.
# midpt <- seq(delta/2, B, delta)    # make mid-points and chop up data
# dclass <- DSdata[,4] %/% delta + 1   # convert distances to cat. distances
# nD <- length(midpt)                # Number of distance intervals
# dclass <- dclass[!is.na(DSdata[,3])] # Observed categorical observations
# nind <- length(dclass)             # Total number of individuals detected
# ntransects <- length(unique(DSdata$transect)) # Total number of sites
# nsites <- length(unique(DSdata$site))
# 
# # Get max number of transects per site
# tPerSite <- data.frame(site = unique(DSdata$site),
#                        transects = rep(NA,47))
# sites <- unique(DSdata$site)
# for(i in 1:length(sites)){
#   a <- DSdata[DSdata["site"]==sites[i],]
#   tPerSite$transects[i] <- length(unique(a$transect))
# }

# Prepare site level covariate data
DSsiteCovs <- list(summerWarmth=scale(siteCovs$summerWarmth),
              precip=scale(siteCovs$precip),
              januaryMinTemp=scale(siteCovs$januaryMinTemp),
              ndvi=scale(siteCovs$ndvi),
              logs=scale(siteCovs$logs),
              elevation=scale(siteCovs$elevation),
              latitude=scale(siteCovs$latitude),
              Site=as.numeric(as.factor(siteCovs$Site)),
              roughness=scale(siteCovs$roughness),
              northness=scale(siteCovs$northness))

# Prepare replicate level covariates
meta <- table(factor(transect.covs$Site, levels = levels(as.factor(transect.covs$Site))),
                 transect.covs$replicate)
searchSpeed <- matrix(data = NA, nrow = 47, ncol = 4, dimnames = list(unique(transect.covs$site),
                                                                    unique(transect.covs$replicate)))

for(s in 1:47){
  for(k in 1:4){
    searchSpeed[s,k] <- ifelse(meta[s,k]>0, subset(transect.covs, site == s & replicate == k)$search.speed, 0)
  }
}

DSreplicateCovs <- list(searchSpeed=searchSpeed)



# Bundle and summarize data set
# str( win.data <- list(ntransects=ntransects, nsites=nsites, nind=nind, B=B, nD=nD, midpt=midpt,
#                       delta=delta, ncap=ncap, 
#                       searchSpeed=DScovs$searchSpeed[,1],
#                       transectLength=DScovs$transectLength,
#                       summerWarmth=DScovs$summerWarmth[,1],
#                       precip=DScovs$precip[,1],
#                       januaryMinTemp=DScovs$januaryMinTemp[,1],
#                       ndvi=DScovs$ndvi[,1],
#                       logs=DScovs$logs[,1],
#                       elevation=DScovs$elevation[,1],
#                       latitude=DScovs$latitude[,1],
#                       Site=DScovs$Site,
#                       roughness=DScovs$roughness[,1],
#                       northness=DScovs$northness[,1],
#                       dclass=dclass,
#                       transect=transect,
#                       site=site) )

save(obsCovData, DSdata, DSsiteCovs, DSreplicateCovs, file = "./data/DSData.RData")

# # Data augmentation: add a bunch of "pseudo-individuals"
# nz <- 500                        # Augment by 500
# nind <- sum(DSdata$y == 1)
# y <- c(DSdata[,2], rep(0, nz))     # Augmented detection indicator y
# site <- c(DSdata[,1], rep(NA, nz)) # Augmented site indicator, unknown (i.e., NA) for augmented inds.
# d <- c(DSdata[,3], rep(NA,nz))     # Augmented distance data (with NAs)
# 
# B = round(max(DSdata$perp.dist, na.rm = TRUE))+2
# 
# covList <- as.list(obsCovData)
# DSdata <- list(nsites = length(unique(covList$Site)),
#                site = site,
#                nind = nind,
#                y=y,
#                d=d,
#                B=B)

# Left off here


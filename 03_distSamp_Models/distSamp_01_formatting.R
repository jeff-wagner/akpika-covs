# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Bayesian HDS formatting
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Unmarked formatting" imports and explores the cleaned distance sampling and covariate data and formats it for analysis with unmarked.
# ---------------------------------------------------------------------------

# Read in the observation and covariate data from our data management scripts.
source("02_dataCovariates/covs_02_CompileCovariates.r")
source("01_dataSurveys/data_01_Observations.r")


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

dim(pika.obs.tr)        #Now have 180 observations; removed 5 observations (4.9% of the data) 
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
# covs.cor <- transect.covs %>% 
#   select(latitude, longitude, tempc, windms, day.of.year, dist.road, lowshrub, tallshrub, talus, 
#          eds, aspect, wetness, elevation, slope, roughness, exposure, heatload, relief, position,
#          radiation, evi2, nbr, ndmi, ndsi, ndvi, ndwi, precip, summerWarmth, januaryMinTemp, logs, 
#          search.time, t.length, start.hr, 
#          #veg.height, lowshrub.cover, shrubCover, 
#          northness, eastness)

mod.covs <- transect.covs %>% 
  select(ndvi,summerWarmth, novemberMinTemp, logs, snowDepth, 
         snowMeltCycles, search.time, t.length, start.hr)

cor <- cor(mod.covs, use="pairwise")


# Define site-level covariates 
siteCovs <- transect.covs[!duplicated(transect.covs["Site"]),]

# Visualize correlations: only slope and roughness are highly correlated (r=0.85)
# Anything > 0.60 or < -0.60 we considered correlated and won't consider in same model
library(ggstatsplot)
png("../AKpika_HDS/output/covariateCorrelations.png", height = 6, width = 6, units = "in", res = 300)
ggcorrmat(
  data     = select(siteCovs, summerWarmth, novemberMinTemp, snowDepth, snowMeltCycles,
                    logs, ndvi),
  type = "parametric",
  colors   = c("#B2182B", "white", "#B2182B"),
  title    = "Correlalogram for pika model covariates"
)
dev.off()


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

# Part 3: Format the observation data for JAGS  ------------------------------------------------------------

obsCovData <- left_join(transect.covs, pika.obs.alltrans, by = c("Site"="Site", "transect")) %>% 
  select(-'compare.transcovs.obs$transect', -'Location.y', -'Observer.y', -'observer.y') %>% 
  rename("Location" = "Location.x", "Observer" = "Observer.x", "observer" = "observer.x",
         "Transect" = "transect.y")

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

# Prepare site level covariate data
DSsiteCovs <- list(
              location=as.numeric(siteCovs$Location),
              latitude=scale(siteCovs$latitude),
              dist.road=scale(siteCovs$dist.road),
              occ_status=as.numeric(siteCovs$occ.status),
              twi=scale(siteCovs$wetness),
              slope=scale(siteCovs$slope),
              exposure=scale(siteCovs$exposure),
              heatload=scale(siteCovs$heatload),
              radiation=scale(siteCovs$radiation),
              summerWarmth=scale(siteCovs$summerWarmth),
              precip=scale(siteCovs$precip),
              januaryMinTemp=scale(siteCovs$januaryMinTemp),
              novemberMinTemp=scale(siteCovs$novemberMinTemp),
              ndvi=scale(siteCovs$ndvi),
              logs=scale(siteCovs$logs),
              freezeThaw=scale(siteCovs$ftc),
              snowDepth=scale(siteCovs$snowDepth),
              snowMelt=scale(siteCovs$snowMeltCycles),
              elevation=scale(siteCovs$elevation),
              Site=as.numeric(as.factor(siteCovs$Site)),
              roughness=scale(siteCovs$roughness),
              northness=scale(siteCovs$northness)
              )

# Prepare replicate level covariates
meta <- table(factor(transect.covs$Site, levels = levels(as.factor(transect.covs$Site))),
                 transect.covs$replicate)
searchSpeed <- matrix(data = NA, nrow = 47, ncol = 4, dimnames = list(unique(transect.covs$site),
                                                                    unique(transect.covs$replicate)))
searchTime <- matrix(data = NA, nrow = 47, ncol = 4, dimnames = list(unique(transect.covs$site),
                                                                    unique(transect.covs$replicate)))

for(s in 1:47){
  for(k in 1:4){
    searchSpeed[s,k] <- ifelse(meta[s,k]>0, subset(transect.covs, site == s & replicate == k)$search.speed, 0)
    searchTime[s,k] <- ifelse(meta[s,k]>0, subset(transect.covs, site == s & replicate == k)$search.time, 0)
  }
}

DSreplicateCovs <- list(searchSpeed=scale(searchSpeed),
                        searchTime=scale(searchTime))



save(cor, obsCovData, DSdata, DSsiteCovs, DSreplicateCovs, file = "./data/DSData.RData")

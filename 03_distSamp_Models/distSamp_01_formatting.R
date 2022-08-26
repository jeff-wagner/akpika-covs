# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Unmarked formatting
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


#Truncate data at 100m since there are few observations beyond that
pika.obs.tr <- pika.obs.t %>% 
  filter(perp.dist < 100)

dim(pika.obs.tr)        #Now have 182 observations; removed 3 observations (1.6% of the data) 
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
         search.time, trans.length, start.hr, veg.height, lowshrub.cover, shrubCover, northness, eastness)

cor <- cor(covs.cor, use="pairwise")

# Just topographic & climate
topoCovs <- transect.covs %>% 
  select(latitude, longitude, wetness, elevation, slope, roughness, exposure, heatload, relief, position, radiation, 
         evi2, nbr, ndmi, ndsi, ndvi, ndwi, precip, summerWarmth, januaryMinTemp, logs, northness, eastness)
topoCor <- cor(topoCovs, use = "pairwise")

# Visualize correlations: only slope and roughness are highly correlated (r=0.85)
# Anything > 0.60 or < -0.60 we considered correlated and won't consider in same model
library(psych)
pairs.panels(covs.cor,ellipses = F)  
pairs.panels(topoCor, ellipses = F)

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


# Part 3: Format the observation data for unmarked  ------------------------------------------------------------
# Aggregate detections into 50m or 100 m intervals
# formatDistData is a data structure needed for the distsamp function in unmarked.
# dists.order is the observation data. You have to tell the unmarked package which column is your distCol (the 
# perpendicular distance), which in this file is 'dist'. Additionally, you have to identify the transectNameCol
# (the transect names), which is 'trans' in this data file. 
pika.obs.alltrans[order(pika.obs.alltrans$transect),]
levels(pika.obs.alltrans$transect)
pika.obs.alltrans$transect <- factor(pika.obs.alltrans$transect, levels = levels(transect.covs$transect))

# Create the yDat object
yDat <- formatDistData(pika.obs.alltrans, distCol="perp.dist", transectNameCol="transect", 
                       dist.breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
yDat
dim(yDat)    #now we have 119 rows, 71 transects with pika obs and 48 without.

dim(transect.covs)  #119 transect-level covariates

# Part 4: Combine the observation and covariate data in unmarked format  -----------------------------------------
# unmarkedFrameDS is the name for the data frame used for distance sampling in the unmarked package
# The unmarked package distsamp model needs inputs for siteCovs(site or transect-level covariates),
# which we called 'covs', whether you are working with line or point data (ours is line transects),
# the distance breaks you decided upon (here the bins are 100m), and tlength (the transect length),
# which we called 'length' in the covs file. The unitsIn must match for the distance breaks and 
# transect lenghts. Here, we are working in meters.
umf <- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=transect.covs, survey="line", 
                       dist.breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                       tlength=transect.covs$trans.length, unitsIn="m")

# Summary of the unmarked data frame for analysis
summary(umf)

# Part 4: View the data and final cleanup  -----------------------------------------------------------------------
# VIEW THE DATA TO MAKE SURE EACH TRANSECT'S DATA IS ALLIGNED WITH THE PROPER TRANSECT COVARIATES!
umf

hist(umf, freq=TRUE, xlab="distance (m)", main="", cex.lab=0.8, cex.axis=0.8)

# Remove unnecessary objects from the environment
rm(compare.transcovs.obs, pika.obs.t, nopika)


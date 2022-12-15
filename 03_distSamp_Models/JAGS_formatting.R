source('./00_init/initscript.R')
load('./data/DSData.RData')

# Prepare other data
B = round(max(DSdata$perp.dist, na.rm = TRUE))+2 # rounded max detection distance
site <- DSdata[!is.na(DSdata[,9]),2]
delta <- 5                         # distance bin width for rect. approx.
midpt <- seq(delta/2, B, delta)    # make mid-points and chop up data
dclass <- DSdata[,10] %/% delta + 1   # convert distances to cat. distances
nD <- length(midpt)                # Number of distance intervals
dclass <- dclass[!is.na(DSdata[,10])] # Observed categorical observations
nind <- length(dclass)             # Total number of individuals detected
nsites <- length(unique(DSdata$site)) # Total number of sites

# site replicate info: this code will determine how many times each site was survyed replicates are only identified sequentially, not strictly chronologically therefore, replicate 1 simply refers to the first time a site was surveyed and replicate 'nrep[s]' refers to the last time a site was surveyed the sequence 1:nrep[s] identifies all the replicates in which a site was surveyed

sites <- data.frame(Site = unique(DSdata$Site),
                    site = as.numeric(as.factor(unique(DSdata$Site))),
                    totalreps = NA)

for(i in 1:length(unique(DSdata$Site))){
  sites$totalreps[i] = max(DSdata[DSdata$Site==sites$Site[i],]$replicate, na.rm = TRUE)
}

repcols <- paste0("rep.",seq(1,max(sites$totalreps)))
#rep_effort <- 1*st_drop_geometry(sites[,repcols])
K <- max(sites$totalreps)
nreps <- sites$totalreps

# now organize detections in 3-D array.  create empty array to hold the number of gorups counted in each distance class bin at each site in each replicate up to the maximum number of replicates (6)
y3d<-array(data=NA, dim=c(length(sites$site),nD,K)) 

# create a table of counts in each distance class at each site in each replicate, this takes the data and restructures it as site by replicate data. This table will also produce the 'zeroes' needed to fit our data correclty, filling in a 0 count where and when no animals are found.

indiv <- DSdata[!is.na(DSdata[9]),] # Subset for observations only
indiv$dclass <- dclass

testarr <- table(factor(indiv$Site, levels = levels(as.factor(sites$Site))),
                 indiv$dclass, indiv$replicate)

# populate the y3d array with the site counts in each distance bin in correct site order (the table function puts them in alphabetical order and this loop puts them back in numerical order)
for (i in 1:K){
  y3d[,,i] <- testarr[order(as.numeric(row.names(testarr[,,i]))),,i]
}

# The three-part mutinomial uses our individual observation twice, once in the y3d array (broken down by distance class) and once in 'nobs' a total count across all distance bins of individuals by site and replicate.  This code also introduces 'zeroes' into our data wherever and whenever we went through a site and did not see any pronghorn. 
nobs <- apply(y3d, c(1,3), sum) # Total detections per site and occasion
ncap <- nobs + 1

# a few other tidbits of data that make the model easier to write
area<-sites$site_length_km*2*B # surveyed area at each site (needed for site-specific density)

# Import the expected occurence probabilities for each site, distance class, and replicate & the total replicate survey areas
load('./data/spData.RData')


# bundle data for jags

str( data <- list(y3d=y3d,
                  pi3d=pi3d,
                  nsites=nsites, 
                  K=K, 
                  nD=nD, 
                  midpt=midpt, 
                  delta=delta,
                  B=B, 
                  nobs = nobs,
                  ncap = ncap,
                  nreps=nreps,
                  area=site_area,
                  site=site,
                  nind=nind,
                  summerWarmth=DSsiteCovs$summerWarmth,
                  searchSpeed=DSreplicateCovs$searchSpeed
                  ))

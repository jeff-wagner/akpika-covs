data <- readRDS('./data/obsCovData.RData')

sites <- data.frame(site = as.character(unique(data$Site.x)),
                      totalreps = NA)
for(i in 1:length(unique(data$Site.x))){
  sites$totalreps[i] = max(data[data$Site.x==sites$site[i],]$replicate, na.rm = TRUE)
}

repcols <- paste0("rep.",seq(1,max(sites$totalreps)))

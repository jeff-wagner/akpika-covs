# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# Model Averaging
# Author: Jeff Wagner
# Last Updated: 2022-08-17
# Usage: Must be executed in R 4.0.0+.
# Description: "Model Averaging" makes model-averaged estimates of pika density using the top model set.
# ---------------------------------------------------------------------------

# Read in the model predictions
load("04_modelPredictions/ws_modelPredictions.RData")

library(AICcmodavg)

# Define covariates from top models for model averaging
avg.covs <- data.frame(Site=transect.covs$Site,
                       location=transect.covs$Location,
                       search.speed=transect.covs$search.speed,
                       precip=transect.covs$precip,
                       summerWarmth=transect.covs$summerWarmth,
                       januaryMinTemp=transect.covs$januaryMinTemp,
                       ndvi=transect.covs$ndvi,
                       logs=transect.covs$logs,
                       wetness=transect.covs$wetness,
                       elevation=transect.covs$elevation,
                       roughness=transect.covs$roughness,
                       northness=transect.covs$northness)

mods <- list(climate=climate, 
             climateProductivity=climateProductivity, 
             climateTopo=climateTopo, 
             productivity=productivity)
modNames <- names(mods)

# Define modavg function for detection probability
modavg.AICunmarkedFitDS <-
  function(cand.set, parm, modnames = NULL, second.ord = TRUE, nobs = NULL, 
           uncond.se = "revised", conf.level = 0.95, exclude = NULL, warn = TRUE,
           c.hat = 1, parm.type = NULL, ...){
    
    ##note that parameter is referenced differently from unmarked object - see labels( )
    
    ##check if named list if modnames are not supplied
    if(is.null(modnames)) {
      if(is.null(names(cand.set))) {
        modnames <- paste("Mod", 1:length(cand.set), sep = "")
        warning("\nModel names have been supplied automatically in the table\n")
      } else {
        modnames <- names(cand.set)
      }
    }
    
    
    ##check for parm.type and stop if NULL
    if(is.null(parm.type)) {stop("\n'parm.type' must be specified for this model type, see ?modavg for details\n")}
    
    
    #####MODIFICATIONS BEGIN#######
    ##remove all leading and trailing white space and within parm
    parm <- gsub('[[:space:]]+', "", parm)
    parm.strip <- parm #to use later
    
    ##if (Intercept) is chosen assign (Int) - for compatibility
    if(identical(parm, "(Intercept)")) parm <- "Int"
    
    ##reverse parm
    reversed.parm <- reverse.parm(parm)
    reversed.parm.strip <- reversed.parm #to use later
    exclude <- reverse.exclude(exclude = exclude)
    #####MODIFICATIONS END######
    
    
    
    ##Distance sampling model
    ##lambda - abundance
    if(identical(parm.type, "lambda")) {
      ##extract model formula for each model in cand.set
      mod_formula <- lapply(cand.set, FUN = function(i) labels(coef(i@estimates@estimates$state)))
      parm <- paste("lam", "(", parm, ")", sep="")
      if(!is.null(reversed.parm)) {reversed.parm <- paste("lam", "(", reversed.parm, ")", sep="")}
      not.include <- lapply(cand.set, FUN = function(i) i@formula[[3]])
      parm.type1 <- "state"
    }
    
    ##detect
    if(identical(parm.type, "detect")) {
      ##check for key function used
      keyid <- unique(sapply(cand.set, FUN = function(i) i@keyfun))
      if(length(keyid) > 1) stop("\nDifferent key functions used across models:\n",
                                 "cannot compute model-averaged estimate\n")
      if(identical(keyid, "uniform")) stop("\nDetection parameter not found in models\n")
      ##set key prefix used in coef( )
      if(identical(keyid, "halfnorm")) {
        parm.key <- "sigma"
      }
      if(identical(keyid, "hazard")) {
        parm.key <- "shape"
      }
      if(identical(keyid, "exp")) {
        parm.key <- "rate"
      }
      
      ##label for intercept - label different with this model type
      # if(identical(parm, "Int")) {parm <- "(Intercept)"}
      
      mod_formula <- lapply(cand.set, FUN = function(i) labels(coef(i@estimates@estimates$det)))
      parm <- paste("p", "(", parm, ")", sep="")
      if(!is.null(reversed.parm)) {reversed.parm <- paste("p", "(", parm.key, "(", reversed.parm, "))", sep="")}
      not.include <- lapply(cand.set, FUN = function(i) i@formula[[2]])
      parm.type1 <- "det"
    }
    
    ##################
    ##extract link function
    check.link <- sapply(X = cand.set, FUN = function(i) eval(parse(text = paste("i@estimates@estimates$",
                                                                                 parm.type1, "@invlink",
                                                                                 sep = ""))))
    unique.link <- unique(check.link)
    select.link <- unique.link[1]
    
    if(length(unique.link) > 1) {stop("\nIt is not appropriate to compute a model averaged linear predictor\n",
                                      "with different link functions\n")}
    ##################
    
    nmods <- length(cand.set)
    
    ##setup matrix to indicate presence of parms in the model
    include <- matrix(NA, nrow=nmods, ncol=1)
    ##add a check for multiple instances of same variable in given model (i.e., interactions)
    include.check <- matrix(NA, nrow=nmods, ncol=1)
    
    ##################################
    ##################################
    ###ADDED A NEW OBJECT TO STRIP AWAY lam( ) from parm on line 35
    ###to enable search with regexpr( ) 
    
    
    ##iterate over each formula in mod_formula list
    for (i in 1:nmods) {
      idents <- NULL
      idents.check <- NULL
      form <- mod_formula[[i]]
      
      
      ######################################################################################################
      ######################################################################################################
      ###MODIFICATIONS BEGIN
      ##iterate over each element of formula[[i]] in list
      if(is.null(reversed.parm)) {
        for (j in 1:length(form)) {
          idents[j] <- identical(parm, form[j])
          ##added parm.strip here for regexpr( )
          idents.check[j] <- ifelse(attr(regexpr(parm.strip, form[j], fixed=TRUE), "match.length")== "-1" , 0, 1)  
        }
      } else {
        for (j in 1:length(form)) {
          idents[j] <- identical(parm, form[j]) | identical(reversed.parm, form[j])
          idents.check[j] <- ifelse(attr(regexpr(parm.strip, form[j], fixed=TRUE), "match.length")=="-1" & attr(regexpr(reversed.parm.strip, form[j],
                                                                                                                        fixed=TRUE), "match.length")=="-1" , 0, 1)  
        }
      }
      ###MODIFICATIONS END
      ######################################################################################################
      ######################################################################################################
      
      
      include[i] <- ifelse(any(idents==1), 1, 0)
      include.check[i] <- ifelse(sum(idents.check)>1, "duplicates", "OK")
    }
    
    #####################################################
    #exclude == NULL; warn=TRUE:  warn that duplicates occur and stop
    if(is.null(exclude) && identical(warn, TRUE)) {
      #check for duplicates in same model
      if(any(include.check == "duplicates")) {
        stop("\nSome models include more than one instance of the parameter of interest. \n",
             "This may be due to the presence of interaction/polynomial terms, or variables\n",
             "with similar names:\n",
             "\tsee \"?modavg\" for details on variable specification and \"exclude\" argument\n")
      }
      
    }
    
    #exclude == NULL; warn=FALSE:  compute model-averaged beta estimate from models including variable of interest,
    #assuming that the variable is not involved in interaction or higher order polynomial (x^2, x^3, etc...),
    #warn that models were not excluded
    if(is.null(exclude) && identical(warn, FALSE)) {
      if(any(include.check == "duplicates")) {
        warning("\nMultiple instances of parameter of interest in given model is presumably\n",
                "not due to interaction or polynomial terms - these models will not be\n",
                "excluded from the computation of model-averaged estimate\n")
      }
      
    }
    
    #warn if exclude is neither a list nor NULL
    if(!is.null(exclude)) {
      if(!is.list(exclude)) {stop("\nItems in \"exclude\" must be specified as a list")}
    }
    
    
    #if exclude is list  
    if(is.list(exclude)) {
      
      #determine number of elements in exclude
      nexcl <- length(exclude)
      
      #check each formula for presence of exclude variable in not.include list
      #not.include <- lapply(cand.set, FUN = formula)
      
      #set up a new list with model formula
      forms <- list()
      for (i in 1:nmods) {
        form.tmp <- as.character(not.include[i]) #changed from other versions as formula returned is of different structure for unmarked objects
        if(attr(regexpr("\\+", form.tmp), "match.length")==-1) {
          forms[i] <- form.tmp
        } else {forms[i] <- strsplit(form.tmp, split=" \\+ ")}
      }
      
      #additional check to see whether some variable names include "+"
      check.forms <- unlist(lapply(forms, FUN=function(i) any(attr(regexpr("\\+", i), "match.length")>0)[[1]]))
      if (any(check.forms==TRUE)) stop("\nPlease avoid \"+\" in variable names\n")
      
      ##additional check to determine if intercept was removed from models
      check.forms <- unlist(lapply(forms, FUN=function(i) any(attr(regexpr("\\- 1", i), "match.length")>0)[[1]]))
      if (any(check.forms==TRUE)) stop("\nModels without intercept are not supported in this version, please use alternative parameterization\n")
      
      
      #search within formula for variables to exclude
      mod.exclude <- matrix(NA, nrow=nmods, ncol=nexcl)
      
      #iterate over each element in exclude list
      for (var in 1:nexcl) {
        
        #iterate over each formula in mod_formula list
        for (i in 1:nmods) {
          idents <- NULL
          form.excl <- forms[[i]]
          
          #iterate over each element of forms[[i]]
          for (j in 1:length(form.excl)) {
            idents[j] <- identical(exclude[var][[1]], form.excl[j])
          }
          mod.exclude[i,var] <- ifelse(any(idents == 1), 1, 0)
        }    
        
      }
      
      #determine outcome across all variables to exclude
      to.exclude <- rowSums(mod.exclude)
      
      
      #exclude models following models from model averaging  
      include[which(to.exclude >= 1)] <- 0
      
      
    }
    
    
    
    ##add a check to determine if include always == 0
    if (sum(include) == 0) {stop("\nParameter not found in any of the candidate models\n") }
    
    new.cand.set <- cand.set[which(include == 1)] #select models including a given parameter
    new.mod.name <- modnames[which(include == 1)]    #update model names
    
    new_table <- aictab(cand.set = new.cand.set, modnames = new.mod.name,
                        second.ord = second.ord, nobs = nobs, sort = FALSE, c.hat = c.hat) #recompute AIC table and associated measures
    new_table$Beta_est <- unlist(lapply(new.cand.set, FUN = function(i) coef(i)[paste(parm)])) #extract beta estimate for parm
    ##if reversed.parm is not null and varies across models, potentially check for it here
    new_table$SE <- unlist(lapply(new.cand.set, FUN = function(i) sqrt(diag(vcov(i)))[paste(parm)]))
    ##if reversed.parm is not null and varies across models, potentially check for it here
    
    ##if c-hat is estimated adjust the SE's by multiplying with sqrt of c-hat
    if(c.hat > 1) {
      new_table$SE <- new_table$SE*sqrt(c.hat)
    } 
    
    ##AICc
    ##compute model-averaged estimates, unconditional SE, and 95% CL
    if(c.hat == 1 && second.ord == TRUE) {
      Modavg_beta <- sum(new_table$AICcWt*new_table$Beta_est)
      
      ##unconditional SE based on equation 4.9 of Burnham and Anderson 2002
      if(identical(uncond.se, "old")) {
        Uncond_SE <- sum(new_table$AICcWt*sqrt(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2))
      }
      
      ##revised computation of unconditional SE based on equation 6.12 of Burnham and Anderson 2002; Anderson 2008, p. 111
      if(identical(uncond.se, "revised")) {
        Uncond_SE <- sqrt(sum(new_table$AICcWt*(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2)))
      }
    }
    
    ##QAICc
    if(c.hat > 1 && second.ord == TRUE) {
      Modavg_beta <- sum(new_table$QAICcWt*new_table$Beta_est)
      
      ##unconditional SE based on equation 4.9 of Burnham and Anderson 2002
      if(identical(uncond.se, "old")) {      
        Uncond_SE <- sum(new_table$QAICcWt*sqrt(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2))
      }
      
      ##revised computation of unconditional SE based on equation 6.12 of Burnham and Anderson 2002; Anderson 2008, p. 111
      if(identical(uncond.se, "revised")) {
        Uncond_SE <- sqrt(sum(new_table$QAICcWt*(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2)))
      }
    }     
    
    
    ##AIC
    if(c.hat == 1 && second.ord == FALSE) {
      Modavg_beta <- sum(new_table$AICWt*new_table$Beta_est)
      
      ##unconditional SE based on equation 4.9 of Burnham and Anderson 2002
      if(identical(uncond.se, "old")) {
        Uncond_SE <- sum(new_table$AICWt*sqrt(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2))
      }
      
      ##revised computation of unconditional SE based on equation 6.12 of Burnham and Anderson 2002; Anderson 2008, p. 111
      if(identical(uncond.se, "revised")) {
        Uncond_SE <- sqrt(sum(new_table$AICWt*(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2)))
      }
    }
    
    
    ##QAIC
    if(c.hat > 1 && second.ord == FALSE) {
      Modavg_beta <- sum(new_table$QAICWt*new_table$Beta_est)
      
      ##unconditional SE based on equation 4.9 of Burnham and Anderson 2002
      if(identical(uncond.se, "old")) {
        Uncond_SE <- sum(new_table$QAICWt*sqrt(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2))
      }
      
      ##revised computation of unconditional SE based on equation 6.12 of Burnham and Anderson 2002; Anderson 2008, p. 111
      if(identical(uncond.se, "revised")) {
        Uncond_SE <- sqrt(sum(new_table$QAICWt*(new_table$SE^2 + (new_table$Beta_est- Modavg_beta)^2)))
      }  
    }     
    
    
    zcrit <- qnorm(p = (1 - conf.level)/2, lower.tail = FALSE)
    Lower_CL <- Modavg_beta - zcrit*Uncond_SE
    Upper_CL <- Modavg_beta + zcrit*Uncond_SE
    out.modavg <- list("Parameter"=paste(parm), "Mod.avg.table" = new_table, "Mod.avg.beta" = Modavg_beta,
                       "Uncond.SE" = Uncond_SE, "Conf.level" = conf.level, "Lower.CL" = Lower_CL,
                       "Upper.CL" = Upper_CL)
    
    class(out.modavg) <- c("modavg", "list")
    return(out.modavg)
  }

# Model-averaged estimates
modavgEstimates <- list(
  'p(Intercept)' = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "(Intercept)", parm.type = "detect"),
  'p(search.speed)' = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(search.speed)", parm.type = "detect"),
  precip = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(precip)", parm.type = "lambda"),
  summerWarmth = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(summerWarmth)", parm.type = "lambda"),
  januaryMinTemp = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(januaryMinTemp)", parm.type = "lambda"),
  ndvi = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(ndvi)", parm.type = "lambda"),
  logs = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(logs)", parm.type = "lambda"),
  wetness = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(wetness)", parm.type = "lambda"),
  elevation = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(elevation)", parm.type = "lambda"),
  roughness = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(roughness)", parm.type = "lambda"),
  northness = modavg.AICunmarkedFitDS(cand.set = mods, modnames = modNames, parm = "scale(northness)", parm.type = "lambda"))


df.modavgEstimates <- data.frame(Parameter = names(modavgEstimates),
                                 NumMods = NA,
                                 Estimate = NA,
                                 SE = NA,
                                 Lower.CL = NA,
                                 Upper.CL = NA)

for(i in 1:length(modavgEstimates)){
  df.modavgEstimates$NumMods[i] <- length(modavgEstimates[[i]]$Mod.avg.table$Modnames)
  df.modavgEstimates$Estimate[i] <- modavgEstimates[[i]]$Mod.avg.beta
  df.modavgEstimates$SE[i] <- modavgEstimates[[i]]$Uncond.SE
  df.modavgEstimates$Lower.CL[i] <- modavgEstimates[[i]]$Lower.CL
  df.modavgEstimates$Upper.CL[i] <- modavgEstimates[[i]]$Upper.CL
}

# Model-averaged predictions
      # detection
      modavg.p <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "detect",
                                newdata=avg.covs)
      
      df.modavg.p <- data.frame(Predicted = modavg.p$mod.avg.pred,
                                   lower = modavg.p$lower.CL,
                                   upper = modavg.p$upper.CL,
                                   avg.covs)
      # density
      modavg.pred <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata=avg.covs)
      
      df.modavg.Pred <- data.frame(Predicted = modavg.pred$mod.avg.pred,
                             lower = modavg.pred$lower.CL,
                             upper = modavg.pred$upper.CL,
                             avg.covs)

# Predictions for explanatory variables  ---------------------------------------------------------------------
# Define the dataframes -------------------------------------
df.avgPrecip <- data.frame(precip = precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                           logs=mean.logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=mean.roughness,
                           elevation=mean.elevation, northness=mean.northness)
df.avgSummerWarmth <- data.frame(precip = mean.precip, summerWarmth=summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                                 logs=mean.logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=mean.roughness,
                                 elevation=mean.elevation, northness=mean.northness)
df.avgJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=januaryMinTemp,
                                   logs=mean.logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=mean.roughness,
                                   elevation=mean.elevation, northness=mean.northness)
df.avgLogs <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                         logs=logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=mean.roughness,
                         elevation=mean.elevation, northness=mean.northness)
df.avgNDVI <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                         logs=mean.logs, ndvi=ndvi, wetness=mean.wetness, roughness=mean.roughness,
                         elevation=mean.elevation, northness=mean.northness)
df.avgWetness <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                            logs=mean.logs, ndvi=mean.ndvi, wetness=wetness, roughness=mean.roughness,
                            elevation=mean.elevation, northness=mean.northness)
df.avgRoughness <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                              logs=mean.logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=roughness,
                              elevation=mean.elevation, northness=mean.northness)
df.avgElevation <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                              logs=mean.logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=mean.roughness,
                              elevation=elevation, northness=mean.northness)
df.avgNorthness <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                              logs=mean.logs, ndvi=mean.ndvi, wetness=mean.wetness, roughness=mean.roughness,
                              elevation=mean.elevation, northness=northness)
df.p <- data.frame(search.speed = seq(min(avg.covs$search.speed), max(avg.covs$search.speed), length = 100))

# Make predictions -----------------------------------------
predict.avgPrecip <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgPrecip)
predict.avgSummerWarmth <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                      newdata = df.avgSummerWarmth)
predict.avgJanuaryMinTemp <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                        newdata = df.avgJanuaryMinTemp)
predict.avgLogs <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                              newdata = df.avgLogs)
predict.avgNDVI <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                              newdata = df.avgNDVI)
predict.avgWetness <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                 newdata = df.avgWetness)
predict.avgRoughness <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                   newdata = df.avgRoughness)
predict.avgElevation <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                   newdata = df.avgElevation)
predict.avgNorthness <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                   newdata = df.avgNorthness)
predict.avgP <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "detect",
                           newdata = df.p)

# Convert to dataframes --------------------------------
df.predict.avgPrecip <- data.frame(Predicted = predict.avgPrecip$mod.avg.pred,
                                   lower = predict.avgPrecip$lower.CL,
                                   upper = predict.avgPrecip$upper.CL,
                                   df.avgPrecip)
df.predict.avgSummerWarmth <- data.frame(Predicted = predict.avgSummerWarmth$mod.avg.pred,
                                         lower = predict.avgSummerWarmth$lower.CL,
                                         upper = predict.avgSummerWarmth$upper.CL,
                                         df.avgSummerWarmth)
df.predict.avgJanuaryMinTemp <- data.frame(Predicted = predict.avgJanuaryMinTemp$mod.avg.pred,
                                           lower = predict.avgJanuaryMinTemp$lower.CL,
                                           upper = predict.avgJanuaryMinTemp$upper.CL,
                                           df.avgJanuaryMinTemp)
df.predict.avgLogs <- data.frame(Predicted = predict.avgLogs$mod.avg.pred,
                                 lower = predict.avgLogs$lower.CL,
                                 upper = predict.avgLogs$upper.CL,
                                 df.avgLogs)
df.predict.avgNDVI <- data.frame(Predicted = predict.avgNDVI$mod.avg.pred,
                                 lower = predict.avgNDVI$lower.CL,
                                 upper = predict.avgNDVI$upper.CL,
                                 df.avgNDVI)
df.predict.avgWetness <- data.frame(Predicted = predict.avgWetness$mod.avg.pred,
                                    lower = predict.avgWetness$lower.CL,
                                    upper = predict.avgWetness$upper.CL,
                                    df.avgWetness)
df.predict.avgRoughness <- data.frame(Predicted = predict.avgRoughness$mod.avg.pred,
                                      lower = predict.avgRoughness$lower.CL,
                                      upper = predict.avgRoughness$upper.CL,
                                      df.avgRoughness)
df.predict.avgElevation <- data.frame(Predicted = predict.avgElevation$mod.avg.pred,
                                      lower = predict.avgElevation$lower.CL,
                                      upper = predict.avgElevation$upper.CL,
                                      df.avgElevation)
df.predict.avgNorthness <- data.frame(Predicted = predict.avgNorthness$mod.avg.pred,
                                      lower = predict.avgNorthness$lower.CL,
                                      upper = predict.avgNorthness$upper.CL,
                                      df.avgNorthness)
df.predict.avgP <- data.frame(Predicted = predict.avgP$mod.avg.pred,
                              lower = predict.avgP$lower.CL,
                              upper = predict.avgP$upper.CL,
                              df.p)
save.image(file = "05_modelAveraging/ws_modAvg.RData")

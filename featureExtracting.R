
fixedThreshold = c(0.6, 0.7);
imp_threshold = TRUE
perf_threshold = TRUE

#fixed thresholds estimation TODO
if(length(fixedThreshold)>0){
  for(j in 1:length(fixedThreshold)){
    
    threshold <- paste("fixedThreshold_", as.character(fixedThreshold[j]), sep="")
    slope <- paste("slope_at_", as.character(fixedThreshold[j]), sep="")
    thresholdFit <- paste("fixedThreshold_", as.character(fixedThreshold[j]), "_fit", sep="")
    slopeFit <- paste("slope_at_", as.character(fixedThreshold[j]), "_fit", sep="")
    
    fitData <- filter(fitData, gamma_fit + lambda_fit < 1) %>%
      #threshold estimate
      group_by(id) %>% mutate(!!threshold:= PSfunction(gamma, lambda, sigmoid, core, fixedThreshold[j], param1, param2, inverse = TRUE)) %>%
      #threshold estimate fit
      mutate(!!thresholdFit:= PSfunction(gamma_fit, lambda_fit, sigmoid_fit, core_fit, fixedThreshold[j], param1_fit, param2_fit, inverse = TRUE))
      #slope
      fitData[slope] <- PSfunction(fitData$gamma, fitData$lambda, fitData$sigmoid, fitData$core, fitData[threshold], fitData$param1, fitData$param2, type="pdf")
      #slope
      fitData[slopeFit] <- PSfunction(fitData$gamma_fit, fitData$lambda_fit, fitData$sigmoid_fit, fitData$core_fit, fitData[thresholdFit], fitData$param1_fit, fitData$param2_fit, type="pdf")
  }
}
rm(j, threshold, slope, thresholdFit, slopeFit)

#extracting improvement threshold, point where the slope is the most prevalent
if(imp_threshold){
  fitData <- filter(fitData, gamma_fit + lambda_fit < 1) %>%
    #threshold estimate
    mutate(imp_threshold = imp_threshold(gamma, lambda, sigmoid, core, param1, param2)) %>%
    #slope
    mutate(imp_slope=PSfunction(gamma, lambda, sigmoid, core, imp_threshold, param1, param2, type="pdf")) %>%
    #threshold estimate
    mutate(imp_threshold_fit=imp_threshold(gamma_fit, lambda_fit, sigmoid_fit, core_fit, param1_fit, param2_fit)) %>%
    #slope
    mutate(imp_slope_fit=PSfunction(gamma_fit, lambda_fit, sigmoid_fit, core_fit, imp_threshold_fit, param1_fit, param2_fit, type="pdf")) 
}

#extracting performence threshold, midpoint between the best and worst achievable performance
if(perf_threshold){
  fitData <- filter(fitData, gamma_fit + lambda_fit < 1) %>%
    #threshold estimate
    group_by(id) %>% mutate(perf_threshold= perf_threshold(gamma, lambda, sigmoid, core, param1, param2)) %>%
    #slope
    mutate(perf_slope= PSfunction(gamma, lambda, sigmoid, core, perf_threshold, param1, param2, type="pdf")) %>%
    #threshold estimate
    mutate(perf_threshold_fit= perf_threshold(gamma_fit, lambda_fit, sigmoid_fit, core_fit, param1_fit, param2_fit)) %>%
    #slope
    mutate(perf_slope_fit= PSfunction(gamma_fit, lambda_fit, sigmoid_fit, core_fit, perf_threshold_fit, param1_fit, param2_fit, type="pdf")) 
}

#adding variables numberOfLevels, numberOfObservations, numberOfObervationsPerLevel
fitDataUnwrapped <- unnest(fitData, data)

if(FALSE){
fitDataUnwrapped <- fitDataUnwrapped %>%
  #number of levels
    group_by(id) %>% mutate(numberOfLevels = n()) %>%
  #number of observations
    group_by(id) %>% mutate(numberOfObservations = sum(obsNumber)) %>%
  #number of observations per level
    group_by(id) %>% mutate(observationMean = mean(obsNumber)) %>%
  #mean distance of threshold and level
    mutate(LevelthresholdDistance = level - threshold) %>%
    group_by(id) %>% mutate(meanLevelthresholdDistance = mean(LevelthresholdDistance)) %>%
  #mean distance of threshold and level
    group_by(id) %>% mutate(meanLevelthresholdDistance = median(LevelthresholdDistance))
#fitData$numberOfLevels <- rep(NaN, nrow(fitData))
#fitData$numberOfObservations <- rep(NaN, nrow(fitData))
#fitData$meanOfObservationsPerLevel <- rep(NaN, nrow(fitData))
#fitData$meanthresholdLevelDistance <- rep(NaN, nrow(fitData))
#for(i in 1:nrow(fitData)){
#  dat <- fitData[i,]$data[[1]]
#
#  fitData[i,]$numberOfLevels <- nrow(dat)
#  #number of observations
#  fitData[i,]$numberOfObservations <- sum(dat$obsNumber)
#  #number of observations per level
#  fitData[i,]$meanOfObservationsPerLevel <- mean(dat$obsNumber)
#  #mean distance of threshold and level
#  tres <- fitData[i,]$threshold[[1]]
#  fitData[i,]$meanthresholdLevelDistance <- mean(abs(dat$level - tres))
#}
#rm(i, tres)
}
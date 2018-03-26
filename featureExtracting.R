#filtering unimpropriate data
wrong <- filter(fitData, fitGamma + fitLambda >= 1)
fitData <- filter(fitData, fitGamma + fitLambda < 1) %>%
  #treshold estimate
  group_by(id) %>% mutate(treshold=PSfunction(gamma, lambda, sigmoid, core, 0.5, param1, param2, inverse = TRUE)) %>%
  #slope
  group_by(id) %>% mutate(slope=PSfunction(gamma, lambda, sigmoid, core, treshold, param1, param2, type="pdf")) %>%
  #treshold estimate
  group_by(id) %>% mutate(fitTreshold=PSfunction(fitGamma, fitLambda, fitSigmoid, fitCore, 0.5, fitParam1, fitParam2, inverse = TRUE)) %>%
  #slope
  group_by(id) %>% mutate(fitSlope=PSfunction(fitGamma, fitLambda, fitSigmoid, fitCore, fitTreshold, fitParam1, fitParam2, type="pdf")) 



#adding variables numberOfLevels, numberOfObservations, numberOfObervationsPerLevel
fitDataUnwrapped <- unnest(fitData, data)

fitDataUnwrapped <- fitDataUnwrapped %>%
  #number of levels
    group_by(id) %>% mutate(numberOfLevels = n()) %>%
  #number of observations
    group_by(id) %>% mutate(numberOfObservations = sum(obsNumber)) %>%
  #number of observations per level
    group_by(id) %>% mutate(observationMean = mean(obsNumber)) %>%
  #mean distance of treshold and level
    mutate(LevelTresholdDistance = level - treshold) %>%
    group_by(id) %>% mutate(meanLevelTresholdDistance = mean(LevelTresholdDistance)) %>%
  #mean distance of treshold and level
    group_by(id) %>% mutate(meanLevelTresholdDistance = median(LevelTresholdDistance))
#fitData$numberOfLevels <- rep(NaN, nrow(fitData))
#fitData$numberOfObservations <- rep(NaN, nrow(fitData))
#fitData$meanOfObservationsPerLevel <- rep(NaN, nrow(fitData))
#fitData$meanTresholdLevelDistance <- rep(NaN, nrow(fitData))
#for(i in 1:nrow(fitData)){
#  dat <- fitData[i,]$data[[1]]
#
#  fitData[i,]$numberOfLevels <- nrow(dat)
#  #number of observations
#  fitData[i,]$numberOfObservations <- sum(dat$obsNumber)
#  #number of observations per level
#  fitData[i,]$meanOfObservationsPerLevel <- mean(dat$obsNumber)
#  #mean distance of treshold and level
#  tres <- fitData[i,]$treshold[[1]]
#  fitData[i,]$meanTresholdLevelDistance <- mean(abs(dat$level - tres))
#}
#rm(i, tres)

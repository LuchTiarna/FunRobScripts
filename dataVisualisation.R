#data visualization
#fitDataVisualization <- unnest(fitData, data)

minBoundary <- min(fitDataUnwrapped$level)
maxBoundary <- max(fitDataUnwrapped$level)
definitionRange <- (0:25)/25 * (maxBoundary - minBoundary) + minBoundary


definitionRange <- data.frame(level_vis = definitionRange, join = rep(1, length(definitionRange)))

DataVis <- fitData
DataVis$data <- NULL

DataVis$join <- rep(1, nrow(DataVis))
DataVis <- full_join(DataVis, definitionRange)

DataVis$join <- NULL
rm(maxBoundary, minBoundary, definitionRange)

DataVis <- DataVis %>%
           mutate(hitPercentage_vis = PSfunction(gamma, lambda, sigmoid, core, level_vis, param1, param2)) %>%
           mutate(hitPercentage_fit_vis = PSfunction(gamma_fit, lambda_fit, sigmoid_fit, core_fit, level_vis, param1_fit, param2_fit))
           
plot <- ggplot(data=DataVis)
plot <- plot + geom_line( mapping=aes(x=level_vis, y=hitPercentage_vis))
plot <- plot + geom_line( mapping=aes(x=level_vis, y=hitPercentage_fit_vis, group=as.factor(id)), alpha=0.2, colour="red")

#improvement threshold
if(imp_threshold){
  thresholdValues <- PSfunction(DataVis$gamma, DataVis$lambda, DataVis$sigmoid, DataVis$core, DataVis$imp_threshold, DataVis$param1, DataVis$param2)
  fitThresholdValues <- PSfunction(DataVis$gamma_fit, DataVis$lambda_fit, DataVis$sigmoid_fit, DataVis$core_fit, DataVis$imp_threshold_fit, DataVis$param1_fit, DataVis$param2_fit)

  plot <- plot + geom_point( mapping=aes(x=imp_threshold, y=thresholdValues, group=as.factor(id)), colour="green", shape="triangle")
  plot <- plot + geom_point( mapping=aes(x=imp_threshold_fit, y=fitThresholdValues, group=as.factor(id)), colour="yellow", shape="triangle")
}
#performance threshold
if(perf_threshold){
  thresholdValues <- PSfunction(DataVis$gamma, DataVis$lambda, DataVis$sigmoid, DataVis$core, DataVis$perf_threshold, DataVis$param1, DataVis$param2)
  fitThresholdValues <- PSfunction(DataVis$gamma_fit, DataVis$lambda_fit, DataVis$sigmoid_fit, DataVis$core_fit, DataVis$perf_threshold_fit, DataVis$param1_fit, DataVis$param2_fit)
  
  plot <- plot + geom_point( mapping=aes(x=perf_threshold, y=thresholdValues, group=as.factor(id)), colour="green", shape="square")
  plot <- plot + geom_point( mapping=aes(x=perf_threshold_fit, y=fitThresholdValues, group=as.factor(id)), colour="yellow", shape="square")
}

#fixed thresholds
if(FALSE){#length(fixedThreshold)>0){
  for(j in 1:length(fixedThreshold)){
    
    threshold <- paste("fixedThreshold_", as.character(fixedThreshold[j]), sep="")
    thresholdFit <- paste("fixedThreshold_", as.character(fixedThreshold[j]), "_fit", sep="")

    plot <- plot + geom_point( mapping=aes(x:=!!threshold, y:=PSfunction(gamma, lambda, sigmoid, core, !!threshold, param1, param2), group = as.factor(id)), colour="green")
    plot <- plot + geom_point( mapping=aes(x:=!!thresholdFit, y:=PSfunction(gamma_fit, lambda_fit, sigmoid_fit, core_fit, !!thresholdFit, param1_fit, param2_fit), group = as.factor(id)), colour="yellow")
  }
}

#plot <- plot + geom_point( mapping=aes(x=fitTreshold, y=0.5), colour="yellow")
#plot <- plot + geom_point( mapping=aes(x=treshold, y=0.5), colour="green")
plot <- plot + facet_grid(sigmoid_fit + core_fit~sigmoid + core + gamma + lambda + param1 + param2)
plot

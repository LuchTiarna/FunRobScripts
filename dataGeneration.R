#Data generation
set.seed(70)


x <- log(seq(1.1,10,2))

#Multigenerate
dat <- multigenerate(list(c("gumbel_r","ab", 0, 0, 0.8900461,0.3),
                          c("gumbel_l","weibull", 0, 0,1, 3),
                          c("gauss","linear", 0, 0, 6, -6),
                          c("logistic","linear", 0, 0, 8, -8)),
                      x, #levels
                      rep(40,length(x)) , #number of trials per level
                      5 #number of response simulations per FUNCION
                      )



#Multifit

#functions to fit the data
fitters <-  list(c("gumbel_r", "ab"),
                 c("gumbel_l","weibull"),
                 c("gauss", "linear"),
                 c("logistic", "linear")
                )

results <- NULL
id <- 0

for(i in 1:nrow(dat)){
  for(fitter in fitters){
    id <- id + 1

    if(is.character(fitter[[1]])){
      sigmoidName <- fitter[[1]]
    }else{
      sigmoid <- fitter[[1]]
      sigmoidName <- as.character(substitute(sigmoid))
    }

    if(is.character(fitter[[2]])){
      coreName <- fitter[[2]]
    }else{
      core <- eval(fitter[[2]])
      coreName <- as.character(substitute(core))
    }

    level <- dat$data[[i]]$level
    simulatedHitPercentage <- dat$data[[i]]$simulatedHitPercentage
    obsNumber <- dat$data[[i]]$obsNumber
    
    fitfunction <- function(params){
      x <- PSfunction(params[1], params[2], sigmoidName, coreName, level, params[3], params[4])
      LE <- likelihood_log(simulatedHitPercentage,  x, obsNumber, rm.nan = FALSE)
      
      return(-LE)
    }

    fit <- NULL
    tryCatch({
      fit <- optim(c(0.05,0.05,1,1),fn=fitfunction)
    },error = function(e){warning(e);fit <- NULL})

    if(is.null(fit)){
      next
    }

    responses <- unnest(dat[i,], data)
    responses <- responses[complete.cases(responses),]
    params <- fit$par
    responses$fitedHitPercentage <- PSfunction(params[1], params[2], sigmoidName, coreName, responses$level, params[3], params[4])

    responses$id <- rep(id, nrow(responses))
    responses$sigmoid_fit <-  rep(fitter[1],nrow(responses))
    responses$core_fit <-  rep(fitter[2],nrow(responses))

    
    responses$gamma_fit <- rep(params[1], nrow(responses))
    responses$lambda_fit <- rep(params[2], nrow(responses))
    for(l in 3:length(params))
    {
      responses[[paste("param", as.character(l-2), "_fit", sep="")]] <-  rep(params[l], nrow(responses))
    }

    if(is.null(results)){
      results <- nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage, fitedHitPercentage))
    }else{
      row <- nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage, fitedHitPercentage))
      results <-bind_rows(results, row)
    }
  }
}

fitData <- results

rm(dat, fit, fitters, responses, results, row, coreName, fitter, i,l, id, params, sigmoidName, x, level, obsNumber, simulatedHitPercentage, fitfunction)

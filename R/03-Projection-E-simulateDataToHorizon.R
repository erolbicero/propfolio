#' Simulate prices to horizon based on fitted marginal distributions and copula - using Meucci's methods
#'
#' @param numSimulations is a scalar dictating number of simulations (rows) to conduct
#' @param numPeriodsForward is a scalar dictating number of periods forward (columns) the investment horizon is
#' @param vineCopulaFit is a vine copula object
#' @param signalMarginalFit is a list of marginal distribution fits for the signal risk factors
#' @param residualMarginalFit is a list of marginal distribution fits for the noisy risk factors
#' @param eigenVectors is a matrix of eigenVectors used to convert the horizon risk factors back into invariants
#'
#' @return a matrix of invariants projected to the investment horizon - to then be used in the pricing function
#'
#' @examples
#' none
#'
#' @export

#Verify this function
simulateDataToHorizon <- function(numSimulations, numPeriodsForward, vineCopulaFit, signalMarginalFit, residualMarginalFit, eigenVectors = diag((length(signalMarginalFit)+length(residualMarginalFit)))){
  
  numDimensions <- length(signalMarginalFit)+length(residualMarginalFit)
  
  simulatedNPeriodData <- lapply(1:numPeriodsForward
                                 ,function(y){       
                                   
                                   #****Make sure you handle the Bivariate case when you generalize it
                                   #SectorSPDRCopulaSim <- CDVineSim(50400, SectorSPDRCopulaFit$family, SectorSPDRCopulaFit$par, SectorSPDRCopulaFit$par2, SectorSPDRCopulaFit$type)
                                   VineCopulaSim <- if(class(vineCopulaFit)=="RVineMatrix"){
                                     VineCopula::RVineSim(N = numSimulations, RVM = vineCopulaFit)}
                                   else {BiCopSim(N = numSimulations, family = vineCopulaFit$family, par = vineCopulaFit$par, par2 = vineCopulaFit$par2)}
                                   
                                   
                                   
                                   MetaDistributionSignal <- qsstd(  p = VineCopulaSim[,1]
                                                                     , mean = signalMarginalFit[[1]]$estimate["mean"]
                                                                     , sd =   signalMarginalFit[[1]]$estimate["sd"]
                                                                     , nu =   signalMarginalFit[[1]]$estimate["nu"]
                                                                     , xi =   signalMarginalFit[[1]]$estimate["xi"]
                                   )
                                   
                                   MetaDistributionNoise  <- rstd(   n = numSimulations
                                                                     , mean = residualMarginalFit[[1]]$mean
                                                                     , sd =   residualMarginalFit[[1]]$sd
                                                                     , nu =   residualMarginalFit[[1]]$nu
                                   )
                                   
                                   
                                   
                                   for(x in 2:(length(signalMarginalFit)))
                                   {
                                     MetaDistributionSignal <- cbind(MetaDistributionSignal, qsstd(p = VineCopulaSim[,x], mean = signalMarginalFit[[x]]$estimate["mean"], sd = signalMarginalFit[[x]]$estimate["sd"], nu = signalMarginalFit[[x]]$estimate["nu"], xi = signalMarginalFit[[x]]$estimate["xi"]))
                                   }
                                   
                                   for(z in 2:(length(residualMarginalFit)))
                                   {
                                     if(length(residualMarginalFit[[z]])==2){ #If Normal Distribution
                                            MetaDistributionNoise <- cbind(MetaDistributionNoise, rnorm( n = numSimulations, mean = residualMarginalFit[[z]]$mean, sd = residualMarginalFit[[z]]$sd))
                                     } else{ #else: student T
                                            MetaDistributionNoise <- cbind(MetaDistributionNoise, rstd( n = numSimulations, mean = residualMarginalFit[[z]]$mean, sd = residualMarginalFit[[z]]$sd, nu = residualMarginalFit[[z]]$nu))
                                     }
                                   }
                                   
                                   
                                   MetaDistributionData <- cbind(MetaDistributionSignal, MetaDistributionNoise)
                                   MetaMultivariateDistribution <- MetaDistributionData %*% t(eigenVectors)
                                   
                                   return(MetaMultivariateDistribution)
                                   
                                 })
  
  #projection (see if you can use apply)
  simulatedNPeriodProjection<-matrix(0 , nrow = numSimulations, ncol = numDimensions )
  for(x in 1:numDimensions)
  {
    for (y in 1:numPeriodsForward)
    {
      simulatedNPeriodProjection[,x] <- simulatedNPeriodProjection[,x] + simulatedNPeriodData[[y]][,x]
      
    }
    
  }
  
  return(simulatedNPeriodProjection)
  ####  return(simulatedNPeriodData[[1]])
}

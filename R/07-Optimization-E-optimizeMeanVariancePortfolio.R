#' Optimize mean variance portfolio using Meucci's methods (holdings and simulated prices at the horizon) - brute force approach
#'
#' @param meanVector is a vector of mean price changes
#' @param covarianceMatrix is a square, semi-positive definite covariance matrix of price changes
#' @param horizonPriceVector is a vector of security prices at the horizon
#' @param horizonPnLMatrix is a matrix of prices changes at the horizon
#' @param wealthBalance is the balance in the account (used to calculate holdings)
#' @param maxWeightConstraint is a list of symbols what will be subject to a max inverse volatility weight
#' @param colNames names of symbols
#' @param longOnly logical, whether to keep the original OHLC xts matrices or to drop them
#'
#' @return a list of quadratic programming efficient frontier results
#'
#' @examples
#' none
#'
#' @export
optimizeMeanVariancePortfolio <- function(meanVector, covarianceMatrix, horizonPriceVector, horizonPnLMatrix, wealthBalance, maxWeightConstraint, colNames,longOnly){
  
  minHoldings <- ifelse(longOnly,0,-10000) #these are holdings
  
  covarianceMatrixReturnsBased <- robust::covRob(data = sapply(1:length(horizonPriceVector),function(x){horizonPnLMatrix[,x] / horizonPriceVector[x]}))$cov
  
  numSecurities <- ncol(covarianceMatrix)
  
  QuadProgFrontier<-
    lapply(seq(from = -1000, to = 1000, by = 0.05)
           ,function(x){
             tryCatch(
               quadprog::solve.QP(Dmat = covarianceMatrix
                                  , dvec = meanVector
                                  , Amat = t(rbind(   rep(1,numSecurities)*horizonPriceVector #full budget invest
                                                      , rep(1,numSecurities)*meanVector #min return
                                                      , diag(numSecurities) #greater than 0, no shorting
                                                      , diag(numSecurities)*-1*horizonPriceVector #max allocation
                                  ))
                                  , bvec = c(        wealthBalance #full budget invest
                                                     , x #vary minimum return required
                                                     ,rep(minHoldings,numSecurities) #greater than 0, no shorting
                                                     , rep(-wealthBalance,numSecurities)*ifelse(colNames %in% maxWeightConstraint$names, maxWeightConstraint$maxWeight, maxWeightConstraint$constraintWeight)
                                  )
                                  , meq = 2)
               , error = function(e){return(NA)}
             )}
    )
  
  #clean up NA values
  NAindex<-do.call(c,lapply(QuadProgFrontier,length))
  NAindex<-which(NAindex>1)
  QuadProgFrontier <- QuadProgFrontier[NAindex]
  QuadProgFrontier <- lapply(QuadProgFrontier,function(x){x$solution})
  
  
  #now calculate statistics
  QuadProgFrontierStats <- lapply(QuadProgFrontier
                                  ,function(Holdings){
                                    holdingToWeight <- as.vector((Holdings * horizonPriceVector)/sum(Holdings * horizonPriceVector))
                                    covariance <- covarianceMatrixReturnsBased
                                    
                                    portfolioValue <- sum(as.vector(Holdings * horizonPriceVector))
                                    
                                    
                                    aggResult <- list(
                                      weights = holdingToWeight
                                      , holdings = as.vector(Holdings)
                                      , expectedReturns = sum(as.vector(Holdings * meanVector)/portfolioValue)
                                      , standardDeviation = sqrt(holdingToWeight %*%  covariance %*% holdingToWeight)
                                    )
                                    
                                    return(aggResult)
                                    
                                  }
  )

  holdings<-do.call(rbind,lapply(QuadProgFrontierStats
                                 ,function(FStats){
                                   HLD<-round(FStats$holdings,5)
                                   return(HLD)
                                 }
  )
  )
  
  expectedReturn<-sapply(QuadProgFrontierStats
                         ,function(FStats){
                           ER<-round(FStats$expectedReturns,5)
                           return(ER)
                         }
  )
  
  standardDeviation<-sapply(QuadProgFrontierStats
                            ,function(FStats){
                              SD<-round(FStats$standardDeviation,5)
                              return(SD)
                            }
  )
  ###standardDeviation <- do.call(c,standardDeviation)
  #Why are there two of these?
  weights<-lapply(QuadProgFrontierStats
                  ,function(FStats){
                    WT<-round(FStats$weights,5)
                    return(WT)
                  }
  )
  
  weights <- do.call(rbind, weights)  
  
  optimalIdx <- which((expectedReturn/standardDeviation)==max(expectedReturn/standardDeviation))
  
  
  QuadProgFrontierResults <- list(holdings = holdings
                                  , weights = weights
                                  , expectedReturn = expectedReturn
                                  , standardDeviation = standardDeviation
                                  , optimalResult = list(holdings = holdings[optimalIdx,]
                                                         , weights = weights[optimalIdx,]
                                                         , expectedReturn = expectedReturn[optimalIdx]
                                                         , standardDeviation = standardDeviation[optimalIdx]
                                                         , noriskSharpe = expectedReturn[optimalIdx]/standardDeviation[optimalIdx]
                                                         , optimalIndex = optimalIdx
                                  )
  )
  
  return(QuadProgFrontierResults)
  
}

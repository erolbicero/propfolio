#' Calculate projected prices at investment horizon
#'
#' @param simulatedHorizonInvariants is a matrix of invariants, used to project prices, given a pricing function
#' @param horizonPriceVector is a vector of prices used to begin the projection from
#'
#' @return a matrix prices at the horizon
#'
#' @examples
#' none
#'
#' @export
horizonPrices <- function(simulatedHorizonInvariants, horizonPriceVector){
  projectedHorizonPrices<-lapply(1:9
                                  ,function(x){horizonPriceVector[x] * exp(simulatedHorizonInvariants[,x])
                                  })
  projectedHorizonPrices <- do.call(cbind,projectedHorizonPrices)
  return(projectedHorizonPrices)
}

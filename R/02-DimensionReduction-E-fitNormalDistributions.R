#' fit normal distributions
#'
#' @param timeSeriesMatrix description TBD
#'
#' @return return normal distribution fits
#'
#' @examples
#' none
#'
#' @export
fitNormalDistributions <- function(timeSeriesMatrix){
  
  listOfParameters<-apply(X = coredata(timeSeriesMatrix)
                          , MARGIN = 2
                          , FUN = function(colVector){
                            fitdistrplus::fitdist(data = colVector
                                                  , distr = "norm"
                                                  , method = "mle"
                            )
                          }
  )
  
  return(listOfParameters)
  
}

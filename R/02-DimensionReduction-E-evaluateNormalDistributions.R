#' eval normal distributions
#'
#' @param timeSeriesMatrix description TBD
#' @param listOfParameters description TBD
#'
#' @return evaluated normal distribution fits
#'
#' @examples
#' none
#'
#' @export
evaluateNormalDistributions<-function(timeSeriesMatrix, listOfParameters){
  
  normalFitList<-lapply(1:ncol(timeSeriesMatrix)
                        ,function(i){
                          
                          evaluationResult<-ADGofTest::ad.test(
                            x = coredata(timeSeriesMatrix[,i])
                            , distr.fun = pnorm
                            , mean = listOfParameters[[i]]$estimate["mean"]
                            , sd = listOfParameters[[i]]$estimate["sd"]
                          )
                          return(evaluationResult)
                          
                        }
  )
  return(normalFitList)
  
}

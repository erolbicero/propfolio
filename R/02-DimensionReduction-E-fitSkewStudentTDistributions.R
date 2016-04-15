#then fit skew t distributions for the marginals (or test them here)
#' fit skew t distributions
#'
#' @param timeSeriesMatrix description TBD
#'
#' @return return skew t distribution fits
#'
#' @examples
#' none
#'
#' @export
fitSkewStudentTDistributions<-function(timeSeriesMatrix){
  
  listOfParameters<-apply(X = coredata(timeSeriesMatrix)
                          , MARGIN = 2
                          , FUN = function(colVector){
                            fGarch::sstdFit(x=colVector)
                          }
  )
  
  return(listOfParameters)
  
}

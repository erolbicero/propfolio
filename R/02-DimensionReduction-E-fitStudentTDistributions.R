#then fit t distributions for the marginals (or test them here)
#' fit t distributions
#'
#' @param timeSeriesMatrix description TBD
#'
#' @return return t distribution fits
#'
#' @examples
#' none
#'
#' @export
fitStudentTDistributions<-function(timeSeriesMatrix){
  listOfParameters<-lapply(X = timeSeriesMatrix
                           ,FUN = function(colVector){
                             fGarch::stdFit(x=colVector)
                           }
  )
  
  return(listOfParameters)
  
}

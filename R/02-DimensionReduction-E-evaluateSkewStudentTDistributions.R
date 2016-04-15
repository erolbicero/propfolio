#' eval skew t distributions
#'
#' @param timeSeriesMatrix description TBD
#' @param listOfParameters description TBD
#'
#' @return evaluated skew student t distribution fits
#'
#' @examples
#' none
#'
#' @export
evaluateSkewStudentTDistributions<-function(timeSeriesMatrix, listOfParameters){
  
  skewStudentTFitList <- lapply(1:ncol(timeSeriesMatrix),function(i){
    
    evaluationResult<-ADGofTest::ad.test(
      x = coredata(timeSeriesMatrix[,i])
      , distr.fun = psstd
      , mean = listOfParameters[[i]]$estimate["mean"]
      , sd = listOfParameters[[i]]$estimate["sd"]
      , nu = listOfParameters[[i]]$estimate["nu"]
      , xi = listOfParameters[[i]]$estimate["xi"]
    )
    return(evaluationResult)
  }
  )
  
  return(skewStudentTFitList)    
}

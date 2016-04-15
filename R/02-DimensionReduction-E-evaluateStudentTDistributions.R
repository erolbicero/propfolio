#evaluate student t fits
#' eval t distributions
#'
#' @param timeSeriesMatrix description TBD
#' @param listOfParameters description TBD
#'
#' @return evaluated student t distribution fits
#'
#' @examples
#' none
#'
#' @export
evaluateStudentTDistributions<-function(timeSeriesMatrix, listOfParameters){
  
  studentTFitList <- lapply(1:ncol(timeSeriesMatrix),function(i){
    
    evaluationResult<-ADGofTest::ad.test(
      x = coredata(timeSeriesMatrix[,i])
      , distr.fun = pstd
      , mean = listOfParameters[[i]]$par["mean"]
      , sd = listOfParameters[[i]]$par["sd"]
      , nu = listOfParameters[[i]]$par["nu"]
    )
    return(evaluationResult)
  }
  )
  
  return(studentTFitList)    
}

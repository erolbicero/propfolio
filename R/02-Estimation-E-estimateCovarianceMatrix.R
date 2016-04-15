#dummy text
#' detailed function to estimate robust covariance matrix based on some assumptions
#'
#' @param timeSeriesMatrix
#'
#' @return robust covariance matrix
#'
#' @examples
#' none
#'
#' @export
estimateCovarianceMatrix<-function(timeSeriesMatrix){
  #if this gets more advanced you might need to use a case/switch 
  #based on conditions that you decide are optimal
  
  #check if start dates are equal or not; will indicate the covariance matrix estimation technique
  startDateTest<-testIfStartDatesEqual(timeSeriesMatrix)
  
  if(startDateTest)
  {
    #You have to decide how you impute data
    #Use robust covariance, which is a form of MCD
    covarianceMatrix<-covRob(data=timeSeriesMatrix
                             ,na.action = na.omit
    )$cov    
    
  }
  else
  { #If start dates are not equal use Bayesian techniques for monotone missingness
    covarianceMatrix<-bmonomvn(y=timeSeriesMatrix
                               ,method = "ng"
    )$S
    
  }
  
  return(covarianceMatrix)
}

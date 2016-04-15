#' Fit signal distributions for signal principal components - assumes skew student t distributions
#'
#' @param timeSeriesMatrix xts time series matrix
#' @param signalEigenValues signal eigenvalues determined by dominantEigenvectors
#' @param signalEigenVectors signal eigenvectors determined by dominantEigenvectors
#'
#' @return a list with skew student t distribution fits for each component
#'
#' @examples
#' none
#'
#' @export
fitMarginalSignal <- function(timeSeriesMatrix, signalEigenValues, signalEigenVectors){
  
  #Extract signal vectors
  signalFactors <- tsEigenProduct(timeSeriesMatrix, signalEigenVectors)
  
  skewStudentTFits <- fitSkewStudentTDistributions(timeSeriesMatrix = signalFactors
  )
  skewStudentTFitEvaluation <- evaluateSkewStudentTDistributions(timeSeriesMatrix = signalFactors
                                                                 ,listOfParameters = skewStudentTFits)
  
  print(lapply(skewStudentTFitEvaluation,function(x){return(as.vector(x$p.value))}))
  
  return(list(fits = skewStudentTFits, evaluation = skewStudentTFitEvaluation))
  
  
}

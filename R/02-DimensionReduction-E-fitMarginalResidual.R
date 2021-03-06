#' Fit marginal distributions for noisy principal components - assumes student t distributions or normal distributions
#'
#' @param timeSeriesMatrix xts time series matrix
#' @param resudualEigenValues noisy eigenvalues determined by dominantEigenvectors
#' @param residualEigenVectors noisy eigenvectors determined by dominantEigenvectors
#'
#' @return a list with normal or student t distribution fits for each component
#'
#' @examples
#' none
#'
#' @export
fitMarginalResidual <- function(timeSeriesMatrix, residualEigenValues, residualEigenVectors){
  
  
  #Extract noisy vectors
  residualFactors <- tsEigenProduct(timeSeriesMatrix, residualEigenVectors)
  
  #fit student T and Normal distributions
  studentFits <- fitStudentTDistributions(residualFactors)
  
  normalFits <- fitNormalDistributions(residualFactors)
  
  #take the lowest AD value of all the tests to identify the proper distribution
  normalFitEvaluation <- evaluateNormalDistributions(
    timeSeriesMatrix = residualFactors
    , listOfParameters = normalFits
  )
  
  studentFitEvaluation <- evaluateStudentTDistributions(
    timeSeriesMatrix = residualFactors
    , listOfParameters = studentFits
  )
  
  #these have to be same length
  normalADStats <- ADStatExtract(normalFitEvaluation)
  studentADStats <- ADStatExtract(studentFitEvaluation)
  
  ADStats <- cbind( NORMAL = normalADStats
                    , STUDENT = studentADStats
  )
  
  #compare, want lowest AD stat
  ADResult <- ifelse(ADStats[,"NORMAL"] < ADStats[,"STUDENT"],"NORMAL", "STUDENT")
  
  #obtain average noisy Eigenvalues
  EigenNoise <- mean(residualEigenValues)
  
  #now create distributions for residuals using Eigenvalues, recall you want avg Eigenvalues
  residualDistributions <-
    lapply(1:length(ADResult)
           , function(index){
             switch(ADResult[index]
                    , NORMAL = list(mean = 0, sd = sqrt(EigenNoise))
                    , STUDENT = list(mean = 0, sd = sqrt(EigenNoise), nu = as.numeric(studentFits[[index]]$par["nu"]))
             )
           }
    )
  
  return(list(fits = residualDistributions, evaluation = c(normalFitEvaluation, studentFitEvaluation)))
  
}

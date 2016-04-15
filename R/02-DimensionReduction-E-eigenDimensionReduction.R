#eigen value decomposition based on Estimation
#' Wrapperfor base::eigen, used in case this function switches to Rcpp eigen
#'
#' @param covarianceMatrix which is a numeric square matrix
#'
#' @return a list object with eigenvalues and eigenvectors
#'
#' @examples
#' none
#'
#' @export
eigenDimensionReduction<-function(covarianceMatrix){
  eigenDecomposition<-base::eigen(covarianceMatrix)
  
  eigenResult<-list(eigenValues = eigenDecomposition$values
                    ,eigenVectors = eigenDecomposition$vectors
  )
  
  return(eigenResult)
}

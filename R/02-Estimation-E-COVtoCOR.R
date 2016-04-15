#Write a function to convert the covariance matrix to correlation matrix
#' convert covariance matrix to correlation matrix
#'
#' @param covarianceMatrix
#'
#' @return correlation matrix
#'
#' @examples
#' none
#'
#' @export
COVtoCOR<-function(covarianceMatrix){
  
  D<-diag(sqrt(diag(covarianceMatrix)))
  D.inv<-solve(D)
  Rho<-D.inv%*%covarianceMatrix
  Rho<-Rho%*%D.inv
  rownames(Rho)<-rownames(covarianceMatrix)
  colnames(Rho)<-colnames(covarianceMatrix)
  
  return(Rho)
}

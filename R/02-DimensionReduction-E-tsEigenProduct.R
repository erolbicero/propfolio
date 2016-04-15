#' Convenience funciton to matrix multiply an xts object with  a set of eigenvectors while preserving the date-index
#'
#' @param timeSeriesMatrix is an xts time series matrix
#' @param EigenVectors is a matrix of eigenvectors
#'
#' @return an xts object
#'
#' @examples
#' none
#'
#' @export
tsEigenProduct <- function(timeSeriesMatrix,EigenVectors){
  
  EigenProduct<-xts(x=coredata(timeSeriesMatrix) %*% EigenVectors
                    ,order.by=index(timeSeriesMatrix)
  )
  
  return(EigenProduct)
  
}

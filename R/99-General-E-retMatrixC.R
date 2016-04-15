#' Calculate continuous (log) returns for a matrix
#'
#' @param priceMatrix is an xts price matrix 
#'
#' @return a continuous return xts matrix 
#'
#' @examples
#' none
#'
#' @export

retMatrixC<-function(priceMatrix)
{
  CReturns <- lapply(  X=priceMatrix
                       , FUN=retVectorC
  )
  CReturns <- do.call(merge, CReturns)
  
  return(CReturns)
}

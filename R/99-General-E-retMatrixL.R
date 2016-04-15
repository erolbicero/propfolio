#' Calculate linear returns for a matrix
#'
#' @param priceMatrix is an xts price matrix 
#'
#' @return a linear return xts matrix 
#'
#' @examples
#' none
#'
#' @export

retMatrixL<-function(priceMatrix)
{
  LReturns <- lapply(  X=priceMatrix
                       , FUN=retVectorL
  )
  LReturns <- do.call(merge, LReturns)
  
  return(LReturns)
}

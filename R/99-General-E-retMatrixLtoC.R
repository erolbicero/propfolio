#' Convert linear to continuous (log) returns for a matrix
#'
#' @param LRetMatrix is a log return xts matrix 
#'
#' @return a linear return xts matrix 
#'
#' @examples
#' none
#'
#' @export

retMatrixLtoC <-function(LRetMatrix)
{
  LtoCReturns <- lapply(  X=LRetMatrix
                       , FUN=retVectorLtoC
  )
  LtoCReturns <- do.call(merge, LtoCReturns)
  
  return(LtoCReturns)
}

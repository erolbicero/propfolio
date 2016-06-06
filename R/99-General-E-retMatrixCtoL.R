#' Convert continuous (log) returns to linear returns for a matrix
#'
#' @param CRetMatrix is a log return xts matrix 
#'
#' @return a linear return xts matrix 
#'
#' @examples
#' none
#'
#' @export

retMatrixCtoL <-function(CRetMatrix)
{
  CtoLReturns <- lapply(  X=CRetMatrix
                       , FUN=retVectorCtoL
  )
  CtoLReturns <- do.call(merge, CtoLReturns)
  
  return(CtoLReturns)
}

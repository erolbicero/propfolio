#' convert matrix object, to a list of columns - used to enable use of lapply over apply(,1,FUN), which is significantly faster
#'
#' @param matrixToConvert a numerical matrix
#'
#' @return a list of the columns of the matrix
#'
#' @examples
#' none
#'
#' @export
toColList <- function(matrixToConvert){
  
  resultList <- 
    do.call(c
            ,apply(
              matrixToConvert
              ,2
              ,function(x){list(x)}
            )
    )
  return(resultList)
}
#' convert matrix object, to a list of rows - used to enable use of lapply over apply(,1,FUN), which is significantly faster
#'
#' @param matrixToConvert a numerical matrix
#'
#' @return a list of the rows of the matrix
#'
#' @examples
#' none
#'
#' @export
toRowList <- function(matrixToConvert){

  resultList <- 
  do.call(c
        ,apply(
                  matrixToConvert
                  ,1
                  ,function(x){list(x)}
               )
        )
  return(resultList)
}
#' compute drawdown for time series matrix
#'
#' @param timeSeriesVector xts time series matrix
#' @param continuous logical; if true, will compute index levels for continuous returns, if FALSE, will compute index levels for linear returns; defaults to FALSE
#'
#' @return a matrix of drawdown levels
#'
#' @examples
#' none
#'
#' @export
computeDrawdownMatrix <- function(timeSeriesMatrix, continuous = FALSE){

resultMatrix <- 
do.call(xts::merge.xts,
lapply(timeSeriesMatrix
       , computeDrawdownVector
       , continuous = continuous
       )
)
return(resultMatrix)

}
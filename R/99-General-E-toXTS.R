#' convert data frame object, where first column is a date vector, to an xts object
#'
#' @param rawDataFrame
#'
#' @return xts object
#'
#' @examples
#' none
#'
#' @export
toXTS <- function(rawDataFrame){
  xtsObject <- xts( x = rawDataFrame[,-1] #exclude date column
                    , order.by = as.Date(rawDataFrame[,1]) #select date column
  )
  
  return(xtsObject)
}
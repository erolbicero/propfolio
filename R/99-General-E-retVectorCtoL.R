#' Convert log to linear returns for a vector
#'
#' @param timeseriesCRetVector is an xts log return vector 
#'
#' @return a linear return xts object 
#'
#' @examples
#' none
#'
#' @export

#Linear Returns
retVectorCtoL <- function(timeseriesCRetVector)
{
  return(base::exp(timeseriesCRetVector) - 1)
}


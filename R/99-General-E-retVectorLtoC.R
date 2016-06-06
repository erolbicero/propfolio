#' Convert linear to log returns for a vector
#'
#' @param timeseriesLRetVector is an xts linear return vector 
#'
#' @return a log-return xts object 
#'
#' @examples
#' none
#'
#' @export

#Linear Returns
retVectorLtoC <- function(timeseriesLRetVector)
{
  return(base::log(1 + timeseriesLRetVector))
}

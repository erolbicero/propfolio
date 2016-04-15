#' convert an xts object, where first column is a date vector, to a data frame object
#'
#' @param rawXtsObject
#'
#' @return data.frame object
#'
#' @examples
#' none
#'
#' @export
toDF <- function(rawXtsObject){
  objectNames <- names(rawXtsObject)
  dataFrameObject <- data.frame( Date = index(rawXtsObject)
                                 , coredata(rawXtsObject)
                                 , stringsAsFactors = FALSE
                          )
  
  colnames(dataFrameObject)[-1] <- objectNames
  
  return(dataFrameObject)
}
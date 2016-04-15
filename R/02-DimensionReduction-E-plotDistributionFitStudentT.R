#' plot t distributions
#'
#' @param timeSeriesVector description TBD
#' @param listOfParameters description TBD
#'
#' @return plot
#'
#' @examples
#' none
#'
#' @export
plotDistributionFitStudentT<-function(timeSeriesVector, listOfParameters){
  hist(timeSeriesVector, prob=TRUE)
  curve(dstd(x, mean=listOfParameters$par["mean"], sd=listOfParameters$par["sd"], nu=listOfParameters$par["nu"]), add=TRUE)
}
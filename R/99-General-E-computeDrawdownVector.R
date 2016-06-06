#' compute drawdown for time series vector
#'
#' @param timeSeriesVector xts time series vector
#' @param continuous logical; if true, will compute index levels for continuous returns, if FALSE, will compute index levels for linear returns; defaults to FALSE
#'
#' @return a vector of drawdown levels
#'
#' @examples
#' none
#'
#' @export
computeDrawdownVector <- function(timeSeriesVector, continuous = FALSE){

if(!continuous){
          indexLevel <- cumprod(1 + timeSeriesVector)
} else {
          indexLevel <- cumprod(exp(timeSeriesVector))
}

listOfIDX <-
lapply(1:nrow(indexLevel)
       ,function(x){
                    return(1:x) 
        }
        )

maxVector <-
do.call(c,
lapply(listOfIDX
       ,function(IDX){
         return(max(indexLevel[IDX]))
       }
       )
)

maxVector <- xts::xts( x = maxVector
                      , order.by = index(indexLevel)
                    )

resultVector <- indexLevel/maxVector - 1
colnames(resultVector) <- colnames(timeSeriesVector)

return(resultVector)

}
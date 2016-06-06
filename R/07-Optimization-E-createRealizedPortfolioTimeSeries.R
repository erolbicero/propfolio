#' extract optimization statistics from the optimization object
#'
#' @param linearReturnMatrix a linear return xts matrix
#' @param weightMatrix a weight xts object, with same number of columns as the linearReturnMatrix
#' @param includeConstituents logical, if TRUE, the output will append the unweighted constituents of the portfolio; defaults to FALSE
#' @param benchmark an xts vector of linear returns; if not NA, then will append this to the resulting xts object; defaults to NA (not inlcuded)
#' @param portfolioName a character string, defaults to "Portfolio"
#' @param benchmarkName a character string, used to rename a benchmark that's included; defaults to "Benchmark"
#'
#' @return an xts matrix of results OR a list of matrices depending on the selection
#'
#' @examples
#' FUNCTION STILL UNDER DEVELOPMENT
#'
#' @export
createRealizedPortfolioTimeSeries <- function(linearReturnMatrix, weightMatrix, includeConstituents = FALSE, benchmark = NA, portfolioName = "Portfolio", benchmarkName = "Benchmark", rebalance = TRUE){

  validDates <- zoo::index(xts::merge.xts(linearReturnMatrix, weightMatrix, join = "inner"))
  
  if(rebalance){
            realizedReturn <- PerformanceAnalytics::Return.portfolio(R = linearReturnMatrix[validDates]
                                                                 , weights = weightMatrix[validDates]
                                                                 , geometric = TRUE #linear returns
                                                                    )
  } else {
            realizedReturn <- xts::xts(x=apply(coredata(linearReturnMatrix[validDates]) * coredata(weightMatrix[validDates]),1,sum), order.by = validDates)
  }
  
  colnames(realizedReturn) <- portfolioName

  if(!is.na(unique(benchmark)[1])){
    realizedReturn <- xts::merge.xts(realizedReturn, Benchmark = benchmark, join = "left")
    colnames(realizedReturn)[ncol(realizedReturn)] <- benchmarkName
  }
  
  if(includeConstituents){
  realizedReturn <- xts::merge.xts(realizedReturn, linearReturnMatrix, join = "left")
  }


  return(realizedReturn)

}
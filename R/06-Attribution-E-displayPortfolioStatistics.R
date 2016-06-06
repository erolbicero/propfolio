#' display portfolio risk and return statistics
#'
#' @param linearReturnMatrix a linear return xts matrix
#' @param weightMatrix a weight xts object, with same number of columns as the linearReturnMatrix
#' @param includeConstituents logical, if TRUE, the output will append the unweighted constituents of the portfolio; defaults to FALSE
#' @param benchmark an xts vector of linear returns; if not NA, then will append this to the resulting xts object; defaults to NA (not inlcuded)
#' @param portfolioName a character string, defaults to "Portfolio"
#' @param benchmarkName a character string, used to rename a benchmark that's included; defaults to "Benchmark"
#' @param riskFreeRate a numeric value, presumably greater than 0, small and positive, that's passed to the Sharpe Ratio calculations
#'
#' @return prints summary statistics output including annualized risk, return, Sharpe Ratio, Drawdown, VaR, ETL, Skew, Kurtosis and Information Ratio
#'
#' @examples
#' FUNCTION STILL UNDER DEVELOPMENT
#'
#' @export
displayPortfolioStatistics <- function(
 linearReturnMatrix
, weightMatrix
, includeConstituents = TRUE
, benchmark = NA
, portfolioName = "Portfolio"
, benchmarkName = "Benchmark"
, riskFreeRate = 0
){

benchmark <- benchmark[index(linearReturnMatrix)] #filter for dates so that there are no NAs
  
portfolioMatrix <-
  createRealizedPortfolioTimeSeries(linearReturnMatrix =  linearReturnMatrix
                                             , weightMatrix = weightMatrix
                                             , includeConstituents = includeConstituents
                                             , benchmark = benchmark
                                             , portfolioName = portfolioName
                                             , benchmarkName = benchmarkName
                                             )
  
scalingFactor <-
switch(xts::periodicity(portfolioMatrix)$scale
        , daily = 252
        , weekly = 52
        , monthly = 12
        , quarterly = 4
        , yearly = 1
      )

portfolioStatistics <- 
rbind(
  PerformanceAnalytics::Return.annualized(R = portfolioMatrix
                                        , scale = scalingFactor
                                        )
, PerformanceAnalytics::StdDev.annualized(x = portfolioMatrix
                                          , scale = scalingFactor
                                        )
, PerformanceAnalytics::SemiDeviation(R = portfolioMatrix)*sqrt(scalingFactor)

, PerformanceAnalytics::SharpeRatio.annualized(R = portfolioMatrix, scale = scalingFactor, Rf = riskFreeRate)
, PerformanceAnalytics::AdjustedSharpeRatio(R = portfolioMatrix, scale = scalingFactor, Rf = riskFreeRate)
, PerformanceAnalytics::AverageDrawdown(R = portfolioMatrix)
, PerformanceAnalytics::AverageRecovery(R = portfolioMatrix)
, `VaR 95` = PerformanceAnalytics::VaR(R = portfolioMatrix, p = 0.95)
, `VaR 99` = PerformanceAnalytics::VaR(R = portfolioMatrix, p = 0.99)
, `ETL 95` = PerformanceAnalytics::ETL(R = portfolioMatrix, p = 0.95)
, `ETL 99` = PerformanceAnalytics::ETL(R = portfolioMatrix, p = 0.99)
, `Worst Loss` = t(data.frame(apply(portfolioMatrix,2,function(x){x<-x[!is.na(x)]; return(min(x))})))
, PerformanceAnalytics::skewness(x = portfolioMatrix)
, PerformanceAnalytics::kurtosis(x = portfolioMatrix)
)


if(!is.na(unique(benchmark)[1])){
  portfolioStatistics <- 
    rbind(portfolioStatistics
      , `Information Ratio` =  sapply(portfolioMatrix
                , function(x){
                           PerformanceAnalytics::InformationRatio(Ra = x, Rb = portfolioMatrix[ , benchmarkName], scale = scalingFactor)
                            }
                            )
    )
}

print(portfolioStatistics)

}

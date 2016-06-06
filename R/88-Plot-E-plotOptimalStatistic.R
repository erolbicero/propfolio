#' Plot time series object in ggplot
#'
#' @param optimizationResultsObject a list containing rolling statistics for an optimization
#' @param statisticType character string, one of "opHoldings", "opWeights", "opER", "opSD", "opSharpe", "opIndex" for Optimal result; one of "mvHoldings", "mvWeights", "mvER", "mvSD", "mvSharpe", "mvIndex" for Minimum Variance result; the following are not available for this funciton: "ER" for expected return, "SD" for standard deviation, and "EF" for efficient frontier
#' @param datePeriodType character string, one of "train", "test", currently assumes one month ahead, defaults to "train"
#' @param plotTitle character string of plot title
#' @param yTitle character string of y-axis title
#' @param xTitle title for x-axis, defaults to "Date"
#' @param annualize logical that will annualize Expected Return, Standard Deviation or Sharpe ratio, if it's not applicable, it will be ignored; defaults to FALSE
#' @param interactive logical, whether or not to render in plotly for interactive use of plot; defaults to FALSE
#'
#' @return a timeseries plot (ggplot)
#'
#' @examples
#' none
#'
#' @export
plotOptimalStatistic <- 
  function(optimizationResultsObject, statisticType, datePeriodType = "train"
           , plotTitle = NULL
           , yTitle = NULL
           , xTitle = "Date"
           , legendTitle = NULL
           , annualize = FALSE, interactive = FALSE)
  {
    
    #check first for invalid statistics
    switch(statisticType
    , ER =  {stop("ER not available for plotting as it returns a list object")}
    , SD =  {stop("SD not available for plotting as it returns a list object")}
    , EF =  {stop("EF not available for plotting as it returns a list object")}
    )
    
    #available statistics
    definedStatisticVector <- c(
      "opHoldings"
      , "opWeights"
      , "opER"
      , "opSD"
      , "opSharpe"
      , "opIndex"
      , "mvHoldings"
      , "mvWeights"
      , "mvER"
      , "mvSD"
      , "mvSharpe"
      , "mvIndex"
    )
    
    #error check
    if(!(statisticType %in% definedStatisticVector)){stop("statisticType is not defined")}
    
    #if statistic is defined...
    #store user titles
    if(!is.null(plotTitle)){
      userPlotTitle <- plotTitle
    } else {
      userPlotTitle <- NA
    }
    
    if(!is.null(yTitle)){
      userYTitle <- yTitle
    } else {
      userYTitle <- NA
    }
    
    if(!is.null(legendTitle)){
      userLegendTitle <- legendTitle
    } else {
      userLegendTitle <- NA
    }
    
    timeSeriesObject <- 
      createOptimalStatisticTimeSeries(optimizationResultsObject = optimizationResultsObject
                                       , statisticType = statisticType
                                       , datePeriodType = datePeriodType
                                       , annualize = annualize
                                       , indexType = "timeSeries"
      )
    
    #run this first to fail in case ER/SD is selected
    switch(statisticType
           , opHoldings = {
             plotTitle <- "Optimal Number of Holdings per Asset";
             yTitle <- "Number of Units";
             yLabelType <- "comma";
             legendTitle <- "Ticker";
           }
           , opWeights = {
             plotTitle <- "Optimal Asset Weights";
             yTitle <- "Percentage of Portfolio";
             yLabelType <- "percent";
             legendTitle <- "Ticker";
           }
           , opER =  {
             plotTitle <- "Optimal Expected Return";
             yTitle <- "Expected Return [%]";
             yLabelType <- "percent";
             colnames(timeSeriesObject) <- "E(R)";
             legendTitle <- "Statistic";
           }
           , opSD =   {
             plotTitle <- "Optimal Standard Deviation";
             yTitle <- "Standard Deviation [%]";
             yLabelType <- "percent";
             colnames(timeSeriesObject) <- "SD";
             legendTitle <- "Statistic";
           }
           , opSharpe = {
             plotTitle <- "Optimal Sharpe Ratio";
             yTitle <- "Sharpe Ratio";
             yLabelType <- "comma";
             colnames(timeSeriesObject) <- "Sharpe";
             legendTitle <- "Statistic";
           }
           , opIndex = {
             plotTitle <- "Optimal Solution Index";
             yTitle <- "Index Value for Optimal Solution";
             yLabelType <- "comma";
             colnames(timeSeriesObject) <- "Index";
             legendTitle <- "Statistic";
           }
           , mvHoldings = {
             plotTitle <- "Minimum Variance Number of Holdings per Asset";
             yTitle <- "Number of Units";
             yLabelType <- "comma";
             legendTitle <- "Ticker";
           }
           , mvWeights = {
             plotTitle <- "Minimum Variance Asset Weights";
             yTitle <- "Percentage of Portfolio";
             yLabelType <- "percent";
             legendTitle <- "Ticker";
           }
           , mvER = {
             plotTitle <- "Minimum Variance Expected Return";
             yTitle <- "Expected Return [%]";
             yLabelType <- "percent";
             colnames(timeSeriesObject) <- "E(R)";
             legendTitle <- "Statistic";
           }
           , mvSD = {
             plotTitle <- "Minimum Variance Standard Deviation";
             yTitle <- "Standard Deviation [%]";
             yLabelType <- "percent";
             colnames(timeSeriesObject) <- "SD";
             legendTitle <- "Statistic";
           }
           , mvSharpe = {
             plotTitle <- "Minimum Variance Sharpe Ratio";
             yTitle <- "Sharpe Ratio";
             yLabelType <- "comma";
             colnames(timeSeriesObject) <- "Sharpe";
             legendTitle <- "Statistic";
           }
           , mvIndex = {
             plotTitle <- "Minimum Variance Solution Index";
             yTitle <- "Index Value for Optimal Solution";
             yLabelType <- "comma";
             colnames(timeSeriesObject) <- "Index";
             legendTitle <- "Statistic";
           }
           , stop("statisticType is not defined")
    )
    
    #If user supplied titles, override them
    if(!is.na(userPlotTitle)){
      plotTitle <- userPlotTitle
    }
    
    if(!is.na(userYTitle)){
      yTitle <- userYTitle
    } 
    
    if(!is.na(userLegendTitle)){
      legendTitle <- userLegendTitle
    } 

    plotTimeSeriesObject(xtsTimeSeriesObject = timeSeriesObject
                         , plotTitle = plotTitle
                         , yTitle = yTitle
                         , yLabelType = yLabelType
                         , xTitle = xTitle
                         , legendTitle = legendTitle
                         , levels = "raw"
                         , interactive = interactive
    )
  }
#' extract optimization statistics from the optimization object
#'
#' @param optimizationResultsObject a list containing rolling statistics for an optimization
#' @param statisticType character string, one of "opHoldings", "opWeights", "opER", "opSD", "opSharpe", "opIndex" for Optimal result; one of "mvHoldings", "mvWeights", "mvER", "mvSD", "mvSharpe", "mvIndex" for Minimum Variance result; or for a list of Efficient Fronter numbers, "ER" for expected return, "SD" for standard deviation, and "EF" for a list of matrices where the first column is standard deviation, and second column is expected return
#' @param datePeriodType character string, one of "train", "test", currently assumes one month ahead, defaults to "train"
#' @param annualize logical that will annualize Expected Return, Standard Deviation or Sharpe ratio, if it's not applicable, it will be ignored; defaults to FALSE
#' @param indexType character string, one of "timeSeries" or "raw"; where "timeSeries" generates a daily xts object of the statistic, and "raw" appends the rolling time period to the statistic
#' @param indexTimeSeriesFreq character string, one of "daily", "weekly", "monthly", or "yearly"; only applicable if indexType = "timeSeries", defaults to "daily"
#'
#' @return an xts matrix of results OR a list of matrices depending on the selection
#'
#' @examples
#' FUNCTION STILL UNDER DEVELOPMENT
#'
#' @export
createOptimalStatisticTimeSeries <- function(optimizationResultsObject, statisticType, datePeriodType = "train", annualize = FALSE, indexType = "timeSeries", indexTimeSeriesFreq = "daily"){
  
  statisticName <- switch (statisticType
                           , opWeights = c("optimalResult","weights")
                           , opHoldings = c("optimalResult","holdings")
                           , opER = c("optimalResult","expectedReturn")
                           , opSD = c("optimalResult","standardDeviation")
                           , opSharpe = c("optimalResult","noriskSharpe")
                           , opIndex = c("optimalResult","optimalIndex")
                           
                           , mvWeights = c("minVarResult","weights")
                           , mvHoldings = c("minVarResult","holdings")
                           , mvER = c("minVarResult","expectedReturn")
                           , mvSD = c("minVarResult","standardDeviation")
                           , mvSharpe = c("minVarResult","noriskSharpe")
                           , mvIndex = c("minVarResult","minVarIndex")
                           
                           , ER = "expectedReturn"
                           , SD = "standardDeviation"
                           , EF = c("expectedReturn","standardDeviation")
                           , stop("statisticType is not defined")
  )  
  
  optimizationFreq <- optimizationResultsObject$portfolioFreq
  
  #extract date indices since they're used in both cases
  optimizationDateIndices <- do.call(rbind,lapply(optimizationResultsObject$portfolioStats
                                                  ,function(x){
                                                    #x$dateIndex
                                                    
                                                    if(datePeriodType == "train"){ #in sample
                                                      datePeriodTypeResult <-  x$pseudoISPeriodPlot
                                                    } else {
                                                      if(datePeriodType == "test"){ #out of sample
                                                        datePeriodTypeResult <- x$OOSPeriod
                                                        
                                                      } else {stop("invalid datePeriodType, must be one of \"train\" for in sample period and \"test\" for out of sample period")}
                                                    }
                                                    
                                                    
                                                    return(datePeriodTypeResult)
                                                  }
  )
  )
  
  if(!(statisticName[1] %in% c("expectedReturn", "standardDeviation"))){
    
    #if you can annualize it, e.g. can't annualize holdings or weights 
    if(annualize & statisticName[2] %in% c("expectedReturn" ,"standardDeviation", "noriskSharpe")){  
      
      
      statisticMatrix<-  do.call(rbind,lapply(optimizationResultsObject$portfolioStats
                                              ,function(x){
                                                
                                                if(statisticName[2] == "expectedReturn"){
                                                  statisticResult <- tsconv:::annualizeReturn(  rawReturn = x[[statisticName[1]]][[statisticName[2]]]
                                                                                       , freq = optimizationFreq
                                                                                       , geometric = TRUE
                                                  )
                                                } else {
                                                  statisticResult <- tsconv:::annualizeStandardDeviation(rawStandardDeviation = x[[statisticName[1]]][[statisticName[2]]]
                                                                                                , freq = optimizationFreq
                                                  )
                                                }
                                                return(statisticResult)
                                              }
      )
      )
    } else {
      
      statisticMatrix <- do.call(rbind,lapply(optimizationResultsObject$portfolioStats
                                              ,function(x){
                                                return(x[[statisticName[1]]][[statisticName[2]]])
                                              }
      )
      )
      
      
    }
    
    
    if(indexType == "timeSeries"){
      rawDateSequence <- data.frame(   tsconv:::splitTimeRange(timeRangeString = optimizationDateIndices, dateSelect = "first")
                                       , tsconv:::splitTimeRange(timeRangeString = optimizationDateIndices, dateSelect = "last")
                                       , stringsAsFactors = FALSE
      )
      rawDateSequence <- toRowList(matrixToConvert = rawDateSequence)
  ##############  
      DateSequence <- 
      switch(indexTimeSeriesFreq
      , daily = lapply(X = rawDateSequence
                       , FUN = function(x){
                         return(
                           tsconv:::createWeekdaySequence( start = x[1] #as.Date(x[1])
                                                  , end = x[2] #as.Date(x[2])
                           )
                         )
                       }
                      )
      , weekly = lapply(X = rawDateSequence
                        , FUN = function(x){
                          return(
                            tsconv:::createWeeklySequence( start = x[1] #as.Date(x[1])
                                                   , end = x[2] #as.Date(x[2])
                            )
                          )
                        }
                      )
      , monthly = lapply(X = rawDateSequence
                         , FUN = function(x){
                           return(
                             tsconv:::createMonthlySequence( start = x[1] #as.Date(x[1])
                                                   , end = x[2] #as.Date(x[2])
                             )
                           )
                         }
                    )
      , yearly = lapply(X = rawDateSequence
                        , FUN = function(x){
                          return(
                            tsconv:::createYearlySequence( start = x[1] #as.Date(x[1])
                                                   , end = x[2] #as.Date(x[2])
                            )
                          )
                        }
                      )
      , stop("Invalid indexTimeSeriesFreq, must be one of daily, weekly, monthly or yearly")
      )
        #weekdayDateSequence <-  
  ################
      statisticTimeSeriesResult <-
        do.call(rbind
                ,lapply(1:length(DateSequence)
                        , function(x){
                          
                          statisticsVector <- statisticMatrix[x,]
                          dateSequenceVector <- DateSequence[[x]]
                          do.call(rbind
                                  ,lapply(dateSequenceVector
                                          , function(y){
                                            #c(data.frame(Date = y, stringsAsFactors = FALSE), data.frame(statisticsVector))
                                            #xts(x = statisticsVector, order.by = y)
                                            #you can't create an xts vector with a single date, thus we need to loop through statistics and then merge them
                                            do.call(merge
                                                    ,lapply(statisticsVector
                                                            , function(z){
                                                              return(xts::xts(x = z, order.by = y))
                                                            }
                                                    )
                                            )
                                          }
                                  )
                          )
                        }
                )
        )
    } else {
      if(indexType == "raw"){
        
        statisticTimeSeriesResult <- data.frame( statisticMatrix
                                                 , stringsAsFactors = FALSE
        )
        rownames(statisticTimeSeriesResult) <- optimizationDateIndices
        
      } else {stop("invalid indexType, must be one of \"timeSeries\" for a daily xts object or \"raw\" to append the time range")}
    }
    
    
    #this names names the resulting timeseries/matrix
    if(ncol(statisticTimeSeriesResult)==length(optimizationResultsObject$portfolioNames)){
      names(statisticTimeSeriesResult) <- optimizationResultsObject$portfolioNames
    } else {
      if(ncol(statisticTimeSeriesResult)==1){
        names(statisticTimeSeriesResult) <- do.call(paste0, as.list(statisticName))
        #stringr::str_sub(string = statisticName, start = gregexpr("\\.",statisticName)[[1]][1] + 1)
      }else{names(statisticTimeSeriesResult) <- NULL}
    }
    
    return(statisticTimeSeriesResult)
    
    #if it's SD or ER, create list
  } else {
    if(length(statisticName)==1){ #if user requested a single vector (will either be SD or ER)
      
      if(annualize){
        
        statisticList<-  lapply(optimizationResultsObject$portfolioStats
                                ,function(x){
                                  if(statisticName == "expectedReturn"){            
                                    statisticResult <- tsconv:::annualizeReturn(rawReturn = x[[statisticName]]
                                                                       , freq = optimizationFreq
                                                                       , geometric = TRUE
                                    )
                                  } else {
                                    statisticResult <- tsconv:::annualizeStandardDeviation(rawStandardDeviation = x[[statisticName]]
                                                                                  , freq = optimizationFreq
                                    )
                                    
                                  }
                                  
                                  return(statisticResult)
                                }
        )
      } else {
        statisticList<-  lapply(optimizationResultsObject$portfolioStats
                                ,function(x){
                                  return(x[[statisticName]])
                                }
        )
        
        
      }
    } else { #if user wanted frontier, statisticName will have length 2
      
      
      statisticList<-  lapply(optimizationResultsObject$portfolioStats
                              ,function(x){
                                if(annualize){
                                  resultMatrix <-
                                    cbind( `Standard Deviation` = tsconv:::annualizeStandardDeviation(rawStandardDeviation = x[[statisticName[2]]]
                                                                                             , freq = optimizationFreq
                                    )
                                    ,`Expected Return ` = tsconv:::annualizeReturn(rawReturn = x[[statisticName[1]]]
                                                                          , freq = optimizationFreq
                                                                          , geometric = TRUE
                                    )
                                    )
                                } else {
                                  
                                  resultMatrix <-
                                    cbind( `Standard Deviation` = x[[statisticName[2]]]
                                           ,`Expected Return ` = x[[statisticName[1]]]
                                    )
                                  
                                }
                                return(resultMatrix)
                              }
      )
      
    }
    
    names(statisticList) <- optimizationDateIndices
    
    return(statisticList)
    
  }
  
}
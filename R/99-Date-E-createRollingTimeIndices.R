#' create index of time-stamps for vectorized functions
#'
#' @param initialTimeVector vector of dates YYYY-MM-DD
#' @param estimationLength a positive integer indicating the size of the estimation period
#' @param timeUnit a character string, one of "weeks", "days","weeks","months","years", indicating the unit size for estimationLength and investmentHorizon
#' @param investmentHorizonLength a positive integer indicating the size of the investment horizon (out of sample period)
#'
#' @return a character matrix of xts-subsettable date ranges, one column for the in-sample periods, and another for the out-of-sample periods
#'
#' @examples
#' FUNCTION STILL UNDER DEVELOPMENT
#'
#' @export
createRollingTimeIndices <- function(initialTimeVector
                                            , estimationLength = 18 #window size
                                            , timeUnit = "months" #c("days","weeks","months","years")
                                            , investmentHorizonLength = 1
                                            , windowType = "rolling" #"anchored"
                                            #, snap = FALSE #works only for months, weeks, years; snap to begin/end of period (e.g. Year is Jan 1/Dec 31, month is 1st to last, week is Mon/Fri etc and weeks, snap to
                                           
){

  #need to handle "snap"
  #need to handle anchored
  #need to do error checks
  
  #estimation length *must* be greater than horizon length
  if(estimationLength < investmentHorizonLength){stop("estimationgLength must be greater than or equal to investmentHorizonLength")}
  
  workingTimeVector <- initialTimeVector
  dateIndices <- NULL
  firstRun <- TRUE
  
  while(TRUE){
  
  ###beginningPeriodDate <- xts::first(workingTimeVector)
  ###estimationPeriod <- beginningPeriodDate
  if(windowType == "rolling"){
                              estimationPeriod <- xts::first(workingTimeVector)

                              estimationPeriod  <- paste0(estimationPeriod + ifelse(!firstRun & estimationLength == investmentHorizonLength,1,0)
                                                          ,"/"
                                                          ,addPeriodToDate(initialDate = estimationPeriod
                                                                           , period = timeUnit
                                                                           , numPeriods = estimationLength
                                                          )
                                                          )
                              
                              } else {
                                    if(windowType == "anchored"){
                                          estimationPeriod <- xts::first(initialTimeVector)
                                          
                                          estimationPeriod  <- paste0(estimationPeriod
                                                                      ,"/"
                                                                      ,addPeriodToDate(initialDate = xts::first(workingTimeVector)
                                                                                       , period = timeUnit
                                                                                       , numPeriods = estimationLength
                                                                      )
                                                                      )
                                          
                                          
                                          
                                          
                                        } else {
                                          stop("invalid windowType, must be \"rolling\" or \"anchored\"")
                                        }
                              }
  
                                      
  
  # ifelse(windowType == "rolling"
  #        , xts::first(workingTimeVector)
  #        , ifelse(windowType == "anchored"
  #                 , xts::first(initialTimeVector)
  #                 , stop("invalid windowType, must be \"rolling\" or \"anchored\"")
  #        )
  # )
  
  # estimationPeriod  <- paste0(estimationPeriod
  #                               ,"/"
  #                               ,addPeriodToDate(initialDate = estimationPeriod
  #                                               , period = timeUnit
  #                                               , numPeriods = estimationLength
  #                                                 )
  #                               )
  
  OOSPeriod <- addPeriodToDate(initialDate = splitTimeRange(timeRangeString = estimationPeriod
                                                            , dateSelect = "last"
                                                            )
                                  , period = timeUnit
                                  , numPeriods = investmentHorizonLength)
  
  #check if stop
  if(OOSPeriod > xts::last(initialTimeVector)){break}
  
  OOSPeriod <- paste0(splitTimeRange(timeRangeString = estimationPeriod
                                     , dateSelect = "last"
                                      ) + 1
                      ,"/"
                      ,OOSPeriod
                      )
  
  pseudoISPeriodDate <-  splitTimeRange(timeRangeString = estimationPeriod
                                      , dateSelect = "last"
                                        )
  pseudoISPeriodPlot <- paste0(addPeriodToDate(initialDate = pseudoISPeriodDate
                                               , period = timeUnit
                                               , numPeriods = -investmentHorizonLength
                                               ) + 1 #if you don't add 1, then there's overlap on the end date
                              ,"/"
                              ,pseudoISPeriodDate
                              )
  
  dateIndices <- rbind(dateIndices
                ,cbind(
                      estimationPeriod = estimationPeriod 
                    , OOSPeriod = OOSPeriod
                    , pseudoISPeriodPlot = pseudoISPeriodPlot
                    )
                )
  
  #remove investment horizon length from beginning
  newEndDate <- splitTimeRange(timeRangeString = OOSPeriod, dateSelect = "last")
  removalTimePeriod <- addPeriodToDate(initialDate = newEndDate ###beginningPeriodDate
                                       , period = timeUnit
                                       , numPeriods = -estimationLength ###investmentHorizonLength
                                      )
  workingTimeVector <- xtsFilter(  timeVector = workingTimeVector
                                 , dateRangeString = paste0(removalTimePeriod ####+ 1
                                                            ,"/"
                                                            )
                                 )
  
  firstRun <- FALSE
  }
  
  return(dateIndices)

}
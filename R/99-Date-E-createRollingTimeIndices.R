#' create index of time-stamps for vectorized functions
#'
#' @param initialTimeVector vector of dates YYYY-MM-DD
#' @param endTimeDate default to Sys.Date()
#' @param isExpanding logical, default FALSE
#'
#' @return vector of YYYY-MM dates
#'
#' @examples
#' none
#'
#' @export
createRollingTimeIndices <- function(initialTimeVector ,endTimeDate = Sys.Date() ,isExpanding = FALSE){
  
  ###Concatenate this to everything if expanding
  
  firstDay <- initialTimeVector[1]
  lastDay <-  initialTimeVector[length(initialTimeVector)]
  nextMonthDate <- nextMonth(lastDay)
  
  monthIDX <- lubridate::month(firstDay) #as.numeric(month(nextMonthDate))
  yearIDX <- lubridate::year(firstDay) #as.numeric(year(nextMonthDate))
  yearIDXend <- lubridate::year(endTimeDate)
  monthIDXend <- lubridate::month(endTimeDate)
  
  #first pass
  dateIndices <- paste0("/",yearIDX,"-",monthIDX:12)
  
  #subsequent passes
  dateIndices <- c(dateIndices,c(sapply( (yearIDX+1):(yearIDXend-1)
                                         ,function(yearIDXNum){
                                           paste0("/",yearIDXNum,"-",1:12)
                                         }
                                         
  )))
  
  #final pass
  dateIndices <- c(dateIndices, paste0("/",yearIDXend,"-",1:monthIDXend))
  
  if(isExpanding){
    
    #as.numeric(as.Date("2001-01-10")-as.Date("2001-01-01"))
    #dateDifference <- as.numeric(as.Date(lastDay)-as.Date(firstDay))
    #forwardAnchoredDate <- as.Date(lastDay) + dateDifference
    forwardAnchoredDate <- prevMonth(firstDay)
    endForwardAnchoredDate <- prevMonth(endTimeDate)
    
    ###fwd <- as.Date("2015-01-10") + as.numeric(as.Date("2015-01-10")-as.Date("2015-01-01"))
    ###as.Date(ifelse(isWeekend(fwd),ifelse(isWeekend(fwd-1),ifelse(isWeekend(fwd-2),"",fwd) ,fwd),fwd))
    ###rm(fwd)
    #     forwardAnchoredDate <-
    #       as.Date(
    #         ifelse(isWeekend(forwardAnchoredDate)
    #                ,ifelse(
    #                  isWeekend(forwardAnchoredDate+1)
    #                  ,ifelse(
    #                    isWeekend(forwardAnchoredDate+2)
    #                    ,""
    #                    ,forwardAnchoredDate) 
    #                  ,forwardAnchoredDate)
    #                ,forwardAnchoredDate)
    #       )
    #     
    #     endForwardAnchoredDate <- as.Date(lastDay) - dateDifference
    #     endForwardAnchoredDate <-
    #       as.Date(
    #         ifelse(isWeekend(endForwardAnchoredDate)
    #                ,ifelse(
    #                  isWeekend(endForwardAnchoredDate+1)
    #                  ,ifelse(
    #                    isWeekend(endForwardAnchoredDate+2)
    #                    ,""
    #                    ,endForwardAnchoredDate) 
    #                  ,endForwardAnchoredDate)
    #                ,endForwardAnchoredDate)
    #       )
    
    
    
    #if it's a rolling window
    #anchorDate <- paste0(year(forwardAnchoredDate),"-",month(forwardAnchoredDate))
    
    
    anchorIndices <- paste0(lubridate::year(forwardAnchoredDate),"-",lubridate::month(forwardAnchoredDate):12)
    #paste0(year(anchorDate),"-",month(anchorDate))
    #print(anchorIndices)
    anchorIndices <- c(anchorIndices,c(sapply( (   lubridate::year(forwardAnchoredDate)+1):(lubridate::year(endForwardAnchoredDate)-1)
                                               ,function(yearIDXNum){
                                                 paste0(yearIDXNum,"-",1:12)
                                               }
                                               
    )))
    
    anchorIndices <- c(anchorIndices, paste0(lubridate::year(endForwardAnchoredDate),"-",1:lubridate::month(endForwardAnchoredDate)))
    
    dateIndices <- paste0(anchorIndices,dateIndices)
    
  }
  
  return(dateIndices)
  #return(anchorIndices)
  
  
}
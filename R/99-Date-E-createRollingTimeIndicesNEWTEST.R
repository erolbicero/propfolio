#' create index of time-stamps for vectorized functions
#'
#' @param initialTimeVector vector of dates YYYY-MM-DD
#' @param numPeriods default to 52*5 weeks
#'
#' @return vector of YYYY-MM dates
#'
#' @examples
#' FUNCTION STILL UNDER DEVELOPMENT
#'
#' @export
createRollingTimeIndicesNEWTEST <- function(initialTimeVector
                                            #,endTimeDate = Sys.Date()
                                            ,numPeriods = (52*5)
                                            #,periodType = c("days","weeks","months","years")
){
  #This will be the ROLLING one, since you need to give it a size and a date
  
  #so add a startdate (default null) for expanding, you have what you need here
  #all you have to do is exclude dates before, or append the first date to it
  
  #then after that, you need to roll the first date, which will be exhilirating
  
  ###Concatenate this to everything if expanding
  
  firstDay <- initialTimeVector[1]
  lastDay <-  initialTimeVector[length(initialTimeVector)]
  nextMonthDate <- nextMonth(lastDay)
  
  monthIDX <- lubridate::month(firstDay) #as.numeric(month(nextMonthDate))
  yearIDX <- lubridate::year(firstDay) #as.numeric(year(nextMonthDate))
  #yearIDXend <- as.numeric(year(endTimeDate))
  #monthIDXend <- as.numeric(month(endTimeDate))
  
  yearIDXend <- lubridate::year(lastDay)
  monthIDXend <- lubridate::month(lastDay)
  
  #calculate num period deltas
  deltaYear <- numPeriods %/% 52 
  deltaMonth <- (numPeriods %% 52) %/% 4 
  deltaWeek <- (numPeriods %% 52) %% 4 
  
  #*********
  #So for any negative month-index, you need to subtract a year, and then
  #add 12 to any negative month-indices, upto and including 0 (this will be Dec)
  
  #first pass
  #store this first: (monthIDX:12) - deltaMonth
  monthIndices <- (monthIDX:12) - deltaMonth
  dateIndices <- paste0((yearIDX - deltaYear) - ifelse(monthIndices<=0,1,0),"-",((monthIDX:12) - deltaMonth) + ifelse(monthIndices<=0,12,0),"/",yearIDX,"-",monthIDX:12)
  
  
  #subsequent passes
  monthIndicesSub <- (1:12) - deltaMonth
  dateIndices <- c(dateIndices,c(sapply( (yearIDX+1):(yearIDXend-1)
                                         ,function(yearIDXNum){
                                           paste0((yearIDXNum - deltaYear)- ifelse(monthIndicesSub<=0,1,0),"-",((1:12) - deltaMonth) + ifelse(monthIndicesSub<=0,12,0),"/",yearIDXNum,"-",1:12)
                                         }
                                         
  )))
  
  ####index(XLF["2009-9/2014-9"])
  
  #final pass
  monthIndicesEnd <- (1:monthIDXend) - deltaMonth
  dateIndices <- c(dateIndices, paste0((yearIDXend - deltaYear) - ifelse(monthIndicesEnd<=0,1,0),"-",((1:monthIDXend) - deltaMonth) + ifelse(monthIndicesEnd<=0,12,0), "/",yearIDXend,"-",1:monthIDXend))
  
  
  #FILTER START DATES
  #This is done like this since vectorized functions are faster
  #and these would have to be compared in realtime
  #thus create all indices and exclude them afterward
  
  #stringr::str_sub(string = dateIndices, end = regexpr("/",dateIndices[1:2]))
  #regexpr("a","bbbacc")
  
  startDates <- lapply(dateIndices  
                       , function(x){stringr::str_sub(string = x, end = (regexpr("/",x)[1] -1) )}
  )
  
  #find start dates that occur after first date in sequence  
  dateIndicesKeep <- firstDay <= as.Date(timeDate::timeLastDayInMonth(paste0(startDates,"-1")))
  
  dateIndices <- dateIndices[dateIndicesKeep]
  
  return(dateIndices)
  #return(anchorIndices)
  
  
}
prevMonth <- function(dateObject){
  
  prevMonthNumber <-
    ifelse(as.character(lubridate::month(dateObject))=="01"
           ,"12"
           ,ifelse(
             lubridate::month(dateObject)-1 < 10
             ,paste0("0",as.character(lubridate::month(dateObject)-1))
             ,as.character(lubridate::month(dateObject)-1)
           )
    )
  
  prevAssociatedYear <- prevYear(dateObject, prevMonthNumber)
  
  endOfMonth <- timeLastDayInMonth(paste0(prevAssociatedYear,"-",prevMonthNumber,"-","01"))
  
  return(endOfMonth)
  
}
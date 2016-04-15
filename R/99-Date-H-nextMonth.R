nextMonth <- function(dateObject){
  
  #if it's 12th month, go to 01
  nextMonthNumber <-
    ifelse(as.character(lubridate::month(dateObject))=="12"
           ,"01"
           #if it's <10, make it "0" + 1, else take the number (10 or 11)
           ,ifelse(
             lubridate::month(dateObject)+1 < 10
             ,paste0("0",as.character(lubridate::month(dateObject)+1))
             ,as.character(lubridate::month(dateObject)+1)
           )
    )
  
  nextAssociatedYear <- nextYear(dateObject, nextMonthNumber)
  
  endOfMonth <- timeLastDayInMonth(paste0(nextAssociatedYear,"-",nextMonthNumber,"-","01"))
  
  return(endOfMonth)
  
}
nextYear <- function(dateObject, nextMonth){
  ifelse(nextMonth=="01"
         ,as.character(lubridate::year(dateObject) + 1)
         ,as.character(lubridate::year(dateObject))
  )
}
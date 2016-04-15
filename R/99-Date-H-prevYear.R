prevYear <- function(dateObject, prevMonth){
  ifelse(prevMonth=="12"
         ,as.character(lubridate::year(dateObject) - 1)
         ,as.character(lubridate::year(dateObject))
  )
}

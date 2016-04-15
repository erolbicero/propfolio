nextMonthNumber <- function(dateObject){
  ifelse(as.character(lubridate::month(dateObject))=="12"
         ,"01"
         ,ifelse(
           lubridate::month(dateObject)+1 < 10
           ,paste0("0",as.character(lubridate::month(dateObject)+1))
           ,as.character(lubridate::month(dateObject)+1)
         )
  )
}
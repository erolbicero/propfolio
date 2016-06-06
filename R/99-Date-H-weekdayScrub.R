weekdayScrub <- 
function(dateVector){
zoo::as.Date(
  as.vector(
    ifelse(timeDate::isWeekday(dateVector)
           , zoo::as.Date(dateVector)
           , zoo::as.Date(  
             as.vector(
               ifelse(timeDate::isWeekday(zoo::as.Date(dateVector) + 1)
                      , zoo::as.Date(dateVector) + 1
                      , zoo::as.Date(dateVector) + 2
               )
             )
           )
    )
  )
)
}
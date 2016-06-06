createWeeklySequence <- function(start, end){
  
  resultSequence <- timeDate::timeSequence(from = start, to = end, by = "week")
  resultSequence <- weekdayScrub(resultSequence)
  # resultSequence <- zoo::as.Date(
  #                     as.vector(
  #                         ifelse(timeDate::isWeekday(resultSequence)
  #                                , zoo::as.Date(resultSequence)
  #                                , zoo::as.Date(  
  #                                 as.vector(
  #                                         ifelse(timeDate::isWeekday(zoo::as.Date(resultSequence) + 1)
  #                                               , zoo::as.Date(resultSequence) + 1
  #                                               , zoo::as.Date(resultSequence) + 2
  #                                               )
  #                                         )
  #                                         )
  #                   )
  #                   )
  #                   )

  return(resultSequence)
  
}
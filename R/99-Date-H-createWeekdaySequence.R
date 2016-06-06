createWeekdaySequence <- function(start, end){
  
  resultSequence <- timeDate::timeSequence(from = start, to = end, by = "day")
  resultSequence <- zoo::as.Date(resultSequence[timeDate::isWeekday(resultSequence)])

  return(resultSequence)
  
}
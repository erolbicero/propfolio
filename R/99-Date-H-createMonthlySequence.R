createMonthlySequence <- function(start, end){
  
  resultSequence <- timeDate::timeSequence(from = start, to = end, by = "month")
  resultSequence <- weekdayScrub(resultSequence)
  return(resultSequence)
  
}
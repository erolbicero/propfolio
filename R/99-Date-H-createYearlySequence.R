createYearlySequence <- function(start, end){
  
  resultSequence <- timeDate::timeSequence(from = start, to = end, by = "year")
  resultSequence <- weekdayScrub(resultSequence)
  return(resultSequence)
  
}
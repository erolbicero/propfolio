splitTimeRange <- function(timeRangeString, dateSelect){
  
  splitLocation <- as.vector(gregexpr(pattern = "/", text = timeRangeString)[[1]])
  
  if(dateSelect == "first"){
  
    splitTime <- stringr::str_sub(string = timeRangeString, end = splitLocation + 1)
      
  } else{
    if(dateSelect == "last"){
      
      splitTime <- stringr::str_sub(string = timeRangeString, start = splitLocation + 1)
      
    } else { stop("invalid date selection type")}
  }
  
  splitTime <- zoo::as.Date(splitTime)
  
  return(splitTime)
  
}
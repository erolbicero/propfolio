# Hidden funciton
# adds n weeks to date

addWeeks <- function(initialDate, numWeeks = 1){
  
  initialDate <- zoo::as.Date(initialDate)
  # weekNum <- stringr::str_sub(string = initialDate, end = 4)
  # weekNumPlusNumWeeks <- as.numeric(weekNum) + numWeeks
  # 
  # newDate <- paste0(as.character(weekNumPlusNumWeeks),stringr::str_sub(string = initialDate, start = 5))
  newDate <- initialDate + 7 * numWeeks 
  
  return(newDate)
  
}
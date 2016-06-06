# Hidden funciton
# adds n days to date

addDays <- function(initialDate, numDays = 1){
  
  initialDate <- zoo::as.Date(initialDate)
  newDate <- initialDate + numDays 
  
  return(newDate)
  
}
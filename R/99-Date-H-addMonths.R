# Hidden funciton
# adds n months to date

addMonths <- function(initialDate, numMonths = 1){
  
  initialDate <- zoo::as.Date(initialDate)
  endFebFlag <- as.numeric(stringr::str_sub(string = initialDate, start = 6, end = 7)) == 2 &
    as.numeric(stringr::str_sub(string = initialDate, start = stringr::str_length(initialDate) - 1, end = stringr::str_length(initialDate))) == 29 & lubridate::leap_year(initialDate)

  timeLastDayEndFebFlag <- FALSE
  
  initialDate <- initialDate - endFebFlag*1
  
  monthNum <- stringr::str_sub(string = initialDate, start = 6, end = 7)
  monthNum <- gsub(pattern = "-", replacement = "", x = monthNum)
  monthNum <- as.numeric(monthNum)
  numYearsToAdd <- 0
  
  monthNumPlusNumMonths <- monthNum + numMonths
  
  if(monthNumPlusNumMonths < 0){
    isNegative <-TRUE
    #ifelse(monthNumPlusNumMonths < -12,monthNumPlusNumMonths <- 12 - (abs(monthNumPlusNumMonths) %% 12), monthNumPlusNumMonths <- 12 + monthNumPlusNumMonths)
    numYearsToAdd <- (abs(monthNumPlusNumMonths) %/% 12) * ifelse(isNegative,-1,1)
    monthNumPlusNumMonths <- 12 - (abs(monthNumPlusNumMonths) %% 12)
    monthNumPlusNumMonths <- ifelse(monthNumPlusNumMonths == 12, 0, monthNumPlusNumMonths)
  } else {
    isNegative <- FALSE
  }
  
  numYearsToAdd <- numYearsToAdd + (abs(monthNumPlusNumMonths) %/% 12) * ifelse(isNegative,-1,1)
  newMonth <- abs(monthNumPlusNumMonths) %% 12
  
  if(newMonth == 0) {
    numYearsToAdd <- numYearsToAdd - 1
    newMonth <- 12
  } 
  if(monthNumPlusNumMonths > 0 & monthNumPlusNumMonths < 12 & !isNegative){
    numYearsToAdd <- 0
    newMonth <- monthNumPlusNumMonths
  }
  if(abs(monthNumPlusNumMonths) > 0 & abs(monthNumPlusNumMonths) < 12 & isNegative){
    numYearsToAdd <- numYearsToAdd - 1
  }
  
  newDate <- paste0(  stringr::str_sub(string = initialDate, end = 5)
                    , paste0(ifelse(newMonth<10,"0",""),as.character(newMonth))
                    , stringr::str_sub(string = initialDate, start = 8)
                    )
  
  
  #check if invalid 31st
  newMonth <- stringr::str_sub(string = newDate, start = 6, end = 7)
  newMonth <- gsub(pattern = "-", replacement = "", x = newMonth)
  newMonth <- as.numeric(newMonth)
  newDay <- stringr::str_sub(string = newDate, start = stringr::str_length(newDate) - 1, end = stringr::str_length(newDate))
  newDay <- gsub(pattern = "-", replacement = "", x = newDay)
  newDay <- as.numeric(newDay)
  #correct invalid 31st dates, April, June, September and November (February is handeld elsewhere due to leap years)
  if(newMonth %in% c(4, 6, 9, 11) & newDay == 31){newDate <- paste0(stringr::str_sub(string = newDate, end = 8),"30")}
  
  if(newMonth == 2 & newDay == 29){
    testDate <- zoo::as.Date(paste0(stringr::str_sub(string = newDate, end = 4),"-02-01"))
    if(!lubridate::leap_year(testDate)){newDate <- paste0(stringr::str_sub(string = newDate, end = 8),"28") }
    #newDate <- paste0(stringr::str_sub(string = newDate, end = 8),"30")
    timeLastDayEndFebFlag <- TRUE
  }
  
  if(newMonth == 2 & newDay > 29){
    testDate <- zoo::as.Date(paste0(stringr::str_sub(string = newDate, end = 4),"-02-01"))
    if(!lubridate::leap_year(testDate)){newDate <- paste0(stringr::str_sub(string = newDate, end = 8),"28") }
    if(lubridate::leap_year(testDate)){newDate <- paste0(stringr::str_sub(string = newDate, end = 8),"29") }
    #newDate <- paste0(stringr::str_sub(string = newDate, end = 8),"30")
    timeLastDayEndFebFlag <- TRUE
  }
  
  newDate <- zoo::as.Date(newDate)
  
  if(abs(numYearsToAdd) > 0){newDate <- addYears(initialDate = newDate, numYears = numYearsToAdd)}
  
  
  if(#abs(numYearsToAdd) == 0 & 
     endFebFlag
     ){
  #restore leap year date for end of Feb
  endFebFlagNewDate <-
  as.numeric(stringr::str_sub(string = newDate, start = 6, end = 7)) == 2 &
  as.numeric(stringr::str_sub(string = newDate, start = 9, end = 10)) == 28 #this is b/c you subtracted the date at the top
  
  #add date back unless it's a non-leap year and end of Feb
  if(!(!lubridate::leap_year(newDate) & endFebFlagNewDate)){ newDate <- newDate + 1}
  
  #if Feb 29 was triggered and corrected, the year may change afterwards
  #so here we check if the year is a leap year and change it back to 29th day if it is
  }
  
  if(timeLastDayEndFebFlag){newDate <- zoo::as.Date(timeDate::timeLastDayInMonth(newDate))}
  
  return(newDate)
  
}

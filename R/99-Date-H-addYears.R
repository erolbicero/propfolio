# Hidden funciton
# adds n years to date

addYears <- function(initialDate, numYears = 1){
  
  initialDate <- zoo::as.Date(initialDate)
  
  endFebFlag <- as.numeric(stringr::str_sub(string = initialDate, start = 6, end = 7)) == 2 &
  as.numeric(stringr::str_sub(string = initialDate, start = 9, end = 10)) == 29 & lubridate::leap_year(initialDate)
  
  initialDate <- initialDate - endFebFlag*1
  
  # endFebFlag <- as.numeric(stringr::str_sub(string = initialDate, start = 6, end = 7)) == 2 &
  #   as.numeric(stringr::str_sub(string = initialDate, start = 9, end = 10)) == 29
  
  ##This is old method, slower
  yearNum <- stringr::str_sub(string = initialDate, end = 4)
  yearNumPlusNumYears <- as.numeric(yearNum) + numYears

  newDate <- paste0(as.character(yearNumPlusNumYears),stringr::str_sub(string = initialDate, start = 5))
  newDate <- zoo::as.Date(newDate)
  
  # newDate <- initialDate + 365 * numYears - endFebFlag*1
  # isLeapYear <- lubridate::leap_year(newDate) & as.numeric(stringr::str_sub(string = initialDate, start = 6, end = 7)) == 2 &
  #   as.numeric(stringr::str_sub(string = initialDate, start = 9, end = 10)) == 29
  # newDate <- newDate - isLeapYear*1

  if(endFebFlag){
        #restore leap year date for end of Feb
        endFebFlagNewDate <-
        as.numeric(stringr::str_sub(string = newDate, start = 6, end = 7)) == 2 &
        as.numeric(stringr::str_sub(string = newDate, start = 9, end = 10)) == 28 #this is b/c you subtracted the date at the top
  
        #add date back unless it's a non-leap year and end of Feb

              if(!(!lubridate::leap_year(newDate) & endFebFlagNewDate)){ newDate <- newDate + 1}
              }
    
  return(newDate)
}
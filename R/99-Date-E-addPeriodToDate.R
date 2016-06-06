#' function to add or subtract periods to dates
#'
#' @param initialDate valid date in the YYYY-MM-DD format
#' @param period a character string, one of "days", "weeks", "months" or "years"
#' @param numPeriods an integer (can be negative) specifying the number of periods to add (or subtract)
#'
#' @return a date; note: this will account for leap years, if Feb 29 is passed, it will default to Feb 28th for non-leap years
#'
#' @examples
#' none
#'
#' @export
addPeriodToDate <- function(initialDate, period, numPeriods){
  newDate <- switch(period
                      , years = addYears(initialDate = initialDate, numYears = numPeriods)
                      , months = addMonths(initialDate = initialDate, numMonths = numPeriods)
                      , weeks = addWeeks(initialDate = initialDate, numWeeks = numPeriods)
                      , days = addDays(initialDate = initialDate, numDays = numPeriods)
                      , stop("invalid period, must be one of \"years\", \"months\", \"weeks\", or \"days\"")
                    )
  
  return(newDate)
}
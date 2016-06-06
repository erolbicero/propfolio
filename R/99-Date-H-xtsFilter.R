xtsFilter <- function(timeVector, dateRangeString){
  
  dummyXTS <- xts::xts(x = base::rep(x = 0, times = length(timeVector))
                       , order.by = timeVector
                      )
  
  dummyXTS <- dummyXTS[dateRangeString]
  
  filteredDateRange <- zoo::index(dummyXTS)
  
  return(filteredDateRange)
  
}
annualizeStandardDeviation <- function(rawStandardDeviation, freq){
  #c("days","weeks","months","years")
  scalingFactor <- switch(freq
                          , days = 252
                          , weeks = 52
                          , months = 12
                          , years = 1
                          )
  
  annualStandardDeviation <- rawStandardDeviation*sqrt(scalingFactor)
  
  return(annualStandardDeviation)
}
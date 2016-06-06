annualizeReturn <- function(rawReturn, freq, geometric = TRUE){
  #c("days","weeks","months","years")
  scalingFactor <- switch(freq
                          , days = 252
                          , weeks = 52
                          , months = 12
                          , years = 1
                          )
  if(geometric){
  annualReturn <- (rawReturn + 1)^scalingFactor - 1
  } else {
    annualReturn <- (rawReturn*scalingFactor)
  }
  
  return(annualReturn)
}
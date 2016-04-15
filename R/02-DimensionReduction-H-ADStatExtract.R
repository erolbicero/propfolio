#*******HIDE THIS
ADStatExtract <- function(statList){
  extractedStatistic <- lapply(1:length(statList)
                               , function(listIndex){
                                 statistic <- statList[[listIndex]]$statistic["AD"]
                                 return(statistic)
                               }
  )
  
  extractedStatistic <- unlist(extractedStatistic)
  return(extractedStatistic)
}


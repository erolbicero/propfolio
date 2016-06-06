fixNaNsInSimulatedData <-
function(copulaVector, simulatedDataVector, numSimulations, percentDataToRemove = 0.05){
#if there are NaNs, remove the kink which approaches 0, then linearly interpolate to fill in the CDF
  #sort based on copula, join copula, skewvector, and then append the current order
  dataToFix <- cbind(copulaVector, simulatedDataVector, 1:length(simulatedDataVector))
  dataToFixNames <- c("Copula", "SimData", "OrigOrder")
  colnames(dataToFix) <- dataToFixNames
  
  #Sort by copula, now we get the cdf
  dataToFix <- dataToFix[order(dataToFix[ ,"Copula"]), ]
  
  #remove kink in data
  vectorToFix <- dataToFix[ ,"SimData"]
  firstNaN <- min(which(is.nan(vectorToFix)))
  lastNaN <- max(which(is.nan(vectorToFix)))
  sizeToRemove <- ceiling(numSimulations*percentDataToRemove)
  vectorToFix <- c( vectorToFix[1:(firstNaN - sizeToRemove)], rep(NaN, sizeToRemove), vectorToFix[(firstNaN + 1):length(vectorToFix)])
  vectorToFix <- zoo::na.approx(object = vectorToFix)
  
  dataToFix <- cbind(dataToFix[,"Copula"], vectorToFix, dataToFix[,"OrigOrder"])
  colnames(dataToFix) <- dataToFixNames
  
  #re-sort data
  dataToFix <- dataToFix[order(dataToFix[,"OrigOrder"]),]
  
  simulatedDataVector <- dataToFix[,"SimData"]
  
  return(simulatedDataVector)
}
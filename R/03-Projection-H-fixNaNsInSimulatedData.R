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
  
  #if there is sufficient data at tail end
  if(firstNaN > sizeToRemove*1.5){
  
  vectorToFix <- c( vectorToFix[1:(firstNaN - sizeToRemove)], rep(NaN, sizeToRemove), vectorToFix[(firstNaN + 1):length(vectorToFix)])
  vectorToFix <- zoo::na.approx(object = vectorToFix)

  } else { #otherwise need to crudely approximate tail end
    
    #minimize distance between approximated slope and end point
    
    vectorToFix <- dataToFix[,"SimData"]
    
    slopeFractionSequence <- seq(from = 0,to = 0.1, by = 1/numSimulations)
    
    slopeDistances <- 
    sapply(slopeFractionSequence
           , function(x){
             #x<-0
             
             slope <- vectorToFix[base::round(lastNaN+numSimulations*x + 1,0)] - vectorToFix[lastNaN + 1]
             vectorToFix[firstNaN:lastNaN] <- NaN
             for(xx in lastNaN:(ceiling(firstNaN+sizeToRemove/10))){vectorToFix[xx] <- vectorToFix[xx + 1] - slope}
             #plot(vectorToFix])
             
             #get distance
             NaNIDX <- which(is.nan(vectorToFix))
             
             distance <- vectorToFix[max(NaNIDX) + 1] - vectorToFix[min(NaNIDX) - 1]
             return(distance)
           }
           )
    
    slopeFraction <- slopeFractionSequence[max(which(slopeDistances > 0))]
    
    slope <- vectorToFix[base::round(lastNaN+numSimulations*slopeFraction + 1,0)] - vectorToFix[lastNaN + 1]
    vectorToFix[firstNaN:lastNaN] <- NaN
    for(xx in lastNaN:(ceiling(firstNaN+sizeToRemove/10))){vectorToFix[xx] <- vectorToFix[xx + 1] - slope}
    #plot(vectorToFix)
    vectorToFix <- zoo::na.spline(vectorToFix)

  }
  
  dataToFix <- cbind(dataToFix[,"Copula"], vectorToFix, dataToFix[,"OrigOrder"])
  colnames(dataToFix) <- dataToFixNames
  
  #re-sort data
  dataToFix <- dataToFix[order(dataToFix[,"OrigOrder"]),]
  
  simulatedDataVector <- dataToFix[,"SimData"]
  
  return(simulatedDataVector)
}
#*******HIDE THIS
evaluateStudentTDistributionsEigen<-function(PCAFactorTimeSeriesMatrix, listOfParameters, eigenValues){
  
  studentTFitList <- lapply(1:ncol(PCAFactorTimeSeriesMatrix),function(i){
    
    evaluationResult<-ADGofTest::ad.test(
      x = coredata(PCAFactorTimeSeriesMatrix[,i]) * sqrt(eigenValues[i])
      , distr.fun = pstd
      , mean = 0
      , sd = 1
      , nu = listOfParameters[[i]]$par["nu"]
    )
    return(evaluationResult)
  }
  )
  
  return(studentTFitList)    
}

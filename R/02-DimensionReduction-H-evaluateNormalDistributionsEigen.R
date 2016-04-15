evaluateNormalDistributionsEigen<-function(PCAFactorTimeSeriesMatrix, eigenValues){
  
  normalFitList<-lapply(1:ncol(PCAFactorTimeSeriesMatrix)
                        ,function(i){
                          
                          evaluationResult<-ADGofTest::ad.test(
                            x = coredata(PCAFactorTimeSeriesMatrix[,i])*sqrt(eigenValues[i])
                            , distr.fun = pnorm
                            , mean = 0
                            , sd = 1
                          )
                          return(evaluationResult)
                          
                        }
  )
  return(normalFitList)
  
}

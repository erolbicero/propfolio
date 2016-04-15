#Now calculate dominant-residual model by obtaining PCA Factors
#*******HIDE THIS
calculatePCAFactors<-function(timeSeriesMatrix,dominantEigenVectors){
  
  tsEigenProduct(timeSeriesMatrix,dominantEigenVectors)
  
}

#' Identify dominant eigenvalues
#'
#' @param eigenResult which is a list of eigenvalues and eigenvectors
#' @param fitThreshold for Marcenko Pasteur distribution which decides if RMT will be used or a simple "minimim distance" method (Scree-plot approach)
#'
#' @return a list object with eigenvalues and eigenvectors
#'
#' @examples
#' none
#'
#' @export
dominantEigenvectors <- function(eigenResult, fitThreshold = 0.3){
  
  #First check if you can use RMT by fitting a Marcenko-Pastur Distribution
  eigenMPDistFit <- fitdistrplus::fitdist(
    data = eigenResult$eigenValues
    , distr = "mp" 
    , method = "mle"
    #Start where Q = 1
    , start = list(ndf = length(eigenResult$eigenValues), pdim = length(eigenResult$eigenValues)) 
  )
  
  #Check fit, if p >> 0.05, then good fit, use it, if not, use ad-hoc method
  eigenMPDistFitADTest <- ADGofTest::ad.test(
    x = eigenResult$eigenValues
    , distr.fun = pmp
    , ndf = eigenMPDistFit$estimate["ndf"]
    , pdim = eigenMPDistFit$estimate["pdim"]
  )
  
  #check if fit is good
  fitResult <- (eigenMPDistFitADTest$p.value >= fitThreshold)
  
  if (fitResult)  {
    print("Random Matrix Theory")
    print(paste("p-value:", eigenMPDistFitADTest$p.value))
    
    #will be used if fit is good
    Q <- eigenMPDistFit$estimate["ndf"]/eigenMPDistFit$estimate["pdim"]
    
    #calculate Eigenvalue cutoff
    #YOU FIT IT TO STD=1, so use standardized lambda high
    lambdaHigh <- var(eigenResult$eigenValues) * (1 + sqrt(1/Q))^2
    lambdaHighStandard <- (1 + sqrt(1/Q))^2
    print(paste("Lambda High:", lambdaHigh))
    print(paste("Lambda High Standard:", lambdaHighStandard))
    
    #extract relevant eigenvectors
    signalEigenValues <- eigenResult$eigenValues[eigenResult$eigenValues > lambdaHighStandard]
    noiseEigenValues <- eigenResult$eigenValues[eigenResult$eigenValues <= lambdaHighStandard]
    
    signalEigenVectors <- eigenResult$eigenVectors[,eigenResult$eigenValues > lambdaHighStandard]
    noiseEigenVectors  <- eigenResult$eigenVectors[,eigenResult$eigenValues <= lambdaHighStandard]
    
    
  }
  else  {
    #Use minimum distance method
    print("Minimum Distance")    
    
    #normalize eigenvalues, by taking ratio of N+1 to N
    #normEigenValues <- cbind(eigenValues[-length(eigenValues)], eigenValues[-1])
    #normEigenValues <- normEigenValues[,1]/normEigenValues[,2]
    
    #scale to x-axis
    
    normEigenValues <- (eigenResult$eigenValues/sum(eigenResult$eigenValues))*10
    ###Former minimum distance
    distance <- sqrt(normEigenValues^2 + (1:length(normEigenValues))^2)
    indexCutoff <- which(distance<=min(distance)) + 1 #take another eigenvalue as default
    
    #Arbitrary % data explained    
    ###normEigenValues <- cumsum(eigenResult$eigenValues/sum(eigenResult$eigenValues))
    ###indexCutoff <- max(which(normEigenValues<=0.9))
    
    
    signalEigenValues <- eigenResult$eigenValues[1:indexCutoff]
    noiseEigenValues <- eigenResult$eigenValues[(indexCutoff+1):length(eigenResult$eigenValues)]
    
    signalEigenVectors <- eigenResult$eigenVectors[,1:indexCutoff] 
    noiseEigenVectors  <- eigenResult$eigenVectors[,(indexCutoff+1):length(eigenResult$eigenValues)]
  }
  
  return( list( signal = list(eigenValues = signalEigenValues
                              ,eigenVectors = signalEigenVectors)
                , noise = list(eigenValues = noiseEigenValues
                               ,eigenVectors = noiseEigenVectors)
  )
  )
  
}

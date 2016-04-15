#' Fit a Vine Copula
#'
#' @param dataObject
#'
#' @return vineCopulaObject description TBD
#'
#' @examples
#' none
#' @export
fitCopula <- function(dataObject){
  
  #If it's only one column, there's no copula
  if(is.vector(dataObject))
  {
    print("No copula possible with vector")
    
    copulaModel <- NULL
    
  } else 
  {   #If it's bivariate  
    if (ncol(dataObject) == 2)
    {
      #fit to [0,1]
      dataObject <- VineCopula::pobs(dataObject)
      
      copulaModel <- VineCopula::BiCopSelect(
        u1 = dataObject[,1]
        , u2 = dataObject[,2]
        
      )
      
      biCopFit <- VineCopula::BiCopGofTest(u1 = dataObject[,1]
                                           , u2 = dataObject[,2]
                                           , family = copulaModel$family
                                           , par = copulaModel$par
                                           , par2 = copulaModel$par2
                                           , method = ifelse(copulaModel$family == 2, "white", "kendall")
      )
      print(biCopFit)
      
    } else
    { #If it's multivariate
      if (ncol(dataObject) > 2)
      {
        #fit to [0,1]
        dataObject <- VineCopula::pobs(dataObject)
        
        copulaModel <- list()
        
        #fit a R-vine copula
        copulaModel[[1]] <- VineCopula::RVineStructureSelect(
          data = dataObject
          , type = 0 #R-vine
          , familyset = c(
            1  # = Gaussian copula 
            , 2  # = Student t copula (t-copula)
            , 3  # = Clayton copula 
            , 4  # = Gumbel copula 
            , 5  # = Frank copula 
            , 6  # = Joe copula 
            , 13 # = rotated Clayton copula (180 degrees; “survival Clayton”) 
            , 14 # = rotated Gumbel copula (180 degrees; “survival Gumbel”) 
            , 16 # = rotated Joe copula (180 degrees; “survival Joe”) 
            , 23 # = rotated Clayton copula (90 degrees) 
            , 24 # = rotated Gumbel copula (90 degrees) 
            , 26 # = rotated Joe copula (90 degrees) 
            , 33 # = rotated Clayton copula (270 degrees) 
            , 34 # = rotated Gumbel copula (270 degrees) 
            , 36 # = rotated Joe copula (270 degrees) 
          )
          
        )
        
        #fit a C-vine copula
        copulaModel[[2]] <- VineCopula::RVineStructureSelect(
          data = dataObject
          , type = 1 #C-vine
          , familyset = c(
            1  # = Gaussian copula 
            , 2  # = Student t copula (t-copula)
            , 3  # = Clayton copula 
            , 4  # = Gumbel copula 
            , 5  # = Frank copula 
            , 6  # = Joe copula 
            , 13 # = rotated Clayton copula (180 degrees; “survival Clayton”) 
            , 14 # = rotated Gumbel copula (180 degrees; “survival Gumbel”) 
            , 16 # = rotated Joe copula (180 degrees; “survival Joe”) 
            , 23 # = rotated Clayton copula (90 degrees) 
            , 24 # = rotated Gumbel copula (90 degrees) 
            , 26 # = rotated Joe copula (90 degrees) 
            , 33 # = rotated Clayton copula (270 degrees) 
            , 34 # = rotated Gumbel copula (270 degrees) 
            , 36 # = rotated Joe copula (270 degrees) 
          )
          
        )
        
        #compare and take best fitting one
        KS <- list()
        
        KS[[1]]<-
          VineCopula::RVineGofTest(data = dataObject
                                   , RVM = copulaModel[[1]] #R-Vine
                                   , method = "ECP2"
                                   , statistic = "KS"
                                   
          )
        
        KS[[2]]<-
          VineCopula::RVineGofTest(data = dataObject
                                   , RVM = copulaModel[[2]] #C-Vine
                                   , method = "ECP2"
                                   , statistic = "KS"
          )
        
        KSstat <- c(KS[[1]]$KS,KS[[2]]$KS)
        
        copulaType <- min(which(KSstat==min(KSstat))) 
        #outer min in case there's a tie
        #this defaults to R-Vine fit
        
        
        copulaModel <- copulaModel[[copulaType]]
        #compute fit
        #         vineFit <- VineCopula::RVineGofTest(data = dataObject
        #                                             , RVM = copulaModel[[copulaType]]
        #                                             , method = "ECP2"
        #                                             , statistic = "KS"
        #         )
        #print(paste("Copula Type",vineFit))
        ifelse(copulaType==1,print("R-vine"),print("C-vine"))
        print(KS[copulaType])
        
      } else { print("No data passed")
        copulaModel <- NULL
      } 
    }
  }
  
  return(copulaModel)
  
}

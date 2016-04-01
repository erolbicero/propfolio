
##DEFINE inputs
#Import equity function
##GENERALIZE THIS TO DAILY/WEEKLY/MONTHLY/YEARLY
  #Use a switch statement
importEquityPricesToXtsMatrix <- function(symbolVector, startDate = "2000-01-01", endDate = Sys.Date(), priceType = "Adjusted" ){
  
  quantmod::getSymbols(symbolVector
                       , src = "yahoo"
                       , verbose = TRUE
                       , warnings = TRUE
                       , auto.assign = TRUE
                       , return.class = "xts"
                       , index.class = "Date"
                       , from = startDate
                       , to = endDate
                       )
  
  xtsPriceMatrix <- do.call(merge
                            ,lapply(symbolVector
                                    , function(symbolPrices){
                                      
                                      symbolPriceXTSVector <- get(symbolPrices)[,names(get(symbolPrices))==paste0(symbolPrices,".",priceType)]
                                      
                                      return(symbolPriceXTSVector)
                                      
                                      }
                                    )
                            )
  
  names(xtsPriceMatrix) <- gsub(pattern = paste0(".",priceType)
                                , replacement = ""
                                , x = names(xtsPriceMatrix)
                                )
  
  return(xtsPriceMatrix)
  
}


#Import equity function
importEquityPricesToXtsMatrixWeekly <- function(symbolVector, startDate = "2000-01-01", endDate = Sys.Date(), priceType = "Adjusted" ){
  
  quantmod::getSymbols(symbolVector
                       , src = "yahoo"
                       , verbose = TRUE
                       , warnings = TRUE
                       , auto.assign = TRUE
                       , return.class = "xts"
                       , index.class = "Date"
                       , from = startDate
                       , to = endDate
  )
  
  weeklyPrices <- lapply( X = symbolVector
                                ,function(x){
                                  return(to.weekly(get(x)))
                                }
                        )  
  

  xtsPriceMatrix <- do.call(merge
                            ,lapply(weeklyPrices
                                    , function(symbolPrices){
                                      
                                      symbolPriceXTSVector <- symbolPrices[ , grepl(pattern = priceType, x = names(symbolPrices))]
                                      
                                      return(symbolPriceXTSVector)
                                      
                                    }
                            )
  )
  
  names(xtsPriceMatrix) <- symbolVector
  
  return(xtsPriceMatrix)
  
}

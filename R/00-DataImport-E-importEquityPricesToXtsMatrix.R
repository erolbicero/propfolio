
##DEFINE inputs

#Import equity function
#add verification/error checking for priceType and freq

#' Import symbols from Yahoo! Finance using getSymbols directly into a price matrix as xts object
#'
#' @param symbolVector is a vector of symbols to download
#' @param startDate staring date of the matrix
#' @param endDate ending date of the matrix
#' @param priceType one of Open/High/Low/Close/Adjusted
#' @param freq one of daily/weekly/monthly/quarterly or yearly
#' @param keep logical, whether to keep the original OHLC xts matrices or to drop them
#'
#' @return an xts object with number of columns equal to length of symbolVector
#'
#' @examples
#' none
#'
#' @export

#' @import RMTstat
#' @import fGarch
#' @importFrom zoo index

# quantmod::getSymbols
# xts::to.daily
# xts::to.weekly
# xts::to.monthly
# xts::to.quarterly
# xts::to.yearly

importEquityPricesToXtsMatrix <- function(symbolVector, startDate = "2000-01-01", endDate = Sys.Date(), priceType = "Adjusted", freq = "daily", keep = TRUE ){
  
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
  
  xtsPriceList <-
  switch(freq
          , daily = {
            
            lapply( X = symbolVector
                    ,function(x){
                      return(xts::to.daily(get(x)))
                    }
            )
                    }
          , weekly =  {
            
            lapply( X = symbolVector
                    ,function(x){
                      return(xts::to.weekly(get(x)))
                    }
            )
          }
          , monthly =  {
            
            lapply( X = symbolVector
                    ,function(x){
                      return(xts::to.monthly(get(x)))
                    }
            )
          }
          , quarterly =  {
            
            lapply( X = symbolVector
                    ,function(x){
                      return(xts::to.quarterly(get(x)))
                    }
            )
                      }
          , yearly =  {
            
            lapply( X = symbolVector
                    ,function(x){
                      return(xts::to.yearly(get(x)))
                    }
            )
          }
          , { #default
            
            lapply( X = symbolVector
                    ,function(x){
                      return(xts::to.daily(get(x)))
                    }
            )
          }
    
  )
   
  
  xtsPriceMatrix <- do.call(merge
                            ,lapply(xtsPriceList
                                    , function(symbolPrices){
                                      
                                      symbolPriceXTSVector <- symbolPrices[ , grepl(pattern = priceType, x = names(symbolPrices))]
                                      
                                      return(symbolPriceXTSVector)
                                      
                                    }
                            )
  )
  #cleanup
  #rm(xtsPriceList)
  
  names(xtsPriceMatrix) <- symbolVector
  
  # if(!keep){
  #   for(i in symbolVector){
  #             #rm(get(eval(parse(text=i))), envir = .GlobalEnv)
  #             rm(list = i, envir = .GlobalEnv)
  #         }
  # }else{
  #   
  #   for(i in symbolVector){
  #     #Create in global environment
  #     symbolExpression <- paste(i,"<<-",i)
  #     eval(parse(text = symbolExpression))
  #   }
  #   
  #   }
  
  
  if(keep){

    for(i in symbolVector){
      #Create in global environment
      symbolExpression <- paste(i,"<<-",i)
      eval(parse(text = symbolExpression))
    }

    }
  
  return(xtsPriceMatrix)
  
}

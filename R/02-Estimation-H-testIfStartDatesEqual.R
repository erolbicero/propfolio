#THIS WILL GET HIDDEN
#*******HIDE THIS
testIfStartDatesEqual<-function(timeSeriesMatrix)
{
  #determine all the start dates for each vector in the matrix
  #if they're all the same, then we can use ROBUST
  #if not, then we use BMONOMVN
  
  #if this gets more compicated you might need to go beyond TRUE/FALSE
  
  identifyStartDates<- function(timeSeriesMatrix){
    as.Date(
      apply(X=timeSeriesMatrix
            ,MARGIN=2
            ,FUN=function(timeSeriesVector){
              
              #ensure that vector is an xts object
              #exclude all NAs prior to extracting dates
              filterVector<-timeSeriesVector[!is.na(timeSeriesVector)]
              xtsVector<-xts(x=filterVector
                             , order.by=as.Date(names(filterVector))
              )
              
              return(
                #identify earliest date
                min(
                  #Ensure they're read as dates
                  as.Date(
                    #Extract the dates
                    index(
                      #All filled in dates
                      xtsVector
                    )
                  )
                )
              )
            }
      )
    )
  }
  
  startDateVector<-identifyStartDates(timeSeriesMatrix)
  
  #check if all values are equal to first value
  #returns vector of 1s and 0s
  isEqual<-sapply(X=startDateVector[-1]
                  ,FUN=function(dateValue){
                    ifelse(test= dateValue == startDateVector[1], yes=1, no=0)  
                  }
                  
  )
  
  #take product of isEqual, if all 1s it'll return 1, if one 0 then 0
  isEqual<-prod(isEqual)
  
  isEqual<-ifelse(test=isEqual==1,yes=TRUE,no=FALSE)
  
  return(isEqual)
}

library(dplyr)
# LLD 
msRecallPbocLLD <- function(dfSTPbocLLD, dfRecallPbocPDD, monthOffSet, ...){
  if(NROW(dfSTPbocLLD) == 0){
    return(dfSTPbocLLD)
  } else {
    dfSTPbocLLD <- left_join(dfSTPbocLLD, dfRecallPbocPDD, by = c('ID_LTRA' = 'ID_PTRA', 'CATG' = 'CATG'), copy = FALSE)
    dfSTPbocLLD2 <- within(dfSTPbocLLD, {
      if(monthOffSet < 24){
       remained24MStr <- msRecallPboc24MRemaindStrFunc(RCDL_RP24M, monthOffSet)
      } else {
        remained24MStr <- NA
      }
      
      TIMEL_LSTINQ <- ifelse(is.na(TIMEL_LSTINQ) & !is.na(TIMEL_SETTLE), TIMEL_SETTLE, TIMEL_LSTINQ)
      
      TIMEL_SRPRCD24M1 <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -(24 + monthOffSet - 1))
      TIMEL_ERPRCD24M1 <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -monthOffSet)
      
      mapStratTime <- TIMEL_SRPRCD24M 
      mapEndTime <- TIMEL_LSTINQ - months(24)
      day(mapEndTime) <- 30
      
      # map function
      middleStr <- msMap24MStrFunc(mapStratTime, mapEndTime, TIME_NUM_DQ)
      
      
      frontTime <- floor(difftime(TIMEL_LSTINQ, TIMEL_ISS, units = 'days')/30)
      frontCompTime <- 24 + monthOffSet
      is.na(frontTime) <- 9999
      
      
      if(frontTime == frontCompTime){
        frontStr <- '*'
      } else if(frontTime < frontCompTime){
        frontStr <- rep('/', frontCompTime - frontTime)
      } else if(frontTime > frontCompTime ){
        frontStr <- NA
      }
      
      all24MStr = paste0(na.omit(c(frontStr, middleStr, remained24MStr)), collapse = '')
      
    })
    
    return(dfSTPbocLLD2)
  }
}

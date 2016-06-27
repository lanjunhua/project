
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
      
       allLiveTime <- as.numeric(floor(difftime(as.Date(TIMEL_LSTINQ), as.Date(TIMEL_ISS), units = 'days')/30))
       allLiveTime[is.na(allLiveTime)] <- 9999
       frontCompTime <- 24 + monthOffSet
  
       #dateDiff <- msSysVarsPbocTimeDiffV(TIMEL_SRPRCD24M1, TIMEL_ISS)
       frontStr <- msRecallPbocFrontStrFunc(TIMEL_ISS, TIMEL_SRPRCD24M1, monthOffSet)
  
       mapStratTime <- ifelse(allLiveTime > frontCompTime, TIMEL_SRPRCD24M1, ifelse(allLiveTime == frontCompTime, TIMEL_SRPRCD24M1 + months(1),
                                                                               TIMEL_SRPRCD24M1 + months(frontCompTime - allLiveTime)))
       mapEndTime <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -24)
       mapLength <- msSysVarsPbocTimeDiffV(mapEndTime, mapStratTime)
       middleStr <- msRecallPbocMap24MStrFunc(mapStratTime, mapLength, TIME_NUM_DQ)
      
      all24MStr = paste0(na.omit(c(frontStr, middleStr, remained24MStr)), collapse = '')
      
    })
    
    return(dfSTPbocLLD2)
  }
}


library(dplyr)
# LLD 
msRecallPbocLLD <- function(dfSTPbocLLD, dfRecallPbocPDD, monthOffSet, ...){
  if(NROW(dfSTPbocLLD) == 0){
    return(dfSTPbocLLD)
  } else {
    dfRecallPbocLLD <- left_join(dfSTPbocLLD, dfRecallPbocPDD, by = c('ID_LTRA' = 'ID_PTRA', 'CATG' = 'CATG'), copy = FALSE)
    dfRecallPbocLLD2 <- within(dfRecallPbocLLD, {

      remained24MStr <- msRecallPboc24MRemaindStrFunc(RCDL_RP24M, monthOffSet)
      
      TIMEL_LSTINQ <- ifelse(is.na(TIMEL_LSTINQ) & !is.na(TIMEL_SETTLE), TIMEL_SETTLE, TIMEL_LSTINQ)
      TIMEL_LSTINQ <- ifelse(is.na(TIMEL_LSTINQ) & STUL_ACCT == '呆账' & !is.na(TIMEL_LRP), TIMEL_LRP, TIMEL_LSTINQ)
      
      TIMEL_SRPRCD24M1 <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -(24 + monthOffSet - 1))
      day(TIMEL_SRPRCD24M1) <- 1 
      TIMEL_ERPRCD24M1 <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -monthOffSet)
      
      allLiveTime <- msSysVarsPbocTimeDiffV(TIMEL_LSTINQ, TIMEL_ISS)
      allLiveTime[is.na(allLiveTime)] <- 9999
      frontCompTime <- 24 + monthOffSet
      
      dateDiff <- msSysVarsPbocTimeDiffV(TIMEL_SRPRCD24M1, TIMEL_ISS)
      
      frontStr <- msRecallPbocFrontStrFunc(TIMEL_ISS, TIMEL_SRPRCD24M1, monthOffSet)
      
      mapStratTime <- as.Date(as.POSIXct.Date(ifelse(allLiveTime > frontCompTime, TIMEL_SRPRCD24M1, ifelse(allLiveTime == frontCompTime, TIMEL_SRPRCD24M1 + months(1),
                                                                                   TIMEL_SRPRCD24M1 + months(frontCompTime - allLiveTime)))))
      
      
      mapEndTime <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -24)
      mapLength <- msSysVarsPbocTimeDiffV(mapEndTime, mapStratTime) + 1
      middleStr <- msRecallPbocMap24MStrFunc(mapStratTime = mapStratTime, mapLength = mapLength, TIME_NUM_DQ = TIME_NUM_DQ)
      
      
      all24MStr = paste0(na.omit(c(frontStr, middleStr, remained24MStr)), collapse = '')
      
    })
    
    return(dfRecallPbocLLD2)
  }
}

# all function

# 24M calculate
library(lubridate)
library(data.table)
library(dplyr)
require(RMySQL)
monthOffSet <- 12

# get the df just contain the 'ID_UNQF' and 'TIME_REP'
msSysVarsPbocTempRepTime <- function(dfSTPbocPBI){
  dfSysVarsPbocRepTime <- dfSTPbocPBI[, c('ID_UNQF', 'TIME_RECT')]
  names(dfSysVarsPbocRepTime) <- c('ID_UNQF', 'TIME_REP')
  dfSysVarsPbocRepTime$TIME_REP <- as.Date(dfSysVarsPbocRepTime$TIME_REP)
  return(dfSysVarsPbocRepTime)
}
dfRecallPbocPBI <- msSysVarsPbocTempRepTime(dfSTPbocPBI)


# PDD table aggregate
msRecallPbocPDD <- function(dfSTPbocPDD, ...){
  dfSTPbocPDD <- within(dfSTPbocPDD, {
    TIME_NUM_DQ <- paste(TIME_DQ, NUM_DQM, sep = ':')
  })
  dfRecallPbocDQStr <- aggregate(TIME_NUM_DQ ~ ID_PTRA + CATG, data = dfSTPbocPDD, function(x){paste(x, collapse = ',')})
  return(dfRecallPbocDQStr)
}
dfRecallPbocPDD <- msRecallPbocPDD(dfSTPbocPDD)


# function to get the 24M remainded str
msRecallPboc24MRemaindStrFunc <- Vectorize(function(RCDL_RP24M, monthOffSet){
  if(is.na(RCDL_RP24M)) return(NA)
  if(monthOffSet < 24){
    remaind24MStr <- paste0(unlist(strsplit(RCDL_RP24M, ""))[1:(24 - monthOffSet)], collapse = '')
  } else {
    remaind24MStr <- NA
  }
  return(remaind24MStr)
})

#tempTest1 <- c('//////////////////////*N', '********N***************')
#msRecallPboc24MRemaindStrFunc(tempTest1, c(12, 25))

# just for front part
msRecallPbocFrontStrFunc <- Vectorize(function(TIMEL_ISS, TIMEL_SRPRCD24M1, monthOffSet){
  dateDiff <- msSysVarsPbocTimeDiffV(TIMEL_SRPRCD24M1, TIMEL_ISS)
  if(dateDiff > 0){
    frontStr <- NA
  } else if(dateDiff == 0){
    frontStr <- '*'
  } else if(dateDiff < 0 & dateDiff > -24){
    frontStr <- paste(rep('/', abs(dateDiff)), collapse = '')
  } else if(dateDiff <= -24){
    frontStr <- NA
  }
  return(frontStr)
})

#tempTIME_ISS <- c('2006-12-26', '2013-04-16', '2008-04-03', '2015-03-04', '2016-01-11')
#tempSRPR24   <- c('2013-04-01', '2013-04-01', '2007-07-01', '2013-03-01', '2013-04-01')
#monthOffSet <- 12
#msRecallPbocFrontStrFunc(tempTIME_ISS, tempSRPR24, monthOffSet)


# the function for month calculate
msSysVarsPbocMonthCal <- function(timeInput, timeRange){
  timeInput <- as.POSIXlt(as.Date(timeInput))
  timeInput$mon <- timeInput$mon + timeRange
  timeOutput <- as.POSIXlt(as.Date(timeInput))
  timeOutput$mday <- ifelse(timeOutput$mday < timeInput$mday, 0, timeOutput$mday)
  return(as.Date(timeOutput))
}

#timeInput <- c('2010-08-31', '2016-02-29')
#msSysVarsPbocMonthCal(timeInput, -35)


# the function to calculate two diffrent date 
msSysVarsPbocTimeDiffV <- Vectorize(function(TimeEnd, TimeStart){
  TimeEnd   <- as.character(TimeEnd)
  TimeStart <- as.character(TimeStart)
  tempTimeEndY   <- as.numeric(t(as.data.frame(strsplit(TimeEnd, "[-|.|/]")))[, 1])
  tempTimeEndM   <- ifelse(is.na(TimeEnd), NA, as.numeric(t(as.data.frame(strsplit(TimeEnd, "[-|.|/]")))[, 2]))
  tempTimeStartY <- as.numeric(t(as.data.frame(strsplit(TimeStart, "[-|.|/]")))[, 1])
  tempTimeStartM <- ifelse(is.na(TimeStart), NA, as.numeric(t(as.data.frame(strsplit(TimeStart, "[-|.|/]")))[, 2]))
  TimeDiff <- (tempTimeEndY - tempTimeStartY) * 12 + (tempTimeEndM - tempTimeStartM)
  return(TimeDiff)
})


# just the used for map function
msRecallPbocMap24MStrFunc <- Vectorize(function(mapStratTime, mapLength, mapEndTime = NULL, TIME_NUM_DQ,...){
  if(mapLength <= 0) return(NA)
  
  # get time series
  timeSeries <- seq.Date(as.Date(mapStratTime), by = 'month', length.out = mapLength)
  nObs <- length(timeSeries)
  timeSeriesNames <- sapply(timeSeries, function(x){paste(substr(x, 1,4), substr(x, 6,7), sep = '.')})
  timeSeriesValue <- rep('N', nObs)
  
  
  if(is.na(TIME_NUM_DQ)){
    return(paste(timeSeriesValue, collapse = ''))
  } else {
    strTIME_NUM_DQ <- unlist(strsplit(TIME_NUM_DQ, ","))
    strTIME_NUM_DQ <- sort(strTIME_NUM_DQ)
    strTIME_DQ <- substr(strTIME_NUM_DQ, 1, 7)
    strNUM_DQ  <- substr(strTIME_NUM_DQ, 9, 9)
    
    intersectTime <- sort(intersect(timeSeriesNames, strTIME_DQ))
    indexPdd <- strTIME_DQ %in% intersectTime
    
    strNUM_DQ_N <- strNUM_DQ[indexPdd]
    
    indexMap <- timeSeriesNames %in% intersectTime
    timeSeriesValue[indexMap] <- strNUM_DQ_N
    
    mapStr <- paste(timeSeriesValue, collapse = '')
    return(mapStr)
  }
})

#tempMapStratTime <- c('2013-04-01','2013-03-01')
#tempMapLength <- c(12, 12)
#tempStr <- c('2014.03:7,2014.02:7,2014.01:7,2013.12:7,2013.11:7,2013.10:7,2013.09:7,2013.08:7,2013.07:7,2012.09:7,2012.08:7,2012.07:7,2012.06:7,2011.12:7'
#             ,'2013.12:1,2013.01:1')



# LLD 
msRecallPbocLLD <- function(dfSTPbocLLD, dfRecallPbocPDD, monthOffSet, ...){
  if(NROW(dfSTPbocLLD) == 0){
    return(dfSTPbocLLD)
  } else {
    dfRecallPbocLLD <- left_join(dfSTPbocLLD, dfRecallPbocPBI, by = c('ID_UNQF' = 'ID_UNQF'), copy = FALSE)
    dfRecallPbocLLD <- left_join(dfSTPbocLLD, dfRecallPbocPDD, by = c('ID_LTRA' = 'ID_PTRA', 'CATG' = 'CATG'), copy = FALSE)
    dfRecallPbocLLD2 <- within(dfRecallPbocLLD, {
      
      TIMEL_LSTINQ <- ifelse(is.na(TIMEL_LSTINQ) & !is.na(TIMEL_SETTLE), TIMEL_SETTLE, TIMEL_LSTINQ)
      #TIMEL_LSTINQ <- ifelse(is.na(TIMEL_LSTINQ) & STUL_ACCT == '呆账' & !is.na(TIMEL_LRP), TIMEL_LRP, TIMEL_LSTINQ)
      
      timeDiffAllLive <- msSysVarsPbocTimeDiffV(TIMEL_LSTINQ, TIMEL_ISS)
      timeDiffAllLive[is.na(timeDiffAllLive)] <- -1
      
      remained24MStr <- ifelse(timeDiffAllLive <= monthOffSet, NA, msRecallPboc24MRemaindStrFunc(RCDL_RP24M, monthOffSet))
      
     
      TIMEL_SRPRCD24M1 <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -(24 + monthOffSet - 1))
      day(TIMEL_SRPRCD24M1) <- 1 
      TIMEL_ERPRCD24M1 <- msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -monthOffSet)
      
      allLiveTime <- msSysVarsPbocTimeDiffV(TIMEL_LSTINQ, TIMEL_ISS)
      allLiveTime[is.na(allLiveTime)] <- 9999
      frontCompTime <- 24 + monthOffSet
      
      
      frontStr <- msRecallPbocFrontStrFunc(TIMEL_ISS, TIMEL_SRPRCD24M1, monthOffSet)
      
      mapStratTime <- as.Date(as.POSIXct.Date(ifelse(allLiveTime > frontCompTime, TIMEL_SRPRCD24M1, ifelse(allLiveTime == frontCompTime, TIMEL_SRPRCD24M1 + months(1),
                                                                                   TIMEL_SRPRCD24M1 + months(frontCompTime - allLiveTime)))))
      
      
      mapEndTime <- as.Date(as.POSIXct.Date(ifelse(is.na(STUL_ACCT) | STUL_ACCT %in% c('正常', '逾期', '冻结', '止付')
                           , msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -24)
                           , msSysVarsPbocMonthCal(as.Date(TIMEL_LSTINQ), -monthOffSet))))
      mapLength <- msSysVarsPbocTimeDiffV(mapEndTime, mapStratTime) + 1
      middleStr <- msRecallPbocMap24MStrFunc(mapStratTime = mapStratTime, mapLength = mapLength, TIME_NUM_DQ = TIME_NUM_DQ)
      
      
      all24MStr <- sapply(data.frame(rbind(frontStr, middleStr, remained24MStr))
                          ,function(x){if(all(is.na(x))) return(NA) else return(paste(na.omit(x), collapse = ''))})
      
      timeDueRepLSTINQ <- msSysVarsPbocTimeDiffV(TIME_REP, TIMEL_INST)
      all24MStr <- ifelse(timeRepLSTINQ >= 24 + monthOffSet, NA, timeRepLSTINQ)
      
    })
   
    
    return(dfRecallPbocLLD2)
  }
}

#head(dfRecallPbocLLD2[, c('mapStratTime', 'mapEndTime', 'all24MStr')])
#head(dfRecallPbocLLD2[, c('mapStratTime', 'mapEndTime', 'mapLength', 'frontStr', 'middleStr', 'remained24MStr', 'all24MStr')])
#dfRecallPbocLLD2[4, ]

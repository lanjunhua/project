# just the used for map function
msRecallPbocMap24MStrFunc <- Vectorize(function(mapStratTime, mapLength, mapEndTime = NULL, TIME_NUM_DQ,...){
  if(mapLength <= 0) return(NA)
  
  # get time series
  # b = data.frame(date = seq.Date(as.Date("1999-1-1"), by = "months", length = 26))
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

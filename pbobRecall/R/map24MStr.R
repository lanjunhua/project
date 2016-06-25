# just the used for map function
msRecallPbocMap24MStrFunc <- function(mapStratTime, mapEndTime, TIME_NUM_DQ,...){
  # get time series
  # b = data.frame(date = seq.Date(as.Date("1999-1-1"), by = "months", length = 26))
  timeSeries <- seq.Date(mapStratTime, mapEndTime, by = 'month')
  nObs <- length(timeSeries)
  timeSeriesNames <- sapply(timeSeries, function(x){paste(substr(x, 1,4), substr(x, 6,7), sep = '.')})
  timeSeriesValue <- rep('N', nObs)

  
  if(is.na(TIME_NUM_DQ)){
    return(timeSeriesValue)
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
    
    mapStr <- paste0(timeSeriesValue, collapse = '')
    return(mapStr)
  }
}

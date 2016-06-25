# the function for month calculate
msSysVarsPbocMonthCal <- function(timeInput, timeRange){
  timeInput <- as.POSIXlt(as.Date(timeInput))
  timeInput$mon <- timeInput$mon + timeRange
  timeOutput <- as.POSIXlt(as.Date(timeInput))
  timeOutput$mday <- ifelse(timeOutput$mday < timeInput$mday, 0, timeOutput$mday)
  return(as.Date(timeOutput))
}

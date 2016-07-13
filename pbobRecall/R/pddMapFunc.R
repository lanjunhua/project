library(dplyr)
library(data.table)

# the function to calculate two diffrent date 
msPubFuncPbocGetMonthDiff <- Vectorize(function(TimeEnd, TimeStart){
  TimeEnd   <- as.character(TimeEnd)
  TimeStart <- as.character(TimeStart)
  tempTimeEndY   <- as.numeric(t(as.data.frame(strsplit(TimeEnd, "[-|.|/]")))[, 1])
  tempTimeEndM   <- ifelse(is.na(TimeEnd), NA, as.numeric(t(as.data.frame(strsplit(TimeEnd, "[-|.|/]")))[, 2]))
  tempTimeStartY <- as.numeric(t(as.data.frame(strsplit(TimeStart, "[-|.|/]")))[, 1])
  tempTimeStartM <- ifelse(is.na(TimeStart), NA, as.numeric(t(as.data.frame(strsplit(TimeStart, "[-|.|/]")))[, 2]))
  TimeDiff <- (tempTimeEndY - tempTimeStartY) * 12 + (tempTimeEndM - tempTimeStartM)
  return(TimeDiff)
})

# deal with the pdd map function 
msRecallPbocRPRcdPdd <- function(.data){
  sp <- max(.data$pos)
  ep <- min(.data$pos)
  
  rpStr60 <- rep('N', 60)
  rpStr60[(60 - .data$pos)] <- .data$NUM_DQM
  paste(rpStr60, collapse = '')
}

# deal with three card calculate different
msRecallPbocPddCmp <- function(input){
  dfLCPdd <- input[input$CATG != '准贷记卡', ]
  dfSPdd <- input[input$CATG == '准贷记卡', ]

  dfSPd3Pdd <- dfSPdd[dfSPdd$NUM_DQM == 3, ]
  dfSPd3to2Pdd <- within(dfSPd3Pdd, { 
    NUM_DQM <- 2
    pos <- pos + 1
  })
  
  dfSPd3to1Pdd <- within(dfSPd3Pdd, {
    NUM_DQM <- 1
    pos <- pos + 2
  })
  
  dfSPdd1 <- rbind(dfSPd3Pdd, dfSPd3to2Pdd, dfSPd3to1Pdd)
  dfSPdd2 <- dfSPdd1[, lapply(.SD, max), by = c('ID_UNQF', 'ID_TRA', 'CATG', 'pos'), .SDcols = c('NUM_DQM')]
  
  dfPddLCS <- rbind(dfLCPdd, dfSPdd2)
  return(dfPddLCS)
}

# function just for the pdd part
msRecallPbocPddMain <- function(dfPdd, dfAcct){
  dfPddAppend <- inner_join(dfAcct, dfPdd,  by = c('ID_TRA' = 'ID_PTRA', 'CATG' = 'CATG'), copy = FALSE)
  
  dfPddAppend$pos <- msPubFuncPbocGetMonthDiff(dfPddAppend$TIME_LSTINQ, dfPddAppend$TIME_DQ)
  # deal with semi-credit card
  dfPddAppend <- msRecallPbocPddCmp(dfPddAppend[, c('ID_UNQF', 'ID_TRA', 'CATG', 'pos', 'NUM_DQM'),with = FALSE])
  result <- dfPddAppend %>% group_by(ID_TRA, CATG) %>%
    do(msRecallPbocRPRcdPdd(.)) %>%
    data.table
}

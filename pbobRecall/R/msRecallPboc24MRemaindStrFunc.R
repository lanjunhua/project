# function to get the 24M remainded str
msRecallPboc24MRemaindStrFunc <- Vectorize(function(RCDL_RP24M, monthOffSet){
  if(is.na(RCDL_RP24M)) return(NA)
  remaind24MStr <- paste0(unlist(strsplit(RCDL_RP24M, ""))[1:(24 - monthOffSet)], collapse = '')
  return(remaind24MStr)
})

#tempTest1 <- c('//////////////////////*N', '********N***************')
#ms24MRemaindStrFunc(tempTest1, 12)

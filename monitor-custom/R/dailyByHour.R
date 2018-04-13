###########################
# dailyByHour.R
# Function accepts dataList, infoList, and textList, and will return a 
# diurnal plot using monitorPlot_latestDailyByHour() based on input
###########################

dailyByHour <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.info("----- dailyByHour() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  # ----- Extract variables from the 'infoList' and 'dataList' objects ------------------------
  
  ws_monitor <- dataList$ws_monitor
  monitorID <- infoList$monitorid
  lookbackDays <- infoList$lookbackdays
  size <- infoList$size
  
  style <- ifelse(size <= 500, "small", "large")
  
  # ----- Draw the plot -------------------------------------------------------
  
  monitorPlot_latestDailyByHour(ws_monitor, monitorID, lookbackDays = lookbackDays, size = style, previousDays = FALSE)
  
}

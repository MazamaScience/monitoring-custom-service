#' @title Create Hourly Barplot
#' @param infoList list containing information specifying a unique product. Generated with createInfoList()
#' @param dataList list containing all data needed to generate a product. Generated with createDataList()
#' @param textList list including character strings for labeling. Generated with createTextList()
#' @description Creates barplot of hourly mean PM2.5 levels
#' @return Returns a png plot object.

hourlyBarPlot <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.debug("----- hourlyBarPlot() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)
  
  # ----- Extract variables from the 'infoList' object ------------------------
  
  tlim <- infoList[["tlim"]]
  monitorIDs <- infoList[["monitorIDs"]]
  
  mon <- dataList$ws_monitor %>%
    monitor_subset(tlim = tlim)
  
  numMons <- length(mon[["meta"]][["monitorID"]])
  
  # ----- Create Plot ---------------------------------------------------------
  
  par(mfrow = c(numMons,1), mar = c(1.5, 1, 1, .5))
  
  for (index in 1:numMons) {
    
    single_mon <- monitor_subset(mon, monitorIDs = monitorIDs[index])
    title <- paste("Average hourly PM2.5 for:", monitorIDs[index])
    
    monitorPlot_hourlyBarplot(
      single_mon, main = title, shadedNight = TRUE
    )
  }
}

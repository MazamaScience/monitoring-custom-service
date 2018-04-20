#' @title Create NowCast Plot
#' @param ws_monitor emphws_monitor object
#' @param size size in pixels of the image to be saved
#' @param tlim optional vector with start and end times (integer or character representing YYYYMMDD[HH]). Defaults to the last 10 days.
#' @description Creates timeseries with hourly PM2.5 values and NowCast values
#' @return plot image as .png

timeseries <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.info("----- timeseries() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  # ----- Extract variables from the 'infoList' object ------------------------
  
  ws_monitor <- dataList$ws_monitor
  siteName <- ws_monitor$meta$siteName
  timezone <- ws_monitor$meta$timezone
  monitorID <- infoList$monitorid
  tlim <- infoList$tlim

  # Make the title
  title <- paste("Hourly PM2.5 Values and NowCast \n Site:", siteName, "\n ID:", monitorID)
  
  monitorPlot_timeseries(ws_monitor,
                         monitorID = infoList$monitorid,
                         tlim = infoList$tlim)
  
  addAQILines()

}


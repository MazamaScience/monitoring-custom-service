#' @title Create monitor map plot

mapPlot <- function(dataList = NULL, infoList = NULL, textList = NULL) {
  
  logger.debug("----- mapPlot() -----")
  
  if (is.null(dataList)) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(textList)) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)
  
  
  # ----- Extract variables from the 'infoList' object ------------------------
  
  mon <- dataList$ws_monitor %>% 
    monitor_subset(tlim = infoList[["tlim"]])
  
  
  # ----- Create Plot ---------------------------------------------------------
  
  monitorGoogleMap(
    ws_monitor = mon,
    centerLon = infoList[["mapCenter"]][["lon"]],
    centerLat = infoList[["mapCenter"]][["lat"]],
    zoom = infoList[["mapCenter"]][["zoom"]]
    )
  
}

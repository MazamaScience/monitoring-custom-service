########################################################################
# demo/createDataList.R
#
# Create a list of data needed to generate the presentation.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createDataList <- function(infoList=NULL, dataDir=NULL) {
  
  logger.trace("----- createDataList() -----")
  
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(dataDir) ) stop(paste0("Required parameter 'dataDir' is missing."), call.=FALSE)
  if ( is.null(infoList$monitorIDs) ) stop(paste0("Required parameter 'intoList$monitorIDs' is missing."), call.=FALSE)
  
  # Get parameters
  monitorIDs <- infoList$monitorIDs
  
  logger.debug("monitorID = '%s'", paste0(monitorIDs, collapse=","))
  
  # ----- Load data -----------------------------------------------------------
  
  # Load latest monitoring data (most recent 45 days)
  logger.debug("loading latest monitoring data from %s", dataDir)
  result <- try({
    load(file.path(dataDir, "airnow_pm25_latest.RData"))   # NOTE:  v3 monitorIDs
    load(file.path(dataDir, "airsis_pm25_latest.RData"))   # NOTE:  v3 monitorIDs
    load(file.path(dataDir, "wrcc_pm25_latest.RData"))     # NOTE:  v3 monitorIDs
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    stop(paste0('Error loading data: ', err_msg))
  }

  combinedData <- monitor_combine( list(airnow_pm25_latest,
                                        airsis_pm25_latest,
                                        wrcc_pm25_latest)
                                 )
  
  # ----- Validate data -------------------------------------------------------
  
  # Check for bad monitorIDs
  badMonitorIDs <- setdiff(monitorIDs, combinedData$meta$monitorID)
  goodMonitorIDs <- intersect(monitorIDs, combinedData$meta$monitorID)
  if ( length(badMonitorIDs) > 0 ) {
    logger.debug("The following monitors are not in the latest data: %s", paste0(badMonitorsIDs, collapse = ", "))
  }
  if ( length(goodMonitorIDs) == 0 ) {
    stop("No data available for selected monitors", call. = FALSE)
  }
  
  # Combine all monitor data into a single ws_monitor object
  ws_monitor <- monitor_subset(combinedData, monitorIDs = goodMonitorIDs)
  
  # Is there data for the given tlim?
  if ( monitor_isEmpty(monitor_subset(ws_monitor, tlim = infoList$tlim)) ) {
    stop(paste("No data availabe at the specified dates") , call. = FALSE)
  }
  
  # ----- Create data structures ----------------------------------------------
  
  # Create a dataframe for tabular presentation
  tableData <- ws_monitor$meta[,c("siteName", "countyName", "stateCode", "agencyName")]
  tableData$countyName <- stringr::str_to_title(tableData$countyName)
  names(tableData) <- c("Site","County","State","Agency")

  # Create dataList
  dataList <- list(ws_monitor = ws_monitor,
                   tableData = tableData)
  
  
  return(dataList)
  
}

########################################################################
# createDataList.R
#
# Create a list of data needed to generate products.
#
# Author: Helen Miller, Jonathan Callahan
########################################################################

createDataList <- function(infoList=NULL, dataDir=NULL) {
  
  logger.info("----- createDataList() -----")
  
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(dataDir) ) stop(paste0("Required parameter 'dataDir' is missing."), call.=FALSE)
  
  # Create dataList
  dataList <- list()
  
  # Get parameters
  monitorid <- infoList$monitorid

  logger.debug("monitorID = '%s'", paste0(monitorid,collapse=","))
  
  # Load latest monitoring data (most recent 45 days) -------------------------
  
  # NOTE:  Host data directories are mounted at dataDir as specified in the docker-compose file
  
  result <- try({
    logger.info("loading latest monitoring data from %s", dataDir)
    # Combine monitors from all sources and subset to get the monitorids requested
    load(file.path(dataDir, "airnow_PM2.5_latest10.RData"))
    load(file.path(dataDir, "airsis_PM2.5_latest10.RData"))
    load(file.path(dataDir, "wrcc_PM2.5_latest10.RData"))
    latestData <- monitor_combine(list(airnow_PM2.5_latest10,airsis_PM2.5_latest10,wrcc_PM2.5_latest10))
  }, silent=TRUE)

  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    stop(paste0('Error loading data: ', err_msg))
  }

  # Are there any monitors with given monitorid?
  if ( !monitorid %in% latestData$meta$monitorID ) {
    stop( paste0(monitorid, " is not a valid monitor ID") , call. = FALSE)
  }
  dataList$ws_monitor <- monitor_subset(latestData, monitorIDs = monitorid, dropMonitors = FALSE)

  return(dataList)
  
}

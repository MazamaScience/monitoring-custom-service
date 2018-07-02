########################################################################
# dailyhourlybarplot/createDataList.R
#
# Create a list of data needed to generate the plot.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createDataList <- function(infoList = NULL, dataDir = NULL) {

  logger.trace("----- createDataList() -----")

  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(dataDir)) stop(paste0("Required parameter 'dataDir' is missing."), call. = FALSE)
  if (is.null(infoList$monitorIDs)) stop(paste0("Required parameter 'intoList$monitorIDs' is missing."), call. = FALSE)

  # Get parameters
  monitorIDs <- infoList$monitorIDs

  logger.debug("monitorID = '%s'", paste0(monitorIDs, collapse = ","))

  # ----- Load data -----------------------------------------------------------

  # Load latest monitoring data (most recent 45 days)
  combinedData <- loadDaily()

  # ----- Validate data -------------------------------------------------------

  # Check for bad monitorIDs
  badMonitorIDs <- setdiff(monitorIDs, combinedData$meta$monitorID)
  goodMonitorIDs <- intersect(monitorIDs, combinedData$meta$monitorID)
  if ( length(badMonitorIDs) > 0 ) {
    logger.debug(
      "The following monitors are not in the latest data: %s",
      paste0(badMonitorIDs, collapse = ", "))
  }
  if ( length(goodMonitorIDs) == 0 ) {
    stop("No data available for the selected monitors", call. = FALSE)
  }

  # Get the timezone from the first goodMonitorID
  timezone <- combinedData$meta[goodMonitorIDs[1], 'timezone']

  # Subset the data based on monitorIDs
  ws_monitor <- monitor_subset(combinedData,
                               monitorIDs = goodMonitorIDs,
                               tlim = infoList$tlim,
                               timezone = timezone,
                               dropMonitors = FALSE)

  # Is there any data left?
  if ( monitor_isEmpty(monitor_subset(ws_monitor)) ) {
    stop(paste("No data available for the specified dates"), call. = FALSE)
  }
  
  # NOTE:  siteName is used in the table and for facet_wrap() in the ggplot code.
  # NOTE:  Bad things happen if siteName == NA. Here we replace  missing values
  # NOTE:  with monitorID.
  badSiteMask <- is.na(ws_monitor$meta$siteName)
  ws_monitor$meta$siteName[badSiteMask] <- ws_monitor$meta$monitorID[badSiteMask]

  # ----- Create data structures ----------------------------------------------

  # Create a dataframe for tabular presentation
  tableData <-
    ws_monitor$meta[, c("siteName", "countyName", "stateCode", "agencyName")]
  tableData$countyName <- stringr::str_to_title(tableData$countyName)
  names(tableData) <- c("Site", "County", "State", "Agency")

  # Create dataList
  dataList <- list(
    ws_monitor = ws_monitor,
    tableData = tableData
  )


  return(dataList)

}
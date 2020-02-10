########################################################################
# dailyhourlybarplot/createDataList.R
#
# Create a list of data needed to generate the plot.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createDataList <- function(infoList = NULL, dataDir = NULL) {


  logger.debug("----- createDataList() -----")

  # ----- Setup ----------------------------------------------------------------

  MazamaCoreUtils::stopIfNull(infoList, "Required parameter 'infoList' is missing.")
  MazamaCoreUtils::stopIfNull(dataDir, "Required parameter 'dataDir' is missing.")

  # ----- Get infoList parameters ----------------------------------------------

  # monitorIDs is already a vector of monitorID values
  monitorIDs <- infoList$monitorIDs
  
  logger.trace("monitorID = '%s'", paste0(monitorIDs, collapse = ","))
  
  startdate <- infoList$startdate
  enddate <- infoList$enddate

  logger.trace("Getting parameters from infoList:")

  logger.trace("startdate = '%s'", printUTC(startdate))
  logger.trace("enddate = '%s'", printUTC(enddate))

  # ----- Load ws_monitor data -------------------------------------------------

  ## NOTE:
  #  Host data directories are mounted at dataDir as specified in the
  #  docker-compose file

  if ( startdate > lubridate::now(tzone = "UTC") - lubridate::days(10) ) {

    # * Recent data uses local files -------------------------------------------

    result <- try({

      logger.trace("loading latest monitoring data from %s", dataDir)

      ws_monitor <- PWFSLSmoke::monitor_load(
        startdate = startdate,
        enddate = enddate,
        monitorIDs = monitorIDs,
        dataDir = dataDir
      )

    }, silent = TRUE)

  } else {

    # * Archival data loads as needed ------------------------------------------

    result <- try({

      logger.trace("Loading archival data with `monitor_load()`")

      ws_monitor <- PWFSLSmoke::monitor_load(
        startdate = startdate,
        enddate = enddate,
        monitorIDs = monitorIDs
      )

    }, silent = TRUE)

  }

  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    stop(paste0('Error loading data: ', err_msg))
  }

  # # ----- Load and subset data ------------------------------------------------
  
  # # Load latest monitoring data (most recent 45 days)
  # dailyData <- loadDaily()
  # latestData <- loadLatest()
  # ws_monitor <- monitor_join(dailyData, latestData, monitorIDs)
  
  # ----- Validate data -------------------------------------------------------
  
  # Check for bad monitorIDs
  badMonitorIDs <- setdiff(monitorIDs, ws_monitor$meta$monitorID)
  goodMonitorIDs <- intersect(monitorIDs, ws_monitor$meta$monitorID)
  if ( length(badMonitorIDs) > 0 ) {
    logger.trace(
      "The following monitors are not found in the most recent 45 days of data: %s",
      paste0(badMonitorIDs, collapse = ", "))
  }
  if ( length(goodMonitorIDs) == 0 ) {
    stop("No data available for the selected monitors", call. = FALSE)
  }
  
  # NOTE:  siteName is used in the table and for facet_wrap() in the ggplot code.
  # NOTE:  Bad things happen if siteName == NA. Here we replace  missing values
  # NOTE:  with monitorID.
  badSiteMask <- is.na(ws_monitor$meta$siteName)
  ws_monitor$meta$siteName[badSiteMask] <- ws_monitor$meta$monitorID[badSiteMask]
  
  # ----- Create data structures ----------------------------------------------
  
  # Create a dataframe for tabular presentation
  tableData <- ws_monitor$meta[, c("siteName", "countyName", "stateCode", "agencyName")]
  tableData$countyName <- stringr::str_to_title(tableData$countyName)
  names(tableData) <- c("Site", "County", "State", "Agency")
  
  # Create dataList
  dataList <- list(
    ws_monitor = ws_monitor,
    tableData = tableData
  )
  
  
  return(dataList)
  
}

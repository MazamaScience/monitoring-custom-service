########################################################################
# outlook/createDataList.R
#
# Create a list of data needed to generate products.
#
# Author: Helen Miller, Jonathan Callahan
########################################################################

createDataList <- function(infoList=NULL, dataDir=NULL) {
  
  logger.debug("----- createDataList() -----")
  
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(dataDir) ) stop(paste0("Required parameter 'dataDir' is missing."), call.=FALSE)
  if ( is.null(infoList$monitorIDs) ) stop(paste0("Required parameter 'intoList$monitorIDs' is missing."), call.=FALSE)
  
  # Get parameters
  monitorIDs <- infoList$monitorIDs
  
  logger.debug("monitorID = '%s'", paste0(monitorIDs, collapse=","))
  
  # ----- Load data -----------------------------------------------------------

  if (lubridate::year(lubridate::ymd(infoList$startdate, tz = "UTC")) == 2017) {
    
    # Load historical 2017 data
    result <- try({
      
      logger.debug("Loading 2017 historical data from %s", dataDir)
      load(file.path(dataDir, "airnow_PM2.5_2017.RData"))
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      stop(paste0('Error loading data: ', err_msg))
    }
    
    # TODO:  Only working with AIRSIS data for now because converstion to old
    # TODO:  monitorIDs is easiest there.
    
    # Convert data to v3 format
    airnow_PM2.5_2017[["meta"]][["monitorID"]] <- airnow_PM2.5_2017[["meta"]][["siteID"]]
    names(airnow_PM2.5_2017[["data"]])[-1] <- airnow_PM2.5_2017[["meta"]][["monitorID"]]
    
    loadedData <- list(airnow_PM2.5_2017)
    
  } else if (
    lubridate::as_date(lubridate::ymd(infoList$enddate, tz = "UTC")) >= 
    lubridate::today() - lubridate::ddays(45)
  ) {
    
    # Load latest monitoring data (most recent 45 days)
    result <- try({
      
      logger.debug("loading latest monitoring data from %s", dataDir)
      
      # Combine monitors from all latest sources
      load(file.path(dataDir, "airnow_pm25_latest.RData"))
      load(file.path(dataDir, "airsis_pm25_latest.RData"))
      load(file.path(dataDir, "wrcc_pm25_latest.RData"))
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      stop(paste0('Error loading data: ', err_msg))
    }
    
    loadedData <- list(airnow_pm25_latest, airsis_pm25_latest, wrcc_pm25_latest)
    
  } else {
    stop("could not load data for given date range")
  }
  
  combinedData <- monitor_combine(loadedData)
  
  # ----- Validate data -------------------------------------------------------

  # Check for bad monitorIDs
  badMonitorIDs <- setdiff(monitorIDs, combinedData$meta$monitorID)
  goodMonitorIDs <- intersect(monitorIDs, combinedData$meta$monitorID)
  if ( length(badMonitorIDs) > 0 ) {
    logger.debug("The following monitors are not in the data: %s", paste0(badMonitorIDs, collapse = ", "))
  }
  if ( length(goodMonitorIDs) == 0 ) {
    stop("No data available for selected monitors", call. = FALSE)
  }
  
  # Combine all monitor data into a single ws_monitor object
  ws_monitor <- monitor_subset(combinedData, monitorIDs = goodMonitorIDs)
  
  # Is there data for the given tlim?
  if ( is.null(monitor_subset(ws_monitor, tlim = infoList$tlim)$data) ) {
    stop(paste("No data availabe at the specified dates") , call. = FALSE)
  }
  
  # ----- Create data structures ----------------------------------------------
  
  # Create a dataframe for tabular presentation
  tableData <- ws_monitor$meta[,c("siteName", "countyName", "stateCode", "agencyName")]
  tableData$countyName <- stringr::str_to_title(tableData$countyName)
  names(tableData) <- c("Site","County","State","Agency")
  
  # Create a dataframe for forcast information
  
  forecastData <- 
    tibble(
      monitorID = unlist(infoList[["monitorIDs"]]),
      Yesterday = unlist(infoList[["AQIPredictions_tm1"]]),
      Comment = unlist(infoList[["monitorComments"]]),
      Today = unlist(infoList[["AQIPredictions_t"]]),
      Tomorrow = unlist(infoList[["AQIPredictions_tp1"]])
    ) %>% 
    filter(monitorID %in% ws_monitor[["meta"]][["monitorID"]]) %>% 
    # Station must be added after filtering the monitorIDs, since fake
    # monitor info doe not exist in ws_monitor
    mutate(Station = ws_monitor[["meta"]][["siteName"]]) %>% 
    select(Station, everything(), -monitorID)
  
  
  # Create dataList
  dataList <- list(
    ws_monitor = ws_monitor,
    tableData = tableData,
    forecastData = forecastData)
  
  return(dataList)
  
}

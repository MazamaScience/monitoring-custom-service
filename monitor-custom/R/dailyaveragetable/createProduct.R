########################################################################
# dailyaveragetable/createProduct.R
#
# Create a table of daily average monitor readings.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

#' @title Create a table of daily average monitor readings
#'
#' @description
#' Function that grabs the appropriate parameters from `infoList`, data from
#' `dataList`, generates a table, and saves it as an image or spreadsheet.

createProduct <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.debug("----- createProduct() [dailyaveragetable] -----")

  if (is.null(dataList)) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(textList)) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)

  # ----- Get parameters ------------------------------------------------------

  ws_monitor <- dataList$ws_monitor
  monitorIDs <- infoList$monitorIDs
  plotPath <- infoList$plotPath

  # ----- Calculate tlim ------------------------------------------------------
  
  # Create starttime and endtime in monitor local time
  # NOTE:  Don't use lubridate::today() as it generates class 'Date' which causes confusion.
  # NOTE:  Instead, stick with lubridate::now() which generates class 'POSIXct'.
  timezone <- ws_monitor$meta$timezone[1] # Use first available timezone
  now <- lubridate::now(tzone = timezone)
  today <- lubridate::floor_date(now, unit = 'day')
  
  starttime <- today - lubridate::ddays(infoList$days)
  endtime <- lubridate::floor_date(now, unit = 'hour')
  
  # ----- Aggregate data -------------------------------------------------------
  
  # Define the date range
  dateRange <- MazamaCoreUtils::dateRange(
    startdate = infoList$startdate,
    enddate = infoList$enddate,
    timezone = infoList$timezone,
    unit = "day",
    ceilingEnd = TRUE
  )
  
  # Calculate daily data from monitors
  monitorData <-
    ws_monitor %>%
    monitor_subset(monitorIDs = monitorIDs) %>%
    monitor_subset(tlim = dateRange)
  
  if (infoList$useaqi == 'true') {
    dailyData <-
      monitorData %>%
      monitor_nowcast() %>%
      monitor_dailyStatistic()
      
  } else {
    dailyData <-
      monitorData %>%
      monitor_dailyStatistic()
  }
  
  # ----- Save table -----------------------------------------------------------
  
  if ( infoList$outputfiletype == "xlsx" ) {
  
    # Create xlsx spreadsheet
    writexl::write_xlsx(
      x = dailyData$data,
      path = plotPath
    )
    
  } else if ( infoList$outputfiletype == "png") {
    
    # # Create table
    # table <- flextable::flextable(
    #   dailyData$data
    # ) 
    # %>%
    #   flextable::bg(
    #     bg = "#FFFFFF",
    #     part = "all"
    #   ) %>%
    #   flextable::bold(
    #     part = "header"
    #   ) %>%
    #   flextable::autofit(
    #     add_w = 0.1,
    #     add_h = 0.1,
    #     part = c("body", "header")
    #   )
    # 
    # flextable::save_as_image(
    #   table,
    #   path = plotPath
    # )
    
  }
  
  return(invisible())
}

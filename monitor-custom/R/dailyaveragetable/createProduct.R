########################################################################
# dailyaveragetable/createProduct.R
#
# Create the a daily and hourly summary barplot.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

#' @title Create daily and hourly summary barplot
#'
#' @description
#' Function that grabs the appropriate parameters from `infoList`, data from
#' `dataList`, passes it along to `AirMonitorPlots::monitor_ggDailyHourlyBarplot()`, and
#' then saves the returned graphic.

createProduct <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.debug("----- createProduct() [dailyaveragetable] -----")

  if (is.null(dataList)) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(textList)) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)

  # ----- get parameters ------------------------------------------------------

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
  endtime <- lubridate::floor_date(now, unit = 'hour')
  starttime <- today - lubridate::ddays(infoList$days)
  # tlim <- as.POSIXct(c(starttime, endtime)) # Guarantee they are of class POSIXct
  
  # # Subset the data based on monitorIDs
  # ws_monitor <- monitor_subset(ws_monitor,
  #                              tlim = tlim,
  #                              dropMonitors = FALSE)
  # 
  # # Is there any data left?
  # if ( monitor_isEmpty(monitor_subset(ws_monitor)) ) {
  #   stop(paste("No data available for the specified dates"), call. = FALSE)
  # }
  
  # ----- Aggregate data ---
  
  dateRange <- MazamaCoreUtils::dateRange(
    startdate = infoList$startdate,
    enddate = infoList$enddate,
    timezone = infoList$timezone,
    unit = "day",
    ceilingEnd = TRUE
  )
  
  # Get data from monitors
  
  monData <-
    ws_monitor %>%
    monitor_subset(monitorIDs = monitorIDs)
  
  if (infoList$useaqi == 'true') {
    dailyData <-
      monData %>%
      monitor_nowcast() %>%
      monitor_dailyStatistic() %>%
      monitor_subset(tlim = dateRange)
  } else {
    dailyData <-
      monData %>%
      monitor_dailyStatistic() %>%
      monitor_subset(tlim = dateRange)
  }
  
  # ----- Save table -----------------------------------------------------------
  
  if ( infoList$outputfiletype == "png") {
    
    # Create table
    table <- flextable::flextable(
      dailyData$data
    ) %>%
      flextable::bg(
        bg = "#FFFFFF",
        part = "all"
      ) %>%
      flextable::autofit(
        add_w = 0.1,
        add_h = 0.1,
        part = c("body", "header")
      )
    
    flextable::save_as_image(
      table,
      path = plotPath
    )
    
  } else if ( infoList$outputfiletype == "xlsx" ) {
    
    oldOptions <- options()
    options(xlsx.datetime.format="yyyy-mm-dd")
    options(xlsx.date.format="yyyy-mm-dd")
    
    xlsx::write.xlsx2(
      x = dailyData$data,
      file = plotPath,
      row.names = FALSE
    )
    
    options(oldOptions)
    
  }
  
  return(invisible())
}

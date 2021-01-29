########################################################################
# dailyhourlybarplot/createProduct.R
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

  logger.debug("----- createProduct() [dailyhourlybarplot] -----")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dataList)
  MazamaCoreUtils::stopIfNull(infoList)
  MazamaCoreUtils::stopIfNull(textList)

  # ----- Get parameters ------------------------------------------------------

  ws_monitor <- dataList$ws_monitor

  # TODO:  labels should be found in textList, not infoList
  title <- infoList$title
  xLabel <- infoList$xLabel
  yLabel <- infoList$yLabel

  # ----- Calculate tlim ------------------------------------------------------

  # # Create starttime and endtime in monitor local time
  # # NOTE:  Don't use lubridate::today() as it generates class 'Date' which causes confusion.
  # # NOTE:  Instead, stick with lubridate::now() which generates class 'POSIXct'.
  # timezone <- ws_monitor$meta$timezone[1] # Use first available timezone
  # now <- lubridate::now(tzone = timezone)
  # today <- lubridate::floor_date(now, unit = 'day')
  # endtime <- lubridate::floor_date(now, unit = 'hour')
  # starttime <- today - lubridate::ddays(infoList$days)
  # # tlim <- as.POSIXct(c(starttime, endtime)) # Guarantee they are of class POSIXct
  #
  # # # Subset the data based on monitorIDs
  # # ws_monitor <- monitor_subset(ws_monitor,
  # #                              tlim = tlim,
  # #                              dropMonitors = FALSE)
  # #
  # # # Is there any data left?
  # # if ( monitor_isEmpty(monitor_subset(ws_monitor)) ) {
  # #   stop(paste("No data available for the specified dates"), call. = FALSE)
  # # }

  # ----- Create plot ---------------------------------------------------------

  # NOTE:  Elements of infoList use all lower case because that is the preferred
  # NOTE:  casing in the API. (Allows us to tolerate mis-casing in hand-typed
  # NOTE:  URLs by just downcasing everything.)

  # Create plot
  gg <- AirMonitorPlots::monitor_ggDailyHourlyBarplot(
    ws_monitor = ws_monitor,
    monitorIDs = infoList$monitorids,
    startdate = infoList$startdate,
    enddate = infoList$enddate,
    columns = infoList$columns,
    title = title,
    xLabel = xLabel,
    yLabel = yLabel,
    includeLink = infoList$includelink,
    hourlyDataType = infoList$hourlytype
  )

  # ----- Save plot -----------------------------------------------------------

  ggsave(
    filename = infoList$plotPath,
    plot = gg,
    width = infoList$width,
    height = infoList$height,
    dpi = infoList$dpi,
    units = infoList$units
  )

  return(invisible())
}

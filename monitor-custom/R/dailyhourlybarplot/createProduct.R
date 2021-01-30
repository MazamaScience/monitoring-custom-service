########################################################################
# dailyhourlybarplot/createProduct.R
#
# Create the a daily and hourly summary barplot.
#
# Author: Tate Brasel, Spencer Pease, Jonathan Callahan
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

  # TODO:  labels should be found in textList, not infoList
  title <- infoList$title
  xLabel <- infoList$xLabel
  yLabel <- infoList$yLabel

  # ----- Create plot ---------------------------------------------------------

  # NOTE:  Elements of infoList use all lower case because that is the preferred
  # NOTE:  casing in the API. (Allows us to tolerate mis-casing in hand-typed
  # NOTE:  URLs by just downcasing everything.)

  # Create plot
  plot <- AirMonitorPlots::monitor_ggDailyHourlyBarplot(
    ws_monitor = dataList$ws_monitor,
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
    plot = plot,
    width = infoList$width,
    height = infoList$height,
    dpi = infoList$dpi,
    units = infoList$units
  )

  return(invisible())
}

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
#' `dataList`, passes it along to `PWFSLSmokePlots::createTarnayPlot()`, and
#' then saves the returned graphic.

createProduct <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.debug("----- createProduct() [dailyhourlybarplot] -----")

  if (is.null(dataList)) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(textList)) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)

  # ----- get parameters ------------------------------------------------------

  ws_monitor <- dataList$ws_monitor
  monitorIDs <- infoList$monitorIDs
  columns <- infoList$columns
  includeLink <- infoList$includelink
  includeThirdCol <- infoList$includethirdcol
  hourlyType <- infoList$hourlytype
  title <- infoList$title
  xLabel <- infoList$xLabel
  yLabel <- infoList$yLabel

  plotPath <- infoList$plotPath
  width <- infoList$width
  height <- infoList$height
  dpi <- infoList$dpi
  units <- infoList$units

  # ----- Calculate tlim ------------------------------------------------------
  
  # Create starttime and endtime in monitor local time
  # NOTE:  Don't use lubridate::today() as it generates class 'Date' which causes confusion.
  # NOTE:  Instead, stick with lubridate::now() which generates class 'POSIXct'.
  timezone <- ws_monitor$meta$timezone[1] # Use first available timezone
  now <- lubridate::now(tzone = timezone)
  today <- lubridate::floor_date(now, unit = 'day')
  endtime <- lubridate::floor_date(now, unit='hour')
  starttime <- today - lubridate::ddays(infoList$lookbackdays)
  tlim <- as.POSIXct(c(starttime, endtime)) # Guarantee they are of class POSIXct
  
  # # Subset the data based on monitorIDs
  # ws_monitor <- monitor_subset(ws_monitor,
  #                              tlim = tlim,
  #                              dropMonitors = FALSE)
  # 
  # # Is there any data left?
  # if ( monitor_isEmpty(monitor_subset(ws_monitor)) ) {
  #   stop(paste("No data available for the specified dates"), call. = FALSE)
  # }
  
  # ----- Create plot ---------------------------------------------------------

  plot <- PWFSLSmokePlots::createTarnayPlot(
    monitors = monitorIDs,
    data = ws_monitor,
    tlim = tlim,
    columns = columns,
    title = title,
    xLabel = xLabel,
    yLabel = yLabel,
    includeLink = includeLink,
    hourlyType = hourlyType,
    includeThirdCol = includeThirdCol
  )

  # ----- Save plot -----------------------------------------------------------

  ggsave(
    plotPath,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = units
  )

  return(invisible())
}

########################################################################
# uptime/createProduct.R
#
# Create a Uptime / load timeseries chart.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

#' @title Create Uptime / load timeseries chart
#'
#' @description
#'

createProduct <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.trace("----- createProduct() [uptime] -----")

  if (is.null(dataList)) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(textList)) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)

  # ----- get parameters ------------------------------------------------------

  uptimeData <- dataList$uptimeData
  serverid <- infoList$serverid
  ymax <- infoList$ymax

  plotPath <- infoList$plotPath
  width <- infoList$width
  height <- infoList$height
  dpi <- infoList$dpi
  units <- infoList$units

  # ----- Create plot ---------------------------------------------------------

  basePlot <- ggplot(uptimeData, aes(x = datetime)) +
    geom_step(aes(y = load_15_min)) +
    ylim(0, max(ymax, max(uptimeData$load_15_min) * 1.1)) +
    labs(
      title = paste("Uptime for:", serverid),
      x = "Time",
      y = "Load (15 Minute Average)") +
    ggthemes::theme_hc()

  if ( infoList$lookbackdays < 3 ) {

    # start <- lubridate::floor_date(range(uptimeData$datetime)[1], unit="day")
    # end <- lubridate::ceiling_date(range(uptimeData$datetime)[2], unit="day")
    # minor_breaks <- seq.POSIXt(start, end, by="3 hour")

    uptimePlot <- basePlot +
      scale_x_datetime(
        date_labels = "%b %d",
        date_breaks = "1 day",
        date_minor_breaks = "3 hours" # TODO:  Why aren't these showing?
      )

  } else {

    uptimePlot <- basePlot +
      scale_x_datetime(
        date_labels = "%b %d",
        date_breaks = "1 day"
      )

  }

  # ----- Save plot -----------------------------------------------------------

  ggsave(
    plotPath,
    plot = uptimePlot,
    width = width,
    height = height,
    dpi = dpi,
    units = units
  )

  return(invisible())
}

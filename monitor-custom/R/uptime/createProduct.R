########################################################################
# uptime/createProduct.R
#
# Create the a 'tarnay' summary barplot.
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
  serverID <- infoList$serverid
  yMax <- infoList$ymax

  plotPath <- infoList$plotPath
  width <- infoList$width
  height <- infoList$height
  dpi <- infoList$dpi
  units <- infoList$units

  # ----- Create plot ---------------------------------------------------------

  uptimePlot <- ggplot(uptimeData, aes(x = datetime)) +
    geom_step(aes(y = load_15_min)) +
    ylim(0, min(yMax, max(uptimeData$load_15_min) * 1.25)) +
    labs(
      title = paste("Uptime for:", serverID),
      x = "Time",
      y = "Load (15 Minute Average)") +
    ggthemes::theme_hc() +
    scale_x_datetime(
      date_labels = "%m/%d %H:%M"
    )

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

########################################################################
# tarnayplot/createProduct.R
#
# Create the a 'tarnay' summary barplot.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

# TODO: update description
#' @title Create 'tarnay' summary barplot
#'
#' @description
#' Function that grabs the appropriate parameters from `infoList`, data from
#' `dataList`, passes it along to `PWFSLSmokePlots::createTarnayPlot()`, and
#' then saves the returned graphic.

createProduct <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.trace("----- createProduct() [tarnayPlot] -----")

  if (is.null(dataList)) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)
  if (is.null(textList)) stop(paste0("Required parameter 'textList' is missing."), call. = FALSE)

  # ----- get parameters ------------------------------------------------------

  ws_monitor <- dataList$ws_monitor
  monitorIDs <- infoList$monitorIDs
  columns <- infoList$columns

  plotPath <- infoList$plotPath
  width <- infoList$width
  height <- infoList$height
  dpi <- infoList$dpi
  units <- infoList$units

  # ----- Create plot ---------------------------------------------------------

  plot <- PWFSLSmokePlots::createTarnayPlot(
    monitors = monitorIDs,
    data = ws_monitor,
    columns = columns
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

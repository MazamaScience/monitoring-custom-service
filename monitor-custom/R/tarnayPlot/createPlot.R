########################################################################
# tarnayplot/createPlot.R
#
# Create the a 'tarnay' summary barplot.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

#' @title Create 'tarnay' summary barplot
#' @description Function passing appropriate parameters to plot function and
#'    saving the plot

createPlot <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.trace("----- createPresentation() -----")

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

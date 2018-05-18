########################################################################
# uptime/textList_en.R
#
# English language text strings.
#
# Author: Jonathan Callahan
########################################################################

createTextList <- function(dataList=NULL, infoList=NULL) {

  logger.trace("----- createTextList() -----")

  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)

  # Commonly used labels
  textList <- list(
    AQI_category_labels = c('Good','Moderate','USG','Unhealthy','Very Unhealthy','Hazardous'),
    AQI_legend_title = 'AQI Level',
    date = 'Date',
    pm25_name = 'PM2.5'
  )

  return(textList)
}

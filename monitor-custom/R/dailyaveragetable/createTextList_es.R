########################################################################
# dailyaveragetable/textList_es.R
#
# Spanish language text strings.
#
# Author: Mazama Science
########################################################################

createTextList <- function(dataList = NULL, infoList = NULL) {

  logger.debug("----- createTextList() -----")

  MazamaCoreUtils::stopIfNull(dataList, "Required parameter 'dataList' is missing.")
  MazamaCoreUtils::stopIfNull(infoList, "Required parameter 'infoList' is missing.")

  # Commonly used labels
  textList <- list(
    AQI_category_labels = AQI_es$names,
    AQI_legend_title = 'Nivel AQI',
    date = 'Fecha',
    pm25_name = 'PM2.5'
  )

  return(textList)
  
}

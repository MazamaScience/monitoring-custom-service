########################################################################
# dailyaveragetable/textList_en.R
#
# English language text strings.
#
# Author: Mazama Science
########################################################################

createTextList <- function(dataList = NULL, infoList = NULL) {

  logger.debug("----- createTextList() -----")

  MazamaCoreUtils::stopIfNull(dataList)
  MazamaCoreUtils::stopIfNull(infoList)

  # Commonly used labels
  textList <- list(
    AQI_category_labels = AQI_en$names,
    AQI_legend_title = 'AQI Level',
    date = 'Date',
    pm25_name = 'PM2.5'
  )

  return(textList)

}

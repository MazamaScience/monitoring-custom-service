########################################################################
# tarnayplot/textList_es.R
#
# Spanish language text strings.
#
# Author: Jonathan Callahan
########################################################################

createTextList <- function(dataList=NULL, infoList=NULL) {

  logger.trace("----- createTextList() -----")

  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call. = FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)

  # Commonly used labels
  textList <- list(
    AQI_category_labels = c('Buena','Moderada','IGS','Insalubre','Muy Insalubre','Peligrosa'),
    AQI_legend_title = 'Nivel AQI',
    date = 'Fecha',
    pm25_name = 'PM2.5'
  )

  return(textList)
}

########################################################################
# textList_en.R
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
  
  # Title slide
  textList$ts_title <- "Testing Generation of PowerPoint"
  textList$ts_sub <- "with a subtitle"
  
  # Footer for all slides
  textList$footer <- "footer goes here"
  
  # Summary table slide
  textList$sumTbl_title <- "Selected Monitors"
  
  # Daily Barplot slide
  textList$dbp_title <- 'PM2.5 Daily Mean and Hourly Max'
  
  return(textList)
  
}

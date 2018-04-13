# For testing

if ( FALSE ) {
  
  dataList <- list()
  infoList <- list(size=500)
  textList <- list()
  
  
  png("aqi.png", infoList$size, infoList$size)
  AQILegend(dataList, infoList, textList)
  dev.off()
  
}


AQILegend <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.info("----- AQILegendPlot() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  cexTitle = infoList$size/150
  cexBig = infoList$size/250
  par(mar = c(0,0,0,0))
  plot(c(0,7),c(0,7), axes=FALSE, col="transparent", ann=FALSE)
  text(3.5, 6.75, "Air Quality Index", cex = cexTitle, font = 2)
  for ( i in 1:length(AQI$colors) ) {
    rect(.1,i-1+.1,.9,i-.1, col = AQI$colors[i], border = 1, lwd = 3)
  }
  
  # TODO:  Remove this when upgrading to PWFSLSmoke >= 1.0.19
  Titles <- c(
    "Hazardous",
    "Very Unhealthy",
    "Unhealthy",
    "Unhealthy for Sensitive Groups",
    "Moderate",
    "Good" 
  )
  
  
  for (i in 1:6) {
    text(1.2, i-.35, Titles[7-i], cex = cexBig, adj = c(0, 1))
  }
  
  
}

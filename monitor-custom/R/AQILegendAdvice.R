# For testing

if ( FALSE ) {
  
  dataList <- list()
  infoList <- list(size=700)
  textList <- list()
  
  
  png("aqiadvice.png", infoList$size, infoList$size)
  AQILegendAdvice(dataList, infoList, textList)
  dev.off()
  
}


AQILegendAdvice <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.info("----- AQILegendAdvice() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  cexTitle = infoList$size/150
  cexSub = infoList$size/300
  cexBig = infoList$size/375
  cexSmall = infoList$size/425
  par(mar = c(0,0,0,0))
  plot(c(0,7),c(0,7), axes=FALSE, col="transparent", ann=FALSE)
  text(3.5, 6.75, "Air Quality Index", cex = cexTitle, font = 2)
  text(3.5, 6.25, "Actions to protect yourself", cex = cexSub)
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
  Advice1 <- c(
    "None",
    "Unusually sensitive individuals should consider limiting",
    "People within Sensitive Groups should reduce prolonged",
    "People within Sensitive Groups should avoid all physical",
    "Everyone should avoid prolonged or heavy exertion.",
    "Everyone should avoid any outdoor activity."
  )
  
  Advice2 <- c(
    "",
    "prolonged or heavy exertion.",
    "or heavy outdoor exertion.",
    "outdoor activity.",
    "",
    ""
  )
  
  
  for (i in 1:6) {
    text(1, i-.15, Titles[7-i], cex = cexBig, font= 2, adj = c(0, 1))
  }
  for (i in 1:6) {
    text(1, i-.45, Advice1[i],  cex = cexSmall, adj = c(0, 1))
  }
  for (i in 2:4) {
    text(1, i-.7, Advice2[i], cex = cexSmall, adj = c(0, 1))
  }
  
  
  
  
}

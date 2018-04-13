#' @title Create Map of Monitor Location
#' @param ws_monitor emphws_monitor object
#' @param size size in pixels of the image to be saved
#' @param maptype Defines type of map for basemap. Maptype values include: "roadmap", "satellite", "terrain", "hybrid", "mobile". Defaults to "roadmap".
#' @param pincolor Specifies color of pin at monitor location. Options include 'yellow', 'blue', 'green', 'lightblue', 'orange', 'pink', 'purple', 'red'. 
#' @description Creates map of monitor location using Google Maps imagery.
#' @return plot image as .png

locationMap <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.info("----- locationMap() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  # ----- Extract variables from the 'dataList' object ------------------------
  ws_monitor <- dataList$ws_monitor

  # Get lat/lon of monitor
  latitude <- ws_monitor$meta$latitude
  longitude <- ws_monitor$meta$longitude
  
  # Specify marker parameters
  marker <- paste0("&markers=color:", infoList$pincolor, "|",as.character(latitude),",",as.character(longitude))
  
  # Make the image
  gmap <- RgoogleMaps::GetMap(maptype = infoList$maptype,
                              zoom = infoList$zoom, 
                              size = c(infoList$plotwidth, infoList$plotheight), 
                              center = c(latitude, longitude), 
                              markers = marker)
  monitormap <- RgoogleMaps::PlotOnStaticMap(MyMap = gmap)

}


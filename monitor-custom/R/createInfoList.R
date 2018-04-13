########################################################################
# createInfoList.R
#
# Create an infoList from a jug request object.
#
# Besides basic conversion from strings to other data types, a lot of
# specific choices can made here that will be used later on in different
# plotting scripts.
#
# Author: Helen Miller, Jonathan Callahan
########################################################################

createInfoList <- function(req=NULL) {
  
  logger.info("----- createInfoList() -----")
  
  if ( is.null(req) ) stop(paste0("Required parameter 'req' is missing."), call.=FALSE)
  
  # Initialize the infoList from the request parameters
  infoList <- req$params
  names(infoList) <- tolower(names(infoList))
  
  logger.debug("req$params")
  logger.debug(capture.output(str(req$params)))
  
  # ----- Check for required parameters ----------------------------------------
  
  result <- try({
    if ( is.null(infoList$monitorid) && infoList$plottype != "aqilegend" && infoList$plottype != "aqilegendadvice" ) {
      stop("monitorID must be specified", call. = FALSE)
    }
  }, silent = TRUE)
  if ("argument is of length zero" %in% result[1] && infoList$plottype != "aqilegend" && infoList$plottype != "aqilegendadvice" ){
    stop("monitorID must be specified")
  }
  stopOnError(result)
  
  # Make sure plottype is specified
  if ( is.null(infoList$plottype) ) {
    stop("plottype must be specified", call. = FALSE)
  } 
  infoList$plottype <- tolower(infoList$plottype)
  
  # ----- Minumum set of infoList parameters from the UI -----------------------
  
  # Set defaults 
  infoList$language <- tolower(ifelse(is.null(infoList$language),'en',infoList$language))
  infoList$responsetype <- tolower(ifelse(is.null(infoList$responsetype), "png", infoList$responsetype))
  infoList$lookbackdays <- ifelse(is.null(infoList$lookbackdays), 7, as.numeric(infoList$lookbackdays))
  infoList$maptype <- tolower(ifelse(is.null(infoList$maptype), "roadmap", infoList$maptype))
  infoList$zoom <- ifelse(is.null(infoList$zoom), 9, as.numeric(infoList$zoom))
  infoList$pincolor <- tolower(ifelse(is.null(infoList$pincolor), "red", infoList$pincolor))
  infoList$style <- tolower(ifelse(is.null(infoList$style), "web", infoList$style))
  infoList$size <- ifelse(is.null(infoList$size), 
                          ifelse(infoList$style == "mobile", 
                                 350,
                                 ifelse(infoList$plottype == "locationmap", 250, 700)), 
                          as.numeric(infoList$size))
  
  # NOTE:  Max size for Google plots is 640:
  if ( infoList$plottype == "locationmap" && infoList$size > 640 ) {
    logger.info("Resizing locationmap from %d to 640.", infoList$size)
    infoList$size = 640
  }
  
  # NOTE:  plotwidth and plotheight are undocumented parameters for expert use only
  
  # Use plotwidth and plotHeight if they are passed in. Otherwise just use size.
  infoList$plotwidth <- ifelse(is.null(infoList$plotwidth), infoList$size, as.numeric(infoList$plotwidth))
  infoList$plotheight <- ifelse(is.null(infoList$plotheight), infoList$size, as.numeric(infoList$plotheight))
  
  # Make sure all specified parameters are allowed
  if ( !infoList$pincolor %in% c('yellow', 'blue', 'green', 'orange', 'pink', 'purple', 'red', 'brown', 'black', 'gray', 'white') ){
    stop("invalid pincolor", call. = FALSE)
  }
  if ( !infoList$maptype %in% c("roadmap", "satellite", "terrain", "hybrid", "mobile") ) {
    stop("invalid maptype", call. = FALSE)
  }
  if ( !infoList$responsetype %in% c('json', 'png') ) {
    stop("invalid responsetype", call. = FALSE)
  }
  if ( !infoList$style %in% c("web", "mobile") ) {
    stop("invalid style", call. = FALSE)
  }
  
  # TODO:  Sort out what's going on with startdate, enddate, in this next chunk.
  # TODO:  Shouldn't enddate, perhaps in string format, be part of infoList?
  
  # # Set tlim 
  # endtime <- strptime(infoList$enddate, format = "%Y%m%d")
  # starttime <- endtime - lubridate::days(infoList$lookbackdays-1)
  # startdate <- strftime(starttime, format = "%Y%m%d")
  
  # infoList$tlim <- c(startdate,infoList$enddate)
  # 
  
  # NOTE:  enddate is specified here for creating the uniqueList. Enddate for plotting is specified
  # NOTE:  in the plotting function using localTime to set the default.
  timeStamp <- ifelse(is.null(infoList$enddate), strftime(lubridate::now(tzone = "UTC"), format="%Y%m%d%H", tz = "UTC"), infoList$enddate)
  
  infoList$enddate <- ifelse(is.null(infoList$enddate), strftime(lubridate::now(tzone = "UTC"), format="%Y%m%d", tz = "UTC"), infoList$enddate)
  
  # 
  # endtime <- parseDatetime(infoList$enddate)
  # 
  # endtime <- lubridate::now() 
  # starttime <- lubridate::now(tzone = ws_monitor$meta[monitorID, "timezone"]) - lubridate::days(lookbackDays-1)
  # lubridate::hour(starttime) <- 0
  # lubridate::minute(starttime) <- 0
  # lubridate::second(starttime) <- 0
  # tlim = c(starttime, endtime)
  # 
  # infoList$tlim <- tlim
  
  # Create uniqueID based on parameters that affect the plot
  
  if (infoList$plottype == "aqilegend" || infoList$plottype == "aqilegendadvice") {
    uniqueList <- list(infoList$plottype,
                       infoList$size)
  } else if (infoList$plottype == "locationmap" || infoList$plottype == "esrilocationmap") {
    uniqueList <- list(infoList$plottype,
                       infoList$style,
                       infoList$monitorid,
                       infoList$size,
                       infoList$pincolor,
                       infoList$maptype,
                       infoList$zoom)
  } else {
    uniqueList <- list(infoList$plottype,
                       infoList$style,
                       infoList$monitorid,
                       infoList$size,
                       infoList$lookbackdays,
                       timeStamp)
  }
  
  
  infoList$uniqueID <- digest::digest(uniqueList, algo="md5")
  
  return(infoList)
  
}

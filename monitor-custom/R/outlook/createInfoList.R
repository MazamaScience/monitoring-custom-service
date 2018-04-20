########################################################################
# outlook/createInfoList.R
#
# Create an infoList from a jug request object.
#
# Besides basic conversion from strings to other data types, a lot of
# specific choices can made here that will be used later on in different
# plotting scripts.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createInfoList <- function(req = NULL,
                           cacheDir = NULL) {
  
  logger.trace("----- createInfoList() -----")
  
  if ( is.null(req) ) stop(paste0("Required parameter 'req' is missing."), call. = FALSE)
  if ( is.null(cacheDir) ) stop(paste0("Required parameter 'cacheDir' is missing."), call.=FALSE)
  
  # Initialize the infoList from the request parameters 
  infoList <- req$params
  names(infoList) <- tolower(names(infoList))
  
  logger.debug("req$params")
  logger.debug(capture.output(str(req$params)))
  
  # ----- Check for required parameters ----------------------------------------

  requiredParams <- c("outlookurl")
  
  # determine which needed parameters are missing
  missingParams <- requiredParams[!(requiredParams %in% names(infoList))]
  
  if (length(missingParams) > 0) {
    errortxt <- paste(
      "Required parameter(s) missing with no default:",
      paste0(missingParams, collapse = ', '))
      
    stop(errortxt, call. = FALSE)
  }
  
  # ----- Set parameter defaults ----------------------------------------------
  
  # Set defaults 
  infoList$language <- tolower(ifelse(is.null(infoList$language),'en',infoList$language))
  infoList$responsetype <- tolower(ifelse(is.null(infoList$responsetype), "raw", infoList$responsetype))
  infoList$plottheme <- tolower(ifelse(is.null(infoList$plottheme), "base", infoList$plottheme))
  
  # NOTE:  plotwidth and plotheight are undocumented parameters for expert use only
  # Use plotwidth and plotHeight if they are passed in.
  infoList$plotwidth <- ifelse(is.null(infoList$plotwidth), 1920, as.numeric(infoList$plotwidth))
  infoList$plotheight <- ifelse(is.null(infoList$plotheight), 1080, as.numeric(infoList$plotheight))
  
  # Validate parameters
  if ( !infoList$language %in% c('en','es') ) { stop("invalid language", call. = FALSE) }
  if ( !infoList$responsetype %in% c('raw', 'json') ) { stop("invalid responsetype", call. = FALSE) }

  # ----- Get elements from the Outlook ---------------------------------------
  
  outlookList <- jsonlite::fromJSON(infoList$outlookurl)
  # For testing:
  #outlookList <- jsonlite::fromJSON("https://wildlandfiresmoke.net/ara/deployments/2017/SCaliforniaFires/outlooks/20171210-SCaliforniaFiresMain-Outlook.20171210164719.json")
  
  # Get the monitor IDs
  monitorIDs <- outlookList[["outlook"]][["monitorIDs"]]
  monitorids <- paste0(monitorIDs, collapse = ",")
  
  # Get comment and predictions for each site
  monitorComments <- list()
  AQIPredictions_tm1 <- list()
  AQIPredictions_t <- list()
  AQIPredictions_tp1 <- list()
  
  for (monitor in outlookList[["monitors"]]) {
    
    comment <- monitor$comment
    monitorComments[[monitor$monitorID]] <- comment
    
    AQIPredictions_tm1[[monitor$monitorID]] <- monitor$today_m1_aqiLevel
    AQIPredictions_t[[monitor$monitorID]] <- monitor$today_aqiLevel
    AQIPredictions_tp1[[monitor$monitorID]] <- monitor$today_p1_aqiLevel
    
  }
  
  # Get outlook discussion
  outlookDiscussion <- list(
    fire = outlookList[["outlook"]][["discussion_fire"]],
    smoke = outlookList[["outlook"]][["discussion_smoke"]],
    other = outlookList[["outlook"]][["discussion_other"]]
  )
  
  # Get meta information
  outlookMeta <- list(
    author = outlookList[["outlook"]][["author"]],
    deploymentTitle = outlookList[["outlook"]][["deploymentTitle"]],
    outlookTitle = outlookList[["outlook"]][["outlookTitle"]]
  )
  
  # Get date
  outlookDate <- 
    outlookList[["outlook"]][["today"]] %>% 
    lubridate::ymd() %>% 
    strftime(format = "%Y%m%d", tz = "UTC")
  
  lookBackDays <- 3

  # Get map icons
  iconIndicies <- which(outlookList[["map"]][["iconLocation"]] != "")
  mapIcons <- list(
    coor = outlookList[["map"]][["iconLocation"]][iconIndicies],
    iconName = outlookList[["map"]][["icon"]][iconIndicies]
  )
  
  # Get map coordinates
  mapCenter <- list(
    lon = outlookList[["map"]][["centerLon"]],
    lat = outlookList[["map"]][["centerLat"]],
    zoom = outlookList[["map"]][["zoom"]]
  )
  
  # Add everything to infoList
  infoList$monitorIDs <- monitorIDs
  infoList$monitorids <- monitorids
  infoList$enddate <- outlookDate
  infoList$lookbackdays <- lookBackDays
  infoList$outlookDiscussion <- outlookDiscussion
  infoList$monitorComments <- monitorComments
  infoList$AQIPredictions_tm1 <- AQIPredictions_tm1
  infoList$AQIPredictions_t <- AQIPredictions_t
  infoList$AQIPredictions_tp1 <- AQIPredictions_tp1
  infoList$mapCenter <- mapCenter
  infoList$mapIcons <- mapIcons
  infoList$outlookMeta <- outlookMeta

  endtime <- parseDatetime(infoList$enddate)
  starttime <- endtime - lubridate::days(infoList$lookbackdays)
  infoList$startdate <- strftime(starttime, format = "%Y%m%d", tz = "UTC")
  infoList$tlim <- c(infoList$startdate,infoList$enddate)
  
  # ----- Create uniqueID based on parameters that affect the presentation ----
  
  uniqueList <- list(
    infoList$outlookurl,
    infoList$language,
    infoList$plottheme)
  
  infoList$uniqueID <- digest::digest(uniqueList, algo = "md5")

  # Create paths 
  infoList$basePath <- paste0(cacheDir,"/",infoList$uniqueID)
  infoList$pptxPath <- paste0(infoList$basePath,".pptx")
  infoList$jsonPath <- paste0(infoList$basePath,".json")
  
  # Create plot sizing information
  infoList$units <- "in"
  infoList$dpi <- 150
  infoList$width <- infoList$plotwidth / infoList$dpi
  infoList$height <- infoList$plotheight / infoList$dpi
  
  return(infoList)
  
}

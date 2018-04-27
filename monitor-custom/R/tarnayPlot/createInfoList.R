########################################################################
# tarnayplot/createInfoList.R
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

  if (is.null(req)) stop(paste0("Required parameter 'req' is missing."), call. = FALSE)
  if (is.null(cacheDir)) stop(paste0("Required parameter 'cacheDir' is missing."), call. = FALSE)

  # Initialize the infoList from the request parameters
  infoList <- req$params
  names(infoList) <- tolower(names(infoList))

  logger.debug("req$params")
  logger.debug(capture.output(str(req$params)))

  # ----- Check for required parameters ----------------------------------------

  requiredParams <- c("monitorIDs")

  if ("monitorIDs" %in% requiredParams) {
    infoList <- setMonitorIDs(infoList)
  }

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
  infoList$lookbackdays <- ifelse(is.null(infoList$lookbackdays), 7, trunc(as.numeric(infoList$lookbackdays)))
  infoList$plottheme <- tolower(ifelse(is.null(infoList$plottheme), "base", infoList$plottheme))

  # NOTE:  plotwidth and plotheight are undocumented parameters for expert use only
  # Use plotwidth and plotHeight if they are passed in.
  infoList$plotwidth <- ifelse(is.null(infoList$plotwidth), 1920, as.numeric(infoList$plotwidth))
  infoList$plotheight <- ifelse(is.null(infoList$plotheight), 1080, as.numeric(infoList$plotheight))

  # Validate parameters
  if (!infoList$language %in% c('en','es')) { stop("invalid language", call. = FALSE) }
  if (!infoList$responsetype %in% c('raw', 'json')) { stop("invalid responsetype", call. = FALSE) }
  if (infoList$lookbackdays < 2 ) { infoList$lookbackdays <- 2 }

  # TODO:  Sort out what's going on with startdate, enddate, in this next chunk.
  # TODO:  Shouldn't enddate, perhaps in string format, be part of infoList?
  # TODO:  Don't we need to include the hour so that this plot gets regenerated once per hour

  # NOTE:  enddate is specified here for creating the uniqueList. Enddate for plotting is specified
  # NOTE:  in the plotting function using localTime to set the default.

  infoList$enddate <- ifelse(is.null(infoList$enddate), strftime(lubridate::now(tzone = "UTC"), format = "%Y%m%d", tz = "UTC"), infoList$enddate)

  endtime <- parseDatetime(infoList$enddate)
  starttime <- endtime - lubridate::days(infoList$lookbackdays)
  infoList$startdate <- strftime(starttime, format = "%Y%m%d", tz = "UTC")
  infoList$tlim <- c(infoList$startdate, infoList$enddate)

  # ----- Create uniqueID based on parameters that affect the presentation ----

  uniqueList <- list(
    infoList$monitorids,
    infoList$language,
    infoList$plottheme,
    infoList$startdate,
    infoList$enddate)

  infoList$uniqueID <- digest::digest(uniqueList, algo = "md5")

  # Create paths
  infoList$basePath <- paste0(cacheDir, "/", infoList$uniqueID)
  infoList$plotPath <- paste0(infoList$basePath, ".png")
  infoList$jsonPath <- paste0(infoList$basePath, ".json")

  # Create plot sizing information
  infoList$units <- "in"
  infoList$dpi <- 150
  infoList$width <- infoList$plotwidth / infoList$dpi
  infoList$height <- infoList$plotheight / infoList$dpi

  return(infoList)

}

################################################################################
# dailyaveragetable/createInfoList.R
#
# Create an infoList from a jug request object.
#
# Besides basic conversion from strings to other data types, a lot of
# specific choices can made here that will be used later on in different
# plotting scripts.
#
# Author: Spencer Pease, Jonathan Callahan
################################################################################

createInfoList <- function(req = NULL,
                           cacheDir = NULL) {

  logger.debug("----- createInfoList() -----")

  # ----- Setup ----------------------------------------------------------------

  MazamaCoreUtils::stopIfNull(req)
  MazamaCoreUtils::stopIfNull(cacheDir)

  # Initialize the infoList from the request parameters
  infoList <- req$parameters
  names(infoList) <- tolower(names(infoList))

  logger.trace("req$parameters")
  logger.trace(capture.output(str(req$parameters)))

  # ----- Check for required parameters ----------------------------------------

  requiredParams <- c("monitorIDs")

  # Convert various specifications of monitors into a vector of monitorIDs
  # appropriate for use with the PWFSLSmoke package.

  if ( "monitorIDs" %in% requiredParams ) {
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

  # ----- Create default infoList parameters -----------------------------------

  infoList$language <-
    MazamaCoreUtils::setIfNull(infoList$language, "en") %>% tolower()

  infoList$responsetype <-
    MazamaCoreUtils::setIfNull(infoList$responsetype, "raw") %>% tolower()

  infoList$days <-
    MazamaCoreUtils::setIfNull(infoList$days, 7) %>% as.numeric()

  # Support older API
  if ( "lookbackdays" %in% names(infoList) ) {
    infoList$days <-
      MazamaCoreUtils::setIfNull(infoList$lookbackdays, 7) %>% as.numeric()
  }

  infoList$outputfiletype <-
    MazamaCoreUtils::setIfNull(infoList$outputfiletype, "xlsx") %>% tolower()

  if ( is.null(infoList$timezone) ) {

    infoList$timezone <- "UTC"

  } else {

    ## Note:
    #  Oslon Names are case-sensitive, but each entry is unique regardless of
    #  case. So, we can match ignoring case and look up to index to get the
    #  properly cased name.

    tzIndex <- which(tolower(OlsonNames()) %in% tolower(infoList$timezone))

    if (identical(tzIndex, integer())) {
      stop(paste0("'", infoList$timezone, "' is not a valid timezone."))
    } else {
      infoList$timezone <- OlsonNames()[tzIndex]
    }

  }

  infoList$useaqi <-
    MazamaCoreUtils::setIfNull(infoList$useaqi, "false") %>% tolower()

  # ----- Validate parameters --------------------------------------------------

  # Validate parameters
  if (!infoList$language %in% c("en","es")) { stop("invalid language", call. = FALSE) }
  if (!infoList$responsetype %in% c("raw", "json")) { stop("invalid responsetype", call. = FALSE) }
  if (!infoList$outputfiletype %in% c("png", "pdf", "xlsx")) { stop("invalid file format", call. = FALSE) }
  if (infoList$days < 2 ) { infoList$days <- 2 }
  if (!infoList$useaqi %in% c("true", "false")) { stop("invalid useaqi value. Must be 'true' or 'false'", call. = FALSE) }

  # Handle plot sizing

  if (infoList$outputfiletype == "pdf") {

    # PDF defaults to 8.5 x 11 in page
    infoList$dpi <- ifelse(is.null(infoList$dpi), 300, as.numeric(infoList$dpi))
    infoList$width <- ifelse(is.null(infoList$width), 8.5, as.numeric(infoList$width))
    infoList$height <- ifelse(is.null(infoList$height), 11, as.numeric(infoList$height))

  } else if (infoList$outputfiletype == "png") {

    # png defaults to 800 x 800 px image
    infoList$dpi <- ifelse(is.null(infoList$dpi), 100, as.numeric(infoList$dpi))
    infoList$width <- ifelse(is.null(infoList$width), 8, as.numeric(infoList$width))
    infoList$height <- ifelse(is.null(infoList$height), 8, as.numeric(infoList$height))
  }

   # ----- Create start and end dates -------------------------------------------

  dateRange <- MazamaCoreUtils::dateRange(
    startdate = infoList$startdate,
    enddate = infoList$enddate,
    timezone = infoList$timezone,
    unit = "hour",
    ceilingStart = FALSE,
    ceilingEnd = TRUE,
    days = infoList$days
  )

  # NOTE:  dateRange has day boundaries. Reset dateRange[2] to "now" if infoList
  # NOTE:  did not originally contain startdate and enddate parameters. (default)

  if ( is.null(infoList$startdate) && is.null(infoList$enddate) ) {
    dateRange[2] <- lubridate::now(tzone = infoList$timezone)
  }

  infoList$startdate <- dateRange[1]
  infoList$enddate <- dateRange[2]

  infoList$tlim <- dateRange

  # ----- Create timestamp for caching -----------------------------------------

  ## DETAILS:
  #  If enddate is the end of the current day, create an every-5-minutes
  #  timestamp so that we catch rapid updates to the data throughout the day,
  #  while still benefiting from cache hits.

  # NOTE:  To improve cache hits, this is not used for seldom-requested products.
  timestamp <- ifelse(
    infoList$enddate <= lubridate::now(tzone = infoList$timezone),
    infoList$enddate,
    lubridate::floor_date(lubridate::now(tzone = infoList$timezone), "5 mins")
  )

  # ----- Create uniqueID based on parameters that affect the presentation ----

  uniqueList <- list(
    infoList$monitorIDs,
    infoList$startdate,
    infoList$enddate,
    infoList$language,
    infoList$outputfiletype,
    infoList$useaqi,
    timestamp
  )

  infoList$uniqueID <- digest::digest(uniqueList, algo = "md5")

  # Create paths
  infoList$basePath <- paste0(cacheDir, "/", infoList$uniqueID)
  infoList$plotPath <- paste0(infoList$basePath, ".", infoList$outputfiletype)
  infoList$jsonPath <- paste0(infoList$basePath, ".json")

  return(infoList)

}

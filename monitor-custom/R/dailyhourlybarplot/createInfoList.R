################################################################################
# dailyhourlybarplot/createInfoList.R
#
# Create an infoList from a beaker request object.
#
# Besides basic conversion from strings to other data types, a lot of
# specific choices can made here that will be used later on in different
# plotting scripts.
#
# Author: Tate Brasel, Spencer Pease, Jonathan Callahan
################################################################################

createInfoList <- function(req = NULL, cacheDir = NULL) {

  logger.debug("----- createInfoList() -----")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(req)
  MazamaCoreUtils::stopIfNull(cacheDir)

  # Initialize the infoList from the request parameters
  infoList <- req$parameters
  names(infoList) <- tolower(names(infoList))

  logger.trace("req$parameters")
  logger.trace(capture.output(str(req$parameters)))

  # ----- Check for required parameters ----------------------------------------

  requiredParams <- c("monitorids")

  # Convert various specifications of monitors into a vector of monitorIDs
  # appropriate for use with the PWFSLSmoke package.

  if ( "monitorids" %in% requiredParams ) {
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

  infoList$includelink <-
    MazamaCoreUtils::setIfNull(infoList$includelink, "true") %>% tolower()

  infoList$hourlytype <-
    MazamaCoreUtils::setIfNull(infoList$hourlytype, "nowcast") %>% tolower()

  if ( is.null(infoList$columns) ) {
    if ( length(infoList$monitorids) <= 6 ) {
      infoList$columns = 1
    } else if ( length(infoList$monitorids) <= 12 ) {
      infoList$columns = 2
    } else {
      infoList$columns = 3
    }
  }

  infoList$includethirdcol <-
    MazamaCoreUtils::setIfNull(infoList$includethirdcol, "false") %>% tolower()

  # infoList$title should default to NULL
  # infoList$xlabel should default to NULL
  # infoList$ylabel should default to NULL

  infoList$outputfiletype <-
    MazamaCoreUtils::setIfNull(infoList$outputfiletype, "png") %>% tolower()

  infoList$units <-
    MazamaCoreUtils::setIfNull(infoList$units, "in") %>% tolower()

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

  # ----- Validate parameters --------------------------------------------------

  # Validate parameters
  if (!infoList$language %in% c("en","es")) { stop("invalid language", call. = FALSE) }
  if (!infoList$responsetype %in% c("raw", "json")) { stop("invalid responsetype", call. = FALSE) }
  if (!infoList$outputfiletype %in% c("png", "pdf")) { stop("invalid file format", call. = FALSE) }
  if (!infoList$units %in% c("in", "cm", "mm")) { stop("invalid units", call. = FALSE) }
  if (infoList$days < 2 ) { infoList$days <- 2 }
  if (!infoList$hourlytype %in% c("nowcast", "raw", "none")) { stop("invalid hourly data type", call. = FALSE) }

  if (tolower(infoList$includelink) == "true") {
    infoList$includelink <- TRUE
  } else if (tolower(infoList$includelink) == "false") {
    infoList$includelink <- FALSE
  } else if (!is(infoList$includelink, "logical")) {
    stop("includelink must be either 'true' or 'false'", call. = FALSE)
  }

  if (tolower(infoList$includethirdcol) == "true") {
    infoList$includethirdcol <- TRUE
  } else if (tolower(infoList$includethirdcol) == "false") {
    infoList$includethirdcol <- FALSE
  } else if (!is(infoList$includethirdcol, "logical")) {
    stop("includethirdcol must be either 'true' or 'false'", call. = FALSE)
  }

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
    dateRange[2] <-
      lubridate::now(tzone = infoList$timezone) %>%
      lubridate::floor_date(unit = "hour")
  }

  infoList$startdate <- dateRange[1]
  infoList$enddate <- dateRange[2]

  # ----- Create timestamp for caching -----------------------------------------

  # Create an every-5-minutes timestamp so that we catch rapid updates to the
  # data throughout the day while still benefiting from cache hits.

  if ( is.null(infoList$startdate) && is.null(infoList$enddate) ) {
    timestamp <-
    lubridate::floor_date(lubridate::now(tzone = "UTC"), "5 mins") %>%
    strftime("%Y%m%d%H%M", tz = "UTC")
  } else {
    # No need for rapid updates if we're using archival data
    timestamp = "DUMMY"
  }

  # ----- Create uniqueID based on parameters that affect the presentation ----

  uniqueList <- list(
    infoList$monitorids,
    infoList$startdate,
    infoList$enddate,
    infoList$language,
    infoList$columns,
    infoList$includelink,
    infoList$hourlytype,
    infoList$title,
    infoList$xlabel,
    infoList$ylabel,
    infoList$outputfiletype,
    infoList$includethirdcol,
    infoList$height,
    infoList$width,
    infoList$dpi,
    timeStamp
  )

  infoList$uniqueID <- digest::digest(uniqueList, algo = "md5")

  # Create paths
  infoList$basePath <- paste0(cacheDir, "/", infoList$uniqueID)
  infoList$plotPath <- paste0(infoList$basePath, ".", infoList$outputfiletype)
  infoList$jsonPath <- paste0(infoList$basePath, ".json")

  logger.trace("generated infoList:")
  logger.trace(capture.output(str(infoList)))

  return(infoList)

}

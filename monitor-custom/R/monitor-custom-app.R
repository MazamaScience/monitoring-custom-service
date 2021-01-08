################################################################################
# monitor-custom-app.R
#
# Author: Spencer Pease
#         Jonathan Callahan <jonathan@mazamscience.com>
#
# Top level web framework based on the beakr package.
#
# Source this script and a beakr instance will be running at http://localhost:8080
# until the script is ended.
#
# This script can be run inside a docker container to create an always-up web
# service.
#
# See:  https://github.com/MazamaScience/beakr
# See:  https://github.com/MazamaScience/MazamaCoreUtils
#
################################################################################

# ----- Libraries and scripts --------------------------------------------------

# NOTE:  Use library() so that these package versions will be documented by
# NOTE:  sessionInfo()

suppressPackageStartupMessages({
  library(readr)             # tidyverse file reading
  library(digest)            # creation of uniqueID
  # Mazama Science packages
  library(beakr)             # web service framework
  library(MazamaCoreUtils)   # cache management and more
  library(PWFSLSmoke)        # workhorse package for everything smoke related
  library(AirMonitorPlots)   # plotting functions for monitor data
})

R_files <- list.files("R/sharedUtils", pattern = ".+\\.R", full.names = TRUE)

for (file in R_files) {
  source(file.path(getwd(), file))
}

# Specify global (configurable) variables --------------------------------------

# V4 data files . beakr 0.3.1 . ----
VERSION <- "4.4.0"

# Set up configurable variables

if ( interactive()) { # Running from RStudio

  # beakr instance configuration
  BEAKR_HOST <- "127.0.0.1" # beakr default
  BEAKR_PORT <- "8080"      # beakr default

  # path and cache
  SERVICE_PATH <- "monitor-custom/dev"
  CACHE_SIZE <- 5 # megabytes

  # directories for log output, data, and cache
  DATA_DIR <- file.path(getwd(), "data")
  if ( !file.exists(DATA_DIR) ) dir.create(DATA_DIR)

  LOG_DIR <- file.path(getwd(), "logs")
  if ( !file.exists(LOG_DIR) ) dir.create(LOG_DIR)

  CACHE_DIR <- file.path(getwd(), "output")
  if ( !file.exists(CACHE_DIR) ) dir.create(CACHE_DIR)

  # Clean out the cache (only when running from RStudio)
  removalStatus <- file.remove( list.files(CACHE_DIR, full.names = TRUE) )

} else { # Running from Docker

  # beakr instance configuration
  BEAKR_HOST <- Sys.getenv("BEAKR_HOST")
  BEAKR_PORT <- Sys.getenv("BEAKR_PORT")

  # path and cache
  SERVICE_PATH <- Sys.getenv("SERVICE_PATH")
  CACHE_DIR <- Sys.getenv("CACHE_DIR")
  CACHE_SIZE <- Sys.getenv("CACHE_SIZE") # megabytes

  # directory for log output
  DATA_DIR <- Sys.getenv("DATA_DIR")
  LOG_DIR <- Sys.getenv("LOG_DIR")

}

# Silence other warning messages
options(warn = -1) # -1=ignore, 0=save/print, 1=print, 2=error


# ----- Set up Logging ---------------------------------------------------------

MazamaCoreUtils::initializeLogging(LOG_DIR)

if ( interactive() ) { # Running from RStudio
  logger.setLevel(TRACE)
}

# Capture session info
logger.debug(capture.output(sessionInfo()))

# Log environment variables
logger.debug('BEAKR_HOST = %s', BEAKR_HOST)
logger.debug('BEAKR_PORT = %s', BEAKR_PORT)
logger.debug('SERVICE_PATH = %s', SERVICE_PATH)
logger.debug('CACHE_DIR = %s', CACHE_DIR)
logger.debug('CACHE_SIZE = %s', CACHE_SIZE)
logger.debug('DATA_DIR = %s', DATA_DIR)
logger.debug('LOG_DIR = %s', LOG_DIR)


# ----- Define helper functions ------------------------------------------------

printUTC <- function(x) {
  strftime(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
}

# ----- BEGIN beakr app --------------------------------------------------------

beakr::newBeakr() %>%

  # * Return json dscription of this service -----------------------------------

  # regex matches zero or one final '/'
  beakr::httpGET(paste0("/", SERVICE_PATH, "/?$"), function(req, res, err) {

    logger.info("----- %s -----", req$path)

    json <- jsonlite::toJSON(
      createAPIList(SERVICE_PATH, VERSION),
      pretty = TRUE,
      auto_unbox = TRUE)
    res$setContentType("application/json")

    return(json)

  }) %>%

  # * Create Products ----------------------------------------------------------

  # NOTE:  All subservices are handled the same way. Each has its own subdirectory
  # NOTE:  with files that define the following top-level functions:
  # NOTE:   * createDataList
  # NOTE:   * createInfoList
  # NOTE:   * createProduct
  # NOTE:   * createTextList
  # NOTE:
  # NOTE:  This standard protocol allows the following chunk of code to be
  # NOTE:  run for every custom product.

  # regex matches alphanumerics and zero or one final '/'
  beakr::httpGET(paste0("/", SERVICE_PATH, "/[[:alnum:]]+/?"), function(req, res, err) {

    # Extract lowercase subservice name
    subservice <-
      stringr::str_replace(req$path, SERVICE_PATH, "") %>%
      stringr::str_replace_all("/", "") %>%
      stringr::str_to_lower()

    logger.info("----- %s -----", req$path)

    # Create subservice script paths
    infoListScript <- paste0("R/", subservice, "/createInfoList.R")
    dataListScript <- paste0("R/", subservice, "/createDataList.R")
    productScript <- paste0("R/", subservice, "/createProduct.R")

    # Source these scripts
    result <- try({
      source(infoListScript)        # function to convert request into infoList required by product
      source(dataListScript)        # function to load data required by product
      source(productScript)         # function to create product
    }, silent = TRUE)
    stopOnError(result)

    # Create infoList
    result <- try({
      infoList <- createInfoList(req, CACHE_DIR)
    }, silent = TRUE)
    stopOnError(result)

    # Create a new plot file if it isn't in the cache
    if ( !file.exists(infoList$plotPath) ) {

      # Manage the cache
      MazamaCoreUtils::manageCache(CACHE_DIR, c("json", "png", "pdf", "xlsx")) # TODO:  Other potential output formats?

      result <- try({

        # Get data and text for this product
        dataList <- createDataList(infoList, DATA_DIR)

        # Get language dependent plot labels
        textListScript <- paste(
          "R/", subservice, "/createTextList_", infoList$language, ".R",
          sep = "")
        source(textListScript)
        textList <- createTextList(dataList, infoList)

        # Create product
        createProduct(dataList, infoList, textList)

        logger.info("successfully created %s", infoList$plotPath)

      }, silent = TRUE)
      stopOnError(result)

    } # finished creating product file


    # Create a new json file if it isn't in the cache
    if (!file.exists(infoList$jsonPath)) {

      result <- try({

        logger.debug("writing %s", infoList$jsonPath)

        responseList <- list(
          status = "OK",
          rel_base = paste0(SERVICE_PATH, "/", infoList$basePath),
          plot_path = paste0(SERVICE_PATH, "/", infoList$plotPath)
        )

        json <- jsonlite::toJSON(
          responseList,
          na = "null",
          pretty = TRUE,
          auto_unbox = TRUE)
        write(json, infoList$jsonPath)
        logger.debug("successfully created %s", infoList$jsonPath)

      }, silent = TRUE)
      stopOnError(result)

    } # finished creating json file

    # Return the appropriate file based on infoList$responsetype
    result <- try({

      if (infoList$responsetype == "raw") {

        if (infoList$outputfiletype == "png") {
          res$setContentType("image/png")
        } else if (infoList$outputfiletype == "pdf") {
          res$setContentType("application/pdf")
        } else if (infoList$outputfiletype == "xlsx") {
          res$setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        }

        return(readr::read_file_raw(infoList$plotPath))


      } else if (infoList$responsetype == "json") {

        res$setContentType("application/json")
        return(readr::read_file(infoList$jsonPath))

      } else {

        err_msg <- paste0("Invalid responsetype: ", infoList$responsetype)
        stop(err_msg, call. = FALSE)

      }

    }, silent = TRUE)
    stopOnError(result)

  }) %>%

  # * Serve static files -------------------------------------------------------

  beakr::serveStaticFiles(SERVICE_PATH) %>%

  # * Handle errors ------------------------------------------------------------

  beakr::handleErrors() %>%

  # * Return -------------------------------------------------------------------

  beakr::listen(host = BEAKR_HOST, port = as.integer(BEAKR_PORT))


# ----- END beakr app -----------------------------------------------------------

################################################################################
# monitor-custom-app.R
#
# Author: Spencer Pease
#         Jonathan Callahan <jonathan@mazamscience.com>
#
# Top level web framework based on the jug package.
#
# Source this script and a jug instance will be running at http://localhost:8080
# until the script is ended.
#
# This script can be run inside a docker container to create an always-up web
# service.
#
# See:  https://github.com/MazamaScience/jug
# See:  https://github.com/MazamaScience/MazamaWebUtils
#
################################################################################

# Specficic packages and scripts for this service -----------------------------

# NOTE:  Use library() so that these package versions will be documented by
#        sessionInfo()

suppressPackageStartupMessages({
  library(methods)                # always included for Rscripts
  library(jug)                    # web service framework
  library(MazamaWebUtils)         # cache management
  library(digest)                 # creation of uniqueID
  library(stringr)                # manipulation of data in InfoList

  library(PWFSLSmoke)             # workhorse package for everything smoke
                                  #   related. Includes magrittr and dplyr
  library(PWFSLSmokePlots)        # Custom plots for ws_monitor data
})

# Load all shared utility functions
#   - createAPIList: create a list of API parameters
#   - setMonitorIDs: convert monitor ids t standard format
#   - stopOnError:   error handling/translation
# Additional files are sourced inside of <subservice>/createPlot.R

utilFiles <- list.files("R/sharedUtils", pattern = ".+\\.R", full.names = TRUE)

for (file in utilFiles) {
  source(file.path(getwd(), file))
}

# Specify global (configurable) variables -------------------------------------

VERSION <- "1.0.2"

# Set up configurable variables

if (Sys.getenv("JUG_HOST") == "") { # Running from RStudio

  # jug instance configuration
  JUG_HOST <- "127.0.0.1" # jug default
  JUG_PORT <- "8080"      # jug default

  # path and cache
  SERVICE_PATH <- "monitor-custom/dev"
  CACHE_SIZE <- 100 # megabytes

  # directories for log output, data, and cache
  DATA_DIR <- file.path(getwd(), "data")
  if (!file.exists(DATA_DIR)) dir.create(DATA_DIR)
  LOG_DIR <- file.path(getwd(), "logs")
  if (!file.exists(LOG_DIR)) dir.create(LOG_DIR)
  CACHE_DIR <- file.path(getwd(), "output")
  if (!file.exists(CACHE_DIR)) dir.create(CACHE_DIR)

} else { # Running from Docker

  # jug instance configuration
  JUG_HOST <- Sys.getenv("JUG_HOST")
  JUG_PORT <- Sys.getenv("JUG_PORT")

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


# ----- Set up Logging --------------------------------------------------------

result <- try({
  # Set up logging
  logger.setup(debugLog = file.path(LOG_DIR, "DEBUG.log"),
               infoLog = file.path(LOG_DIR, "INFO.log"),
               errorLog = file.path(LOG_DIR, "ERROR.log"))
}, silent = TRUE)
stopOnError(result, "Could not create log files.")

if (Sys.getenv("JUG_HOST") == "") { # Running from RStudio
  logger.setLevel(TRACE)            # send error messages to console (RStudio)
}

# Capture session info
logger.debug(capture.output(sessionInfo()))


# ----- BEGIN jug app ---------------------------------------------------------

jug() %>%

  # Return json dscription of this service ------------------------------------

  # regex matches zero or one final '/'
  get(paste0("/", SERVICE_PATH, "/?$"), function(req, res, err) {

    logger.info("----- %s -----", SERVICE_PATH)

    json <- jsonlite::toJSON(
      createAPIList(SERVICE_PATH, VERSION),
      pretty = TRUE,
      auto_unbox = TRUE)
    res$content_type("application/json")

    return(json)

  }) %>%

  # Return json dscription of this service ------------------------------------

  # regex ignores capitalization and matches zero or one final '/'
  get(paste0("/", SERVICE_PATH, "/[Aa][Pp][Ii]/?$"), function(req, res, err) {

    logger.info("----- %s/api -----", SERVICE_PATH)

    json <- jsonlite::toJSON(
      createAPIList(SERVICE_PATH, VERSION),
      pretty = TRUE,
      auto_unbox = TRUE)

    res$content_type("application/json")

    return(json)

  }) %>%

  # Products -----------------------------------------------------------------

  # regex matches alphanumerics and zero or one final '/'
  get(paste0("/", SERVICE_PATH, "/[[:alnum:]]+/?"), function(req, res, err) {

    subservice <-
      stringr::str_replace(req$path, SERVICE_PATH, "") %>%
      stringr::str_replace_all("/", "") %>%
      stringr::str_to_lower()

    logger.info("----- %s -----", req$path)

    # Create subservice script paths
    infoListScript <- paste0("R/", subservice, "/createInfoList.R")
    dataListScript <- paste0("R/", subservice, "/createDataList.R")
    productScript <- paste0("R/", subservice, "/createPlot.R")

    # Source these scripts
    result <- try({
      source(infoListScript)        # function to convert request into infoList
                                    #   required by product
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
    if (!file.exists(infoList$plotPath)) {

      # Manage the cache
      MazamaWebUtils::manageCache(CACHE_DIR, c("json", "png", "pdf"))

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
        createPlot(dataList, infoList, textList)

        logger.info("successfully created %s", infoList$plotPath)

      }, silent = TRUE)
      stopOnError(result)

    } # finished creating plot file


    # Create a new json file if it isn't in the cache
    if (!file.exists(infoList$jsonPath)) {

      result <- try({

        logger.debug("writing %s", infoList$jsonPath)

        responseList <- list(
          status <- "OK",
          rel_base <- paste0(SERVICE_PATH, "/", infoList$basePath),
          plot_path <- paste0(SERVICE_PATH, "/", infoList$plotPath)
        )

        json <- jsonlite::toJSON(
          responseList,
          na = "null",
          pretty = TRUE,
          auto_unbox = TRUE)
        write(json, infoList$jsonPath)
        logger.info("successfully created %s", infoList$jsonPath)

      }, silent = TRUE)
      stopOnError(result)

    } # finished creating json file

    # Return the appropriate file based on infoList$responsetype
    result <- try({

      if (infoList$responsetype == "raw") {

        if (infoList$output == "png") {
          res$content_type("image/png")
        } else if (infoList$output == "pdf") {
          res$content_type("application/pdf")
        }

        return(readr::read_file_raw(infoList$plotPath))


      } else if (infoList$responsetype == "json") {

        res$content_type("application/json")
        return(readr::read_file(infoList$jsonPath))

      } else {

        err_msg <- paste0("Invalild responsetype: ", infoList$responsetype)
        stop(err_msg, call. = FALSE)

      }

    }, silent = TRUE)
    stopOnError(result)

  }) %>%

  # Serve static files --------------------------------------------------------

  # NOTE:  As of jug 0.1.7.900, the serve_static_files() function removes any
  # NOTE:  'path' argument from 'req$path' and this can be used to remove the
  # NOTE:  ProxyPass settings in the apache config file. Works quite nicely.

  serve_static_files(SERVICE_PATH) %>%

  # Error handling ------------------------------------------------------------
  simple_error_handler_json() %>%

  # Return --------------------------------------------------------------------
  serve_it(host = JUG_HOST, port = as.integer(JUG_PORT))


# ----- END jug app -----------------------------------------------------------

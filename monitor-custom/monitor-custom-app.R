################################################################################
# monitor-custom-app.R
#
# Author: Helen Miller 
#         Jonathan Callahan <jonathan@mazamscience.com>
#         
# Top level web framework based on the jug package.
#
# Source this script and a jug instance will be running at http://localhost:8080 until the script is ended.
#
# This script can be run inside a docker container to create an always-up web service.
#
# See:  https://github.com/MazamaScience/jug
# See:  https://github.com/MazamaScience/MazamaWebUtils
#
################################################################################

# Specficic packages and scripts for this service -----------------------------

# NOTE:  Use library() so that these package versions will be documented by sessionInfo()

library(methods)           # always included for Rscripts
library(jug)               # web framework
library(MazamaWebUtils)    # cache management
library(readr)             # tidyverse file reading
library(digest)            # creation of uniqueID

library(PWFSLSmoke)        # workhorse package for everything smoke related

# Load R functions
R_files <- list.files('R', pattern=".+\\.R", full.names=TRUE)
for ( file in R_files ) {
  source( file.path(getwd(),file) )
}

# Specify global (configurable) variables -------------------------------------

VERSION <- "4.0.6" # v4 data files . --- . aqilegendadvice plottype

# Set up configurable variables

if ( Sys.getenv("JUG_HOST") == "" ) { # Running from RStudio

  # jug instance configuration
  JUG_HOST <- "127.0.0.1" # jug default
  JUG_PORT <- "8080"      # jug default

  # path and cache
  SERVICE_PATH <- "monitor-custom/dev"
  CACHE_DIR <- "output"
  CACHE_SIZE <- 3 # megabytes

  # directory for log output
  DATA_DIR <- file.path(getwd(),'data')
  if ( !file.exists(DATA_DIR) ) dir.create(DATA_DIR)
  LOG_DIR <- file.path(getwd(),'logs')
  if ( !file.exists(LOG_DIR) ) dir.create(LOG_DIR)

  # Clean out the cache (only when running from RStudio)
  removalStatus <- file.remove( list.files(CACHE_DIR, full.names=TRUE) )
  
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
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error


# ----- Define error function -------------------------------------------------

stopOnError <- function(result, err_msg="") {
  if ( "try-error" %in% class(result) ) {
    # Close all open graphics devices.
    logger.debug('closing %d graphics devices', length(dev.list()) )
    graphics.off()
    # Log the error message
    err_msg <- ifelse(err_msg == "", geterrmessage(), err_msg)
    err_msg <- stringr::str_trim(err_msg)
    logger.error(err_msg)
    # TODO:  Convert opaque R error messages to something more friendly
    if ( stringr::str_detect(err_msg, "Error in 2:n : argument of length 0") ) {
      stop("No data", call.=FALSE)
    } else {
      stop(stringr::str_replace(err_msg, "Error :", ""), call. = FALSE)
    }
  }
}


# ----- Set up Logging --------------------------------------------------------

result <- try({
  # Set up logging
  logger.setup(debugLog=file.path(LOG_DIR, "DEBUG.log"),
               infoLog=file.path(LOG_DIR, "INFO.log"),
               errorLog=file.path(LOG_DIR, "ERROR.log"))
}, silent=TRUE)
stopOnError(result, "Could not create log files.")

if ( Sys.getenv("JUG_HOST") == "" ) { # Running from RStudio
  logger.setLevel(DEBUG)
}

# Log session info
logger.debug(capture.output(sessionInfo()))

# Log environment variables
logger.debug('JUG_HOST = %s', JUG_HOST)
logger.debug('JUG_PORT = %s', JUG_PORT)
logger.debug('SERVICE_PATH = %s', SERVICE_PATH)
logger.debug('CACHE_DIR = %s', CACHE_DIR)
logger.debug('CACHE_SIZE = %s', CACHE_SIZE)
logger.debug('LOG_DIR = %s', LOG_DIR)


# ----- BEGIN jug app ---------------------------------------------------------

jug() %>%
  
  # Return json dscription of this service ------------------------------------
  get(paste0("/",SERVICE_PATH,"/?$"), function(req, res, err) { # regex matches zero or one final '/'
    
    logger.info("----- %s -----", SERVICE_PATH)
    
    json <- jsonlite::toJSON(createAPIList(SERVICE_PATH, VERSION), pretty=TRUE, auto_unbox=TRUE)
    res$content_type("application/json")
    
    return(json)
    
  }) %>%

  # Return json dscription of this service ------------------------------------
  get(paste0("/",SERVICE_PATH,"/[Aa][Pp][Ii]/?$"), function(req, res, err) { # regex ignores capitalization and matches zero or one final '/'
    
    logger.info("----- %s/API -----", SERVICE_PATH)
    
    json <- jsonlite::toJSON(createAPIList(SERVICE_PATH, VERSION), pretty=TRUE, auto_unbox=TRUE)
    res$content_type("application/json")
    
    return(json)
    
  }) %>%
  
  # Return UI -----------------------------------------------------------------
  get(paste0("/",SERVICE_PATH,"/[Uu][Ii]/?$"), function(req, res, err) {  # regex ignores capitalization and matches zero or one final '/'
    
    logger.info("----- %s/UI -----", SERVICE_PATH)
    
    html <- readr::read_file("UI/index.html")
    res$content_type("text/html")
    
    return(html)
    
  }) %>%
  
  # Plot -----------------------------------------------------------------
  get(paste0("/",SERVICE_PATH,"/plot"), function(req, res, err) {

    logger.info("----- %s/plot -----", SERVICE_PATH)
    
    result <- try({
      infoList <- createInfoList(req)
      # Create png, json, and html file names based on uniqueID
      uniqueID <- infoList$uniqueID
      basePath <- paste0(CACHE_DIR,"/",uniqueID)
      pngPath <- paste0(basePath,".png")
      jsonPath <- paste0(basePath,".json")
    }, silent = TRUE)
    stopOnError(result)
    
    # Create new png and json files if they don't already exist
    if ( !file.exists(pngPath) || !file.exists(jsonPath) ) {

      logger.debug('%d graphics devices currently open', length(dev.list()) )
      
      # Manage the cache      
      result <- try({
        MazamaWebUtils::manageCache(CACHE_DIR, c("json","png"))
        # Create the plot
        logger.info("generating %s", infoList$plottype)
        if ( infoList$plottype == "aqilegend" ) {
          
          png(pngPath, width=infoList$plotwidth, height=infoList$plotheight, units="px")
          AQILegend(dataList=list(), infoList, textList=list())
          dev.off()
          
        } else if ( infoList$plottype == "aqilegendadvice" ) {
          
          png(pngPath, width=infoList$plotwidth, height=infoList$plotheight, units="px")
          AQILegendAdvice(dataList=list(), infoList, textList=list())
          dev.off()
          
        } else {
          
          dataList <- createDataList(infoList, DATA_DIR)
          # Get language dependent plot labels
          textListScript = paste('R/createTextList_',infoList$language, '.R', sep="")
          source(textListScript)
          textList <- createTextList(dataList, infoList)
          # Create plot
          png(pngPath, width=infoList$plotwidth, height=infoList$plotheight, units="px")
          if ( infoList$plottype == "dailybarplot" ) {
            dailyBarplot(dataList, infoList, textList)
          } else if ( infoList$plottype == "dailybyhour" ) {
            dailyByHour(dataList, infoList, textList)
          } else if ( infoList$plottype == "timeseries" ) {
            timeseries(dataList, infoList, textList)
          } else if ( infoList$plottype == "locationmap" ) {
            locationMap(dataList, infoList, textList)
          } else if ( infoList$plottype == "esrilocationmap" ) {
            esriLocationMap(dataList, infoList, textList)
          } else {
            stop("invalid plotType", call. = FALSE)
          }
          dev.off()
          
        }
      }, silent = TRUE)
      stopOnError(result)
      
      # Create the json file
      result <- try({
        
        logger.debug("writing %s", jsonPath)

        responseList <- list(status="OK",
                             rel_base=paste0(SERVICE_PATH, "/", basePath),
                             image_path=paste0(SERVICE_PATH, "/", pngPath))
        json <- jsonlite::toJSON(responseList, na="null", pretty=TRUE, auto_unbox=TRUE)
        write(json, jsonPath)
        
      }, silent = TRUE)
      stopOnError(result)
      
    } # finished creating png and json files

    logger.info("successfully created %s", pngPath)
    logger.info("successfully created %s", jsonPath)
    
    # Return the appropriate file based on infoList$responsetype
    result <- try({
      if ( infoList$responsetype == "json" ) {
        res$content_type("application/json")
        return( readr::read_file(jsonPath) )
      } else if ( infoList$responsetype == "png" ) {
        res$content_type("image/png")
        return( readr::read_file_raw(pngPath) )
      } else {
        err_msg <- paste0("Invalild responsetype: ", infoList$responsetype)
        stop(err_msg, call.=FALSE)
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
  serve_it(host=JUG_HOST, port=as.integer(JUG_PORT))


# ----- END jug app -----------------------------------------------------------


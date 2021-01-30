################################################################################
# monitor-custom-test.R
#
# Author: Tate Brasel,
#         Spencer Pease
#         Jonathan Callahan <jonathan@mazamscience.com>
#
# Test version of the app code that can be sourced in RStudio.
#
################################################################################

# Incoming request

subservice <- "dailyaveragetable"

req <- list(
  parameters = list(
    monitorids = "840TT1820013_01,160690012_01,530030004_01",
    startdate = "20200817",
    enddate = "20201011",
    outputfiletype = "xlsx"
  )
)

# ----- Libraries and scripts --------------------------------------------------

# NOTE:  Use library() so that these package versions will be documented by
# NOTE:  sessionInfo()

suppressPackageStartupMessages({
  library(readr)             # tidyverse file reading
  library(digest)            # creation of uniqueID
  library(flextable)         # creation of table graphics
  library(openxlsx)          # creation of spreadsheets
  # Mazama Science packages
  library(MazamaCoreUtils)   # cache management and more
  library(PWFSLSmoke)        # workhorse package for everything smoke related
  library(AirMonitorPlots)   # plotting functions for monitor data
})

R_files <- list.files("R/sharedUtils", pattern = ".+\\.R", full.names = TRUE)

for (file in R_files) {
  source(file.path(getwd(), file))
}

# Specify global (configurable) variables --------------------------------------

# V4 data files . dailyaveragetable . xlsx improvements; csv output
VERSION <- "4.5.1"

# Set up configurable variables

# directories for log output, data, and cache
DATA_DIR <- file.path(getwd(), "data")
if ( !file.exists(DATA_DIR) ) dir.create(DATA_DIR)

LOG_DIR <- file.path(getwd(), "logs")
if ( !file.exists(LOG_DIR) ) dir.create(LOG_DIR)

CACHE_DIR <- file.path(getwd(), "output")
if ( !file.exists(CACHE_DIR) ) dir.create(CACHE_DIR)

# Clean out the cache (only when running from RStudio)
removalStatus <- file.remove( list.files(CACHE_DIR, full.names = TRUE) )

# Silence other warning messages
options(warn = -1) # -1=ignore, 0=save/print, 1=print, 2=error


# ----- Set up Logging ---------------------------------------------------------

MazamaCoreUtils::initializeLogging(LOG_DIR)

logger.setLevel(TRACE)

# ----- Define helper functions ------------------------------------------------

printUTC <- function(x) {
  strftime(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
}

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


# Create subservice script paths
infoListScript <- paste0("R/", subservice, "/createInfoList.R")
dataListScript <- paste0("R/", subservice, "/createDataList.R")
productScript <- paste0("R/", subservice, "/createProduct.R")

# Source these scripts
source(infoListScript)        # function to convert request into infoList required by product
source(dataListScript)        # function to load data required by product
source(productScript)         # function to create product

# Create infoList
infoList <- createInfoList(req, CACHE_DIR)

# Create a new plot file if it isn't in the cache

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





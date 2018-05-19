########################################################################
# uptime/createDataList.R
#
# Create a list of data needed to generate the product.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createDataList <- function(infoList = NULL, dataDir = NULL) {

  logger.trace("----- createDataList() -----")

  if (is.null(infoList)) stop(paste0("Required parameter 'infoList' is missing."), call. = FALSE)

  # ----- Load data -----------------------------------------------------------

  serverID <- infoList$serverid
  logUrl <- paste0('https://', serverID, '.airfire.org/logs/uptime.log')
  startDate <- lubridate::ymd(infoList$startdate)

  uptimeData <-
    readr::read_csv(
      logUrl,
      col_names = c('datetime', 'hms', 'user', 'load_1_min', 'load_5_min', 'load_15_min')
    ) %>%
    mutate(
      datetime = lubridate::ymd_hms(str_sub(datetime, 1, 19)),
      load_1_min = as.numeric(str_replace(load_1_min, "load average: ", ""))
    ) %>%
    select(-hms, -user) %>%
    filter(datetime >= startDate)

  # ----- Validate data -------------------------------------------------------

  #TODO: handle empty

  # ----- Create data structures ----------------------------------------------

  # Create dataList
  dataList <- list(
    uptimeData = uptimeData
  )

  return(dataList)
}

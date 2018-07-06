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

  # NOTE:  Need to watch out for reboots that change the number of commas
  #
  # 2018-06-07 18:16:01 up 35 days, 59 min,  0 users,  load average: 0.05, 0.01, 0.09
  # 2018-06-07 18:31:01 up 1 min,  0 users,  load average: 3.70, 1.99, 0.76
  #
  # Sigh ... Why is nothing ever easy?
  
  # Instead, load the data as lines for further parsing
  lines <- readr::read_lines(logUrl)

  # Pull out elements using
  regex_datetime <- "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})"
  datetimeString <- stringr::str_extract(lines, regex_datetime)

  regex_users <- "([0-9]+ user.?,)"
  usersString <- stringr::str_extract(lines, regex_users)
  # For userCount, use everything to the left of the first ' '
  usersString = stringr::str_split_fixed(usersString, ' ', 2)[,1]
  
  regex_load <- "(load average: .+$)"
  loadString <- stringr::str_extract(lines, regex_load)
  loadString <- stringr::str_replace(loadString, "load average: ", "")
  loadString <- stringr::str_replace_all(loadString, " ", "")
  
  # Now reassemble a cleaned up, artificial CSV file 
  fakeLines <- paste(datetimeString, usersString, loadString, sep=",")
  # Omit any lines with "NA"
  fakeLines <- fakeLines[ !stringr::str_detect(fakeLines, "NA") ]
  fakeFile <- paste(fakeLines, collapse="\n")
 
  uptimeData <- readr::read_csv(fakeFile,
                                col_names = c('datetime', 'userCount', 'load_1_min', 'load_5_min', 'load_15_min'))
  
  # Use dplyr to filter
  startDate <- lubridate::ymd_hm(infoList$startdate) # NOTE:  only to the minute, not second
  uptimeData <-
    uptimeData %>%
    filter(datetime >= startDate)

  # ----- Validate data -------------------------------------------------------

  #TODO: handle empty

  # ----- Load 'free -h' data -------------------------------------------------
  
  # url <- 'https://test-c1.airfire.org/logs/free_memory.log'
  # col_names <- c('datetime','dummy','total','used','free','shared','buff_cache','available')
  # df <- readr::read_fwf(url, readr::fwf_empty(url, col_names=col_names))
  # df$dummy <- NULL
  # 
  # now <- lubridate::now('UTC')
  # starttime <- now - lubridate::ddays(7)
  # tlim <- c(starttime, now)
  # 
  # yhi <- max(df$total, na.rm=TRUE)
  # plot(df$datetime, df$used, las=1, xlim=tlim, ylim=c(0,yhi), type='s')
  # points(df$datetime, df$total, type='s', lwd=2, col='gray50')

  # ----- Create data structures ----------------------------------------------

  # Create dataList
  dataList <- list(
    uptimeData = uptimeData
  )

  return(dataList)
}

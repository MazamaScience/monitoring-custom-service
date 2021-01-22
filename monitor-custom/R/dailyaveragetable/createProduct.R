########################################################################
# dailyaveragetable/createProduct.R
#
# Create a table of daily average monitor readings.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

#' @title Create a table of daily average monitor readings
#'
#' @description
#' Function that grabs the appropriate parameters from `infoList`, data from
#' `dataList`, generates a table, and saves it as an image or spreadsheet.

createProduct <- function(dataList = NULL, infoList = NULL, textList = NULL) {

  logger.debug("----- createProduct() [dailyaveragetable] -----")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dataList)
  MazamaCoreUtils::stopIfNull(infoList)
  MazamaCoreUtils::stopIfNull(textList)

  # ----- Get parameters ------------------------------------------------------

  ws_monitor <- dataList$ws_monitor
  monitorIDs <- infoList$monitorIDs
  plotPath <- infoList$plotPath

  # ----- Calculate tlim ------------------------------------------------------

  # Create starttime and endtime in monitor local time
  # NOTE:  Don't use lubridate::today() as it generates class 'Date' which causes confusion.
  # NOTE:  Instead, stick with lubridate::now() which generates class 'POSIXct'.
  timezone <- ws_monitor$meta$timezone[1] # Use first available timezone
  now <- lubridate::now(tzone = timezone)
  today <- lubridate::floor_date(now, unit = 'day')

  starttime <- today - lubridate::ddays(infoList$days)
  endtime <- lubridate::floor_date(now, unit = 'hour')

  # ----- Aggregate data -------------------------------------------------------

  # Define the date range
  dateRange <- MazamaCoreUtils::dateRange(
    startdate = infoList$startdate,
    enddate = infoList$enddate,
    timezone = infoList$timezone,
    unit = "day",
    ceilingEnd = TRUE
  )

  # Calculate daily data from monitors
  monitorData <-
    ws_monitor %>%
    monitor_subset(monitorIDs = monitorIDs) %>%
    monitor_subset(tlim = dateRange)

  if ( infoList$useaqi == 'true' ) {
    dailyData <-
      monitorData %>%
      monitor_nowcast() %>%
      monitor_dailyStatistic()

  } else {
    dailyData <-
      monitorData %>%
      monitor_dailyStatistic()
  }
  
  # Extract the daily average values and round to 1 decimal place
  # TODO: extract columns based on names rather than indexes
  dailyAverageValuesDf <- dailyData$data[2:ncol(dailyData$data)]
  dailyAverageValuesDf <- round(dailyAverageValuesDf, digits = 1)
  
  # Calculate the UTC datetimes from the local datetimes. Must be done this way
  # since PWFSLSmoke::monitor_dailyStatistic() automatically aggregates the
  # daily average using the most common timezone between all the given monitors
  utcDatetimes <- MazamaCoreUtils::parseDatetime(
    datetime = dailyData$data$datetime,
    timezone = "UTC"
  )
  
  datetimesDf <- data.frame(
    datetime_UTC = utcDatetimes,
    datetime_local = dailyData$data$datetime
  )
  
  # Create a final daily averages dataframe with UTC and local datetime columns
  # and value columns for each monitor
  dailyAveragesDf <- cbind(datetimesDf, dailyAverageValuesDf)

  # ----- Save table -----------------------------------------------------------

  if ( infoList$outputfiletype == "png") {

    # TODO:  Weren't able to get flextable::save_as_image() to work so comment
    # TODO:  this section out for now. If .png (or .pdf or latex) output ever
    # TODO:  becomes a priority we can revisit this.

    # # Create table
    # table <- flextable::flextable(
    #   dailyData$data
    # )
    # # %>%
    # #   flextable::bg(
    # #     bg = "#FFFFFF",
    # #     part = "all"
    # #   ) %>%
    # #   flextable::bold(
    # #     part = "header"
    # #   ) %>%
    # #   flextable::autofit(
    # #     add_w = 0.1,
    # #     add_h = 0.1,
    # #     part = c("body", "header")
    # #   )
    #
    # flextable::save_as_image(
    #   table,
    #   path = plotPath
    # )

    stop("outputfiletype '.png' is not supported")

  } else if ( infoList$outputfiletype == "xlsx" ) {

    # Create a new xlsx spreadsheet
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Daily Averages")
    
    # Write datetime headers
    openxlsx::writeData(
      wb,
      sheet = "Daily Averages",
      x = matrix(c("datetime_UTC", "datetime_local"), nrow = 1),
      startCol = 1,
      startRow = 2,
      colNames = FALSE
    )
    
    # Write monitor ID headers
    openxlsx::writeData(
      wb,
      sheet = "Daily Averages",
      x = matrix(dailyData$meta$monitorID, nrow = 1),
      startCol = 3,
      startRow = 1,
      colNames = FALSE
    )
    
    # Write monitor site name headers
    openxlsx::writeData(
      wb,
      sheet = "Daily Averages",
      x = matrix(dailyData$meta$siteName, nrow = 1),
      startCol = 3,
      startRow = 2,
      colNames = FALSE
    )
    
    # Write daily average values
    openxlsx::writeData(
      wb = wb,
      sheet = "Daily Averages",
      x = dailyAveragesDf,
      startCol = 1,
      startRow = 3,
      colNames = FALSE
    )
    
    # Make datetime columns wider
    openxlsx::setColWidths(
      wb,
      sheet = "Daily Averages",
      cols = 1:2,
      widths = 18
    )
    
    # Save spreadsheet
    openxlsx::saveWorkbook(
      wb = wb,
      file = infoList$plotPath,
      overwrite = TRUE
    )

  }

  return(invisible())
}

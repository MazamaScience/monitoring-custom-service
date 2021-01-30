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
  monitorids <- infoList$monitorids
  plotPath <- infoList$plotPath

  # # ----- Calculate tlim ------------------------------------------------------
  #
  # # Create starttime and endtime in monitor local time
  # # NOTE:  Don't use lubridate::today() as it generates class 'Date' which causes confusion.
  # # NOTE:  Instead, stick with lubridate::now() which generates class 'POSIXct'.
  # timezone <- ws_monitor$meta$timezone[1] # Use first available timezone
  # now <- lubridate::now(tzone = timezone)
  # today <- lubridate::floor_date(now, unit = 'day')
  #
  # starttime <- today - lubridate::ddays(infoList$days)
  # endtime <- lubridate::floor_date(now, unit = 'hour')

  # ----- Aggregate data -------------------------------------------------------

  # # Define the date range
  # dateRange <- MazamaCoreUtils::dateRange(
  #   startdate = infoList$startdate,
  #   enddate = infoList$enddate,
  #   timezone = infoList$timezone,
  #   unit = "day",
  #   ceilingEnd = TRUE
  # )
  #
  # # Calculate daily data from monitors
  # monitorData <-
  #   ws_monitor %>%
  #   monitor_subset(monitorIDs = monitorIDs) %>%
  #   monitor_subset(tlim = dateRange)

  if ( infoList$useaqi == 'true' ) {

    dailyData <-
      ws_monitor %>%
      monitor_nowcast() %>%
      monitor_dailyStatistic()

  } else {

    dailyData <-
      ws_monitor %>%
      monitor_dailyStatistic()

  }

  # Extract the daily average values and round to 1 decimal place
  # TODO: extract columns based on names rather than indexes
  dailyAverageValuesDF <- dailyData$data[2:ncol(dailyData$data)]
  dailyAverageValuesDF <- round(dailyAverageValuesDF, digits = 1)

  # Calculate the UTC datetimes from the local datetimes. Must be done this way
  # since PWFSLSmoke::monitor_dailyStatistic() automatically aggregates the
  # daily average using the most common timezone between all the given monitors
  utcDatetimes <- MazamaCoreUtils::parseDatetime(
    datetime = dailyData$data$datetime,
    timezone = "UTC"
  )

  datetimesDF <- data.frame(
    datetime_local = dailyData$data$datetime,
    datetime_UTC = utcDatetimes
  )

  # Create a final daily averages dataframe with UTC and local datetime columns
  # and value columns for each monitor
  dailyAveragesDF <- cbind(datetimesDF, dailyAverageValuesDF)

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

    # ----- Create .xlsx -------------------------------------------------------

    # Create a new xlsx spreadsheet
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Daily Averages")

    ## Write datetime and and daily average headers

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

    ## Write datetime timestamps and daily average values

    # Write datetime timestamps
    openxlsx::writeData(
      wb,
      sheet = "Daily Averages",
      x = datetimesDF,
      startCol = 1,
      startRow = 2,
      colNames = TRUE
    )

    # Write daily average values
    openxlsx::writeData(
      wb = wb,
      sheet = "Daily Averages",
      x = dailyAverageValuesDF,
      startCol = 3,
      startRow = 3,
      colNames = FALSE
    )

    ## Write NAs in empty daily average cells

    # Find the coordinates of all the empty daily average cells
    emptyDailyAverageCellCoords <- which(is.na(dailyAverageValuesDF), arr.ind = TRUE)

    if ( nrow(emptyDailyAverageCellCoords) > 0 ) {

      for ( i in seq_len(nrow(emptyDailyAverageCellCoords)) ) {

        # Account for coordinate offsets within the spreadsheet
        naCol <- emptyDailyAverageCellCoords[i, 2] + 2 # +2 to account for datetime_local and datetime_UTC cols
        naRow <- emptyDailyAverageCellCoords[i, 1] + 2 # +2 to account for monitor ID and site name row

        # Write Excel NA cell formula
        openxlsx::writeFormula(
          wb,
          sheet = "Daily Averages",
          x = "=NA()",
          xy = c(naCol, naRow)
        )

      }

    }

    ## Apply formatting to sheet

    # Define header style
    headerStyle <- openxlsx::createStyle(
      halign = "CENTER",
      textDecoration = "Bold"
    )

    # Apply header style to header cells
    openxlsx::addStyle(
      wb,
      sheet = "Daily Averages",
      style = headerStyle,
      rows = 1:2,
      cols = 1:ncol(dailyAveragesDF),
      gridExpand = TRUE
    )

    # Make datetime columns wider
    openxlsx::setColWidths(
      wb,
      sheet = "Daily Averages",
      cols = 1:ncol(dailyAveragesDF),
      widths = 18
    )

    # Apply AQI color palette to daily average cells

    # Since the conditional formatting rules don't allow for ranges (ex. 12.0 <= x < 35.5)
    # the rules currently just test '<='. This means that the formatting rules
    # must be applied from the largest PM 2.5 break to the smallest.

    xlsxDailyAverageValueColIndexes <- 3:ncol(dailyAveragesDF)
    xlsxDailyAverageValueRowIndexes <- 3:(nrow(dailyAveragesDF) + 2) # +2 to account for monitor ID and site name

    # First apply conditional color format for the highest AQI breakpoint (250.5 - Inf)
    openxlsx::conditionalFormatting(
      wb,
      sheet = "Daily Averages",
      cols = xlsxDailyAverageValueColIndexes,
      rows = xlsxDailyAverageValueRowIndexes,
      rule = ">250.5",
      style = openxlsx::createStyle(bgFill = "#7E0023")
    )

    # Apply conditional color formats for the rest of the lower AQI breakpoints
    for ( i in seq(from = length(PWFSLSmoke::AQI$colors), to = 2) ) {

      openxlsx::conditionalFormatting(
        wb,
        sheet = "Daily Averages",
        cols = xlsxDailyAverageValueColIndexes,
        rows = xlsxDailyAverageValueRowIndexes,
        rule = paste0("<=", PWFSLSmoke::AQI$breaks_24[i]),
        style = openxlsx::createStyle(bgFill = PWFSLSmoke::AQI$colors[i-1])
      )

    }

    ## Save xlsx workbook

    openxlsx::saveWorkbook(
      wb = wb,
      file = infoList$plotPath,
      overwrite = TRUE
    )

  } else if ( infoList$outputfiletype == "csv" ) {

    # ----- Create .csv --------------------------------------------------------

    # Define an empty matrix
    m <- matrix(
      data = rep("", (nrow(dailyAveragesDF) + 2) * ncol(dailyAveragesDF)),
      nrow = nrow(dailyAveragesDF) + 2,
      ncol = ncol(dailyAveragesDF)
    )

    # Write datetime column headers to matrix
    m[2,1] <- "datetime_local"
    m[2,2] <- "datetime_UTC"

    # Write monitor ID and site name headers to matrix
    m[1,3:ncol(m)] <- matrix(dailyData$meta$monitorID, nrow = 1)
    m[2,3:ncol(m)] <- matrix(dailyData$meta$siteName, nrow = 1)

    # Write datetime columns to matrix
    m[3:nrow(m),1] <- format(datetimesDF$datetime_local, format = "%Y/%m/%d %H:%M:%S %p")
    m[3:nrow(m),2] <- format(datetimesDF$datetime_UTC, format = "%Y/%m/%d %H:%M:%S %p")

    # Write each monitor's daily average column to matrix
    for ( i in seq_len(ncol(dailyAverageValuesDF)) ) {
      m[3:nrow(m), i + 2] <- dailyAverageValuesDF[,i]
    }

    # Convert matrix to data frame
    df <- as.data.frame(m)

    # Save data frame as csv
    readr::write_csv(
      df,
      path = infoList$plotPath,
      col_names = FALSE
    )

  }

  return(invisible())
}


##### DEBUGGING ################################################################

if ( FALSE ) {

  ws_monitor <- PWFSLSmoke::monitor_load(
    monitorIDs = c("530330030_01", "410510080_01", "060750005_01"),
    startdate = "20200910",
    enddate = "20201001"
  )

  dailyData <-
    PWFSLSmoke::monitor_nowcast(ws_monitor) %>%
    PWFSLSmoke::monitor_dailyStatistic()

  # Extract the daily average values and round to 1 decimal place
  # TODO: extract columns based on names rather than indexes
  dailyAverageValuesDF <- dailyData$data[2:ncol(dailyData$data)]
  dailyAverageValuesDF <- round(dailyAverageValuesDF, digits = 1)

  # Calculate the UTC datetimes from the local datetimes. Must be done this way
  # since PWFSLSmoke::monitor_dailyStatistic() automatically aggregates the
  # daily average using the most common timezone between all the given monitors
  utcDatetimes <- MazamaCoreUtils::parseDatetime(
    datetime = dailyData$data$datetime,
    timezone = "UTC"
  )

  datetimesDF <- data.frame(
    datetime_local = dailyData$data$datetime,
    datetime_UTC = utcDatetimes
  )

  # Create a final daily averages dataframe with UTC and local datetime columns
  # and value columns for each monitor
  dailyAveragesDF <- cbind(datetimesDF, dailyAverageValuesDF)

  # Create a new xlsx spreadsheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Daily Averages")

  # Write datetime dataframe
  openxlsx::writeData(
    wb,
    sheet = "Daily Averages",
    x = datetimesDF,
    startCol = 1,
    startRow = 2,
    colNames = TRUE
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
    x = dailyAverageValuesDF,
    startCol = 3,
    startRow = 3,
    colNames = FALSE
  )

  # Write NAs in any empty daily average cells
  emptyAverageCellCoords <- which(is.na(dailyAverageValuesDF), arr.ind = TRUE)

  if ( nrow(emptyAverageCellCoords) > 0 ) {

    for ( i in seq_len(nrow(emptyAverageCellCoords)) ) {
      # Account for coordinate offsets within the spreadsheet
      naCol <- emptyAverageCellCoords[i, 2] + 2
      naRow <- emptyAverageCellCoords[i, 1] + 2

      # Write Excel NA cell formula
      openxlsx::writeFormula(
        wb,
        sheet = "Daily Averages",
        x = "=NA()",
        xy = c(naCol, naRow)
      )
    }

  }

  # Since the conditional formatting rules don't allow for ranges (ex. 12.0 <= x < 35.5)
  # the rules currently just test '<='. This means that the formatting rules
  # must be applied from the largest PM 2.5 break to the smallest.

  xlsxDailyAverageValueColIndexes <- 3:ncol(dailyAveragesDF)
  xlsxDailyAverageValueRowIndexes <- 3:(nrow(dailyAveragesDF) + 2) # +2 to account for monitor ID and site name

  openxlsx::conditionalFormatting(
    wb,
    sheet = "Daily Averages",
    cols = xlsxDailyAverageValueColIndexes,
    rows = xlsxDailyAverageValueRowIndexes,
    rule = ">250.5",
    style = openxlsx::createStyle(bgFill = "#7E0023")
  )

  # Apply conditional formats for the rest of the AQI colors
  for ( i in seq(from = length(PWFSLSmoke::AQI$colors), to = 2) ) {

    openxlsx::conditionalFormatting(
      wb,
      sheet = "Daily Averages",
      cols = xlsxDailyAverageValueColIndexes,
      rows = xlsxDailyAverageValueRowIndexes,
      rule = paste0("<=", PWFSLSmoke::AQI$breaks_24[i]),
      style = openxlsx::createStyle(bgFill = PWFSLSmoke::AQI$colors[i-1])
    )

  }

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
    file = "~/Desktop/dailyaveragetable_debug.xlsx",
    overwrite = TRUE
  )

}


########################################################################
# createAPIList.R
#
# Document the webservice API.
#
# Author: Mazama Science
########################################################################

createAPIList <- function(name = NULL, version = NULL) {
  
  logger.debug("----- createAPIList() -----")
  
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(name, "Required parameter 'name' is missing.")
  MazamaCoreUtils::stopIfNull(version, "Required parameter 'version' is missing.")

  # ----- Define service -------------------------------------------------------

  # NOTE: Template for defining parameters:
  #       parametername = paste(
  #         "Description of parameter",
  #         "[required / default option; options / other options]"
  #       )

  dailyhourlybarplotParamList <- list(
    monitors = paste(
      "monitor ID.",
      "[repeatable parameter, will be deprecated in favor of monitorids]"),
    monitorids = paste(
      "Comma-separated list of monitorIDs.",
      "[ignored if monitors is defined]"),
    startdate = paste(
      "Start date (inclusive).",
      "[default = 'enddate' - 'days'; otherwise string in YYYYmmdd format (e.g. 20190820)]"),
    enddate = paste(
      "End date (exclusive).",
      "[default = 'startdate' + 'days' or tomorrow if 'startdate' is not explicitly specified; otherwise string in YYYYmmdd format (e.g. 20190820)]"),
    days = paste(
      "Days of data to include if either 'startdate' or 'enddate' is not explicitly specified.",
      "[default = 7; otherwise a positive integer]"),
    timezone = paste(
      "Olson timezone name used to interpret 'startdate' and 'enddate'.",
      "[default = 'UTC']"),
    columns = paste(
      "number of columns a faceted plot will have",
      "[default = 1]"),
    includelink = paste(
      "include a footer with a link to more AQI information",
      "[default = TRUE; TRUE|FALSE]"),
    hourlytype = paste(
      "type of hourly data to include in the plot",
      "[default = 'nowcast'; 'nowcast'|'raw'|'none']"),
    title = paste(
      "title of plot",
      "[default to '<data types included> PM2.5 Levels']"),
    xlabel = paste(
      "x-axis label of plot",
      "[default to 'Date, midnight to midnight (<year of data>)']" ),
    ylabel = paste(
      "y-axis label of plot",
      "[default to 'PM2.5 (ug/m^3)']"),
    width =  paste(
      "width of the graphic in given units",
      "[default = 8]"),
    height = paste(
      "height of the graphic in given units",
      "[default = 8]"),
    dpi = paste(
      "dpi (in pixels per unit) of the graphic",
      "[default = 100]"),
    units = paste(
      "units to determine graphic size",
      "[default = in; in|cm|mm]"),
    outputfiletype = paste(
      "file type of the output graphic",
      "[default = png; png|pdf]"),
    responsetype = paste(
      "response type",
      "[default = raw; raw|json]"),
    language = paste(
      "[not implemented] language code",
      "[default = en; en|es]")
  )
  
  dailyaveragetableParamList = list(
    monitors = paste(
      "monitor ID.",
      "[repeatable parameter, will be deprecated in favor of monitorids]"),
    monitorids = paste(
      "Comma-separated list of monitorIDs.",
      "[ignored if monitors is defined]"),
    startdate = paste(
      "Start date (inclusive).",
      "[default = 'enddate' - 'days'; otherwise string in YYYYmmdd format (e.g. 20190820)]"),
    enddate = paste(
      "End date (exclusive).",
      "[default = 'startdate' + 'days' or tomorrow if 'startdate' is not explicitly specified; otherwise string in YYYYmmdd format (e.g. 20190820)]"),
    days = paste(
      "Days of data to include if either 'startdate' or 'enddate' is not explicitly specified.",
      "[default = 7; otherwise a positive integer]"),
    timezone = paste(
      "Olson timezone name used to interpret 'startdate' and 'enddate'.",
      "[default = 'UTC']"),
    outputfiletype = paste(
      "file type of the output graphic",
      "[default = png; png|pdf]"),
    responsetype = paste(
      "response type",
      "[default = raw; raw|json]"),
    language = paste(
      "[not implemented] language code",
      "[default = en; en|es]")
  )
  
  APIList <- list(
    name = "monitor-custom",
    version = version,
    services = list(
      dailyhourlybarplot = list(
        method = "GET",
        params = dailyhourlybarplotParamList
      ),
      dailyaveragetable = list(
        method = "GET",
        params = dailyaveragetableParamList
      )
    )
  )
  
  return(APIList)
  
}

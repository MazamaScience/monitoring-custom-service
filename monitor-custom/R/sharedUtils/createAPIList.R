########################################################################
# createAPIList.R
#
# Document the webservice API.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createAPIList <- function(name=NULL, version=NULL) {

  logger.debug("----- createAPIList() -----")

  if ( is.null(name) ) stop(paste0("Required parameter 'name' is missing."), call. = FALSE)
  if ( is.null(version) ) stop(paste0("Required parameter 'version' is missing."), call. = FALSE)

  # Service definition to be desplayed as JSON when no UI exists
  APIList <- list(
    name = "monitor-custom",
    version = version,
    services = list(
      "dailyhourlybarplot" = list(
        method = "GET",
        params = list(
          monitors = "monitorID [repeatable parameter] [will be deprecated in favor of monitorids]",
          monitorids = "comma-separated list of monitorIDs (ignored if monitors is defined)",
          columns = "number of columns a faceted plot will have [default = 1]",
          includelink = "include a footer with a link to more AQI information [default = TRUE; TRUE|FALSE]",
          hourlytype = "type of hourly data to include in the plot [default = 'nowcast'; 'nowcast'|'raw'|'none']",
          title = "title of plot [default to '<data types included> PM2.5 Levels']",
          xlabel = "x-axis label of plot [default to 'Date, midnight to midnight (<year of data>)']",
          ylabel = "y-axis label of plot [default to 'PM2.5 (ug/m^3)']",
          width =  "width of the graphic in given units [default = 8]",
          height = "height of the graphic in given units [default = 8]",
          dpi = "dpi (in pixels per unit) of the graphic [default = 100]",
          units = "units to determine graphic size [default = in; in|cm|mm]",
          outputfiletype = "file type of the output graphic [default = png; png|pdf]",
          responsetype = "response type [default = raw; raw|json]",
          lookbackdays = "days of data to include [default = 7]",
          language = "[not implemented] language code [default = en; en|es]"
        )
      ),
      "uptime" = list(
        method = "GET",
        params = list(
          serverid = "<serverid>.airfire.org [default = 'tools-c3']",
          ymax = "y-axis maximum [default = 1000]",
          width =  "width of the graphic in given units [default = 10]",
          height = "height of the graphic in given units [default = 6]",
          dpi = "dpi (in pixels per unit) of the graphic [default = 100]",
          units = "units to determine graphic size [default = in; in|cm|mm]",
          outputfiletype = "file type of the output graphic [default = png; png|pdf]",
          responsetype = "response type [default = raw; raw|json]",
          lookbackdays = "days of data to include [default = 7, max=45]",
          language = "[not implemented] language code [default = en; en|es]"
        )
      )
    ) # END of services list
  ) # END of APIList

  return(APIList)
}

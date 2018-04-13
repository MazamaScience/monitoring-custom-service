########################################################################
# createAPIList.R
#
# Document the webservice API.
#
# Author: Helen Miller, Jonathan Callahan
########################################################################

createAPIList <- function(name=NULL, version=NULL) {
  
  logger.info("----- createAPIList() -----")
  
  if ( is.null(name) ) stop(paste0("Required parameter 'name' is missing."), call.=FALSE)
  if ( is.null(version) ) stop(paste0("Required parameter 'version' is missing."), call.=FALSE)
  
  # Service definition to be desplayed as JSON when no UI exists
  # NOTE:  The 'style' parameter is intentionally omitted as an internal-use-only parameter.
  APIList <- list(name="monitor-custom",
                  version=VERSION,
                  services=list(plot=list(method="GET",
                                          params=list(plottype="plot type [required; dailybarplot|dailybyhour|timeseries|aqilegend|aqilegendadvice]",
                                                      monitorid="PWFSL monitor ID [required if plottype is not aqilegend or aqilegendadvice]",
                                                      lookbackdays="days of data to include [default = 7]",
                                                      enddate="enddate of final datum [default = automatically generate today's date; otherwise e.g. 20170903]",
                                                      zoom="zoom level for locationMap [default = 9]",
                                                      size="image size in pixels[default = 500]",
                                                      responsetype="response type [default = png; json|png]")
                  )
                  )
  )
  
  return(APIList)
  
}

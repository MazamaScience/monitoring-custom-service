########################################################################
# createAPIList.R
#
# Document the webservice API.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createAPIList <- function(name=NULL, version=NULL) {
  
  logger.debug("----- createAPIList() -----")
  
  if ( is.null(name) ) stop(paste0("Required parameter 'name' is missing."), call.=FALSE)
  if ( is.null(version) ) stop(paste0("Required parameter 'version' is missing."), call.=FALSE)
  
  # Service definition to be desplayed as JSON when no UI exists
  # NOTE:  The 'style' parameter is intentionally omitted as an internal-use-only parameter.
  APIList <- list(
    name = "monitor-presentation",
    version = version,
    services = list(
      "demo" = list(
        method = "GET",
        params = list(
          monitors = "monitorID [repeatable parameter] [will be deprecated in favor of monitorids]",
          monitorids = "comma-separated list of monitorIDs (ignored if monitors is defined)",
          language = "language code [default = en; en|es]",
          plottheme = "plot theme [default = hc; base|calc|economist|economist_white|excel|few|fivethirtyeight|foundation|gdocs|hc|igray|map|pander|solarized|solarized_2|solid|stata|tufte|wsj]",
          lookbackdays = "days of data to include [default = 7]",
          responsetype = "response type [default = raw; raw|json]"
        )),
      "outlook" = list(
        method = "GET",
        params = list(
          outlookurl = "URL of a Smoke Outlook .json file",
          language = "language code [default = en; en|es]",
          plottheme = "plot theme [default = hc; base|calc|economist|economist_white|excel|few|fivethirtyeight|foundation|gdocs|hc|igray|map|pander|solarized|solarized_2|solid|stata|tufte|wsj]",
          responsetype = "response type [default = raw; raw|json]"
        )),
      "custom" = list(
        method = "GET",
        params = list(
          language = "language code [default = en; en|es]",
          plottheme = "plot theme [default = hc; base|calc|economist|economist_white|excel|few|fivethirtyeight|foundation|gdocs|hc|igray|map|pander|solarized|solarized_2|solid|stata|tufte|wsj]",
          responsetype = "response type [default = raw; raw|json]"
        ))
    ) # END of services list
  ) # END of APIList
  
  return(APIList)
  
}

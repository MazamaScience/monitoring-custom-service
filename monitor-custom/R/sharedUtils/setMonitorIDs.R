#' @title Set Monitor IDs
#'
#' @param infoList 
#'
#' @return infoList
#' 
setMonitorIDs <- function(infoList) {

  logger.debug("Setting monitorIDs")
  
  if ( !is.null(infoList$monitors) ) { # Support deprecated "monitors=XXX&monitors=YYY&monitors=ZZZ"
    
    # Create a vector of monitorIDs
    paramNames <- names(infoList)
    monitorIndices <- which(paramNames == "monitors")
    monitorIDs <- vector("character", length(monitorIndices))
    
    i <- 0
    for (monitorIndex in monitorIndices) {
      i <- i + 1
      monitorIDs[i] <- infoList[[monitorIndex]]
    }
    
    infoList$monitorIDs <- monitorIDs
    # -OR- req$attach("monitorIDs", monitorIDs)
    
    # Now remove the "monitor" elements in reverse order to preserve the indexing
    for (monitorIndex in rev(monitorIndices)) {
      infoList[[monitorIndex]] <- NULL
    }
    
    # Create comma separated request parameter
    infoList$monitorids <- paste0(monitorIDs, collapse = ",")
      
  } else { # Suport modern "monitorids=XXX,YYY,ZZZ"
    
    if ( is.null(infoList$monitorids) ) {
      infoList$monitorIDs <- NULL
    } else {
      # 'monitorids' is already a comma separated list of monitors
      
      # Note: str_split returns a single-element list
      infoList$monitorIDs <- stringr::str_split(infoList$monitorids, ",+")[[1]]
      
    }
  }
  
  return(infoList)
}

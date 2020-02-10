#' @title Set Monitor IDs
#'
#' @description
#' This function takes an `infoList` with potentially many different
#' specifications of monitor IDs and alters the list to have one uniform
#' reference to monitor IDs, located in a `$monitorIDs` element.
#'
#' Currently, this function supports the deprecated method of specifying
#' monitor IDs individually with "monitors=XXX&monitors=YYY&monitors=ZZZ", as
#' well as the prefered method of specifying a single string of comma-separated
#' monitor IDs in "monitorids=XXX,YYY,ZZZ".
#'
#' Monitor IDs stored in either of these locations are moved to a character
#' vector located in `$monitorIDs`.
#'
#' @param infoList A list containing monitor IDs to format.
#'
#' @return infoList with `$monitorIDs` element that is a character vector of
#'   monitor IDs and no other elements referencing monitor IDs.
#'
setMonitorIDs <- function(infoList) {

  logger.debug("----- Setting monitorIDs -----")

  # Support deprecated "monitors=XXX&monitors=YYY&monitors=ZZZ"
  if (!is.null(infoList$monitors)) {

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

    # Now remove the "monitor" elements in reverse order to
    # preserve the indexing
    for (monitorIndex in rev(monitorIndices)) {
      infoList[[monitorIndex]] <- NULL
    }

  # Suport modern "monitorids=XXX,YYY,ZZZ"
  } else {

    # There are no monitor IDs specified
    if (is.null(infoList$monitorids)) {
      infoList$monitorIDs <- NULL

    # 'monitorids' is already a comma separated list of monitors
    } else {

      # Note: str_split returns a single-element list
      infoList$monitorIDs <- stringr::str_split(infoList$monitorids, ",+")[[1]]
      infoList$monitorids <- NULL

    }
  }

  return(infoList)
  
}

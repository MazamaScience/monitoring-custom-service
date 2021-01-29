#' @title Set Monitor IDs
#'
#' @description
#' This function takes an `infoList` with potentially many different
#' specifications of monitor IDs and alters the list to have one uniform
#' reference to monitor IDs, located in a `$monitorids` element.
#'
#' Currently, this function supports the deprecated method of specifying
#' monitor IDs individually with "monitors=XXX&monitors=YYY&monitors=ZZZ", as
#' well as the prefered method of specifying a single string of comma-separated
#' monitor IDs in "monitorids=XXX,YYY,ZZZ".
#'
#' Monitor IDs stored in either of these locations are moved to a character
#' vector located in `$monitorids`.
#'
#' @param infoList A list containing monitor IDs to format.
#'
#' @return infoList with `$monitorids` element that is a character vector of
#'   monitor IDs and no other elements referencing monitor IDs.
#'
setMonitorIDs <- function(infoList) {

  logger.debug("----- Setting monitorIDs -----")

  if ( "monitorid" %in% names(infoList) ) {

    # Create monitorids from a single monitorID
    infoList$monitorids <- infoList$monitorid

  } else if ( "monitorids" %in% names(infoList) ) {

    # Create monitorids from a comma-separated string
    infoList$monitorids <-
      unlist(stringr::str_split(infoList$monitorids, pattern = ","))

  } else if ( "monitors" %in% names(infoList) ) {

    # Support deprecated "monitors=XXX&monitors=YYY&monitors=ZZZ"

    # Create a monitorids from a list of monitorIDs
    paramNames <- names(infoList)
    monitorIndices <- which(paramNames == "monitors")

    if ( length(monitorIndices) > 0 ) {
      monitorids <- vector("character", length(monitorIndices))

      i <- 0
      for (monitorIndex in monitorIndices) {
        i <- i + 1
        monitorids[i] <- infoList[[monitorIndex]]
      }
      infoList$monitorids <- monitorids
      # -OR- req$attach("monitorids", monitorids)

      # Now remove the "monitor" elements in reverse order to preserve the indexing
      for (monitorIndex in rev(monitorIndices)) {
        infoList[[monitorIndex]] <- NULL
      }

      infoList$monitorids <- monitorids
    }

  } else {

    stop("No monitor IDs found in 'infoList'.")

  }

  return(infoList)

}

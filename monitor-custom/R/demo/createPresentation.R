########################################################################
# demo/createPresentation.R
#
# Create the 'demo' presentation.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

#' @title Create Demonstration Presentation
#' @description Function to create a simple demo presentation

createPresentation <- function(dataList = NULL, infoList = NULL, textList = NULL) {
  
  logger.trace("----- createPresentation() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  # ----- Generate plots and tables -------------------------------------------
  
  # Create daily barplot on disk
  g <- dailyBarPlot(dataList, infoList, textList)
  
  dailyBarPlotPath <- paste0(infoList$basePath, "_dailyBarPlot.png")
  ggsave(dailyBarPlotPath,
    g,
    dpi = infoList$dpi,
    units = infoList$units,
    width = infoList$width,
    height = infoList$height,
    scale = 1
  )
  
  # Create flextable in memory
  siteTable <- regulartable(dataList$tableData) %>%
    theme_vanilla() %>%
    width(width = c(2, 1.5, 0.5, 4))
  
  # ----- Create PowerPoint presentation --------------------------------------
  
  pptx_deck <-
    read_pptx("pptx/blank.pptx") %>%
    
    # Title Slide
    add_slide(layout = "Title Slide", master = "Office Theme") %>%
    ph_with_text(type = "ctrTitle", str = textList$ts_title) %>%
    ph_with_text(type = "subTitle", str = textList$ts_sub) %>%
    ph_with_text(type = "ftr", str = textList$footer) %>%
    
    # Summary Slide
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = textList$sumTbl_title) %>%
    ph_with_flextable(siteTable, type = "body") %>%
    ph_with_text(type = "ftr", str = textList$footer) %>%
    ph_with_text(type = "sldNum", str = "1") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
    
    # Slide with Plot
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = textList$dbp_title) %>%
    ph_with_img(type = "body", index = 1, src = dailyBarPlotPath) %>%
    ph_with_text(type = "ftr", str = textList$footer) %>%
    ph_with_text(type = "sldNum", str = "2") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
    
    # Write PowerPoint to disk
    print(target = infoList$pptxPath)
    
  return(invisible())
  
}

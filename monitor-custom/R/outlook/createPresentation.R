########################################################################
# outlook/createPresentation.R
#
# Create the 'outlook' presentation.
#
# Author: Spencer Pease, Jonathan Callahan
########################################################################

createPresentation <- function(dataList = NULL, infoList = NULL, textList = NULL) {
  
  logger.trace("----- createPresentation() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  # ----- Generate plots and tables -------------------------------------------
  
  # Generate map plot on disk
  mapPlotPath <- paste0(infoList$basePath, "_mapplot.png")
  
  png(filename = mapPlotPath)
  mapPlot(dataList, infoList, textList)
  dev.off()
  
  logger.trace('Successfully created mapPlot')
  
  # Generate hourly bar plot on disk
  hourlyBarPlotPath <- paste0(infoList$basePath, "_hourlybarplot.png")
  
  png(
    filename = hourlyBarPlotPath,
    width = 8.5, height = 5,
    units = "in", res = 150)
  
  hourlyBarPlot(dataList, infoList, textList)
  dev.off()
  
  logger.trace('Successfully created hourlyBarPlot')
   
  # Create flextable in memory
  siteTable <- regulartable(dataList$tableData) %>%
    theme_vanilla() %>%
    width(width = c(2, 1.5, 0.5, 4))
  
  logger.trace('Successfully created siteTable')
  
  # Create forecast table in memory
  
  
  forecastTable <- regulartable(dataList$forecastData) %>% 
    theme_vanilla() %>% 
    autofit() %>% 
    theme_zebra() %>% 
    addForecastInfo()
    
  
  logger.trace('Successfully created forecastTable')
  
  # ----- Create PowerPoint presentation --------------------------------------
  
  # Define styles for different elements
  style_text_discussion <- fp_text(font.size = 14)

  # Create the presentation
  pptx_deck <-
    read_pptx("pptx/blank.pptx") %>%
    
    # Title Slide
    add_slide(layout = "Title Slide", master = "Office Theme") %>%
    ph_with_text(type = "ctrTitle", str = "Outlook presentation") %>%
    ph_with_text(type = "subTitle", str = "(Testing)") %>%
    ph_with_text(type = "ftr", str = "Footer") %>%
    
    # Map slide
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "Map plot") %>%
    ph_with_img(type = "body", index = 1, src = mapPlotPath, height = 5, width = 5) %>%
    ph_with_text(type = "sldNum", str = "1") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
    ph_with_text(type = "ftr", str = "Footer") %>%
    
    # Summary Slide
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "Summary table") %>%
    ph_with_flextable(siteTable, type = "body") %>%
    ph_with_text(type = "ftr", str = "Footer") %>%
    ph_with_text(type = "sldNum", str = "2") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
    
    # Hourly Data Slide
    add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with_text(type = "title", str = "Hourly Data") %>%
    ph_with_img(type = "body", index = 1, src = hourlyBarPlotPath, height = 5, width = 8.5) %>%
    ph_with_text(type = "sldNum", str = "3") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
    ph_with_text(type = "ftr", str = "Footer") %>%
    
    # Forecast Slide
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "Forecast") %>%
    ph_with_flextable(forecastTable, type = "body") %>%
    ph_with_text(type = "ftr", str = "Footer") %>%
    ph_with_text(type = "sldNum", str = "4") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
    
    # Text Outlook
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "Outlook Discussion") %>% 
    ph_empty(type = "body") %>% 
    
    ph_add_par(id_chr = "3") %>% 
    ph_add_text(str = "Fire:", id_chr = "3") %>% 
    ph_add_par(level = 2, id_chr = "3") %>% 
    ph_add_text(str = infoList[["outlookDiscussion"]][["fire"]], style = style_text_discussion, id_chr = "3") %>% 
    
    ph_add_par(id_chr = "3") %>%
    ph_add_text(str = "Smoke:", id_chr = "3") %>% 
    ph_add_par(level = 2, id_chr = "3") %>% 
    ph_add_text(str = infoList[["outlookDiscussion"]][["smoke"]], style = style_text_discussion, id_chr = "3") %>%
    
    ph_add_par(id_chr = "3") %>%
    ph_add_text(str = "Other:", id_chr = "3") %>% 
    ph_add_par(level = 2, id_chr = "3") %>% 
    ph_add_text(str = infoList[["outlookDiscussion"]][["other"]], style = style_text_discussion, id_chr = "3") %>%
    
    ph_with_text(type = "ftr", str = "Footer") %>%
    ph_with_text(type = "sldNum", str = "5") %>%
    ph_with_text(type = "dt", str = format(Sys.Date(), "%B %d,%Y")) %>%
  
    # Write PowerPoint to disk
    print(target = infoList$pptxPath)
  
  return(invisible())
  
}


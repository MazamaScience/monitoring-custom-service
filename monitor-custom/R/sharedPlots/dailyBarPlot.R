#' @title Create Daily Barplot
#' @param infoList list containing information specifying a unique product. Generated with createInfoList()
#' @param dataList list containing all data needed to generate a product. Generated with createDataList()
#' @param textList list including character strings for labeling. Generated with createTextList()
#' @description Creates barplot of daily mean PM2.5 levels
#' @return Returns a \pkg{ggplot2} plot object.

dailyBarPlot <- function(dataList=NULL, infoList=NULL, textList=NULL) {
  
  logger.debug("----- dailyBarPlot() -----")
  
  if ( is.null(dataList) ) stop(paste0("Required parameter 'dataList' is missing."), call.=FALSE)
  if ( is.null(infoList) ) stop(paste0("Required parameter 'infoList' is missing."), call.=FALSE)
  if ( is.null(textList) ) stop(paste0("Required parameter 'textList' is missing."), call.=FALSE)
  
  # ----- Extract variables from the 'infoList' object ------------------------

  ws_monitor <- dataList$ws_monitor
  monitorIDs <- infoList$monitorIDs
  tlim <- infoList$tlim
  siteName <- ws_monitor$meta$siteName

  # Get Daily statistics (mean , max)
  
  mon <- monitor_subset(ws_monitor, tlim = tlim, monitorIDs = monitorIDs)
  
  mon_dailyMean <- monitor_dailyStatistic(mon, FUN = get('mean'), minHours=18)
  
  mon_dailyMax <- monitor_dailyStatistic(mon, FUN = get('max'), minHours=18)
  
  # ----- Gather data into tibble (for easy plotting) -------------------------
  
  # 1) convert dataframe to tibble
  # 2) for all columns other than 'datetime' replace -Inf with NaN
  # 3) convert from 'wide' to 'long' (aka 'tidy') format
  mon_dailyMax_tibble <- mon_dailyMax$data %>% 
    as.tibble() %>% 
    mutate_at(
      vars(-datetime),
      funs(replace(., is.infinite(.), NaN)) # NOTE:  max(c(NA,NA),na.rm=TRUE) returns -Inf
    ) %>% 
    gather(
      -datetime,
      key = monitor_ID,
      value = daily_max
    )

  # 1) convert dataframe to tibble
  # 2) convert from 'wide' to 'long' (aka 'tidy') format
  mon_dailyMean_tibble <- mon_dailyMean$data %>% 
    as.tibble() %>% 
    gather(
      -datetime,
      key = monitor_ID,
      value = daily_mean # TODO:  Let's round daily mean to one decimal place
    )
  
  # 1) combine tibble has columns: datetime, monitor_ID, daily_max, daily_mean
  # 2) add column: AQI_category 
  mon_stat_data <- 
    full_join(
      mon_dailyMax_tibble, 
      mon_dailyMean_tibble,
      by = c("datetime", "monitor_ID")
    ) %>% 
    mutate(
      AQI_category = 
        cut(
          daily_mean, 
          AQI$breaks_24, 
          include.lowest = T, 
          labels = AQI$names
        )
    )
  
  # ----- Create Plot ---------------------------------------------------------

  # Create labeller
  
  site_labeller <- function(meta) {
    label <- paste0(
      meta$siteName,
      " (", stringr::str_to_title(meta$countyName), ", ",
      meta$stateCode, ")"
    )
    names(label) <- meta$monitorID
    return(label)
  }
  site_label <- site_labeller(mon$meta)
  
  
  # Create scale for color filling
  color_scale <- AQI$colors
  names(color_scale) <- AQI$names
  
  g <-
    ggplot(
      data = mon_stat_data,
      aes(x = datetime, y = daily_mean, fill = AQI_category)
    ) +
    # Add bars with daily mean
    geom_bar(
      stat = 'identity'
    ) +
    # Aadd points with daily max
    geom_point(
      aes(y = daily_max),
      alpha = .4,
      size = 1.4,
      show.legend = FALSE
    ) +
    # One plot per monitor
    facet_wrap(
      ~ monitor_ID,
      labeller = labeller(monitor_ID = site_label),
      ncol = 2
    ) +
    # Use AQI colors and names
    scale_fill_manual(
      values = color_scale,
      breaks = rev(names(color_scale)), # low values on the bottom
      labels = rev(textList$AQI_category_labels),
      drop = FALSE,
      name = textList$AQI_legend_title
    )
  
  # Omit annotations for ppt as they are included in the slide
  g <- g + labs(x = '', y = '', title = '')


  # Apply user selected theme 
  theme_name <- paste0('theme_', infoList$plottheme)
  if ( exists(theme_name) ) {
    theme_function <- get(theme_name)
    g <- g + theme_function()
  } else {
    logger.debug("Theme '%s' is not a recognized plot theme", theme_name)
    # no theme applied
  }
  
  # Add legend
  g <- g + 
    theme(
      legend.justification = "top",
      legend.position = "right",
      strip.background = element_blank()
    )
  

  return(g)
  
}

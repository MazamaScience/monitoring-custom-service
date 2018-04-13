#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Daily Barplot
#' @param ws_monitor emph{ws_monitor} object
#' @param monitorID monitor ID for a specific monitor in \code{ws_monitor} (optional
#' if \code{ws_monitor} only has one monitor)
#' @param lookbackDays integer number of days to include before today. (<= 14)
#' @param today logical indicating whether to include a bar for today with current nowcast value. 
#' @param minHours minimum number of valid data hours required to calculate each daily average
#' @param aqiBar add colored stacked bar on y-axis indicating AQI levels.
#' @param aqiLines add AQI lines.
#' @param ylim custom y-axis limit, c(ylow, yhigh). Defaults to preset breaks depending on data range optimized 
#' for comparing data from different monitors.
#' @param size Styling depending on the size of the plot ("large" or "small"). "large" is optimized for 750x750 px
#' plots, and suitable for plots equal or greater than 500x500 px. "small" is optimized for 350x350 px plots, and
#' suitable for plots smaller than 500x500 px.
#' @param labels_x_nudge nudge x labels to the left
#' @param labels_y_nudge nudge y labels down
#' @param ... additional arguments to be passed to \code{barplot()}
#' @description Creates a bar plot showing daily average PM 2.5 values for a specific monitor
#' in a emph{ws_monitor} object. Each bar is colored according to its AQI category.
#' 
#' This function is a wrapper around \code{base::barplot} and any arguments to that 
#' function may be used.
#' 
#' Each 'day' is the midnight-to-midnight period in the monitor local timezone.
#' When \code{tlim} is used, it is converted to the monitor local timezone.
#' @details The \code{labels_x_nudge} and \code{labels_y_nudge} can be used to 
#' tweak the date labeling. Units used are the same as those in the plot.
#' @examples
#' \dontrun{
#' }

monitorPlot_latestDailyBarplot <- function(ws_monitor,
                                        monitorID=NULL,
                                        lookbackDays=7,
                                        today=TRUE,
                                        minHours=20,
                                        aqiBar=TRUE,
                                        aqiLines=TRUE,
                                        ylim=NULL,
                                        size="large",
                                        labels_x_nudge=0,
                                        labels_y_nudge=0,
                                        ...) {
  
  
  if (FALSE) {
    ws_monitor <- airnow_loadLatest()
    monitorID <- "80011G015_01"
    lookbackDays <- 7
    today=TRUE
    minHours=20
    aqiBar = TRUE
    aqiLintes = TRUE
    ylim = NULL
    size = "large"
    labels_x_nudge = 0
    labels_y_nudge = 0
  }
  
  # Data Preparation ----------------------------------------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for > 1 monitor. Please specify a monitorID from: '",
                  paste(ws_monitor$meta$monitorID,collapse="', '"),"'"))
    }
  }
  
  # Set tlim based on lookbackDays
  if ( as.numeric(lookbackDays) > 10 || as.numeric(lookbackDays) < 1 || is.na(as.numeric(lookbackDays)) ) {
    stop("lookbackDays must be an integer between 1 and 10")
  } else {
    endtime <- lubridate::now() 
    starttime <- lubridate::now(tzone = ws_monitor$meta[monitorID, "timezone"]) - lubridate::days(lookbackDays-1)
    lubridate::hour(starttime) <- 0
    lubridate::minute(starttime) <- 0
    lubridate::second(starttime) <- 0
    tlim = c(starttime, endtime) 
  }
  
  # # Is there data for the given tlim?
  # if ( sum(!is.na(monitor_subset(dataList$ws_monitor, tlim = infoList$tlim, dropMonitors = FALSE)$data[monitorid])) == 0 ) {
  #   stop( paste("no data for",monitorid,"at the specified dates") , call. = FALSE)
  # }
  
  # Subset to a single monitor
  timezone <- as.character(ws_monitor$meta[monitorID,'timezone'])
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim, dropMonitors = FALSE)
  # draw "no data" plot if there is no data
  if ( sum(!is.na(mon$data[monitorID])) <=3 ) {
    usr_mar <- par("mar")
    par(mar = c(2,2,2,2))
    plot(1,1,col = "transparent", ann = F, axes = F)
    box()
    text(1,1, paste0("Missing or insufficient data\nfor the past ", lookbackDays, " days."), cex = ifelse(size == "small", 2, 4))
    par(usr_mar)
    return()
  }
  
  # Calculate the daily mean
  if (lookbackDays > 1) {
    mon_dailyMean <- monitor_dailyStatistic(mon, FUN=get('mean'), dayStart='midnight',
                                            na.rm=TRUE, minHours=minHours)
    
    # Make sure the mean for today is not included
    if ( mon_dailyMean$data$datetime[nrow(mon_dailyMean$data)] == lubridate::today(tzone = "UTC") ) {
      mon_dailyMean$data <- mon_dailyMean$data[1:nrow(mon_dailyMean$data)-1,]
    }
    
    localTime <- mon_dailyMean$data$datetime
    pm25 <- as.numeric(mon_dailyMean$data[,monitorID])
  } else {
    localTime <- mon$data$datetime[length(mon$data$datetime)]
    xAxisLabels <- character()
    pm25 <- numeric()
  }
  
  
  if ( today ) {
    
    # Get the most recent NowCast value
    mon_nowcast <- monitor_nowcast(mon)
    nowcastIndex <- max(which(!is.na(mon_nowcast$data[,2])))
    if ( lubridate::now(tzone = "UTC") - mon_nowcast$data[nowcastIndex,1] <= lubridate::dhours(6) ) {
      nowcast <- mon_nowcast$data[nowcastIndex,2]
    } else {
      nowcast <- NA
    }
    
  }
  
  # draw "no data" plot if there are no daily means
  if (sum(!is.na(mon_dailyMean$data[monitorID]))+sum(!is.na(nowcast)) == 0 ) {
    usr_mar <- par("mar")
    par(mar = c(2,2,2,2))
    plot(1,1,col = "transparent", ann = F, axes = F)
    box()
    text(1,1, paste0("Missing or insufficient data\nfor the past ", lookbackDays, " days."), cex = ifelse(size == "small", 2, 4))
    par(usr_mar)
    return()
  }
  
  # Plot command default arguments ---------------------------------------------
  
  argsList <- list(...)  
  
  # Defaults for small vs large 
  if (size == "large") {
    argsList$cex.names <- ifelse('cex.names' %in% names(argsList), argsList$cex.names, 1)
    argsList$cex.axis <- ifelse('cex.axis' %in% names(argsList), argsList$cex.axis, 1)
    argsList$cex.main <- ifelse("cex.main" %in% names(argsList), argsList$cex.main, 1.5)
    argsList$cex.lab <- ifelse("cex.lab" %in% names(argsList), argsList$cex.lab, 1.5)
    argsList$tcl <- ifelse("tcl" %in% names(argsList), argsList$tcl, -.5)
    xAxisLabels <- strftime(localTime, "%b %d", tz = timezone)
    mar <- c(3,5,8,1.8)
    argsList$mgp <- c(3,1,0) # margin line for: c(axisTitle, axisLabels, axisLine)
    nowcastText <- "Current \n NowCast"
  } else {
    argsList$cex.names <- ifelse('cex.names' %in% names(argsList), argsList$cex.names, .8)
    argsList$cex.axis <- ifelse('cex.axis' %in% names(argsList), argsList$cex.axis, .8)
    argsList$cex.main <- ifelse("cex.main" %in% names(argsList), argsList$cex.main, 1)
    argsList$cex.lab <- ifelse("cex.lab" %in% names(argsList), argsList$cex.lab, 1)
    argsList$tcl <- ifelse("tcl" %in% names(argsList), argsList$tcl, -.2)
    argsList$padj <- ifelse("padj" %in% names(argsList), argsList$padj, .5)
    xAxisLabels <- strftime(localTime, "%b\n%d", tz = timezone)
    mar <- c(2.6,3.5,3.6,.8)
    argsList$mgp <- c(2,.5,0)
    nowcastText <- "Now-\nCast"
  }
  
  
  # Prepare data and labels 
  if ( today ) {
    argsList$height <- c(pm25, nowcast)
  } else {
    argsList$height <- pm25
  }

  # Format X axis
  
  # X axis labeling 
  # if (size == "small" && !"xaxt" %in% names(argsList)) {argsList$xaxt <- 'n'} # in this case, x-axis will be handled while plotting
  
  if ( today ) {
    if ( !"names.arg" %in% names(argsList) ) { 
      argsList$names.arg <-  c(xAxisLabels, nowcastText) 
      }
  } else {
    if ( !"names.arg" %in% names(argsList) ) {argsList$names.arg <- xAxisLabels}
  }
  
  
  # Y axis labeling
  if ( !('ylab' %in% names(argsList)) ) {
    argsList$ylab <- "PM2.5 (\U00B5g/m3)"
  }
  
  # Set ylim
  if (is.null(ylim)) {
    ylo <- 0
    ymax <- max( argsList$height, na.rm = TRUE )
    if ( ymax <= 50 ) {
      yhi <- 50
    } else if ( ymax <= 100 ) {
      yhi <- 100
    } else if ( ymax <= 200 ) {
      yhi <- 200
    } else if ( ymax <= 400 ) {
      yhi <- 400
    } else {
      yhi <- 600
    }
    argsList$ylim <- c(ylo, yhi)
  } else {
    argsList$ylim <- ylim
  }
  
  # Default colors come from pm25Daily means
  if ( !('col' %in% names(argsList)) ) {
    aqiColors <- AQI$colors
    argsList$col <- aqiColors[ .bincode(argsList$height, AQI$breaks_24, include.lowest=TRUE) ]
  }
  
  # Additional small tweaks
  argsList$las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Title
  if ( !('main' %in% names(argsList)) ) {
    ### argsList$main <- expression(paste("Daily Average PM"[2.5])) # subscript 2.5
    argsList$main <- paste("Daily Average PM2.5 \n Site:", mon$meta$siteName)
  }
  
  argsList$main <- ifelse("main" %in% names(argsList), argsList$main, paste("Daily Average PM2.5 \n Site:", mon$meta$siteName))

  
  # Explicitly declare defaults for use in creating the x axis
  argsList$axes <- ifelse('axes' %in% names(argsList), argsList$axes, TRUE)
  argsList$space <- ifelse('space' %in% names(argsList), argsList$space, 0.2)
  
  # Set graphical parameters
  user_mar <- par("mar")
  par(mar = mar)
  
  # Plotting ------------------------------------------------------------------
  # Make standard barplot
  
  # Draw AQI lines first
  # Create blank plot
  argsListBlank <- argsList
  argsListBlank$col  <- 'transparent'
  argsListBlank$axes <- FALSE
  argsListBlank$main <- ""
  argsListBlank$xlab <- ""
  argsListBlank$ylab <- ""
  argsListBlank$names.arg <- ""
  do.call(barplot,argsListBlank)
  argsList$add <- TRUE
  
  # Add AQI lines
  if (aqiLines) {
    abline(h = AQI$breaks_24, col = adjustcolor(AQI$colors, .5), lwd = 3)
  }
  
  
  do.call(barplot, argsList)
  
  # # If needed, add diagonal x-axis text
  # # Add default X axis
  # if ( size=="small" && argsList$axes ) {
  #   barCount <- length(argsList$height)
  #   indices <- 1:barCount
  #   labels <- argsList$names.arg
  #   labels_x <- (indices - 0.5) + (indices * argsList$space)
  #   labels_y <- -0.07 * (par('usr')[4] - par('usr')[3])
  #   text(labels_x - labels_x_nudge, labels_y - par("usr")[4]*labels_y_nudge, labels, srt=45, cex=argsList$cex.names, xpd=NA)
  # }
  
  # If we gave a 'today' bar, use nowcast (forecast) value and identify it as a forecast with gray border and diagonal lines
  if ( today ) {
    argsList$border <- c(rep("#000000", length(argsList$height)-1), "gray90")
    argsList$col[length(argsList$col)] <- ifelse(argsList$col[length(argsList$col)] %in% AQI$colors[1:2], "gray60", "gray90")
    argsList$density <- c(rep(0, length(argsList$height)-1), 16) #For some reason when I set density to NA, when height[1] is NA, everything comes out black.
    #argsList$lwd <- 5 # NOTE:  lwd is not applied to the rectangles or diagonal lines
    argsList$main <- ""
    argsList$axes <- FALSE
    argsList$names.arg <- NULL
    argsList$ylab <- ""
    argsList$xlab <- NULL
    argsList$add <- TRUE
    do.call(barplot, argsList)
  }
  

  
 
  # Add AQI stacked bar
  if (aqiBar) {
    addAQIStackedBar(width = .02)
  }
  
  
  
  # add gray background for any days with missing data
  usr <- par("usr")
  usr[2]=usr[2]+usr[1]
  usr[1]=0
  dim <- c(usr[2]-usr[1], usr[4]-usr[3])
  width <- (dim[1])/(.2*(length(argsList$height)+1)+length(argsList$height))
  for (i in 1:length(argsList$height)) {
    value <- argsList$height[i]
    if ( is.na(value) ) {
      left <- (argsList$space*width)*(i) + (width)*(i-1)
      bottom <- -usr[4]*.06
      right <- (argsList$space*(width)+width)*i
      top <- usr[4]*1.02
      rect(left, bottom, right, top, col = adjustcolor("black", alpha.f = .1), border = NA, xpd=NA)
      if ( !( today && i == length(argsList$height) ) ) { 
        text(x=(right+left)/2, y=usr[4]*.55, "Insufficient\nData", cex = .8)  
      }
    }
  }
  
  # add text for today
  if ( today ) {
    if (is.na(nowcast)){
      text(x=usr[2]-(width+2*argsList$space)/2, y=usr[4]*.55, "No \n Current \n Nowcast", cex = .8) 
    }
  }
  
  # Reset parameters to user-defined values
  par(mar = user_mar)
  
}


#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Time of Day Spaghetti Plot
#' @param ws_monitor emph{ws_monitor} object
#' @param monitorID id for a specific monitor in the ws_monitor object
#' @param lookbackDays integer number of days to include before today. (<= 14)
#' @param aqiBar add colored stacked bar on y-axis indicating AQI levels.
#' @param aqiLines add AQI lines.
#' @param ylim custom y-axis limit, c(ylow, yhigh). Defaults to preset breaks depending on data range optimized 
#' for comparing data from different monitors.
#' @param size Styling depending on the size of the plot ("large" or "small"). "large" is optimized for 750x750 px
#' plots, and suitable for plots equal or greater than 500x500 px. "small" is optimized for 350x350 px plots, and
#' suitable for plots smaller than 500x500 px.
#' @param shadedNight add nighttime shading based on of middle day in selected period
#' @param previousDays include lines for days before yesterday
#' @param ... additional arguments to pass to \code{plot()}
#' @description Creates a spaghetti plot of PM2.5 levels by hour for one or more days. The average by hour over 
#' the period is also calculated and plotted as a thick red line.
#' @examples
#' monitorPlot_timeOfDaySpaghetti(CarmelValley, tlim=c(20160801,20160809))

monitorPlot_latestDailyByHour  <- function(ws_monitor,
                                           monitorID=NULL,
                                           lookbackDays=7,
                                           aqiBar=TRUE,
                                           aqiLines=TRUE,
                                           ylim=NULL,
                                           size="large",
                                           shadedNight=TRUE,
                                           style="aqidots",
                                           previousDays=FALSE,
                                           ...) {
  
  if (FALSE) {
    ws_monitor = airnow_loadLatest()
    monitorID="060271003_01"
    lookbackDays=2
    aqiBar=TRUE
    aqiLines=TRUE
    ylim=NULL
    size="large"
    shadedNight=TRUE
    style="aqidots"
    previousDays=FALSE
  }
  
  # Data Preparation ----------------------------------------------------------
  
  # Allow single monitor objects to be used without specifying monitorID
  if ( is.null(monitorID) ) {
    if ( nrow(ws_monitor$meta) == 1 ) {
      monitorID <- ws_monitor$meta$monitorID[1]
    } else {
      stop(paste0("ws_monitor object contains data for >1 monitor. Please specify a monitorID from: '",
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
  
  # Subset to a single monitor
  timezone <- as.character(ws_monitor$meta[monitorID,'timezone'])
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, tlim=tlim, dropMonitors = FALSE)
  if ( sum(!is.na(mon$data[monitorID])) <=3 ) {
    usr_mar <- par("mar")
    par(mar = c(2,2,2,2))
    plot(1,1,col = "transparent", ann = F, axes = F)
    box()
    text(1,1, paste0("Missing or insufficient data\nfor the past ", lookbackDays, " days."), cex = ifelse(size == "small", 2, 4))
    par(usr_mar)
    return()
  }
  
  # Insitu data requested
  pm25 <- mon$data[,monitorID]
  
  # Create local time dates and hours
  localTime <- lubridate::with_tz(mon$data$datetime, timezone)
  date <- lubridate::date(localTime)
  hour <- lubridate::hour(localTime)
  
  # Create a new dataframe with columns we can use to uniquely identify separate days
  tbl <- data_frame(localTime,pm25,date,hour)
  lastHour <- lubridate::hour(tbl$localTime[nrow(tbl)])
  uniqueDays <- unique(tbl$date)
  dayCount <- length(uniqueDays)
  # Make sure the data includes 23 hours for the most current day
  if (lastHour < 23) {
    lastDate <- tbl$date[nrow(tbl)]
    additionalRows <- data_frame(localTime = as.POSIXct(lastDate), hour = (lastHour+1):23)
    for (row in 1:nrow(additionalRows)) {
      lubridate::hour(additionalRows[[row, "localTime"]]) <- additionalRows[[row,"hour"]]
    }
    tbl <- add_row(tbl, localTime = additionalRows[["localTime"]], date = lastDate, hour = additionalRows[["hour"]])
    pm25 <- tbl$pm25
  }
  
  # Plot command default arguments --------------------------------------------
  
  argsList <- list(...)
  
  argsList$ylab <- ifelse("ylab" %in% names(argsList), argsList$ylab, "PM2.5 (\U00B5g/m3)")
  argsList$add  <- ifelse("add"  %in% names(argsList), argsList$add , FALSE)
  argsList$main <- ifelse("main" %in% names(argsList), argsList$main, paste0("Hourly PM2.5 Values by Time of Day\nSite: ", mon$meta$siteName))
  argsList$axes <- ifelse("axes" %in% names(argsList), argsList$axes, TRUE)
  argsList$xaxt <- ifelse("xaxt" %in% names(argsList), argsList$xaxt, "s")
  argsList$yaxt <- ifelse("yaxt" %in% names(argsList), argsList$yaxt, "s")
  
  # Defaults for small vs large 
  if (size == "large") {
    # axes
    cex.axis <- ifelse('cex.axis' %in% names(argsList), argsList$cex.axis, 1)
    argsList$mgp <- c(3,1,0) # margin line for: c(axisTitle, axisLabels, axisLine)
    tcl <- ifelse("tcl" %in% names(argsList), argsList$tcl, -0.5) # tick length (default = -0.5)
    padj <- ifelse("padj" %in% names(argsList), argsList$padj, NA) # x-axis tick label adjustment
    # Title and labels
    cex.main <- ifelse("cex.main" %in% names(argsList), argsList$cex.main, 1.5)
    title.line <- 4
    argsList$cex.lab <- ifelse("cex.lab" %in% names(argsList), argsList$cex.lab, 1.5)
    xaxsLabels <- c("Midnight", "3am", "6am", "9am", "Noon", "3pm", "6pm", "9pm")
    argsList$xlab <- ifelse("xlab" %in% names(argsList), argsList$xlab, "Time of Day")
    # Plotting
    mar <- c(5,5,9,3)
    meanLwd <- 20
    todayptcex <- 2
    yestptcex <- 1.5
    # legend
    seg.length <- c(2,2,2)
    widthscale <- 6
    cex.leg <- 1
  } else {
    # axes
    cex.axis <- ifelse('cex.axis' %in% names(argsList), argsList$cex.axis, .9)
    argsList$mgp <- c(2,.5,0)
    tcl <- -0.3
    padj <- ifelse("padj" %in% names(argsList), argsList$padj, -.5)
    # Title and labels
    cex.main <- ifelse("cex.main" %in% names(argsList), argsList$cex.main, 1)
    title.line <- 2
    argsList$cex.lab <- ifelse("cex.lab" %in% names(argsList), argsList$cex.lab, 1)
    xaxsLabels <- c("Mid", "3a", "6a", "9a", "Noon", "3p", "6p", "9p")
    argsList$xlab <- ifelse("xlab" %in% names(argsList), argsList$xlab, "")
    # Plotting
    mar <- c(2.6,3.5,5.2,1)
    meanLwd <- 15
    todayptcex <- 1.5
    yestptcex <- 1
    # legend
    seg.length <- c(1.5,1.5,1.5)
    widthscale <- 5.5
    cex.leg <- .8
  }
  
  # Default to the Y axis using horizontal tick labels
  las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Set ylim
  if (is.null(ylim)) {
    ylo <- 0
    ymax <- max( tbl$pm25, na.rm = TRUE )
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
  
  
  # Set graphical parameters
  user_mar <- par("mar")
  par(mar = mar)
  
  # ----- Plotting -----------------------
  
  # Base plot for background
  if ( !argsList$add ) {
    
    # Create blank plot
    argsListBlank <- argsList
    argsListBlank$col <- 'transparent'
    argsListBlank$axes <- FALSE
    argsListBlank$x <- c(0,23)
    argsListBlank$y <- c(NA,NA)
    argsListBlank$main <- ""
    do.call(plot,argsListBlank)
    
    
    # Add axes
    box()
    if (argsList$axes) {
      if (argsList$xaxt != 'n') {
        axis(1, at=seq(0,21,3), labels = xaxsLabels, col="transparent", col.ticks = 1, mgp = argsList$mgp, cex.axis = cex.axis, tcl = tcl, las = las, padj = padj)
      }
      if (argsList$yaxt != 'n') {
        axis(2, col="transparent", col.ticks = 1, mgp = argsList$mgp, cex.axis = cex.axis, tcl = tcl, las = las)
      }
    }
    
    # Add Title
    title(main = argsList$main, cex.main = cex.main, line = title.line, font.main = argsList$font.main, col.main = argsList$col.main)
    
    # Legend
    usr <- par("usr")
    l <- usr[1]
    r <- usr[2]
    w <- r-l
    legend(x = l+w/2, y=usr[4]*1.01,
           c("Today", "Yesterday", paste0(lookbackDays, " Day Mean")),
           lwd = c(2,2,meanLwd),
           seg.len = seg.length,
           col = c("black", "gray50", adjustcolor("black", .3)),
           pt.cex = c(todayptcex, yestptcex, NA),
           pch = c(21, 21, NA),
           pt.bg = c("green", "green", NA),
           horiz = TRUE,
           cex = cex.leg,
           xpd = NA,
           xjust = 0.5, yjust = 0,
           text.width = w/widthscale,
           bty="n",
           text.font = 3
    )
    
    
    
    # Shaded Night
    if ( shadedNight ) {
      
      # Get the sunrise/sunset information
      ti <- timeInfo(localTime, longitude=mon$meta$longitude, latitude=mon$meta$latitude, timezone=timezone)
      
      # Extract the middle row
      ti <- ti[round(nrow(ti)/2),]
      
      # Get sunrise and sunset in units of hours
      sunrise <- lubridate::hour(ti$sunrise) + lubridate::minute(ti$sunrise)/60
      sunset <- lubridate::hour(ti$sunset) + lubridate::minute(ti$sunset)/60
      
      # Left edge to sunrise
      rect(par('usr')[1], ybottom=par('usr')[3],
           xright=sunrise, ytop=par('usr')[4],
           col=adjustcolor('black',0.1), lwd=0)
      
      # Sunset to right edge
      rect(xleft=sunset, ybottom=par('usr')[3],
           xright=par('usr')[2], ytop=par('usr')[4],
           col=adjustcolor('black',0.1), lwd=0)
      
    }
  }
  
  # Add AQI lines
  if (aqiLines) {
    abline(h = AQI$breaks_24, col = adjustcolor(AQI$colors, .5), lwd = 3)
  }
  
  # Add AQI stacked bar
  if (aqiBar) {
    addAQIStackedBar(width = .02)
  }
  
  
  # Lines for previous days
  if (lookbackDays > 2) {
    if ( previousDays ) {
      previousDates <- uniqueDays[1:(length(uniqueDays)-2)]
      for (thisDay in previousDates) {
        dayTbl <- tbl[tbl$date == thisDay,]
        lines(dayTbl$pm25 ~ dayTbl$hour, col = "gray80", lwd = 1)
      }
    }
  }
  
  
  # Add mean line
  hourMeanTbl <- tbl %>% group_by(as.factor(hour)) %>% summarize(pm25=mean(pm25,na.rm=TRUE))
  points(hourMeanTbl$pm25 ~ seq(0,23,1), col = adjustcolor("black", .3),  type = "l", lwd = meanLwd)
  
  # Line for yesterday
  yesterday <- lubridate::today(tzone = timezone)-lubridate::days(1)
  if ( style == "aqidots" ) {
    dayTbl <- tbl[tbl$date == yesterday,]
    yestbg <- AQI$colors[ .bincode(dayTbl$pm25, AQI$breaks_24, include.lowest=TRUE) ]
    points(dayTbl$pm25 ~ dayTbl$hour, col="gray50", bg = yestbg, pch = 21, cex = yestptcex, type="o", lwd = 2)
  } else {
    dayTbl <- tbl[tbl$date == yesterday,]
    points(dayTbl$pm25 ~ dayTbl$hour, type = "l", col = "gray50", pch = 21, lwd = 2)
  }
  
  # Line for today
  if ( style == "aqidots" ) {
    today <- lubridate::today(tzone = timezone)
    dayTbl <- tbl[tbl$date == today,]
    todaybg <- AQI$colors[ .bincode(dayTbl$pm25, AQI$breaks_24, include.lowest=TRUE) ]
    points(dayTbl$pm25 ~ dayTbl$hour, col=1, bg = todaybg, pch = 21, cex = todayptcex, type="o", lwd = 2)
  } else {
    today <- lubridate::today(tzone = timezone)
    dayTbl <- tbl[tbl$date == today,]
    points(dayTbl$pm25 ~ dayTbl$hour, type = "l", col = 1, pch = 1, lwd = 2)
  }
  
  
  # Reset parameters to user-defined values
  par(mar = user_mar)
  
  
}

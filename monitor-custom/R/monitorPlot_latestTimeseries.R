#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create XXX Plot
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
#' @param dayLines include vertical grid lines at the start of each day
#' @param hourLines include hourly vertical grid lines
#' @param gridLines include horizontal grid lines
#' @param hourlyDots include dots for hourly PM2.5 values
#' @param nowcast include line indicating hourly NowCast values
#' @param ... additional arguments to pass to \code{plot()}
#' @description Creates a timeseries plot of latest PM2.5 data for a single monitor
#' @examples
#' \dontrun{
#' }

monitorPlot_latestTimeseries<- function(ws_monitor,
                                        monitorID = NULL,
                                        lookbackDays = 7,
                                        aqiBar = TRUE,
                                        aqiLines = TRUE,
                                        ylim = NULL,
                                        size = "large",
                                        shadedNight = FALSE,
                                        dayLines = TRUE,
                                        hourLines = TRUE,
                                        gridLines = TRUE,
                                        hourlyDots = TRUE,
                                        nowcast = TRUE,
                                        ...
                                        ) {
  
  
  
  if (FALSE) {
    ws_monitor = airnow_loadLatest()
    monitorID="060271003_01"
    lookbackDays=7
    aqiBar=TRUE
    aqiLines=TRUE
    ylim=NULL
    size="large"
    shadedNight=TRUE
    dayLines = TRUE
    hourLines = TRUE
    gridLines = TRUE
    hourlyDots = TRUE
    nowcast = TRUE
  }
  
  # ----- Data Preparation ----------------------------------------------------
  
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
  mon <- monitor_subset(ws_monitor, monitorIDs=monitorID, dropMonitors = FALSE)
  
  
  # Get NowCast
  if ( nowcast ) {
    mon_nowcast <- monitor_nowcast(mon)
  }
  
  # Cut down according to tlim
  mon <- monitor_subset(mon, tlim = tlim, dropMonitors = FALSE)
  if ( sum(!is.na(mon$data[monitorID])) <=3 ) {
    usr_mar <- par("mar")
    par(mar = c(2,2,2,2))
    plot(1,1,col = "transparent", ann = F, axes = F)
    box()
    text(1,1, paste0("Missing or insufficient data\nfor the past ", lookbackDays, " days."), cex = ifelse(size == "small", 2, 4))
    par(usr_mar)
    return()
  }
  
  
  timezone <- mon$meta$timezone
  times <- lubridate::with_tz(mon$data$datetime, tzone=timezone)
  hours <- lubridate::hour(times)
  
  # ----- Style ---------------------------------------------------------------
  argsList <- list(...)
  
  argsList$ylab <- ifelse("ylab" %in% names(argsList), argsList$ylab, "PM2.5 (\U00B5g/m3)")
  argsList$add  <- ifelse("add"  %in% names(argsList), argsList$add , FALSE)
  argsList$main <- ifelse("main" %in% names(argsList), argsList$main, paste0("Hourly PM2.5 Values and NowCast\nSite: ", mon$meta$siteName))
  argsList$axes <- ifelse("axes" %in% names(argsList), argsList$axes, TRUE)
  argsList$xaxt <- ifelse("xaxt" %in% names(argsList), argsList$xaxt, "s")
  argsList$yaxt <- ifelse("yaxt" %in% names(argsList), argsList$yaxt, "s")
  
  # Defaults for small vs large
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
    argsList$xlab <- ifelse("xlab" %in% names(argsList), argsList$xlab, "Local Time")
    # Plotting
    mar <- c(5,5,9,3)
    hourInterval <- 3 
    # legend
    cex.leg <- 1
  } else {
    # axes
    cex.axis <- ifelse('cex.axis' %in% names(argsList), argsList$cex.axis, .9)
    argsList$mgp <- c(2,.5,0)
    tcl <- -0.3
    padj <- ifelse("padj" %in% names(argsList), argsList$padj, .5)
    # Title and labels
    cex.main <- ifelse("cex.main" %in% names(argsList), argsList$cex.main, 1)
    title.line <- 2
    argsList$cex.lab <- ifelse("cex.lab" %in% names(argsList), argsList$cex.lab, 1)
    argsList$xlab <- ifelse("xlab" %in% names(argsList), argsList$xlab, "")
    # Plotting
    mar <- c(2.6,3.5,5.2,1)
    hourInterval <- 6
    # legend
    cex.leg <- .8
  }
  
  
  if (size == "large") {
  } else {
  }
    
  # Default to the Y axis using horizontal tick labels
  las <- ifelse('las' %in% names(argsList), argsList$las, 1)
  
  # Set ylim
  if (is.null(ylim)) {
    ylo <- 0
    ymax <- max( mon$data[monitorID], na.rm = TRUE )
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
  
  # ----- Plotting ------------------------------------------------------------
  
  # Any changes to overall graphical paramters, eg. mar, 
  # Set graphical parameters
  user_mar <- par("mar")
  par(mar = mar)
  
  # Create blank plot
  argsListBlank <- argsList
  argsListBlank$col  <- 'transparent'
  argsListBlank$axes <- FALSE
  argsListBlank$main <- ""
  argsListBlank$x    <- times
  argsListBlank$y    <- mon$data[[monitorID]]
  do.call(plot,argsListBlank)
  
  
  # Add box and axes
  box()
  # y-axis
  axis(2, col="transparent", col.ticks = 1, mgp = argsList$mgp, cex.axis = cex.axis, tcl = tcl, las = las)
  # x-axis
  if ( argsList$axes && is.null(argsList$names.arg) ) {
    days <- seq(tlim[1], tlim[2], by = "days")
    if ( size == "large" ) {labels <- strftime(days, "%b %d", tz=timezone)} else {labels <- strftime(days, "%b\n%d", tz=timezone)}
    # Now add tick marks
    axis(1, at=days, labels=labels, col="transparent", col.ticks = 1, mgp = argsList$mgp, cex.axis = cex.axis, tcl = tcl, las = las, padj = padj)
  }
  
  # Add grid lines
  if ( dayLines ) {
    abline(v = times[which(hours%%24 == 0)], lty = "dashed", col = "gray80")
  }
  
  if ( hourLines ) {
    abline(v = times[which(hours%%hourInterval == 0)], lty = "dotted", lwd = 1, col = "gray80")
  }
  
  if ( gridLines ) {
    abline(h=axTicks(2)[-1], col="gray80", lty="dashed")
    abline(h=0, col="gray80", lty="dashed")
  }
  
  # Shaded Night
  if ( shadedNight ) {
    lat <- mean(mon$meta$latitude)
    lon <- mean(mon$meta$longitude)
    timeInfo <- PWFSLSmoke::timeInfo(times, lon, lat, timezone)
    addShadedNight(timeInfo)
  }

  # Add AQI lines
  if (aqiLines) {
    abline(h = AQI$breaks_24, col = adjustcolor(AQI$colors, .5), lwd = 3)
  }
  
  
  # Add hourly data
  monitorPlot_timeseries(mon, add = TRUE, pch = 16, col = adjustcolor("gray20", alpha.f = .3))
  
  # Add Nowcast line
  monitorPlot_timeseries(mon_nowcast, add = TRUE, type = "l", lwd = 2)
  
  # Add AQI stacked bar
  if (aqiBar) {
    addAQIStackedBar(width = .02)
  }
  
  # ----- Annotations ---------------------
  
  # Add Title
  title(main = argsList$main, cex.main = cex.main, line = title.line, font.main = argsList$font.main, col.main = argsList$col.main)
  
  # Legend
  usr <- par("usr")
  l <- usr[1]
  r <- usr[2]
  w <- r-l
  legend(x = r, y=usr[4]*1.01,
         legend =  c("Hourly PM2.5 Values", "NowCast"),pch = c(16,NA), lty = c(NA,1), 
         col = c(adjustcolor("gray20", alpha.f = .3),1), lwd = c(NA,2), cex = cex.leg,
         horiz = TRUE,
         xpd = NA,
         xjust = 1, yjust = 0,
         bty="n",
         text.font = 3, 
         text.width = w/3
  )
  
  # Reset margins and other graphical parameters
  par(mar=user_mar)
  
}


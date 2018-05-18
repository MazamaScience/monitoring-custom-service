# uptime chart

createProduct <- function(serverid='tools-c3', ymax=1.5) {
  
  # Read in data
  logUrl <- paste0('https://',serverid,'.airfire.org/logs/uptime.log')
  df <- readr::read_csv(logUrl, col_names=c('datetime','hms','user','load_1_min','load_5_min','load_15_min'))

  # Clean up data  
  df$datetime <- lubridate::ymd_hms( stringr::str_sub(df$datetime, 1, 19) )
  df$hms <- NULL
  df$user <- NULL
  df$load_1_min <- as.numeric( stringr::str_replace(df$load_1_min, "load average: ", "") )

  # Plot data  
  plot(df$load_15_min ~ df$datetime, ylim=c(0,ymax), type='s', lwd=2, las=1, xpd=NA)
  points(df$load_1_min ~ df$datetime, pch=17, cex=df$load_1_min * 1, col='red', xpd=NA)
  
}



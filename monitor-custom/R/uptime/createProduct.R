# uptime chart

df <- readr::read_csv('https://tools-c2.airfire.org/logs/uptime.log', col_names=c('datetime','hms','user','load_1_min','load_5_min','load_15_min'))

df$datetime <- lubridate::ymd_hms( stringr::str_sub(df$datetime, 1, 19) )
df$hms <- NULL
df$user <- NULL
df$load_1_min <- as.numeric( stringr::str_replace(df$load_1_min, "load average: ", "") )

plot(df$load_15_min ~ df$datetime, ylim=c(0,1.5), type='s', lwd=2)
points(df$load_1_min ~ df$datetime, pch=17, cex=df$load_1_min * 1, col='red')



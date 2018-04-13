library("PWFSLSmoke")
airnow <- airnow_loadLatest()
winthrop <- monitor_subset(airnow, monitorIDs = "530470010")
ws_monitor <- winthrop
monitorID <- ws_monitor$meta$monitorID
tlim <- c(20170908,20170914)
size <- 700
siteName <- ws_monitor$meta$siteName
timezone <- ws_monitor$meta$timezone
style <- "web"
infoList <- list(lookbackdays=7)


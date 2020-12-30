# Updates to the monitor-custom service

----

# Version 4.x -- PWFSLSmoke version 4 metadata

## Version 4.4 -- 'beakr' refactor

### monitor-data 4.4.1

 * refactor to use `AirMonitorPlots::monitor_ggDailyHourlyBarplot()`
 * validate existence of `dataDir` if used
 * updated to docker image mazamascience/pwfslsmoke:1.2.113

### monitor-data 4.4.0

 * using beakr 0.3.1
 * refactored directory structure to "latest Mazama standard"
 * removed `uptime` sub-service
 
# Version 1.x -- 2018 Fire Season

## Version 1.1 -- dailyhourlybarplot

### monitor-custom 1.1.7

 * using pwfslsmoke:1.0.33

### monitor-custom 1.1.6

 * using PWFSLSmoke v1.0.29
 * general cleanup

### monitor-custom 1.1.5

 * logging environment variables
 * updated API documentation
 
### monitor-custom 1.1.4

 * uptime service now displays free_memory
 
### monitor-custom 1.1.3

 * updated to use new PWFSLSmokePlots
 
### monitor-custom 1.1.2

 * improved styling for `dailyhourlybarplot()`
 
### monitor-custom 1.1.1

 * consistent versioning and NEWS.md
 
## Verssion 1.0 -- uptime

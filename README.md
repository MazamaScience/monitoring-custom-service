# monitor-custom #

This directory contains a dockerizable webservice that runs R
code and returns various types of custom products.

## Reboot Instructions for an Operational Site

```make production_bounce```

*(Yes, that's it.)*

***
***

Now for the gory details.

## Files ##

The basic files required for a product-only webservice (no UI) are the following:

```
├── Makefile
├── NEWS.md
├── README.md
├── docker-compose-desktop.yml
├── docker-compose-test.yml
├── docker-compose.yml
└── monitor-custom/
    ├── Dockerfile
    ├── Dockerfile-test
    ├── R/
    ├── logs/
    ├── monitor-custom-app.R
    └── output/
```

## Running the app from RStudio ##

Outside of RStudio you must first:

 * `make desktop_download_data`

Inside RStudio you can run the app with:

 * `cd monitor-custom/`
 * "Set As Working Directoroy"
 * open up `monitor-custom-app.R`
 * set breakpoints if desired
 * click the 'Source' button

The app will be available at:

[localhost:8080/monitor-custom/dev/](localhost:8080/monitor-custom/dev/)

## Running the app with Docker ##

You can create and run a docker image with the app by:

 * typing `make desktop_reboot`
 
The app will be available at:

[localhost:8080/monitor-custom/test/](localhost:8080/monitor-custom/test/)
 
The `Makefile` has targets for three different types of deployment: `desktop`, `test`, `production`.

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

## Host Computer Settings ##

### Data Files ###

Latest data .RData files are mounted from the following directory which must exist:

`/data/monitoring/RData`

Currently, data are being generated on 'haze' so any other computer hosting this
docker app must have a crontab to regularly copy latest data files.

### Log Files ###

Log files will be written to a directory named after the deployment type:

`/var/log/monitor-custom/test/app` or `/var/log/monitor-custom/production/app`

except for `desktop` which are written to:

`monitor-custom/logs`

### ProxyPass Settings ###

On the computer 'haze', ProxyPass settings are defined in:

`/etc/apache2/sites-enabled/default-ssl.conf`

## Testing the app ##

You can ask to see the `api`:

[localhost:8080/monitor-custom/test/api](localhost:8080/monitor-custom/test/api)

You can ask for a `plot`:

[localhost:8080/monitor-custom/test/plot?monitorid=530331011&plottype=timeseries](localhost:8080/monitor-custom/test/plot?monitorid=530331011&plottype=timeseries)

You can specify `responsetype=json` to have the service return a json response for use by javascript code in a user interface:

[localhost:8080/monitor-custom/test/plot?monitorid=530331011&plottype=timeseries&responsetype=json](localhost:8080/monitor-custom/test/plot?monitorid=530331011&plottype=timeseries&responsetype=json)

***

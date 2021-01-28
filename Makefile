################################################################################
# Makefile for building and running docker containers
#
# ProxypPass settings are be defined in: /etc/apache2/sites-available/default-ssl.conf
#
# # 6660-6669 monitor-custom ---------------------------------------------------
# # 6661 -- v1 production
# # 6664 -- v4 production
# # 6669 -- test (development)
# ProxyPass /monitor-custom/v1 http://127.0.0.1:6661/monitor-custom/v1
# ProxyPassReverse /monitor-custom/v4 http://127.0.0.1:6661/monitor-custom/v1
# ProxyPass /monitor-custom/v4 http://127.0.0.1:6664/monitor-custom/v4
# ProxyPassReverse /monitor-custom/v4 http://127.0.0.1:6664/monitor-custom/v4
# ProxyPass /monitor-custom/test http://127.0.0.1:6669/monitor-custom/test
# ProxyPassReverse /monitor-custom/test http://127.0.0.1:6669/monitor-custom/test
#
# Test these settings with:    sudo apache2ctl configtest
# Reload these settings with:  sudo service apache2 reload

# Version info: V4 . dailyaveragetable . xlsx improvements; csv output
VERSION=4.5.1

# NOTE:  The SERVICE_PATH should match that found in Dockerfile and Dockerfile-test
SERVICE_PATH_PRODUCTION=monitor-custom/v4
SERVICE_PATH_TEST=monitor-custom/test

clean:
	if [ -d monitor-custom/data ]; then rm -Rf monitor-custom/data; fi
	if [ -d monitor-custom/output ]; then rm -Rf monitor-custom/output; fi
	if [ -d monitor-custom/logs ]; then rm -Rf monitor-custom/logs; fi
	if [ -d output ]; then rm -Rf output; fi
	if [ -d logs ]; then rm -Rf logs; fi

# Update the app version inline (-i) with Makefile version
configure_app:
	sed -i 's%VERSION <- ".*"%VERSION <- "$(VERSION)"%' monitor-custom/R/monitor-custom-app.R

# OSX -- Ugh!
# https://unix.stackexchange.com/questions/13711/differences-between-sed-on-mac-osx-and-other-standard-sed
configure_app_osx:
	sed -i '' 's%VERSION <- ".*"%VERSION <- "$(VERSION)"%' monitor-custom/R/monitor-custom-app.R

# DESKTOP version --------------------------------------------------------------

desktop_download_data:
	curl https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v1/RData/airnow_PM2.5_latest10.RData -o monitor-custom/data/airnow_PM2.5_latest10.RData --create-dirs
	curl https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v1/RData/airsis_PM2.5_latest10.RData -o monitor-custom/data/airsis_PM2.5_latest10.RData --create-dirs
	curl https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v1/RData/wrcc_PM2.5_latest10.RData -o monitor-custom/data/wrcc_PM2.5_latest10.RData --create-dirs
	curl https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v1/RData/airnow_PM2.5_latest45.RData -o monitor-custom/data/airnow_PM2.5_latest45.RData --create-dirs
	curl https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v1/RData/airsis_PM2.5_latest45.RData -o monitor-custom/data/airsis_PM2.5_latest45.RData --create-dirs
	curl https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v1/RData/wrcc_PM2.5_latest45.RData -o monitor-custom/data/wrcc_PM2.5_latest45.RData --create-dirs

# NOTE:  DESKTOP reuses Dockerfile-test but has a separate docker-compse-desktop.yml
desktop_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-desktop:$(VERSION) -t monitor-custom-desktop:latest -f Dockerfile-test .

desktop_build_no-cache:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build --no-cache -t monitor-custom-desktop:$(VERSION) -t monitor-custom-desktop:latest -f Dockerfile-test .

desktop_up:
	docker-compose -f docker/docker-compose-desktop.yml -p monitorcustomdesktop up -d

desktop_down:
	docker-compose -f docker/docker-compose-desktop.yml -p monitorcustomdesktop down

desktop_container_logs:
	docker-compose -f docker/docker-compose-desktop.yml -p monitorcustomdesktop logs -f

desktop_bounce: desktop_down desktop_up

desktop_reboot: desktop_download_data desktop_build desktop_bounce


# TEST version -----------------------------------------------------------------

test_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-test:$(VERSION) -t monitor-custom-test:latest -f Dockerfile-test .

test_build_no-cache:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build --no-cache -t monitor-custom-test:$(VERSION) -t monitor-custom-test:latest -f Dockerfile-test .

test_up:
	docker-compose -f docker/docker-compose-test.yml -p monitorcustomtest up -d

test_down:
	docker-compose -f docker/docker-compose-test.yml -p monitorcustomtest down

test_container_logs:
	docker-compose -f docker/docker-compose-test.yml -p monitorcustomtest logs

test_trace_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/TRACE.log

test_debug_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/DEBUG.log

test_info_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/INFO.log

test_error_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/ERROR.log

test_bounce: test_down test_up

test_reboot: test_build test_bounce


# PRODUCTION version -----------------------------------------------------------

production_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-v4:$(VERSION) -t monitor-custom-v4:latest -f Dockerfile-v4 .

production_build_no-cache:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build --no-cache -t monitor-custom-v4:$(VERSION) -t monitor-custom-v4:latest -f Dockerfile-v4 .

production_up:
	docker-compose -f docker/docker-compose-v4.yml -p monitorcustomv4 up -d

production_down:
	docker-compose -f docker/docker-compose-v4.yml -p monitorcustomv4 down

production_container_logs:
	docker-compose -f docker/docker-compose-v4.yml -p monitorcustomv4 logs

production_trace_log:
	cat /var/log/$(SERVICE_PATH_PRODUCTION)/app/TRACE.log

production_debug_log:
	cat /var/log/$(SERVICE_PATH_PRODUCTION)/app/DEBUG.log

production_info_log:
	cat /var/log/$(SERVICE_PATH_PRODUCTION)/app/INFO.log

production_error_log:
	cat /var/log/$(SERVICE_PATH_PRODUCTION)/app/ERROR.log

production_bounce: production_down production_up

production_reboot: production_build production_bounce


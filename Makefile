################################################################################
# Makefile for building and running docker containers
#
# ProxypPass settings are be defined in: /etc/apache2/sites-available/default-ssl.conf
#
# # 6660-6669 monitor-custom -----------------------------------------------------
# # 6661 -- v1 production
# # 6669 -- test (development)
# ProxyPass /monitor-custom/v1 http://127.0.0.1:6661/monitor-custom/v1
# ProxyPassReverse /monitor-custom/v1 http://127.0.0.1:6661/monitor-custom/v1
# ProxyPass /monitor-custom/test http://127.0.0.1:6669/monitor-custom/test
# ProxyPassReverse /monitor-custom/test http://127.0.0.1:6669/monitor-custom/test
#
# Test these settings with:    sudo apache2ctl configtest
# Reload these settings with:  sudo service apache2 reload

# NOTE:  The commands that are echoed to the screen will contain this string but
# NOTE:  the actual commands that are executed will evalute the backticked command.
DATE=`date --rfc-3339='date'`

# NOTE:  The SERVICE_PATH should match that found in Dockerfile and Dockerfile-test
SERVICE_PATH_PRODUCTION='monitor-custom/v1'
SERVICE_PATH_TEST='monitor-custom/test'


# DESKTOP version -------------------------------------------------------------

desktop_download_data:
	curl https://haze.airfire.org/monitoring/latest/RData/airnow_PM2.5_latest10.RData -o monitor-custom/data/airnow_PM2.5_latest10.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/airsis_PM2.5_latest10.RData -o monitor-custom/data/airsis_PM2.5_latest10.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/wrcc_PM2.5_latest10.RData -o monitor-custom/data/wrcc_PM2.5_latest10.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/airnow_PM2.5_latest45.RData -o monitor-custom/data/airnow_PM2.5_latest45.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/airsis_PM2.5_latest45.RData -o monitor-custom/data/airsis_PM2.5_latest45.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/wrcc_PM2.5_latest45.RData -o monitor-custom/data/wrcc_PM2.5_latest45.RData --create-dirs

# NOTE:  DESKTOP reuses Dockerfile-test but has a separate docker-compse-desktop.yml
desktop_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-desktop:v1.1.2 -t monitor-custom-desktop:latest -f Dockerfile-test .

desktop_up:
	docker-compose -f docker-compose-desktop.yml -p monitorcustomdesktop up -d

desktop_down:
	docker-compose -f docker-compose-desktop.yml -p monitorcustomdesktop down

desktop_logs:
	docker-compose -f docker-compose-desktop.yml -p monitorcustomdesktop logs -f

desktop_buonce: desktop_down desktop_up

desktop_reboot: desktop_down desktop_download_data desktop_build desktop_up


# TEST version -----------------------------------------------------------------

test_build_no-cache:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build --no-cache -t monitor-custom-test:v1.1.2 -t monitor-custom-test:latest -f Dockerfile-test .

test_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-test:v1.1.2 -t monitor-custom-test:latest -f Dockerfile-test .

test_up:
	docker-compose -f docker-compose-test.yml -p monitorcustomtest up -d

test_down:
	docker-compose -f docker-compose-test.yml -p monitorcustomtest down

test_container_logs:
	docker-compose -f docker-compose.yml -p monitorcustomtest logs

test_trace_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/TRACE.log

test_debug_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/DEBUG.log

test_bounce: test_down test_up

test_reboot: test_down test_build test_up


# PRODUCTION version -----------------------------------------------------------

production_build_no-cache:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build --no-cache -t monitor-custom-v1:v1.1.2 -t monitor-custom-v1:latest -f Dockerfile-v1 .

production_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-v1:v1.1.2 -t monitor-custom-v1:latest -f Dockerfile-v1 .

production_up:
	docker-compose -f docker-compose-v1.yml -p monitorcustomv1 up -d

production_down:
	docker-compose -f docker-compose-v1.yml -p monitorcustomv1 down

production_container_logs:
	docker-compose -f docker-compose-v1.yml -p monitorcustomv1 logs

production_trace_log:
	cat /var/log/$(SERVICE_PATH_PRODUCTION)/app/TRACE.log

production_debug_log:
	cat /var/log/$(SERVICE_PATH_PRODUCTION)/app/DEBUG.log

production_bounce: production_down production_up

production_reboot: production_down production_build production_up


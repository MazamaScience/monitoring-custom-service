################################################################################
# Makefile for building and running docker containers
#
# ProxypPass settings are be defined in: /etc/apache2/sites-available/default-ssl.conf
#
# # 6660-6669 monitor-custom -----------------------------------------------------
# # 6661 -- v1 operational
# # 6663 -- v3 operational
# # 6664 -- v4 operational
# # 6669 -- test (development)
# ProxyPass /monitor-custom/v1 http://127.0.0.1:6661/monitor-custom/v1
# ProxyPassReverse /monitor-custom/v1 http://127.0.0.1:6661/monitor-custom/v1
# ProxyPass /monitor-custom/v3 http://127.0.0.1:6663/monitor-custom/v3
# ProxyPassReverse /monitor-custom/v3 http://127.0.0.1:6663/monitor-custom/v3
# ProxyPass /monitor-custom/v4 http://127.0.0.1:6664/monitor-custom/v4
# ProxyPassReverse /monitor-custom/v4 http://127.0.0.1:6664/monitor-custom/v4
# ProxyPass /monitor-custom/test http://127.0.0.1:6669/monitor-custom/test
# ProxyPassReverse /monitor-custom/test http://127.0.0.1:6669/monitor-custom/test
#
# Test these settings with:    sudo apache2ctl configtest
# Reload these settings with:  sudo service apache2 reload

# NOTE:  The commands that are echoed to the screen will contain this string but
# NOTE:  the actual commands that are executed will evalute the backticked command.
DATE=`date --rfc-3339='date'`

# NOTE:  The SERVICE_PATH should match that found in Dockerfile and Dockerfile-test
SERVICE_PATH='monitor-custom/v4'
SERVICE_PATH_TEST='monitor-custom/test'


# DESKTOP version -------------------------------------------------------------

desktop_configure_ui:
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH_TEST)%' \
		monitor-custom/UI/__index.html > monitor-custom/UI/index.html
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH_TEST)%' \ 
		monitor-custom/UI/dist/__dist.js > monitor-custom/UI/dist/dist.js
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH_TEST)%' \
		monitor-custom/UI/dist/__dist.min.js > monitor-custom/UI/dist/dist.min.js

desktop_download_data:
	curl https://haze.airfire.org/monitoring/latest/RData/airnow_PM2.5_latest10.RData -o monitor-custom/data/airnow_PM2.5_latest10.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/airsis_PM2.5_latest10.RData -o monitor-custom/data/airsis_PM2.5_latest10.RData --create-dirs
	curl https://haze.airfire.org/monitoring/latest/RData/wrcc_PM2.5_latest10.RData -o monitor-custom/data/wrcc_PM2.5_latest10.RData --create-dirs

# NOTE:  DESKTOP reuses Dockerfile-test but has a separate docker-compse-desktop.yml
desktop_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-desktop:v1.0.20 -t monitor-custom-desktop:latest -f Dockerfile-test .

desktop_up:
	docker-compose -f docker-compose-desktop.yml -p monitorcustomdesktop up -d

desktop_down:
	docker-compose -f docker-compose-desktop.yml -p monitorcustomdesktop down

desktop_logs:
	docker-compose -f docker-compose-desktop.yml -p monitorcustomdesktop logs -f

desktop_reboot: desktop_down desktop_download_data desktop_build desktop_up


# TEST version -----------------------------------------------------------------

test_configure_ui:
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH_TEST)%' \
		monitor-custom/UI/__index.html > monitor-custom/UI/index.html
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH_TEST)%' \
		monitor-custom/UI/dist/__dist.js > monitor-custom/UI/dist/dist.js
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH_TEST)%' \
		monitor-custom/UI/dist/__dist.min.js > monitor-custom/UI/dist/dist.min.js

test_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-test:v1.0.20 -t monitor-custom-test:latest -f Dockerfile-test .

test_up:
	docker-compose -f docker-compose-test.yml -p monitorcustomtest up -d

test_down:
	docker-compose -f docker-compose-test.yml -p monitorcustomtest down

test_container_logs:
	docker-compose -f docker-compose.yml -p monitorcustomtest logs

test_logs:
	cat /var/log/$(SERVICE_PATH_TEST)/app/DEBUG.log

test_copy_debug_log:
	-cp /var/log/$(SERVICE_PATH_TEST)/app/DEBUG.log ./test_DEBUG.log.$(DATE)

test_reboot: test_copy_debug_log test_down test_build test_up


# OPERATIONAL version ----------------------------------------------------------

operational_configure_ui:
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH)%' \
		monitor-custom/UI/__index.html > monitor-custom/UI/index.html
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH)%' \
		monitor-custom/UI/dist/__dist.js > monitor-custom/UI/dist/dist.js
	sed 's%__SERVICE_PATH__%$(SERVICE_PATH)%' \
		monitor-custom/UI/dist/__dist.min.js > monitor-custom/UI/dist/dist.min.js

operational_build:
	-mkdir monitor-custom/output
	cd monitor-custom; docker build -t monitor-custom-v4:v1.0.20 -t monitor-custom-v4:latest -f Dockerfile-v4 .

operational_up:
	docker-compose -f docker-compose-v4.yml -p monitorcustomv4 up -d

operational_down:
	docker-compose -f docker-compose-v4.yml -p monitorcustomv4 down

operational_container_logs:
	docker-compose -f docker-compose-v4.yml -p monitorcustomv4 logs

operational_logs:
	cat /var/log/$(SERVICE_PATH)/app/DEBUG.log

operational_copy_debug_log:
	-cp /var/log/$(SERVICE_PATH)/app/DEBUG.log ./operational_DEBUG.log.$(DATE)

operational_reboot: operational_copy_debug_log operational_down operational_build operational_up


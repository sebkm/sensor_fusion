.PHONY: liveView test

# deploy-hostname: build and deploy sensor_fusion on SD card
deploy-%:
	NAME=$* rebar3 grisp deploy -n sensor_fusion -v 1.0.0

# screen: show the screen of the grisp connected by usb3 cable
screen:
	sudo screen /dev/ttyUSB1 115200

# remote-hostname: open a remote shell connected to hostname
remote-%:
	erl -sname remote_$* -remsh sensor_fusion@$* -setcookie MyCookie -kernel net_ticktime 8

# shell: open a development shell (not a clean start)
shell:
	rebar3 as computer shell --sname sensor_fusion --setcookie MyCookie

# run_local: start sensor_fusion in release mode (clean start)
run_local:
	./_build/computer/rel/sensor_fusion/bin/sensor_fusion console

# local_release: build sensor_fusion in release mode for the computer
local_release:
	rebar3 as computer release

# start the visualization tool
liveView:
	@cd liveView/ && octave liveView.m

# run the unit tests
test:
	rebar3 eunit

# rm_logs: remove files generated when using the shell
rm_logs:
	@rm -f ./measures/*
	@rm -rf ./logs
	@rm -f ./rebar3.crashdump

# clean: remove build files
clean:
	@rm -rf ./_build

#!/bin/sh

./demo-forwarder-mux 127.0.0.1 3010 --dc 25 > /dev/null 2>&1 &

PID=$(ps aux | grep demo-forwarder-mux | grep -v 'grep' | awk '{ print $2 }')

watch -- "lsof -p ${PID} | grep TCP | wc -l"

#!/bin/sh

./demo-acceptor-mux 127.0.0.1 3010 --dc 30 > /dev/null 2>&1 &

PID=$(ps aux | grep demo-acceptor-mux | grep -v 'grep' | awk '{ print $2 }')

watch -- "lsof -p ${PID} | grep TCP | wc -l"

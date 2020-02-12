#!/bin/bash

# This script is only for macos
# This uses the top command in macos to monitor memory usage
# how to execute this script?
# sh monitor.sh {logfilename}, e.g. sh monitor.sh memoryUseDethybrid

logfilename=$1
rm $logfilename
while true; do
    #time=$(date +'%X');
    #monitormemory=$(top -l 1 | grep 'Preview' | awk -v date='$(date +'%X')' '{print "PID=$1 NAME=$2 MEM=$8 TIME="date}');
    monitormemory=$(top -l 1 | grep 'java' | awk '{printf "PID="$1 " NAME="$2 " MEM="$8}');
    if [[ -n "$monitormemory" ]]; then
        echo $monitormemory >> $logfilename
    fi
	sleep 1;
done

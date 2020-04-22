#!/bin/sh

PID=$(ps aux | grep dns-updater | grep -v grep | awk '{print $2}')

echo sudo kill -HUP $PID
sudo kill -HUP $PID

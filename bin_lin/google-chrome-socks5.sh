#!/bin/bash

if [ `which google-chrome` ]; then
  echo "Found Chrome Browser"
  google-chrome --proxy-server="socks5://localhost:$1"
  exit 0
elif [ `which chromium-browser` ]; then
  echo "Found Chromium Browser"
  chromium-browser --proxy-server="socks5://localhost:$1"
  exit 0
else
  echo "No Chrome based browser found"
  exit 1
fi

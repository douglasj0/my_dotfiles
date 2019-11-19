#!/bin/bash
#
# https://support.umbrella.com/hc/en-us/articles/230561067-Umbrella-Roaming-Client-Manually-Disabling-or-Restarting-
# https://support.umbrella.com/hc/en-us/articles/230560987-Umbrella-Roaming-Client-for-OS-X-Adding-an-Enable-Disable-option-to-the-Menu-Bar-Icon


sudo launchctl unload /Library/LaunchDaemons/com.opendns.osx.RoamingClientConfigUpdater.plist

#sleep 5

#sudo launchctl load /Library/LaunchDaemons/com.opendns.osx.RoamingClientConfigUpdater.plist

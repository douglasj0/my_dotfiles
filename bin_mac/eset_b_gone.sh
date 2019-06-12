#!/bin/bash

# ESET
sudo rm -rf "/Applications/ESET Endpoint Antivirus.app"
sudo rm -rf "/Applications/ESET Remote Administrator Agent.app"
sudo rm -rf "/Library/Application Support/ESET"
sudo rm -rf /Library/LaunchAgents/com.eset.esets_gui.plist
sudo rm -rf /Library/LaunchDaemons/com.eset*
sudo rm -rf "/Library/StagedExtensions/Applications.ESET Endpoint Antivirus.app"

# CYLANCE
sudo launchctl unload /Library/LaunchDaemons/com.cylance.agent_service.plist
sudo rm "/Library/Application\ Support/Cylance/Desktop/registry/LocalMachine/Software/Cylance/Desktop/values.xml"
sudo "/Applications/Cylance/Uninstall\ CylancePROTECT.app/Contents/MacOS/Uninstall\ CylancePROTECT"

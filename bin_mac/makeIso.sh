#!/bin/bash

#############
# Variables #
#############

diskName=$(drutil status | grep "Name:" | awk '{print $4}')
userName=enterUsernameHere
imageName=rename-me

########
# Body #
########

diskutil unmount ${diskName}
dd if=${diskName} of=$[HOME}/Desktop/${imageName}.iso
diskutil mount ${diskName}

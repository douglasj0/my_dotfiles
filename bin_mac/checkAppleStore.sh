#!/bin/sh
# Crontab script to check US Apple Refurb Store for a given product
# (c)2004, Neil Sharp
#
# You must put this script into an accessible directory (/usr/local/bin
# would be a good choice).  Then, add it to your cron jobs
# (crontab -e).  Edit the cron file to look like:
#
#   SHELL=/bin/sh
#   PATH=/etc:/bin:/sbin:/usr/bin:/usr/sbin
#   HOME=/var/log
#
#   */60 * * * * /usr/local/bin/refurb.sh
#
# where "60" is the number of minutes between checks (1 hour
# in this case).
#
#####

#####
#
# Setup what we're looking for here
#   Keep in mind that you need knowledge of the product description
#   to make this work correctly.  Visit the refurb page of the US Apple
#   Store to find a good description.  Examples would be:
#
#     ITEM="PowerBook G4"
#     DESCRIPTOR="15\"" (be sure to 'escape' the double-quotes)
#
#     ITEM="iPod"
#     DESCRIPTOR="20GB"
#
#####
APPLE_ITEM="PowerBook G4"
APPLE_DESCRIPTOR="15\""


#####
# NO NEED TO CHANGE ANYTHING BELOW THIS LINE
#####

# Check whether there are any of this item in the US Apple Store
result=`curl "http://store.apple.com/1-800-MY-APPLE/WebObjects/AppleStore?family=SpecialDeals" 2> /dev/null | grep -i "${APPLE_ITEM}" | grep "${APPLE_DESCRIPTOR}" | grep -v "<B>"`

# Display the results
if [ "$result" != "" ]; then
  dialog_text="The US Apple Refurb Store has ${APPLE_DESCRIPTOR} ${APPLE_ITEM}s in stock!"
  echo $dialog_text | pbcopy
  /usr/bin/osascript -e 'tell application "Finder"' -e 'activate' -e 'set theMessage to the clipboard' -e 'display dialog theMessage buttons "Yea!" default button 1' -e 'end tell'
else
  dialog_text="The US Apple Refurb Store has no ${APPLE_DESCRIPTOR} ${APPLE_ITEM}s in stock."
  echo $dialog_text | pbcopy
  /usr/bin/osascript -e 'tell application "Finder"' -e 'activate' -e 'set theMessage to the clipboard' -e 'display dialog theMessage buttons "Bummer..." default button 1' -e 'end tell'
fi

#####
# End of script
#####

#!/bin/sh
# itunesshare v1.1
#   Prints information about users browsing your iTunes library
#   by Mithras (mithras at myrealbox.com)

# -- Settings --
# We guess at your music folder location
default_music_location="$HOME/Music"

# If it is different, you should set it here
custom_music_location=""
# -----------

# -- Options --
show_hostnames="true" 
# as with netstat or lsof, use '-n' to print IP addresses
#   rather than hostnames
if [ $# -gt 0 ]; then
	if [ $1 == "-n" ]; then
		show_hostnames="false"
	fi
fi
	
# ---------

followalias() # routine to follow an alias
{
while [ $# -gt 0 ]; do
        if [ -f "$1" -a ! -L "$1" ]; then
                item_name=`basename "$1"`
                item_parent=`dirname "$1"`
                item_parent="`cd \"${item_parent}\" 2>/dev/null && pwd || echo \"${item_parent}\"`"
                item_path="${item_parent}/${item_name}"
# nicer version of this thanks to clarkgoble / PaulMcCann
linksource=`osascript<<EOS
 tell app "Finder"
 set theItem to (POSIX file "${item_path}") as alias
 if the kind of theItem is "alias" then
 get the posix path of (original item of theItem as text)
 end if
 end tell
EOS`
                echo "$linksource"
        fi
        shift
done
}

if [ ! -z "$custom_music_location" ]; then
	# user set a custom music location
	music_location="$custom_music_location"
else
	# no custom location; check if Music folder is a folder
	if [ -d "$default_music_location" ]; then
		# the default music location is a folder ; we'll use that
		music_location="$default_music_location"
	else
		# it's not a folder; perhaps it's an alias?
		alias_music_location=`followalias "$default_music_location"`
		if [ ! -z "$alias_music_location" ]; then
			# is an alias ; follow that alias
			music_location="$alias_music_location"
		else
			# not a directory, and not an alias
			echo "** $default_music_location is neither a folder nor an alias"
			echo "** Edit the script to set the location of your iTunes music library."
			exit 1
		fi
	fi
fi
# grab list of inbound connections to port 3689
netconnections=`/usr/sbin/netstat -f inet -W -n | awk '$4 ~ /3689/ { print $3,$5 }' | sed -e 's/\.[^.]*$//g'` 
# filter that list for browsing vs. listening
browseusers=`echo "$netconnections" | awk '$1 != "33304" {print $2 } '`
streamusers=`echo "$netconnections" | awk '$3 == "33304" {print $2 } '`
# get totals for the above lists
browsecount=`echo "$browseusers" | wc -w | sed -e 's/ //g'`
streamcount=`echo "$streamusers" | wc -w | sed -e 's/ //g'`

echo "$browsecount users browsing, $streamcount listening."
echo " "
echo "--- Browsers ---"
for addr in `echo $browseusers`
do
	if [ "$show_hostnames" == "true" ]; then
		# look up the hostname
		nslookup "$addr" 2>&1 | awk '/Name/ {print $2} /Non-existent/ {print $5} '
	else
		echo "$addr"
	fi
done

echo " "
echo "--- Listeners ---"
for addr in `echo $streamusers`
do
	if [ "$show_hostnames" == "true" ]; then
		# look up the hostname
		nslookup "$addr" 2>&1 | awk '/Name/ {print $2} /Non-existent/ {print $5} '
	else
		echo "$addr"
	fi
done
echo " "

echo "--- Files ---"
lsof -c "iTunes" | perl -ne 'if (m/mp3|m4a|m4p/) { print substr($_,60,255) }'


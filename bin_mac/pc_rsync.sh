#!/bin/sh

########################################################################
# Set up the environment
HOSTNAME="aonws10.aon.com"

# directory to backup
#BDIRS="/home /usr/local /etc /var"
#BDIRS="/Volumes/vol01/Mame/"	# leaving / on end copies everything under dir
BDIRS="/cygdrive/c/Docume~1/douglasj"

# excludes file
EXCLUDES=/.backup_excludes

BASEDIR="/home/douglasj"
BACKUPDIR="${BASEDIR}/`date +%A`"
DESTDIR="/vol03/backup/weasel/"
LOG="${BASEDIR}/rsync_weasel.log"
# --archive = archive mode, equivalent to -rlptgoD
# --modify-window=1
OPTS="--archive --compress --numeric-ids
--rsync-path=/usr/local/bin/rsync --verbose --progress --stats
--exclude **/Cache/** --exclude **/Games/** --exclude **/Content.IE5/**
--exclude **/for.home/** --exclude **/SoftDist/**
--delete --delete-excluded"

########################################################################

# Transfer the data
/usr/bin/rsync $OPTS $BDIRS djackson@$HOSTNAME:$DESTDIR >$LOG 2>&1

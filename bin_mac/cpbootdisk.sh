#! /bin/sh

################################################################################
#
# NAME
#    cpboot - make a backup boot disk
#
# SYNOPSIS
#    cpboot [-d] [-f] [-s] [-x] [-y] [-m list] target_disk
#    cpboot -h
#
# EXAMPLE
#    cpboot -y c0t1d0
#    cpboot -m /:0,/usr:6,/opt:3 c4t4d0
#
# DESCRIPTION
#    Makes a bootable copy of the current system disk. Slices of the target
#    disk must be large enough for corresponding file systems of the source
#    disk. For safety reasons does not repartition the target disk.
#
#    If the source system disk is a physical disk the target disk is expected
#    to match slice-for-slice (unless overridden by the "-m" switch). If the
#    source root device is a VM volume the following default mapping applies:
#
#    file system            target slice
#    /                      0
#    swap                   1
#    /opt (optional)        3
#    /var (optional)        5
#    /usr (optional)        6
#
#    Swap space isn't copied but the entry in /etc/vfstab is updated so the
#    correct device is used.
#
# OPTIONS
#
#    d        - Debug. Doesn't actually copy anything.
#
#    f         - Run fsck following newfs.
#
#    h         - Help. Print syntax and exit.
#
#    s        - Do not use snapshot filesystem, even if it's supported.
#
#    x        - Do not update "cpbootdisk" device alias in nvram.
#
#    y         - Do not ask before overwriting target disk (necessary when
#              executed by cron).
#
#    m list    - Define file system to target disk partition mapping. List is
#              a comma separated (no spaces) list of colon separated file
#              system mount point and target disk slice number pairs.
#              e.g. /:0,swap:1,/usr:6,/opt:3,/var:5
#
# HISTORY
#    Nov 28, 2000 - James McAllister - substantial rewrite of original '95
#    vintage cpboot by same author.
#
#    Aug  1, 2001 - James McAllister - minor tweak. Changed the default /var
#    slice to 5 and default /opt slice to 3. This made it easier to have a
#    / and /var setup and have the slices convenient for VM encapsulation.
#
#    Aug 17, 2001 - James McAllister - uses snapshot filesystem if available.
#    This will make it less objectionable to run in init states 2,3 and 4.
#
#    Sep  4, 2001 - James McAllister - added the -x option to not set nvram
#    device alias. This is useful when restoring the primary boot disk by
#    copying the backup boot disk i.e. when you don't want the "cpbootdisk"
#    alias to point to the target disk. Minor changes to error reporting to
#    make it more consistent with other utilities.
#
#    Jul  1, 2002 - James McAllister - fixed a problem setting the nvramrc
#    with eeprom.
#
#    Jul 25, 2002 - James McAllister - correctly handles the global devices
#    file system of clustered systems. Added a "debug" option to report
#    whether the target disk and map are correct.
#
#    Oct 03, 2002 - James McAllister - fixed a problem generating the right
#    cpbootdisk path for PCI-based systems. If backing up the snapshot flops
#    trys again with the live file system. Snapshot seems to flop a lot.
#
#    May 07, 2003 - James McAllister - made Veritas detection more liberal
#    because root device path does not always include "rootdg". Improved
#    code that sets nvramrc. Global devices file system can be a volume.
#
# BUGS
#    Report bugs to jmcallister@dtint.com
#
################################################################################

#-------------------------------------------------------------------------------
#
# usage() - Prints command syntax and exits.
#
#-------------------------------------------------------------------------------

usage () {

    cat <<-!

    Usage
        $PROGRAM [-d] [-f] [-s] [-x] [-y] [-m <map>] <target_disk>
        $PROGRAM -h

    Where <target_disk> is in the form c#t#d#

    Options
        d        - (debug) tests but doesn't modify target disk

        f        - run fsck after newfs

        s        - do not use snapshot file systems, even if available

        x        - do not update "cpbootdisk" device alias in nvram

        y        - do not ask before overwriting target disk

        m <map>  - map file systems to target disk partitions. Map is a
                   comma separated (no spaces) list of pairs of file
                   system names and target slice numbers separated by
                   a colon. e.g. /:0,swap:1,/usr:6,/var:5

        h        - help (print this message)

    Example
        $PROGRAM c0t1d0
        $PROGRAM -d c0t1d0
        $PROGRAM -f -y -m /:0,swap:1,/opt:5 c2t4d0

    !
    exit 2
}

#-------------------------------------------------------------------------------
#
# abort(msg, ...) - Prints error message and exits.
#
#-------------------------------------------------------------------------------

abort () {

    if [ $DEBUG = yes ]; then return; fi

    # Turn off the snapshot if it's still running.
    #
    if [ -n "$SNAPDEV" ] && [ -n "$FS" ]
    then
        fssnap -d $FS 2>/dev/null
    fi

    # Kill processes using the target disk. Clean up temp mount point.
    #
    if [ -d $MNT ]
    then
        fuser -ck $MNT    >/dev/null 2>&1
        sleep 5
        umount $MNT        >/dev/null 2>&1
        rmdir  $MNT        >/dev/null 2>&1
    fi

    while [ $# -gt 0 ]
    do
        echo "ERROR: $1" >&3
        shift
    done
    date "+$PROGRAM exiting on %d %b %Y at %H:%M:%S." >&3
    exit 1
}

#-------------------------------------------------------------------------------
#
# warn(msg, ...) - Prints warnings but does not exit.
#
#-------------------------------------------------------------------------------

warn () {

    while [ $# -gt 0 ]
    do
        echo "WARNING: $1" >&3
        shift
    done
}

#-------------------------------------------------------------------------------
#
# is_used(slice) - Returns 0 (true) if slice is currently mounted, a non-swap
#    device in /etc/vfstab, a Volume Manager component, or not owned by root
#    (could indicate use by a RDBMS). Otherwise returns 1 (false).
#
#-------------------------------------------------------------------------------

is_used () {

    SLICE=$1
    RET=1

    if df -k | grep -w /dev/dsk/${TARGET}s${SLICE} >/dev/null
    then
        warn "${TARGET}s${SLICE} is mounted"
        RET=0
    fi

    if awk "BEGIN {ret = 1} (\$1 == \"${TARGET}s${SLICE}\") && (\$4 != \"swap\") {ret = 0} END {exit ret}" /etc/vfstab
    then
        warn "${TARGET}s${SLICE} is listed in /etc/vfstab"
        RET=0
    fi

    # The only time we allow the entire target disk to be under VM control is
    # when it is called "cpbootdisk". An unrelated partition of the target disk
    # can be used by VM. This is called a simple disk.
    #
    if [ -x /usr/sbin/vxprint ]
    then
        if vxprint -dAQ -F "%name %da_name %da_type" | awk "
            BEGIN {ret = 1}
            \$1 == \"cpbootdisk\" {continue}
            \$3 == \"simple\" && \$2 == \"${TARGET}s${SLICE}\" {ret = 0}
            \$3 == \"sliced\" && \$2 == \"${TARGET}s2\" {ret = 0}
            END {exit ret}
        "
        then
            echo "${TARGET}s${SLICE} overlaps a Volume Manager component"
            RET=0
        fi
    fi

    if [ "`ls -lL /dev/rdsk/${TARGET}s${SLICE} 2>/dev/null | awk '{print $3}'`" != root ]
    then
        warn "/dev/rdsk/${TARGET}s${SLICE} is not owned by root"
        RET=0
    fi

    return $RET
}

#-------------------------------------------------------------------------------
#
# is_swap(slice) - Returns 0 (true) if slice is currently a swap device.
#    Otherwise returns 1 (false).
#
#-------------------------------------------------------------------------------

is_swap () {

    SLICE=$1
    RET=1

    if swap -l | grep -w /dev/dsk/${TARGET}s${SLICE} >/dev/null
    then
        warn "${TARGET}s${SLICE} is a current swap device"
        RET=0
    fi

    if awk "BEGIN {ret = 1} (\$1 == \"${TARGET}s${SLICE}\") && (\$4 == \"swap\") {ret = 0} END {exit ret}" /etc/vfstab
    then
        warn "${TARGET}s${SLICE} is listed as a swap device in /etc/vfstab"
        RET=0
    fi

    return $RET
}

#-------------------------------------------------------------------------------
#
# not_fs(fs) - Returns 0 (true) if fs is not a mounted file system. Otherwise
#    returns 1 (false).
#
#-------------------------------------------------------------------------------

not_fs () {

    FS=$1

    # Space or ( after $FS prevents false matches (e.g. /u matching /usr).
    #
    if df -F ufs | egrep -s "^$FS(\(| )" 
    then
        return 1
    else
        warn "$FS is not a file system mount point"
        return 0
    fi
}

#-------------------------------------------------------------------------------
#
# get_used(filesystem) - Returns Kilobytes used by file system.
#
#-------------------------------------------------------------------------------

get_used () {

    FS=$1
    USED=`df -k $1 | awk 'NR == 2 {print $3}'`
    if [ -z "$USED" ]
    then
        warn "unable to determine the disk space used by $FS"
        USED=0
    fi

    echo $USED
}

#-------------------------------------------------------------------------------
#
# get_size(slice) - Returns partition size in Kilobytes (actually 90% of
#    size to provide a bit of a safety margin).
#
#-------------------------------------------------------------------------------

get_size () {

    SLICE=$1
    SIZE=`prtvtoc -h /dev/rdsk/${TARGET}s2 | awk "\\$1 == \"$SLICE\" {print \\$5}"`
    if [ -z "$SIZE" ]
    then
        warn "slice $SLICE is not defined in the $TARGET partition map"
        SIZE=0
    fi
    SIZE=`expr $SIZE / 2 \* 90 / 100`

    echo $SIZE
}

#-------------------------------------------------------------------------------
#
# map_phys(disk) - maps source disk mount points to target disk slices.
#    e.g. "/,0:/usr,6".
#
#-------------------------------------------------------------------------------

map_phys () {

    DISK=$1
    MAP=

    # Map all mounted partitions of the source disk.
    #
    if [ ! -b /dev/dsk/${DISK}s2 ]
    then
        warn "$DISK is not a physical disk"
    else
        # Looks confusing, but it works.
        MAP=`df | sed -n "s#^\\(/[^ ]*\\) *(/dev/dsk/${DISK}s\\([0-9]\\) *).*\\$#\\1:\\2#p"`
        MAP=`echo $MAP | tr " " ","`
    fi

    # If the physical disk has a swap partition then so does the target disk.
    # Note: this only blesses a partition of the cpboot disk as the official
    # swap device. There might be others in /etc/vfstab including that one.
    #
    if swap -l 2>/dev/null | egrep -s "^/dev/dsk/${DISK}s"
    then
        SLICE=`swap -l 2>/dev/null | sed -n "s#^/dev/dsk/${DISK}s\\([0-9]\\).*\\$#\\1#p"`
        if [ -z "$SLICE" ]
        then
            warn "the source disk $DISK does not contain a swap device"
        else
            MAP=${MAP},swap:$SLICE
        fi
    fi

    echo $MAP
}

#-------------------------------------------------------------------------------
#
# map_vm() - maps common file systems to target disk slices. This is a best
#    guess what to do with Volume Manager file systems. In many cases you will
#    be better off providing your own map via "-m".
#
#-------------------------------------------------------------------------------

map_vm () {

    MAP=

    # Start with the default map and omit file systems that don't exist.
    #
    for FS_SLICE in `echo $DEFAULTMAP | tr ',' ' '`
    do
        FS=`echo $FS_SLICE | cut -s -d: -f1`
        SLICE=`echo $FS_SLICE | cut -s -d: -f2`

        if [ -z "$FS" ] || [ -z "$SLICE" ]
        then
            abort "internal error: the default map \"$DEFAULTMAP\" is mangled"
        fi

        if df -F ufs | egrep -s "^$FS(\(| )" || [ $FS = swap ]
        then
            if [ -z "$MAP" ]
            then
                MAP=${FS}:${SLICE}
            else
                MAP=${MAP},${FS}:${SLICE}
            fi
        fi
    done

    echo $MAP
}

#-------------------------------------------------------------------------------
#
# map_global() - maps the global devices file system for this node to a slice
#    of the target disk. The source device can be a VM volume or a "did" device.
#    If a VM volume, we will assume a target slice of 6, a little flaky I know
#    but you can alway override this with a manual map. If a did, we use the
#    same slice for the target that's used by the source.
#
#-------------------------------------------------------------------------------

map_global () {

    # What is the global devices mount point and device for this node?
    #
    NODENAME=`uname -n`
    NODENUM=`/usr/cluster/bin/scconf -p | nawk "
        /Cluster node name:/ && \\$4 == \\"$NODENAME\\" {foundit = 1}
        /Node ID:/ && foundit {print \\$3 ; exit}
    "`

    if [ -z "$NODENUM" ]
    then
        warn "can't determine cluster node number"
        return
    fi

    GD_FS=/global/.devices/node@$NODENUM
    GD_DEV=`df -k $GD_FS | awk 'NR == 2 {print $1}'`

    if [ -n "$GD_DEV" ]
    then
        if echo $GD_DEV | egrep -s '^/dev/vx/dsk/'
        then
            GD_SLICE=6
        elif echo $GD_DEV | egrep -s '^/dev/did/dsk/'
        then
            GD_SLICE=`echo $GD_DEV | sed -n 's#^/dev/did/dsk/d[0-9]\{1,\}s\([0-9]\)$#\1#p'`
        else
            warn "global devices file system ($GD_FS) device $GD_DEV is not a Volume Manager or did device."
            return
        fi
    else
        warn "could not determine $NODENAME global devices file system device"
        return
    fi

    echo ${GD_FS}:$GD_SLICE
}

#-------------------------------------------------------------------------------
#
# copy_fs(filesystem, slice) - Dumps a file system to a partition. Returns
#    0 on success, 1 on failure.
#
#-------------------------------------------------------------------------------

copy_fs () {

    FS=$1
    SLICE=$2
    RET=0

    if [ $DEBUG = yes ]
    then
        echo File system $FS will be copied to ${TARGET}s${SLICE}.
        return 0
    fi

    echo Preparing ${TARGET}s${SLICE}...
    newfs /dev/rdsk/${TARGET}s${SLICE} </dev/null >/dev/null 2>&1 &
    PID=$!
    wait $PID
    if [ $? -ne 0 ]
    then
        warn "newfs of ${TARGET}s${SLICE} failed"
        return 1
    fi

    if [ $DO_FSCK = yes ]
    then
        echo Checking ${TARGET}s${SLICE}...
        fsck -y /dev/rdsk/${TARGET}s${SLICE} </dev/null >/dev/null 2>&1 &
        PID=$!
        wait $PID
        if [ $? -ne 0 ]
        then
            warn "fsck of ${TARGET}s${SLICE} failed"
            return 1
        fi
    fi

    mount /dev/dsk/${TARGET}s${SLICE} $MNT
    if [ $? -ne 0 ]
    then
        warn "could not mount ${TARGET}s${SLICE}"
        return 1
    fi

    # Create the snapshot device if we're supposed to. If it works, we'll
    # use it in place of the real file system. Notice, the backing store
    # file is on the file system we are dumping to. The file shouldn't
    # grow much and we make sure we have free space on the receiving file
    # systems, so we should be fine.
    #
    if [ $DO_SNAP = yes ]
    then
        SNAPDEV=`fssnap -F ufs -o raw,unlink,bs=$MNT/.snap$$ $FS 2>/dev/null`
    fi
    if [ -n "$SNAPDEV" ]
    then
        echo Using snapshot of $FS...
        SOURCE=$SNAPDEV
    else
        SOURCE=$FS
    fi

    echo Copying $FS to ${TARGET}s${SLICE}...
    ufsdump 0f - $SOURCE 2>/dev/null | (cd $MNT ; ufsrestore rf - 2>/dev/null) &
    PID=$!
    wait $PID
    if [ $? -ne 0 ]
    then
        # If the snapshot didn't work, we have another go with the f/s.
        #
        if [ -n "$SNAPDEV" ]
        then
            echo Snapshot did not work. Using live file system...
            SOURCE=$FS
            ufsdump 0f - $SOURCE 2>/dev/null | (cd $MNT ; ufsrestore rf - 2>/dev/null) &
            PID=$!
            wait $PID
            if [ $? -ne 0 ]
            then
                warn "ufsdump returned an error"
                RET=1
            fi
        else
            warn "ufsdump returned an error"
            RET=1
        fi
    fi

    if [ -n "$SNAPDEV" ]
    then
        fssnap -d $FS 2>/dev/null
        SNAPDEV=
    fi

    rm -f $MNT/restoresymtable
    umount $MNT
    if [ $? -ne 0 ]
    then
        warn "problems unmounting $MNT"
        RET=1
    fi

    return $RET
}

#-------------------------------------------------------------------------------
#
# fix_root() - modifies /etc/vfstab and /etc/system.
#
#-------------------------------------------------------------------------------

fix_root() {

    MAP=$1
    ROOT_SLICE=$2
    SWAP_SLICE=$3

    # If a cluster, what is the global devices file system for this node?
    #
    if [ $DO_GD = yes ]
    then
        NODENAME=`uname -n`
        NODENUM=`/usr/cluster/bin/scconf -p | nawk "
            /Cluster node name:/ && \\$4 == \\"$NODENAME\\" {foundit = 1}
            /Node ID:/ && foundit {print \\$3 ; exit}
        "`
        if [ -n "$NODENUM" ]
        then
            GD_FS=/global/.devices/node@$NODENUM
        else
            warn "can't determine cluster node number"
        fi
    fi

    #
    # Prepare versions of /etc/system and /etc/vfstab suitable for the target
    # boot environment.
    #

    # Comment out any "rootdev" line in /etc/system (necessary when your
    # root is a logical partition, but not when it is a physical one).
    #
    rm -f $TMP_SYSTEM
    sed 's/^\(rootdev:.*\)/* \1/' /etc/system >$TMP_SYSTEM

    # Rebuild /etc/vfstab matching each file system in the map with its
    # corresponding target partition.
    #
    rm -f $TMP_VFSTAB
    cp -p /etc/vfstab $TMP_VFSTAB
    for FS_SLICE in `echo $MAP | tr ',' ' '`
    do
        FS=`echo $FS_SLICE | cut -s -d: -f1`
        SLICE=`echo $FS_SLICE | cut -s -d: -f2`

        if [ -z "$FS" ] || [ -z "$SLICE" ]
        then
            warn "mangled map: $MAP"
            return 1
        fi

        if [ $FS = swap ]; then continue; fi

        if [ $DO_GD = yes ] && [ "$FS" = "$GD_FS" ]
        then
            DID=`/usr/cluster/bin/scdidadm -l -o name /dev/rdsk/$TARGET | tr -d '[:space:]'`
            BLOCK=/dev/did/dsk/${DID}s${SLICE}
            RAW=/dev/did/rdsk/${DID}s${SLICE}
        else
            BLOCK=/dev/dsk/${TARGET}s${SLICE}
            RAW=/dev/rdsk/${TARGET}s${SLICE}
        fi
        rm -f $TMP_VFSTAB.2
        cp -p $TMP_VFSTAB $TMP_VFSTAB.2
        awk "
            BEGIN { OFS = \"\t\" }
            {
                if ( \$3 == \"$FS\" && \$1 !~ /^#/ )
                    print \"$BLOCK\t$RAW\t$FS\", \$4, \$5, \$6, \$7;
                else
                    print \$0
            }
        " <$TMP_VFSTAB.2 >$TMP_VFSTAB
        rm -f $TMP_VFSTAB.2
    done

    # Okay, here are the rules for swap devices. They are my rules and if you
    # don't like them, you can write your own cpboot.
    #
    # 1. If the target disk partition designated for swap is already in the
    #    file, you don't have to do any further processing. Keep any other
    #    swap devices.
    #
    # 2. Swap FILES are ignored. Presumably they are copied to the target disk
    #    with a file system copy (probably on the root file system).
    #
    # 3. If we have a single swap device, the swap device on the target disk
    #    will be substituted for it. In most cases a single swap device will
    #    be slice 1 of the system disk. It could also be a volume if the root
    #    file system is encapsulated.
    #
    # 4. If more than one swap device is defined, keep all of them.
    #
    # 5. The target disk slice designated for swap is ALWAYS included. If
    #    not in a multiple device listing (#4 above) it will be appended.

    # If the target disk doesn't have a swap slice defined, we can't do much
    # more than accept what's there. If the target swap device is already in
    # the vfstab file, don't do anything either.
    #
    if [ -z "$SWAP_SLICE" ] || egrep -s "^/dev/dsk/${TARGET}s${SWAP_SLICE}" $TMP_VFSTAB
    then
        :
    else
        # How many swap devices do we have?
        #
        SWAP_CNT=`awk "BEGIN {cnt = 0} (\\$1 ~ /^\/dev\//) && (\\$4 == \\"swap\\") {cnt += 1} END {print cnt}" $TMP_VFSTAB`

        # If it's not exactly one, just append the target swap device.
        #
        if [ ${SWAP_CNT} -ne 1 ]
        then
            echo "/dev/dsk/${TARGET}s${SWAP_SLICE}\t-\t-\tswap\t-\tno\t-" >>$TMP_VFSTAB
        else
            rm -f $TMP_VFSTAB.2
            cp -p $TMP_VFSTAB $TMP_VFSTAB.2
            awk "
                (\$1 !~ /^\/dev\//) || (\$4 != \"swap\") { print }
                (\$1  ~ /^\/dev\//) && (\$4 == \"swap\") { print \"/dev/dsk/${TARGET}s${SWAP_SLICE}\t-\t-\tswap\t-\tno\t-\" }
            " <$TMP_VFSTAB.2 >$TMP_VFSTAB
            rm -f $TMP_VFSTAB.2
        fi
    fi

    if [ $DEBUG = yes ]
    then
        echo -----
        echo "/etc/vfstab\n"
        cat $TMP_VFSTAB | tr -s '[:space:]' ' ' | sed -e 's/^/    /'
        echo -----
        rm -f $TMP_SYSTEM
        rm -f $TMP_VFSTAB
    else
        # Mount the target root file system and put the new files in place.
        #
        mount /dev/dsk/${TARGET}s${ROOT_SLICE} $MNT
        if [ $? -ne 0 ]
        then
            warn "unable to mount /dev/dsk/${TARGET}s${ROOT_SLICE} on $MNT"
            return 1
        fi

        cat $TMP_SYSTEM > $MNT/etc/system
        cat $TMP_VFSTAB > $MNT/etc/vfstab

        # Leave a little reminder of when we made the cpboot. That way if we ever
        # boot from this disk, we can tell how old (stale) it is.
        #
        date "+Created by $PROGRAM on %d %b %Y at %H:%M:%S." >$MNT/.cpboot

        # Unmount the target root.
        #
        umount $MNT
        if [ $? -ne 0 ]
        then
            warn "unable to unmount $MNT"
            return 1
        fi
    fi
    return 0
}

#-------------------------------------------------------------------------------
#
# do_nvram(device) - prints to stdout updated NVRAM settings. If cpbootdisk is
#    set, replaces the current devalias with device. If cpbootdisk is not set
#    appends a devalias for it using device.
#
#-------------------------------------------------------------------------------

do_nvram () {

    DEVICE=$1
    SEDFILE=/tmp/.sed.$$
    rm -f $SEDFILE

    cat > $SEDFILE <<-EOF
        s/nvramrc=//
        /data not available/d
        /devalias cpbootdisk /c\\
            devalias cpbootdisk $DEVICE
    EOF

    $EEPROM nvramrc | sed -f $SEDFILE

    if $EEPROM | grep -w cpbootdisk >/dev/null 2>&1
    then
        :
    else
        echo "devalias cpbootdisk $DEVICE"
    fi

    rm -f $SEDFILE
}

#-------------------------------------------------------------------------------
#
# make_bootable() - installs boot block and OBP device alias.
#
#-------------------------------------------------------------------------------

make_bootable () {

    ROOT_SLICE=$1

    # Generate OpenBoot device from /devices path. Strip leading "/devices"
    # and trailing ":a". PCI/IDE paths replace "/sd@" and "/dad@" with "/disk@".
    #
    BOOT_DEV=`ls -l /dev/dsk/${TARGET}s${ROOT_SLICE} | sed "
        s%^.*/devices%%
        s%:a\\$%%
        /^\/pci@.*\/ide@/ s%/sd@%/disk@%
        /^\/pci@.*\/ide@/ s%/dad@%/disk@%
        /^\/ssm@.*\/pci@.*\/scsi@/ s%/sd@%/disk@%
    "`
    echo OpenBoot boot path is $BOOT_DEV

    if [ $DEBUG = yes ]
    then
        echo -----
        echo "OpenBoot nvramrc will be set to:\n"
        do_nvram $BOOT_DEV | sed -e 's/^/    /'
        echo -----
        return 0
    fi

    echo Installing bootblock...
    installboot $BOOTBLK /dev/rdsk/${TARGET}s${ROOT_SLICE}

    if [ $DO_NVRAM = yes ]
    then
        echo Setting nvramrc...
        $EEPROM "nvramrc=`do_nvram $BOOT_DEV`"
        $EEPROM "use-nvramrc?=true"

        echo To boot from $TARGET enter \"boot cpbootdisk\" at the ok prompt
    fi

    return 0
}

#-------------------------------------------------------------------------------
#
# Initialize variables, do command line processing, etc.
#
#-------------------------------------------------------------------------------

# Duplicate stdout for use in subroutines where stdout might be redirected
# for inline substitution.
#
exec 3>&1

PROGRAM=`basename $0`
DEBUG=no
DO_FSCK=no
DO_GD=no
DO_NVRAM=yes
DO_SNAP=no
WARN=0 # (true)
DEFAULTMAP=/:0,swap:1,/opt:3,/var:5,/usr:6
MIN_SWAP=128000
MAP=
PATH=/bin:/usr/bin:/sbin:/usr/sbin
PLATFORM=`uname -i`
EEPROM=/usr/platform/$PLATFORM/sbin/eeprom
BOOTBLK=/usr/platform/$PLATFORM/lib/fs/ufs/bootblk
MNT=/tmp/mnt.$$
TMP_SYSTEM=/tmp/.system.$$
TMP_VFSTAB=/tmp/.vfstab.$$
SNAPDEV=
SNAP_BS=

trap 'abort "interrupted by user"' 2    # CTL-C signal
trap 'abort "terminated"' 15            # TERM signal (e.g. from kill)

if [ "$LOGNAME" != root ]
then
    abort "must be root to run"
fi

if [ -x /usr/sbin/fssnap ]
then
    DO_SNAP=yes
fi

# If xntpd is running, we can't do snapshots.
#
if pgrep xntpd >/dev/null
then
    DO_SNAP=no
fi

# If we have cluster 3.0 installed, we need to handle the global devices f/s.
#
if pkginfo -q SUNWscr
then
    DO_GD=yes
fi

while getopts dfhsxym: SWITCH
do
    case $SWITCH in

    d)    DEBUG=yes
        ;;
    f)    DO_FSCK=yes
        ;;
    h | \?)
        usage
        ;;
    s)    DO_SNAP=no
        ;;
    x)    DO_NVRAM=no
        ;;
    y)    WARN=1
        ;;
    m)    MAP=$OPTARG
        ;;
    esac
done
shift `expr $OPTIND - 1`

if [ $# -eq 0 ]
then
    warn "no arguments"
    usage
fi
if [ $# -gt 1 ]
then
    warn "too many arguments"
    usage
fi

TARGET=$1
if [ ! -b /dev/dsk/${TARGET}s2 ]
then
    abort "$TARGET is not a valid disk"
fi

# If our temporary mount point is taken we should abort. We try to pick a
# name that's unlikely to be used, but anything's possible.
# 
if [ $DEBUG = no ]
then
    if [ -d $MNT ]
    then
        abort "$MNT exists"
    fi
    mkdir $MNT 2>/dev/null
    if [ $? -ne 0 ]
    then
        abort "unable to create $MNT"
    fi
fi

date "+Starting $PROGRAM on %d %b %Y at %H:%M:%S."

#-------------------------------------------------------------------------------
#
# Plan what we're going to copy.
#
#-------------------------------------------------------------------------------

# If a map wasn't provided on the command line we need to build one.
#
if [ -z "$MAP" ]
then
    echo Mapping file systems to target disk...

    ROOTDEV=`df -k / | awk 'NR == 2 {print $1}'`
    if echo $ROOTDEV | egrep -s '/dev/dsk/c[0-9]+t[0-9]+d[0-9]+s[0-9]+'
    then
        # Root device is a physical disk.
        #
        SRCDISK=`echo $ROOTDEV | sed -n 's#^/dev/dsk/\(c[0-9]\{1,\}t[0-9]\{1,\}d[0-9]\{1,\}\)s.*$#\1#p'`
        MAP=`map_phys $SRCDISK`

    elif echo $ROOTDEV | egrep -s '/dev/vx/.+'
    then
        # Root device is a Vol Mgr volume.
        #
        MAP=`map_vm`
    else
        abort "can't determine root file system device type"
    fi

    if [ $DO_GD = yes ]
    then
        GD_MAP=`map_global`
        if [ -n "$GD_MAP" ]
        then
            MAP=${MAP},${GD_MAP}
        fi
    fi
fi

if [ -z "$MAP" ]
then
    abort "unable to map file systems to $TARGET slices"
fi

if [ $DEBUG = yes ]
then
    echo Map is \"$MAP\"
fi

# Make sure each slice is not used and the file system fits. For swap make
# sure the target slice has a reasonable size. Make sure we have a root
# file system. And check for a swap device too.
#
echo Checking devices and sizes...
SWAP_SLICE=
ROOT_SLICE=
for FS_SLICE in `echo $MAP | tr ',' ' '`
do
    FS=`echo $FS_SLICE | cut -s -d: -f1`
    SLICE=`echo $FS_SLICE | cut -s -d: -f2`

    if [ -z "$FS" ] || [ -z "$SLICE" ]
    then
        abort "mangled map: $MAP"
    fi

    # This checks for all use except as a swap device, which is okay if that's
    # how it's going to be used on the cpboot disk.
    #
    if is_used $SLICE
    then
        abort "${TARGET}s${SLICE} in use"
    fi

    SLICESZ=`get_size $SLICE`
    if [ ${SLICESZ} -eq 0 ]
    then
        abort "problems determining ${TARGET}s${SLICE} size"
    fi

    if [ "$FS" = swap ]
    then
        SWAP_SLICE=$SLICE
        if [ $SLICESZ -lt $MIN_SWAP ]
        then
            abort "insufficient swap space in ${TARGET}s${SLICE}: ${SLICESZ}KB (${MIN_SWAP}KB required)"
        fi
    else
        if not_fs $FS
        then
            abort "only mounted file systems may be copied"
        fi

        if [ "$FS" = / ]
        then
            ROOT_SLICE=$SLICE
        fi

        # Since the target is a file system, we can't be using it for swap.
        #
        if is_swap $SLICE
        then
            abort "${TARGET}s${SLICE} in use"
        fi

        FSSZ=`get_used $FS`
        if [ ${FSSZ} -eq 0 ]
        then
            abort "problems determining $FS size"
        fi

        if [ $FSSZ -gt $SLICESZ ]
        then
            abort "$FS is too large to fit on ${TARGET}s${SLICE}"
        fi
    fi
done

if [ -z "$ROOT_SLICE" ]
then
    warn "you have not specified a root device"
fi

#-------------------------------------------------------------------------------
#
# Okay, everything looks good. Copy each file system to its intended target.
#
#-------------------------------------------------------------------------------

# If we didn't turn off the warning and stdin is a terminal, give us a chance
# to bail. This is a good way of testing if the target disk is good without
# actually doing the copy.
#
if [ $WARN -eq 0 ] && [ -t 0 ] && [ $DEBUG = no ]
then
    echo Continuing will overwrite $TARGET.
    if ckyorn -d n -p "Do you want to continue?" | egrep -s -i 'n'
    then
        abort "aborted by user"
    fi
fi

# Copy each file system in the list.
#
for FS_SLICE in `echo $MAP | tr ',' ' '`
do
    FS=`echo $FS_SLICE | cut -s -d: -f1`
    SLICE=`echo $FS_SLICE | cut -s -d: -f2`

    if [ -z "$FS" ] || [ -z "$SLICE" ]
    then
        abort "mangled map: $MAP"
    fi

    # Except swap. Of course we won't copy swap.
    #
    if [ "$FS" = swap ]; then continue; fi

    copy_fs $FS $SLICE
    if [ $? -ne 0 ]
    then
        abort "problems copying $FS to ${TARGET}s${SLICE}"
    fi
done

# If the root file system was copied remount the root file system and fix
# /etc/vfstab and /etc/system. Then install a boot block on the disk.
#
if [ -n "$ROOT_SLICE" ]
then
    fix_root $MAP $ROOT_SLICE $SWAP_SLICE
    if [ $? -ne 0 ]
    then
        abort "problems fixing the root filesystem"
    fi

    make_bootable $ROOT_SLICE
    if [ $? -ne 0 ]
    then
        abort "problems installing bootblock"
    fi
fi

# Clean up and flush.
#
##[ $DEBUG = no ] && rmdir $MNT >/dev/null 2>&1
##date "+Finished $PROGRAM on %d %b %Y at %H:%M:%S."

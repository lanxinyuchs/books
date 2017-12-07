#!/bin/bash
## ----------------------------------------------------------------------------
## %CCaseFile:	swm_sudo.sh %
## %CCaseRev:	/main/R2A/R3A/R4A/11 %
## %CCaseDate:	2015-12-17 %
## %CCaseDocNo: %
##
## Author:      etxlg
## Description:
## called as root through wrapper in rcs-rootfs
## assuming this script is here:
## /software/RCS_CXP9021221_1_R2A/SWM_CXC1733929/swm-R2A73/priv/bin/swm_sudo.sh
## and you wish to call it with parameters: -help -me
## this is the complete command to run using os:cmd().
## sudo swm_wrapper.sh -c /software/RCS_CXP9021221_1_R2A -r R2A73 -- -help -me
## swm_wrapper.sh itself is in /addons/bin - normally this is in the path
## the "--" arg is to signal end of args to the wrapper - remaining args are
## passed on to swm_sudo.sh
##
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013-2015 All rights reserved.
## 
## The information in this document is the property of Ericsson.
## 
## Except as specifically authorized in writing by Ericsson, the 
## receiver of this document shall keep the information contained 
## herein confidential and shall protect the same in whole or in 
## part from disclosure and dissemination to third parties.
## 
## Disclosure and disseminations to the receivers employees shall 
## only be made on a strict need to know basis.
## %CCaseCopyrightEnd%
##
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R2A/x      2013-05-22   etxlg       created but not tested
## R2A/1      2013-06-14   etxjotj     First check in.
## R2A/2      2013-07-15   etxjotj     Moved temp fallbaxk fix baxck to erlang
## R2A/3      2013-07-10   etxjotj     Clear homedirectory before using it again
## R2A/4      2013-07-11   etxjotj     Add cleared homedirectory again
## R2A/5      2014-01-27   etxjotj     Tcu03 upgrade variant
## R2A/6      2014-02-06   etxjotj     Fixed relinking in boot flash
## R2A/7      2014-03-12   etxjotj     Added activate step for tcu03 variant
## R2A/8      2014-03-16   etxjotj     Fixed umount spelling
## R2A/10     2014-04-06   erarafo     Added restricted 'rmdir' operation
## R2A/11     2014-04-07   erarafo     Removed -l option from install_ee()
## R2A/15     2014-05-05   erarafo     Removed -t FSTYPE option from `mount'
## R2A/16     2014-05-13   etxjotj     Added up boot pointer support
## R2A/17     2014-05-15   etxjotj     Support for taipan upgrade
## R2A/20     2014-06-10   etxjotj     Changes for EE split
## R2A/21     2014-06-17   etxarnu     Added rcsversion 
## R2A/22     2014-06-17   etxarnu     Changed mounting for *ee_tcu03
## R2A/23     2014-06-17   etxarnu     Fix to also handle non-split RCS
## R2A/24     2014-07-24   etxarnu     Added dns commands
## R2A/25     2014-07-24   etxarnu     Corrected dns -d 
## R2A/26     2014-07-24   etxarnu     Bug fixes
## R2A/27     2014-07-25   etxarnu     Ensure no duplicate dns addresses
## R2A/28     2014-08-01   etxjotj     Disk space operations HS23701
## R2A/29     2014-08-04   etxjotj     Disk space management
## R3A/1      2014-11-21   etxarnu     Added ldconfig
## R3A/2      2015-01-16   etxderb     Added prepare_nl_upgrade
## R3A/3      2015-01-26   etxderb     Fault in redirect in nl_log
## R3A/3      2015-03-20   etxderb     Added reset_boot
## R3A/4      2015-05-08   etxderb     Added hard reset_boot
## R4A/1      2015-08-11   etxarnu     Added clear_applicationtmp
## R4A/2      2015-09-01   etxarnu     Added arp
## R4A/3      2015-09-04   etxjotj     Added fsck
## R4A/4                               Use fsck.ext4
## R4A/5      2015-09-09   etxjotj     Cleanup
## R4A/6      2015-09-24   etxjotj     Backwards compatibility 15B
## R4A/7      2015-10-13   etxarnu     Added ip_addr
## R4A/10     2015-10-11   etxjotj     Corr for HU38994, own swm mount.
## R4A/11     2015-10-17   etxderb     Corr for HU38994, own swm mount for nl
## ------------------------------------------------------------- column 80 --->
##

# PATH=/addons/bin:$PATH  #uncomment if needed

# note on usage for the optional arguments:
# -s if not specified the S10 script will be copied from .../sys/priv/bin/
# -t if not specified there won't be any "to_alias" created
# -v if not specified "version unknown" will be written in /etc/profile
# -c if not specified the new instance will be substituted into the current bootcmd3 -string

usage="Usage: $0 install_ee -i <new instance number> -r <rootfs_image> -b <bootfs_image> [-s <start_script> -t <\"to_alias\"> -u <\"to2_alias\"> -v <swm_version> -c <\"new bootcmd3\">] <mnt pnt>..."

MNT_EE=/opt/rcs_ee/mounts/boot
MNT_SWM=/rcs/swm/boot

## Clear the other homedirectory before installing the next UP

do_clear_ee() {
    while
    getopts h: option
    do
	case $option in
	    h)
		home_other="$OPTARG"
	    ;;
	    *)
		echo "Usage: $0 clear_ee -h <other_home_directory>"
		;;
	esac
    done
    echo ${home_other}
    rm -rf ${home_other}
    mkdir -p ${home_other}
    chown sirpa:users ${home_other}
}

# Backup old /nl to /nl_upgrade and change softlink ./networkloader to refer it
# Later in confirm,, we will have new images in /nl so we can switch back the 
# link and delete /nl_upgrade.  

# $1 string
nl_log() {
   echo "swm_sudo.sh: $1">&2
   #echo "swm_sudo.sh: $1" >> /rcs/networkloader/nl_upgrade.log
}

fatal() {
   nl_log "FATAL $1"
   # exit 1
}

mount_nl_t3()  {
    local boot_mnt=${1:?arg missing}
    local type3_dev

     nl_log "mount_nl_t3 $boot_mnt"
    # MOUNT
    # Check if multiple netloaders are supported
    if [ -f /proc/device-tree/rcs_nodes/boot_nl2_partition ]; then
        nl_log "uboot supports type 2 networkloader, upgrading on sda2"
        if 
            mount --bind $MNT_EE $boot_mnt 
        then
            nl_log "bind mounted $MNT_EE on $boot_mnt"
            mount -o remount,rw $boot_mnt
            return 0
        else
            nl_log "failed to bind mount $MNT_EE on $boot_mnt"
            return 1
        fi 
    else
        nl_log "uboot dont support type 2 networkloader, upgrading on sda1"
        type3_dev="/dev/sda1"
        if 
            mount $type3_dev $boot_mnt
        then
            nl_log "mounted $type3_dev on $boot_mnt"
            return 0
        else
            nl_log "failed to mount $type3_dev on $boot_mnt"
            return 1
        fi     
    fi
}


umount_boot_dev() {
    local boot_mnt=${1:?arg missing}
    umount $boot_mnt
    nl_log "umounted $boot_mnt"
}


do_prepare_nl_upgrade() {
    local nl_mnt=$MNT_SWM
   
    # MOUNT
    mount_nl_t3 $nl_mnt

    # CLEAN UP IF NEEDED
    if [ -d $nl_mnt/nl_upgrade ];
    then
        nl_log "old nl_upgrade exist, deleting"
        rm -rf $nl_mnt/nl_upgrade # clean up in case of failed
    fi
    
    # BACKUP AND MOVE "ACTIVE" SOFTLINK
    # boot files will be upgraded by "cup --nl_boot-install"
    cp -R  $nl_mnt/nl $nl_mnt/nl_upgrade
    rm -f $nl_mnt/networkloader
    ln -s ./nl_upgrade $nl_mnt/networkloader    
    nl_log "backed up old nl and changed link"

    # UMOUNT
    sync
    umount_boot_dev $nl_mnt
    nl_log "preperation of NL done"
}





do_confirm_nl() {
   local mnt_dir=${1:?missing arg}
  
   nl_log "confirm_nl finishing on mountdir $mnt_dir"
   if [ -d $mnt_dir/nl ]; then 
      rm -f $mnt_dir/networkloader
      ln -s ./nl $mnt_dir/networkloader
      rm -rf $mnt_dir/nl_upgrade
      nl_log "confirmation of NL done"
   else
      nl_log "upgrade nl not supported => confirm nl ignored"
   fi
}
# $1 is the mounted dir for type3 boot files.
confirm_nl() {
    local nl_mnt="/tmp/swm_nl_mount"

    if [ -f /proc/device-tree/rcs_nodes/boot_nl2_partition ]; then
	# sda2 already mounted, nl files on same dev as UP bootfiles.
	do_confirm_nl $1
    else 
	nl_log "mounting nl type3 device sda1 on $nl_mnt"
	mkdir -p $nl_mnt
	if mount "/dev/sda1" $nl_mnt
	  then 
	       do_confirm_nl $nl_mnt
	       sync
	       umount $nl_mnt
          else
               fatal "confirm_nl failed to mount /dev/sda1 on $nl_mnt"
        fi
    fi
}



## Redirect the soft links governing the boot selection in tcu03 temporary setup
## This step can be done directly from erlang on dus41. No extra necessary

do_activate_os()
{
   local sda2_mnt
    while
    getopts i:u: option
    do
	case $option in
            i)
		instance="$OPTARG"
		;;
	    u) 
		# obsolete
		uflag="$OPTARG"
		;;
            *)
		echo "$usage"
		exit 1
		;;
	esac
    done

# switch boot directories
# re-mount sda2 as RW

# HU38994 Change mounting solution
    sda2_mnt=/opt/rcs_ee/mounts/boot
    mount --bind $sda2_mnt /rcs/swm/boot
    mount -o remount,rw /rcs/swm/boot

    cd /rcs/swm/boot

    rm -f upgrade
    ln -fs b${instance} upgrade
    
    cd /tmp
    sync

    umount /rcs/swm/boot
}




do_confirm_os()
{
    local actual_mnt

    while
    getopts i: option
    do
	case $option in
            i)
		instance="$OPTARG"
		;;
            *)
		echo "$usage"
		exit 1
		;;
	esac
    done
    
# switch boot directories
# HU38994 New mounting solution
    local sda2_mnt=/opt/rcs_ee/mounts/boot
    actual_mnt=/rcs/swm/boot
    mount --bind $sda2_mnt $actual_mnt
    mount -o remount,rw $actual_mnt


    cd $actual_mnt
    rm -f fallback
    rm -f configured
    ln -fs b${instance} fallback
    ln -fs b${instance} configured

    confirm_nl $actual_mnt

    cd /tmp
    
    sync

    umount /rcs/swm/boot

}

## Manage /etc/resolv.conf

dns()
{
    while
    getopts a:d:s: option
    do
	case $option in
	    a)
		ipaddress="$OPTARG"
		del_ip_from_dns $ipaddress
		add_ip_to_dns $ipaddress
		;;
	    d)
		ipaddress="$OPTARG"
		del_ip_from_dns $ipaddress
		;;
	    s)
		cat /var/run/resolv.conf
		;;
	    *)
		exit 1
		;;
	esac
    done

}

## Very rudimentary version, should be improved with more checks
add_ip_to_dns()
{
    echo "nameserver $1" >> /var/run/resolv.conf
}

del_ip_from_dns()
{
    sed -i "/$1/d" /var/run/resolv.conf 
}


diskfree()
{
    vgs rootvg -o vg_free --units B

}

disksize()
{
    vgs rootvg -o vg_size --units B

}

ldconfig()
{
    /etc/init.d/rcs_ldconfig  $*

}

reset_boot() 
{
    local sda2_mnt=/opt/rcs_ee/mounts/boot
    local actual_mnt=/rcs/swm/boot
    local reset_type=${1:?missing arg}

    mount --bind $sda2_mnt $actual_mnt
    mount -o remount,rw $actual_mnt

    case "${reset_type}" in
       hard) 
          cd $actual_mnt
          rm -f fallback
          rm -f configured
          rm -f upgrade
          rm -f networkloader  
          cd /tmp
          sync
          ;;
        *)
          cd $actual_mnt
          rm -f fallback
          rm -f configured
          rm -f upgrade
          cd /tmp
          sync
        ;;
    esac

    umount /rcs/swm/boot

}

## main

case $1 in
    mount)
	shift
	mount $*
	;;
    umount)
	shift
	umount $*
	;;
    cup)
	shift
        cup $*
	;;
    fsck)
	shift
	fsck.ext4 $*
	;;
    activate_os)
	shift
	do_activate_os $*
	;;
    confirm_os)
	shift
	do_confirm_os $*
	;;
    clear_ee)
        shift
        do_clear_ee $*
	;;
    rcsversion)
	shift
	rcsversion $*
	;;
    diskfree)
	shift
	diskfree $*
	;;
    disksize)
	shift
	disksize $*
	;;
    prepare_nl_upgrade)
	shift
	do_prepare_nl_upgrade $*
	;;
    reset_boot)
	shift
	reset_boot $*
	;;
## 15B compatiblity
    activate_ee_tcu03)
	shift
	do_activate_os $*
	;;
    confirm_ee_tcu03)
	shift
	do_confirm_os $*
	;;
## APPM & SYS Support
    dns)
	shift
	dns $*
	;;
    arp)
	shift
	arp $*
	;;
    ip_addr)
	shift
	ip addr $*
	;;
    ldconfig)
	shift
	ldconfig $*
	;;
    clear_applicationlogs)
	shift
	rm -rf /rcs/applicationlogs/*/*
	;;
    clear_applicationtmp)
	shift
	rm -rf /tmp/applicationtmp/*/*
	;;
    *)
	echo "$0: not supported: $*" >&2
	exit 1
	;;
esac

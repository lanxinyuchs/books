#!/bin/sh

HOSTSHARE="/hostshare"

. /etc/profile.d/rcsenv

copy_files_to_vm() {
   echo "copy_files_to_vm..."
   logger "copy_files_to_vm..."

   echo "INSTALLING CXPs"
   mkdir -p /software
   cp -a $(find $HOSTSHARE/ -maxdepth 1 -name \*_CXP\* -type d) /software/
   cp -a $HOSTSHARE/*-up.xml                                    /software/
   touch /root/sim32_installed
}

symlink_cxps() {
   echo "symlinking /home/sirpa/software/* to /software/*"
   logger "symlinking /home/sirpa/software/* to /software/*"

   mkdir -p /home/sirpa/software
   cd /home/sirpa/software
   ln -s /software/*CXP* .
   [ -e  /home/sirpa/ee/pgh_ldcache ] &&  rm /home/sirpa/ee/pgh_ldcache
   cd -
}



sim32_install() {
    echo "sim32_install..."
    logger "sim32_install..."

    mkdir -p /software
    if [ ! -e /root/sim32_installed ]
    then
        rm -rf /software/*
        echo "SIM32 INSTALLATION SCRIPT..." \
            && copy_files_to_vm \
            && symlink_cxps \
            && echo "VM installation OK" \
            && poweroff \
                || echo "rcs-sim:ERROR: setup.sh"
    fi
}

sim32_start() {
    echo "sim32_start..."
    logger "sim32_start..."

    #start_sim32.sh
}

start() {
    echo "Starting external setup..."
    logger "Starting external setup..."

    if [ -e $HOSTSHARE/.install ] && [ "$(cat $HOSTSHARE/.install)" == "Yes" ]
    then
        sim32_install
    fi

    ## MW start part
    [ -e /root/sim32_installed ] && sim32_start
}

#case "$1" in
#        start)
#           start;;
#        *)
#           echo "Usage: $0 {start}"
#           exit 1
#           ;;
#esac
start
exit 0

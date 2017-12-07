#!/bin/bash
## -----------------------------------------------------------------------------
## %CCaseFile:	start_vrcs.sh %
## %CCaseDate:	2016-10-18 %
## %CCaseDocNo:  %
##
## Author:      etxarnu
## Description: Start VRCS middleware, i.e. the erlang/OTP run-time
## -----------------------------------------------------------------------------
##
## ----------------------------------------------------------
## %CCaseTemplateFile:   %
## %CCaseTemplateId: %
##
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
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
## R6A/1      2016-05-24   etxarnu     Split from start_rcs.sh for VRCS only
## R6A/3      2016-06-09   etxarnu     Ping default route to update arp cache
## R7A/1      2016-09-29   etxarnu     Changed /var/lib/cloud/instance/user-data.txt
#                                      to /var/lib/cloud/vrcs_config.sh
## R7A/2      2016-10-18   etxarnu     Used LTTNG_HOME where needed.
## R7A/3      2016-10-25   erarafo     /vnf and /rcs conditionally as symlinks
## R7A/4      2016-10-27   etxarnu     Don't clear /tmp
## 2016-11-29  etxarnu  Updated for 64bit product structure
## 2017-02-07  etxarnu  Use +Bi switch to erl to ignore <ctrl>-C in shell
## 2017-03-01  etxarnu  VNFC_ID now used directly from sourced env
## 2017-05-23  etxarnu  no_mw now in /home/sirpa as in G2
## 2017-06-29  etxarnu  Copy ug_patches to dev_patches on to-VNF
## 2017-07-14  etxarnu  Set environment vars not set by EE when using earlystart
## 2017-09-07  etxarnu  Corrected is_ug_ongoing
## 2017-09-25  etxarnu  Corrected is_ug_ongoing again
## --------- ---------------------------------------------------- column 80 --->
##
#set -e #exit if command returns error
set -u #exit if a variable is unset
set -x

WAIT_START=/home/sirpa/wait_start
ONCE_WAIT=/home/sirpa/once_wait


# To debug start_vrcs.sh script one can stop here if /home/sirpa/wait_start file exists
if [ -e ${WAIT_START} ]; then
    while [ -e ${WAIT_START} ]
    do
	sleep 10
    done
elif [ -e ${ONCE_WAIT} ]; then
    touch ${WAIT_START}
fi

Arch=`arch` 
if [ $Arch == "x86_64" ]; then
    ArchDir="x86_64"
    ArchLib="lib64"
else
    ArchDir="i686_32"
    ArchLib="lib32"
fi
HST=vrcs_du
Core=${HST}1
DuId=vrcs
#Functions

#
# Define Append Procedure
#
AppendLog()
{
    cat - >> ${logFile}

}

#log a message to stderr and exit
#printf(bash builtin) avoids problem with -- not recognized by echo
#TBA add logging to file, console, and anywhere else
function fatal() {
    DATE=`date "+%Y-%m-%d %H:%M:%S"`
    printf "$DATE start_vrcs.sh: %s\n" "$*"  | AppendLog
#	printf "%s\n" "$*" >&2
    exit 1;
}

#log a message to stderr
#printf(bash builtin) avoids problem with -- not recognized by echo
#TBA add logging to file, console, and anywhere else
function start_log() {
    DATE=`date "+%Y-%m-%d %H:%M:%S"`
    printf "$DATE start_vrcs.sh: %s\n" "$*" | AppendLog
#	printf "%s\n" "$*" >&2
}



#
# Find patched version of a file.
# for now there is only one single dir we look in
#
function find_patch() {
    local file=${1##*/} #get the basename
    local dev=${DEV_PATCHES}/${file}
    local patch

    for patch in $dev; do
        if [ -f $patch ]; then
            echo $patch
            return 0
        fi
    done
    return 1
}

# $1 is the filepath to logfile, create/chown if not existing
# move previous to $1.old
# hopefully ensures that it can't become to big/lg
rotate_startlog()
{
    [ -f $1 ] && {
	mv -f $1 $1.old
    }
	> $1
}

#
# Patch support. A call to find_and_exec_patch $* in the beginning
# of a script will check for a patch for the script and execute the
# patched file instead.
# NOTE: Can not use start_log in this function since directories
# are not yet created (and want to patch as early as possible)!
#
function find_and_exec_patch() {
    local script=${0##*/}  #get the basename
    local flag=${script//[\.-]/_}_patch #substitute "." and "-" to "_"

    if [ ${!flag:-false} = false ]; then
        if patch=$(find_patch $script) && [ -x $patch ]; then
            echo "Patched version $patch of $script found, using it!"
            eval export ${flag}=true
            exec $patch $*
        fi
    else
        unset ${flag}
    fi
}



#ensure that the argument $1 which is expected to contain metacharacters for
#file-expansion resolves to a directory and only one directory
function resolve_dir_path_int() {
    set -- $1
    if [ $# -ne 1 -o  ! -d ${1} ]; then
	echo ""
    else
	echo ${1}
    fi
}

# Check if directory exist in dev_patches and then ordinary
function resolve_dir_path() {
    local in_path="$RCS_CODE_ROOT/${1}"
    local dev_path="$DEV_PATCHES/${1}"

    if [ "$(resolve_dir_path_int $dev_path)" != "" ]; then
	echo $dev_path
    elif [ "$(resolve_dir_path_int $in_path)" = "" ]; then
	start_log "Couldn't resolve path: ${in_path}"
	echo ""
    else
	echo $in_path
    fi
}

#try to ensure that erlang is in the path, if not assume OTP_ROOT ok and add it
#then check again. Exit with error if unsuccesful
function check_fix_path(){
	if
	    type -afP run_erl >/dev/null
	then
	    : all is well
	elif
	    PATH=${OTP_ROOT}/bin:$PATH type -afP run_erl >/dev/null
	then
	    PATH=${OTP_ROOT}/bin:$PATH  #we fixed it
	else
	    fatal "Erlang run-time not in PATH"
	fi
}

function create_directories() {
    local dir
    for dir
    do
	[ -d $dir ] || {
	    mkdir -p $dir || fatal "Failed to create directory: $dir"
	    start_log "Created directory: $dir"

	}
    done
}

## make_release.escript <CodeRoot> <OtpRoot> <LogFile>
## the <Version> (RCS_VERSION) and <System> (RCS_SYSTEM) are  set and written
## to a sourceable script-file: ${RCS_ROOT}${HOME}/releases/start_data
## release created in cwd so change to where we want it
function create_release() {
    local script=${1##*/}  #get the basename
    local script_file_path=${1}
    local fixed_code_root=${CODE_ROOT/\/\//\/}
    local fixed_otp_root=${OTP_ROOT/\/\//\/}
    if [ -x ${DEV_PATCHES}/${script} ]; then
	script_file_path=${DEV_PATCHES}/${script}
	start_log "Patched version ${script_file_path} of $script found, using it!"
    fi
    {
	cd ${RCS_ROOT}${HOME} &&
	${script_file_path} $fixed_code_root $fixed_otp_root $logFile
    } || fatal "Couldn't change directory or make_release.script returned an error"

    if [ ! -f ${RCS_ROOT}${HOME}/releases/start_data ]; then
	fatal "No start_data -file created by make_release.escript"
    fi
}


is_nonsecure() {
    LMA_EBIN=$(readlink -f /software/*MW*/*/lma-*/ebin|head -1)

    SEC_SCRIPT=$(find $MW_CXP -name has_testlicense.escript)
    if [ $( $SEC_SCRIPT $node_type $LMA_EBIN) == "yes" ]; then
	return 0
    else
	return 1
    fi
}

# Allow three mnesia core dumps
manage_mnesia_core() {
    MNESIA_CORE_DIR="/rcs/db/mnesia_cores/"
    NO=`ls -1t ${MNESIA_CORE_DIR}/* 2>/dev/null | wc -l`
    if [ "$NO" -gt 3 ]; then
	REMOVE=`ls -1t ${MNESIA_CORE_DIR}/* | tail -$((NO-3))`
	echo "REMOVE=$REMOVE"
	rm -f $REMOVE || true
    fi
}

# Check /rcs disk and makes sure that at least 25M is available.
check_disk() {
    SIZE=`df -m /rcs | grep "/" |  awk '{ print $4 }'`
    if [ "$SIZE" -lt 25 ]; then
        # Removes in the following ORDER
	ORDER=(
	    "/rcs/applicationlogs/*CXP*/*"
	    "/rcs/comte/*"
	    "/rcs/fi/*"
	    "/rcs/saf_log/*/*"
	    "/rcs/log/*/*"
	    "/rcs/bootlogs/*"
	    "/rcs/sftp/rop/*"
	    "/rcs/networkloader/esi*.gz"
	    "/rcs/networkloader/esi*.gz.gpg"
	    "/rcs/erlang_disk/erl_crash_dump.*"
	)
	for Dir in "${ORDER[@]}"
	do
	    SIZE=`df -m /rcs | grep rcs | awk '{ print $4 }'`
	    if [ "$SIZE" -lt 25 ]; then
		echo_log "Disk /rcs is almost full; removing $Dir"
		rm -rf $Dir || true
	    else
		break
	    fi
	done
    fi
}

# Allow three erl_crash_dumps
# Remove any erl crash dumps in /rcs/erlang/
manage_erl_crash_dumps() {
    NO=`ls -1t ${ERLANG_LOG_DIR}/erl_crash_dump.* | wc -l`
    if [ "$NO" -gt 3 ]; then
	REMOVE=`ls -1t ${ERLANG_LOG_DIR}/erl_crash_dump.* | tail -$((NO-3))`
	rm -f $REMOVE || true
    fi
}




function symlink_files() {
    local file
    for file
    do
	ln -sf $file ${RELEASES_DIR}/cslibs/${ArchDir}/. || fatal "Failed to symlink: $file"
	start_log "Symlinked: $file to  ${RELEASES_DIR}/cslibs/${ArchDir}/"
    done
}

# Create a new /etc/hosts
function make_hosts() {
    local ClusterNet=${1}
    local DuId=${2}
    echo "127.0.0.1  $DuId localhost  localhost.localdomain" > /etc/hosts
    for i in $(seq 1 64)
    do
	[ ! "$DuId" == "${HST}${i}" ] && echo "$ClusterNet.$i   ${HST}${i}" >> /etc/hosts
    done
}

# This function sets up the cluster IP adresses, hostnames etc
function setup_cluster {
    ClusterNet=$(echo $OWN_IP | awk -F. '{ print $1"."$2"."$3 }')
    DuId=${HST}${MPID}
    echo $DuId >/etc/hostname
    hostname $DuId
    ip addr add ${OWN_IP}/24 dev ${OWN_IF}
    ifconfig ${OWN_IF} up
    make_hosts $ClusterNet $DuId
}
# This pings the default route so that arp cache is updated
function ping_default_route {
    DEFAULT_ROUTE=$(ip route show default | awk '/default/ {print $3}')
    ping -c 1 -W 1 $DEFAULT_ROUTE
}



# This function waits for cluster complete on core side
function wait_for_core {
    start_log "doing tftp towards core"
    timeout 2 tftp -g -r cluster_complete   ${Core} >/dev/null 2>&1
    while [ $? -ne 0 ]
    do
	sleep 0.5
        timeout 2 tftp -g -r cluster_complete ${Core} >/dev/null 2>&1
    done
    timeout 2 tftp -g -r cluster_config  -l ${RCS_HOME}/cluster_config ${Core} >/dev/null 2>&1
    start_log "tftp succeeded"
}



# create some handy aliases in .profile
function make_profile {
   if [ ! -e $1/.profile ]; then
       cat <<EOF > $1/.profile
alias ..='cd ..'
alias ll='ls -l'
alias m='more'
alias to='to_erl /tmp/*@${DuId}*/'
OTP_ROOT=$(readlink -f /software/*RCS*MW*/*/otp-*/priv/tgt_* |head -1)
SYS_ROOT=$(readlink -f /software/*RCS*MW*/*/sys-*/priv |head -1)
export PATH=$OTP_ROOT/bin:$SYS_ROOT/bin/:$PATH
EOF
   fi
}

# Name of the "vnf" directory
declare -r VNF_DIR="vnf"

# Set up /$VNF_DIR symlinked to shared disk when applicable.
# Log to a temporary file since regular logging is not yet in place.
function setup_vnf {

  local log=`mktemp /root/setup_vnf_XXXXXXXX`
  cat /dev/null >$log
  echo $log

  if [[ ! -d /shared ]]; then
    echo "no shared disk available, no action" >>$log
    return
  fi

  if [[ -L /$VNF_DIR ]]; then
    echo "already a symlink: /$VNF_DIR, no action" >>$log
    return
  fi

  if [[ -e /$VNF_DIR ]]; then
    echo "exists but not a symlink: /$VNF_DIR, unexpected; no action" >>$log
    return
  fi

  if [[ -d /shared/$VNF_DIR ]]; then
    echo "directory exists: /shared/$VNF_DIR" >>$log
  elif mkdir /shared/$VNF_DIR >>$log 2>&1; then
    echo "directory created: /shared/$VNF_DIR" >>$log
  else
    echo "failed to create directory: /shared/$VNF_DIR" >>$log
    return
  fi

  if ln -s /shared/$VNF_DIR /$VNF_DIR >>$log 2>&1; then
    echo "symlink created: /$VNF_DIR -> /shared/$VNF_DIR" >>$log
  else
    echo "failed to create symlink: /$VNF_DIR -> /shared/$VNF_DIR" >>$log
  fi
}

# Set up /rcs symlinked to shared disk when applicable.
# Log to a temporary file since regular logging is not
# yet in place.
function setup_rcs {

  local log=`mktemp /root/setup_rcs_XXXXXXXX`
  cat /dev/null >$log
  echo $log

  local name=rcs

  if [[ ! -d /shared ]]; then
    echo "no shared disk available, no action" >>$log
    return
  fi

  if [[ -L /$name ]] || mountpoint -q /rcs; then
    echo "already a symlink: /$name, no action" >>$log
    return
  fi

  if [[ ! -d /$name ]]; then
    echo "neither symlink nor directory: /$name, unexpected; no action" >>$log
    return
  fi

  if [[ -z "$VNFC_ID" ]]; then
      echo "could not find own VNFC_ID, no action" >>$log
      return
  else
      echo "own VNFC_ID is: $VNFC_ID" >>$log
  fi
  local targetDir=/shared/$VNFC_ID

  if [[ -d $targetDir ]]; then
    echo "own VNFC directory already exists but /$name is not a link" >>$log
  else
      if mkdir -p $targetDir >>$log 2>&1; then
	  echo "directory created: $targetDir" >>$log
      else
	  echo "failed to create: $targetDir" >>$log
	  return
      fi
  fi

  local dir; for dir in `ls -1 /$name`; do
    for target in $targetDir /$VNF_DIR/; do
      if cp -v -a /$name/$dir $target >>$log 2>&1; then
	echo "recursively copied /$name/$dir -> $target" >>$log
      else
	echo "failed to copy /$name/$dir -> $target" >>$log
	return
      fi
    done
    rm -r -f /$name/$dir
  done

  rm -rf /$name
  if ln -s $targetDir /$name >>$log 2>&1; then
    echo "created symlink: /$name -> $targetDir" >>$log
  else
    echo "failed to create symlink: /$name -> $targetDir" >>$log
  fi
}

# setup config_drive for network_data.json
function setup_config_drive {
    start_log "doing setup config drive"
    blk_dev=$(blkid -t LABEL="config-2" -odevice)
    nt_file="/mnt/config/openstack/2015-10-15/network_data.json"

    if [ -z "$blk_dev" ]; then
        echo "failed to get config drive device."
        start_log "failed to get config drive device."
        return
    fi

    mkdir -p /mnt/config
    mount -r $blk_dev /mnt/config
    if [ -f "$nt_file" ]; then
        start_log "$nt_file exists"
        cp $nt_file /var/lib/cloud/instance/
    else
        start_log "No network_data.json file exists"
    fi
    umount /mnt/config

    start_log "finish setup config drive"
}

function insert_readme() {
  local -r sysEtc=$1/etc
  local -r devPatches=$2
  local -r marker='===end=of=header===do=not=edit=this=line==='
  sed -e "1,/$marker/d" $sysEtc/README-dev_patches.txt >$devPatches/README.txt
}

function is_ug_ongoing() {
    [[ -e /rcs/swm/init_config ]]
}

function copy_ug_patches() {
    local UgPatches=${1}
    local DevPatches=${2}
    if [[ ! -d  $UgPatches ]]; then 
	start_log "no  $UgPatches directory" 
    elif [[ -z `ls -1  $UgPatches` ]]; then 
	start_log "no patches in $UgPatches"
    else 
	cp -v -R $UgPatches/* $DevPatches
    fi
}
###############################################################################
########################### main Main MAIN ####################################
###############################################################################

export USER=root 
export HOME=/root 
export RCS_SIM_ROOT="/"
RCSSIM_PID=1 
HOST=$(hostname)

ERLANG_LOG_DIR=/rcs/erlang
create_directories   $ERLANG_LOG_DIR
logFile=${ERLANG_LOG_DIR}/start_rcs.log
rotate_startlog $logFile

#Redirect all stdout/stderr to ${logFile}
exec >> ${logFile} 2>&1

start_log "===== VRCS environment====="

rcsmw_xml=`grep "<mwinfo/>" /software/*/*.xml | awk -F: '{ print $1 }' `
if [ "${rcsmw_xml}" == "" ]; then
    fatal "<mwinfo/> tag not found in any cxp-xml file"
else
    rcs_software=`dirname ${rcsmw_xml}`
fi

export MW_CXP=${rcs_software}
export OTP_ROOT=$(readlink -f ${MW_CXP}/*CXC*/otp*/priv/tgt_*)
export SYS_ROOT=$(readlink -f ${MW_CXP}/*CXC*/sys-*/priv)
export PATH=/root/dev_patches/:$OTP_ROOT/bin:$SYS_ROOT/bin/:/usr/local/bin:$PATH


export BT="VM"
export RCS_ROOT="/"
export CODE_ROOT="/home/root/software"
RCS_HOME=${RCS_ROOT}${HOME}
RCS_CODE_ROOT="$CODE_ROOT/$(basename ${MW_CXP})"

if [ ! -e /etc/hostname ]; then
    hostname vrcs
    echo "vrcs" > /etc/hostname
fi

export RCS_MODE=simulated
export SNAME=${SNAME:-$USER}
if [ ! -z "${PGH_WDFN+x}" ];then
    export PGH_WDFN=${PGH_WDFN}
fi


grep export /etc/profile.d/vrcs | grep VNF > /tmp/start_vrcs.vnfenv
. /tmp/start_vrcs.vnfenv

PROGRESS_SETUP_VNF=`setup_vnf`
PROGRESS_SETUP_RCS=`setup_rcs`

# The following is needed for TRI but should be moved to a
# file to be sourced by PGH before starting an application
export LTTNG_HOME=${LTTNG_HOME:-${RCS_ROOT}/rcs/lttng}
export PFS_PATH=${PFS_PATH:-${LTTNG_HOME}/tri}

TRI_LOG_DIR=${LTTNG_HOME}/tri/telog
LTTNG_LOG_DIR=${LTTNG_HOME}/tri/lttng
DEF_LOG_DIR=${LTTNG_HOME}/default

export TE_LTTNG_SNAPSHOT_PATH=${TRI_LOG_DIR}
export TE_LTTNG_SESSIONS=rcs

HEART_LOG=${ERLANG_LOG_DIR}/heart.log

export DEV_PATCHES=${RCS_HOME}/dev_patches
UG_PATCHES=${RCS_ROOT}/rcs/swm/ug_patches

PIPE_DIR=${PIPE_DIR:-${RCS_ROOT}/tmp}

export PIPE_DIR=${PIPE_DIR:-/tmp}

find_and_exec_patch $*

CONSUL_LOG_DIR=/var/log/consul
XCM_OBSERVABILITY=/run/xcm/ctl	#XCM compiled in default

create_directories  $CODE_ROOT  $CONSUL_LOG_DIR ${XCM_OBSERVABILITY}
chmod 777 ${XCM_OBSERVABILITY}


# Make sure that /rcs disk has at least 25M before continue.
# This is to avoid crashes at startup due to full disk and by that
# avoid to fall down to NL.
check_disk

# Limit the number of erl_crash_dump files
manage_erl_crash_dumps

# Limit the number of mnesia cores
manage_mnesia_core




setup_config_drive

cat $PROGRESS_SETUP_VNF $PROGRESS_SETUP_RCS | AppendLog
rm $PROGRESS_SETUP_VNF $PROGRESS_SETUP_RCS

CORE_DUMP_DIR=${RCS_ROOT}/rcs/dumps/
export DUMP_DIR=${RCS_ROOT}/rcs/dumps/appdump
export COLI_CMDS_PATH=${RCS_ROOT}/colish

[ ! -e  /home/sirpa/dev_patches ] && ln -s /root/dev_patches /home/sirpa/

make_profile /root

# check if cluster configuration is defined
VRCS_CONFIG=/rcs/networkloader/vrcs_config.sh 

if [ ! -e ${VRCS_CONFIG} ]; then
    INITIAL_CONFIG=/var/lib/cloud/vrcs_config.sh
    if [ $(cat  ${INITIAL_CONFIG} | wc -l) -gt 5 ]; then
	cp ${INITIAL_CONFIG} ${VRCS_CONFIG}
    fi
fi
[ -e ${VRCS_CONFIG} ] && 
{ 
    start_log "Found  ${VRCS_CONFIG} with content"
    cat ${VRCS_CONFIG}
    . ${VRCS_CONFIG}
    setup_cluster
    if [ "$DuId" == "${Core}" ]; then
	create_directories  ${RCS_HOME}/tftproot
	ln -sf ${RCS_HOME}/releases/start_data ${RCS_HOME}/tftproot/
	ln -sf ${RCS_HOME}/cluster_config ${RCS_HOME}/tftproot/
	ln -sf ${RCS_HOME}/cluster_complete ${RCS_HOME}/tftproot/
    else
	echo "Waiting for Core"
	wait_for_core
    fi

}

ping_default_route # to update arp cache

export HOST=$(hostname)
create_directories  ${PIPE_DIR}/${SNAME}@${HOST}

create_directories $TRI_LOG_DIR $LTTNG_LOG_DIR $DEF_LOG_DIR $CORE_DUMP_DIR $COLI_CMDS_PATH  $DUMP_DIR

check_fix_path


resolve_sys=${rcs_software}/*CXC*/sys-*

SYS_DIR=$(resolve_dir_path_int "${resolve_sys}/priv")
sys_priv_path=${SYS_DIR}/bin
sys_arch_bin_path=${SYS_DIR}/tgt_${ArchDir}/bin

up_xml=$(ls ${RCS_HOME}/software/*-up.xml)
node_type=$(grep "<type>" $up_xml  | sed 's:/::' | sed 's:<type>::g' | tr -d ' ')
if [ ! -f ${RCS_HOME}/install_complete ]; then
    eval $(grep "product" $up_xml  | head -1|tr ' ' '\n' | grep = | sed 's:/>::')
    up_dir=/rcs/swm/archive/"$name"_"$id"_"$version"
    if [ ! -d $up_dir ]; then
	mkdir $up_dir
	cd $up_dir
	ln -s ${RCS_HOME}/software/* .
    fi
    if patch=$(find_patch ${sys_priv_path}/make_release.escript) && [ -x $patch ]; then
	start_log "Patched version $patch of ${sys_priv_path}/make_release.escript found, using it!"
	create_release $patch
    else
	create_release ${sys_priv_path}/make_release.escript
    fi
fi

. ${RCS_ROOT}${HOME}/releases/start_data #source to get RCS_VERSION and RCS_SYSTEM

RELEASES_DIR=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}

CXPs=`\ls -1 ${RCS_ROOT}${HOME}/software | grep -v '.cxp'  | grep -v '.xml'`
for CXP in $CXPs
do
  create_directories   ${RCS_ROOT}/rcs/applicationlogs/${CXP%_*}
  create_directories   ${RCS_ROOT}/tmp/applicationtmp/${CXP%_*}
done

create_directories ${RCS_ROOT}/rcs/persistent
chmod 777  ${RCS_ROOT}/rcs/persistent

create_directories ${RELEASES_DIR}/libs/${ArchDir}

create_directories ${RELEASES_DIR}/cslibs/${ArchDir}

export RCS_LIB_ROOT=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}/libs



LD_LIBRARY_PATH=${LD_LIBRARY_PATH:-}


export RCS_LIB_DIR=${RCS_LIB_ROOT}/${ArchDir}

if [ $Arch == "i686" ]; then

#32bit VRCS (with old EE) needs symlinks
LD_LIBRARY_PATH=${RELEASES_DIR}/cslibs/${ArchDir}:${LD_LIBRARY_PATH}
SAFC="$(resolve_dir_path "*CXC*/safc-*/priv/tgt_${ArchDir}/${ArchLib}")"
APPM="$(resolve_dir_path "*CXC*/appm-*/priv/tgt_${ArchDir}/${ArchLib}")"
EQS="$(resolve_dir_path "*CXC*/eqs*/priv/tgt_${ArchDir}/${ArchLib}")"
CEC="$(resolve_dir_path "*CXC*/cec*/priv/tgt_${ArchDir}/${ArchLib}")"
LIH="$(resolve_dir_path "*CXC*/lih*/priv/tgt_${ArchDir}/${ArchLib}")"
CLH="$(resolve_dir_path "*CXC*/clh*/priv/tgt_${ArchDir}/${ArchLib}")"
GMF="$(resolve_dir_path "*CXC*/gmf*/priv/tgt_${ArchDir}/${ArchLib}")"
ALH="$(resolve_dir_path "*CXC*/alh*/priv/tgt_${ArchDir}/${ArchLib}")"
PMS="$(resolve_dir_path "*CXC*/pms*/priv/tgt_${ArchDir}/${ArchLib}")"
SYS="$(resolve_dir_path "*CXC*/sys-*/priv/tgt_${ArchDir}/${ArchLib}")"
OOT="$(resolve_dir_path "*CXC*/oot*/priv/tgt_${ArchDir}/${ArchLib}")"
CERT="$(resolve_dir_path "*CXC*/cert*/priv/tgt_${ArchDir}/${ArchLib}")"
PES="$(resolve_dir_path "*CXC*/pes*/priv/tgt_${ArchDir}/${ArchLib}")"
TIM="$(resolve_dir_path "*CXC*/tim*/priv/tgt_${ArchDir}/${ArchLib}")"

symlink_files ${SAFC}/* ${APPM}/* ${EQS}/*  ${CEC}/*  ${LIH}/*  ${CLH}/* \
              ${GMF}/* ${ALH}/* ${PMS}/* ${SYS}/* ${OOT}/* ${CERT}/* ${PES}/* ${TIM}/*
fi


export LD_LIBRARY_PATH=${DEV_PATCHES}:${OTP_ROOT}/lib:${LD_LIBRARY_PATH}
export SIM_TGT='tgt_'${ArchDir}
HEART_CMD="reboot"
export HEART_COMMAND="echo \`date\` $HEART_CMD >> $HEART_LOG; $HEART_CMD "
start_log "HEART_COMMAND set to: $HEART_COMMAND"

insert_readme ${SYS_DIR} ${DEV_PATCHES}

is_ug_ongoing &&  copy_ug_patches ${UG_PATCHES} ${DEV_PATCHES}

BOOT_FILE=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}/${RCS_SYSTEM}-${RCS_VERSION}

SETCOOKIE="-setcookie rcs"

####  secure VRCS not enabled yet #####
EPMD_ENV=" "
# if is_nonsecure ; then
#     EPMD_ENV=" "
# else
#     EPMD_ENV="-env ERL_EPMD_ADDRESS 127.0.0.1"
# fi

PROTO_DIST="-proto_dist inet6_tcp"

run_cmd="erl -boot ${BOOT_FILE} -boot_var OTP_ROOT $OTP_ROOT \
       -boot_var CODE_ROOT $CODE_ROOT -sname $SNAME  -heart $SETCOOKIE\
       -async_shell_start -smp enable \
       -env ERL_CRASH_DUMP $ERLANG_LOG_DIR/erl_crash_dump.`date +%FT%T` \
       -env ERL_CRASH_DUMP_SECONDS 10 \
       ${EPMD_ENV} \
       ${PROTO_DIST} \
       -config ${RCS_ROOT}${HOME}/releases/${RCS_VERSION}/sys.config \
       -pa ${DEV_PATCHES} \
       +W w +A 64 +Bi +d +C multi_time_warp"

start_log "run_cmd=  $run_cmd"

export RUN_ERL_LOG_GENERATIONS=10
export RUN_ERL_LOG_MAXSIZE=1000000

NO_MW=/home/sirpa/no_mw
ONCE_MW=/home/sirpa/once_mw

if [ -e ${NO_MW} ]; then
    while [ -e ${NO_MW}  ]
    do
	sleep 10
    done
elif [ -e ${ONCE_MW} ]; then
    touch ${NO_MW} 
fi

ulimit -n 2048

#when run from heart this line cause error, workaround - ensure always true
printf "to_erl ${PIPE_DIR}/${SNAME}@${DuId}/\n" || true

run_erl  -daemon ${PIPE_DIR}/${SNAME}@${DuId}/ $ERLANG_LOG_DIR "$run_cmd"
printf "VRCS-MW started with RCS_ROOT= ${RCS_ROOT}"


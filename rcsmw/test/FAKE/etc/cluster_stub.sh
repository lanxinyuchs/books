#! /bin/bash
##
## %CCaseFile:	cluster_stub.sh %
## %CCaseRev:	/main/R3A/R12A/1 %
## %CCaseDate:	2017-11-15 %
## Author: <name>, <e-mail address>
##
## Purpose:
##
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
## ----------------------------------------------------------------------
##
## Revision history:
##
## Rev     Date       Name        What
## -----   -------    --------    -------------------------------------
## R3A/1   2014-11-10 etxpeno     first version
## R3A/2   2014-12-03 etxpeno     start nc_service
## R3A/3   2015-02-11 etxarnu     removed setcookie
## R3A/5   2015-02-13 etxarnu     keep  setcookie for target
## R12A/1  2017-12-15 etxarnu     Adaptation for git migration
##----------------------------------------------------------------------

declare -r HeartbeatPeriod=2

declare Heartbeats=0
declare FailedHeartbeats=1


function log() {
    D=`date +%Y-%m-%dT%H:%M:%S`
    echo $D: $* >>$LOG_FILE
}


function heartbeat() {
#    log "beat"
    Heartbeats=$((Heartbeats + 1))
    if ! $ERL_CALL  $COOKIE_ARGS -sname $TNODE -a 'oi_stetoscope heartbeat' ; then
	FailedHeartbeats=$((FailedHeartbeats + 1))
        log "unsuccessful heartbeat"
    elif [[ $((Heartbeats % 100)) == 1 ]]; then
        log "Successful heartbeats: $Heartbeats"
	FailedHeartbeats=0
    else
	FailedHeartbeats=0
    fi
}


function cleanup() {
    local -r signal=$1; shift
    log "received signal: $signal; exiting"
    exit
}


function get_safe_path() {
    if [ -e $RCS_PATCH_DIR/safe ]; then
	echo "$RCS_PATCH_DIR/safe/lib/safe-*/ebin"
    else
	ARCH=`arch`
	if [ "${ARCH}" = "armv7l" -o "${ARCH}" = "aarch64" ]; then
	    TGT_LIB="tgt_arm-wr6"
	else
	    TGT_LIB=$SIM_TGT
	fi
	echo "$CXP_PATH/*/otp-*/priv/${TGT_LIB}/lib/safe-*/ebin"
    fi
}


function idle() {
    local -r reason="$1"; shift
    log "going idle, reason: $reason"
    while true; do
        sleep 600
        log "alive and idle, reason: $reason"
    done
}


############## Execution begins here ###################################

# Variables defined in cluster_stub_envvar.xml:
# NCNODE
# TESTRELPATH
# OTPSAFERELPATH

# Variables defined as 'exportedenv' (undocumented feature?) in
# cluster_stub_envvar.xml:
# PATH
# HOME
# OTP_ROOT
# RCS_ROOT

# Variables set by the load module handler:
# APP_TMP
# LOG_DIR

# Variables set elsewhere
# SNAME: In the simulator: ${USER}, or ${USER}_wxyz, where _wxyz is
# is a per-simulator-instance unique random suffix (the -u option
# in rcssim enables this suffix).
# On target: ${USER}, which is 'sirpa'.

LOG_FILE=$LOG_DIR/cluster_stub.log
log "########################## LOGGING STARTED ##########################"

exec >> ${LOG_FILE} 2>&1
# Run in debug mode
#set -x

TNODE=${NCNODE}_${SNAME}
ERL_CALL=`readlink -f ${OTP_ROOT}/lib/erl_interface-*/bin/erl_call`

trap "cleanup INT" INT    # unclear if INT can be used at all?
trap "cleanup HUP" HUP

TESTAPPTMP=$RCS_ROOT/tmp/cluster_stub_app
OTPSAFERELPATH=$(get_safe_path)

log "TNODE: $TNODE"
log "OTP_ROOT: $OTP_ROOT"
log "SIM_TGT $SIM_TGT"
log "ERL_CALL: $ERL_CALL"
log "ERL_EPMD_PORT: $ERL_EPMD_PORT"
log "TESTAPPTMP: $TESTAPPTMP"
log "OTPSAFERELPATH: $OTPSAFERELPATH"


if [[ -d $TESTAPPTMP ]]; then
    log "using existing directory: $TESTAPPTMP"
elif mkdir -p $TESTAPPTMP; then
    log "created directory: $TESTAPPTMP"
else
    log "failed to create directory: $TESTAPPTMP"
    idle "failed to create directory"
fi

SETCOOKIE_ARGS=""
COOKIE_ARGS=""
[ "$SIM_TGT" != "tgt_i686" ] && SETCOOKIE_ARGS=" -setcookie $NCNODE"
[ "$SIM_TGT" != "tgt_i686" ] && COOKIE_ARGS=" -c $NCNODE"

# Bring down a previous Erlang node, if present

if $ERL_CALL $COOKIE_ARGS -sname $TNODE -a 'init stop'; then
    log "Stopped a stale Erlang node; wait for 5 s"
    sleep 5
else
    log "Apparently no stale Erlang node present"
fi


# Spawn an Erlang node. It is trusted that TNODE is always
# unique, even in the case of several users running multiple
# simulator instances on a hub host.

ERL_ARGS=""
ERL_ARGS+=" -pa $CXP_PATH/$TESTRELPATH"
ERL_ARGS+=" -pa $OTPSAFERELPATH"
ERL_ARGS+=" -pa $DEV_PATCHES"
ERL_ARGS+=" -sname $TNODE"
ERL_ARGS+=" -kernel net_ticktime 8"
ERL_ARGS+=" -run oi_stetoscope start"
ERL_ARGS+=" -run nc_service start"
ERL_ARGS+=$SETCOOKIE_ARGS


run_erl -daemon $TESTAPPTMP/ $TESTAPPTMP "erl $ERL_ARGS"
log "spawned Erlang node with ARGS= $ERL_ARGS"


# Produce heartbeat

#set +x
while [ $FailedHeartbeats -lt 80 ]; do
#    log "<3 <3 <3 <3 <3 about to heartbeat"
    heartbeat
    if [ $FailedHeartbeats -eq 0 ] ; then
	X=$((Heartbeats - 1))
	log "Initial heartbeat succeeded after $X tries"
	break
    fi
    sleep $HeartbeatPeriod
done

if [ $FailedHeartbeats  -eq 0 ]; then
    while [ $FailedHeartbeats  -lt 3 ]; do
	heartbeat
	sleep $HeartbeatPeriod
    done
else
    log "Failed to start Erlang node; exiting"
fi

log "Erlang node not responding; exiting"

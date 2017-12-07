#! /bin/bash
##
## %CCaseFile:	trapReceiver.sh %
## %CCaseRev:	/main/R2A/R3A/3 %
## %CCaseDate:	2015-03-26 %
## Author: <name>, <e-mail address>
##
## Purpose: This script establishes a listener for SNMP traps.
## COM may be configured to send traps to this listener for
## test purposes.
##
## The script can be run standalone or as an RBS CS application
## running with tag=central.
##
## Results are written to an application log file.
##
##
## Dependencies: The Net-SNMP package, Ericsson mibs in the RCSMW CXP.
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
## Rev        Date       Name        What
## -----      -------    --------    ------------------------------------
## R2A/1      2014-09-17 erarafo     First version
## R2A/2      2014-09-17 erarafo     Fixed environment issues and bugs
## R2A/3      2014-09-18 erarafo     Change of static port to 11001
## R2A/4      2014-09-22 erarafo     Steps toward simulator
## R2A/5      2014-09-22 erarafo     Equal to R2A/3
## R2A/6      2014-09-23 erarafo     Uses a monitor script now
## R2A/7      2014-09-24 erarafo     Refactored search for MIB files
## R3A/1      2014-10-02 erarafo     /home/sirpa/dev_patches in subprocess PATH
## R3A/2      2014-10-02 erarafo     Go idle if child process not there
## R3A/3      2015-03-26 erarafo     PortBase dynamically assigned
## ----------------------------------------------------------------------

set -o nounset

declare -r PacemakerPeriod=4

declare -r LogDir=${LOG_DIR:-/tmp}
declare -r Log=$LogDir/trapReceiver-log.txt

declare -r BasicMibDirs=/usr/share/snmp/mibs

declare -r BoardType=${BT:-undefined}

declare -r Script=trapReceiver.sh
declare -r ScriptDir=`dirname $0`

declare -r Self=$$
declare -r UserName=`id --user --name`

function help() {
  cat <<EOF
Usage is: $Script [OPTIONS]
Options are:
  -h       this help
EOF
}

function info() {
  printf "%s\n" "$Script: INFO: $1" >>$Log
}

function warning() {
  printf "%s\n" "$Script: WARNING: $1" >>$Log
}

function idle() {
  info "going idle"
  while true; do
    sleep 60
  done
}

function die() {
  printf "%s\n" "$Script: FATAL: $1" >>$Log
  idle
}


function architecture() {
  if [[ -r /etc/issue ]] && grep --silent 'Wind River Linux' /etc/issue; then
    echo board
  else
    echo sim
  fi
}


## findMibFile
##
## Echoes the absolute path of one of the files named
## ERICSSON-ALARM-MIB.mib. The RBS CS middleware CXP
## is searched; it is recognized by a symlink named
## RCSMW-* or RCS-SIM*.
##
## If no file is found then nothing is echoed.

function findMibFile() {
  local -r softwareDir=`dirname $CXP_PATH`
  info "softwareDir: $softwareDir"

  local -r cxpPathRcs=`find $softwareDir \
    -maxdepth 1 \
    -mindepth 1 \
    -type l \
    | sed \
      -e '/[/]RCSMW-/p' \
      -e '/[/]RCS-SIM/p' \
      -e d`
  info "RCS CXP path, symlink: $cxpPathRcs"

  local -r cxpPathRcsResolved=`readlink -f $cxpPathRcs`
  info "RCS CXP path, resolved: $cxpPathRcsResolved"

  find $cxpPathRcsResolved \
    -type f \
    -name ERICSSON-ALARM-MIB.mib \
  | head -n 1
}


function pacemaker() {
  local -r pid=$1; shift
  local -r MaxSignalFailures=3
  local failureCount=$((0))

  while [[ $failureCount -le $MaxSignalFailures ]]; do
    if ! kill -s SIGUSR1 $pid; then
      failureCount=$((failureCount + 1))
      info "failed to send SIGUSR1 to process $pid, count: $failureCount"
    fi
    sleep $PacemakerPeriod
  done
  info "maximum failure count reached: $MaxSignalFailures"
  idle
}


declare MonitorReady=false

function handleMonitorReady() {
  MonitorReady=true
}

function waitForMonitorReady() {
  while [[ $MonitorReady != true ]]; do
    sleep 1
  done
}


########################################################################
# Execution begins here

trap handleMonitorReady SIGUSR1

cat /dev/null >$Log

declare OptionPatterns=""
OptionPatterns+="h"

while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

if [[ $# -gt 0 ]]; then
  die "unknown arguments: $*, try -h for help"
fi

declare -r Version=`echo "%CCaseRev:	/main/R2A/R3A/3 %" | sed -e 's|.*CCaseRev:[ \t]*||' -e 's|.$||' -e 's|[ \t]*$||'`

info "starting, user: $UserName, pid: $Self, version: $Version"
info "architecture: `architecture`"
if [[ `architecture` != sim ]]; then
  info "board type: $BoardType"
fi
info "working directory is: `pwd`"
info "PATH is: $PATH"

declare -r MibFile=`findMibFile`
if [[ -z "$MibFile" ]]; then
  die "cannot find: ERICSSON-ALARM-MIB.mib"
fi

declare -r MibDir=`dirname $MibFile`
info "MIB directory: $MibDir"

export PATH="$RCS_PATCH_DIR:$PATH:$ScriptDir"
info "extended PATH: $PATH"

if command -v trapReceiverMonitor.sh >/dev/null; then
  info "launching: `command -v trapReceiverMonitor.sh`"
else
  warning "cannot execute: trapReceiverMonitor.sh"
  idle
fi


declare PortBase=undefined
declare MaxAttempts

if grep '{snmptrapdPort,[0-9]*}' $RCS_PATCH_DIR/port.conf >/dev/null 2>&1; then
  # Get the UDP port from a port.conf file in dev_patches: This scheme
  # is used in the RBS CS Delivery Check.
  PortBase=`sed -e '/{snmptrapdPort,/!d' -e 's/{snmptrapdPort,//' -e 's/}.*//' $RCS_PATCH_DIR/port.conf`
  MaxAttempts=1
fi

if [[ -n "$PortBase" && -z "`echo $PortBase | tr -d 0123456789`" ]]; then
  # Sane value of PortBase obtained from dev_patches
  true
else
  # Fallback values
  PortBase=11001
  MaxAttempts=25
fi


trapReceiverMonitor.sh \
  $Self \
  $Log \
  $PortBase \
  $APP_TMP \
  $MibDir:$BasicMibDirs \
  $MaxAttempts \
  &

declare -r MonitorPid=$!


info "wait for monitor ready"
waitForMonitorReady


info "starting pacemaker towards: $MonitorPid"
pacemaker $MonitorPid

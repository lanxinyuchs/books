#! /bin/bash
##
## %CCaseFile:	trapReceiverMonitor.sh %
## %CCaseRev:	/main/R2A/R3A/6 %
## %CCaseDate:	2015-03-26 %
## Author: erarafo
##
## Purpose: Start and stop an SNMP trap receiver. The intention
## is that there shall never be a hanging trap receiver process
## if the parent of this script gets killed.
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
## R2A/1      2014-09-22 erarafo     First version; placeholder
## R2A/2      2014-09-23 erarafo     Fully functional and simulator capable
## R2A/3      2014-09-24 erarafo     Timeout increased by 4x
## R2A/4      2014-09-24 erarafo     Log messages adjusted
## R2A/5      2014-09-29 erarafo     Kill very old snmptrapd processes
## R2A/6      2014-09-29 erarafo     Raised MaxAttempts
## R3A/1      2014-10-01 erarafo     Avoid use of WARNING for minor problems
## R3A/2      2014-10-02 erarafo     Workaround for USER not being set
## R3A/3      2014-10-02 erarafo     Revised messages that testcase looks for
## R3A/4      2014-10-06 erarafo     Parameter tweaks
## R3A/5      2014-10-24 erarafo     Allow 15 s for snmptrapd startup
## R3A/6      2015-03-26 erarafo     PortBase dynamically assigned
declare -r ScriptVersion='R3A/5'
## ----------------------------------------------------------------------

set -o nounset

declare -r MaxDaemonStartupDelay=15
declare -r MonitoringPeriod=2
declare -r Timeout=30

declare -r StaleLimit=$((12*3600))

declare -r Script=trapReceiverMonitor.sh

function info() {
  printf "%s\n" "$Script: INFO: $1" >>$Log
}

# Never alter the exact wording of a testcase message,
# it will be used in pattern matching!

function testcaseMessage() {
  printf "%s\n" "$Script: TESTCASE MESSAGE: $1" >>$Log
}

function warning() {
  printf "%s\n" "$Script: WARNING: $1" >>$Log
}


declare LatestSignal=0

function handlePacemaker() {
  LatestSignal=`date +%s`
}


function processAge() {
  local -r pid=$1; shift
  local -r uptime=`sed -e 's| .*||' -e 's|[.].*||' </proc/uptime`
  local -r starttime=$((`cat /proc/$pid/stat | (read _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ st _ && echo $st)` / 100))
  if [[ -z "starttime" ]]; then
    echo 0
  else
    echo $((uptime - starttime))
  fi
}

########################################################################
# Execution begins here

declare -r ParentPid=$1; shift
declare -r Log=$1; shift
declare -r PortBase=$1; shift
declare -r AppTmp=$1; shift
declare -r MibDirs=$1; shift
declare -r MaxAttempts=$1; shift

declare -r Self=$$
declare -r UserName=`id --user --name`

info "starting, version: ${ScriptVersion}, user: $UserName, pid: $Self"

trap handlePacemaker SIGUSR1
kill -s SIGUSR1 $ParentPid
info "ready to receive pacemaker signals"



########################################################################
# Kill apparently stale snmptrapd instances, if any

declare -r SnmpTrapdPids=`pgrep -u $UserName -x snmptrapd`
declare -r SnmpTrapdPidsAll=`pgrep -x snmptrapd`
info "snmptrapd processes not owned by $UserName: $((`echo $SnmpTrapdPidsAll | wc --words` - `echo $SnmpTrapdPids | wc --words`))"
info "own snmptrapd processes -"
declare ProcessAge
declare ProcessDays
declare ProcessHours
declare ProcessSecs
declare ProcessListEntry
for pid in $SnmpTrapdPids; do
  ProcessAge=`processAge $pid`
  ProcessDays=$((ProcessAge / 86400))
  ProcessHours=$(((ProcessAge - ProcessDays*86400)/3600))
  ProcessSecs=$((ProcessAge - ProcessDays*86400 - ProcessHours*3600))
  ProcessListEntry="`printf 'pid: %5d %2dd %2dh %4ds' $pid $ProcessDays $ProcessHours $ProcessSecs`"
  info "$ProcessListEntry"
  if [[ $ProcessAge -gt $StaleLimit ]]; then
    info "killing apparently stale process: $pid"
    kill -TERM $pid
    sleep 2
    if [[ -d /proc/$pid ]]; then
      info "brutally killing apparently stale process: $pid"
      kill -s KILL $pid
    fi
  fi
done




########################################################################
# Make repeated attempts to launch the snmptrapd daemon

declare Attempt=0
declare PortOffset=0
declare Port
declare -r ConfigFile=$AppTmp/trapReceiver-$$.conf
declare DaemonRunning=false
declare SnmptrapdPid

while [[ $DaemonRunning != true ]]; do
  Attempt=$((Attempt+1))

  if [[ $Attempt -gt $MaxAttempts ]]; then
    testcaseMessage "failed, giving up after maximum attempts: $MaxAttempts"
    exit 2
  else
    Port=$((PortBase+PortOffset))
    printf "%s\n" "snmpTrapdAddr $Port" >$ConfigFile
    printf "%s\n" "disableAuthorization yes" >>$ConfigFile

    if grep --silent "^ [0-9][0-9]*: [0-9][0-9]*:`printf '%X' $Port` " </proc/net/udp; then
      info "UDP port apparently in use: $Port"
      PortOffset=$((PortOffset+1))
    else
      info "launching snmptrapd using port: $Port"
      export PATH="$PATH:/usr/sbin"
      SNMP_PERSISTENT_FILE=/dev/null snmptrapd \
	-c $ConfigFile \
	-C \
	-t \
	-n \
	-f \
	-A \
	-Lf$Log \
	-Of \
	-OS \
	-F 'trap from: %b%v\n' \
	-M $MibDirs \
	-m ERICSSON-TOP-MIB:ERICSSON-ALARM-MIB \
	&

      SnmptrapdPid=$!
      info "snmptrapd pid: $SnmptrapdPid"

      declare WaitBegin=`date +%s`
      declare WaitNow=$WaitBegin
      while [[ $DaemonRunning != true && $((WaitNow - WaitBegin)) -le $MaxDaemonStartupDelay ]]; do
        sleep 1
	if grep --silent '^NET-SNMP version' $Log; then
          info "snmptrapd started after $((WaitNow - WaitBegin)) s"
	  testcaseMessage "success, using port: $Port"
	  DaemonRunning=true
	else
          WaitNow=`date +%s`
	fi
      done

      if [[ $DaemonRunning != true ]]; then
        info "snmptrapd not started after ${MaxDaemonStartupDelay} s"
        testcaseMessage "failed, apparently snmptrapd was not started"
        info "no more attempt to launch snmptrapd"
	kill $SnmptrapdPid 2>/dev/null
        info "exiting: $Script, pid: $Self"
        exit 1
      fi
    fi
  fi
done


########################################################################
# Shut down everything when pacemaker signals from parent have ceased

declare now
declare Alive=true
while [[ $Alive == true ]]; do
  now=`date +%s`
  if [[ $LatestSignal -eq 0 ]]; then
    LatestSignal=$now
  elif [[ $((now - LatestSignal)) -le $Timeout ]]; then
    sleep $MonitoringPeriod
  else
    info "timeout; killing snmptrapd and exiting"
    kill $SnmptrapdPid
    Alive=false
  fi
done

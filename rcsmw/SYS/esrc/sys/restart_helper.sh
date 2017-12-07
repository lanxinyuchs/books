#! /bin/bash
##
## %CCaseFile:	restart_helper.sh %
## %CCaseRev:	/main/R2A/R3A/2 %
## %CCaseDate:	2015-02-12 %
## Author: Rabbe Fogelholm
##
## Purpose: Spawn new instances of start_rcs.sh, one at time. This
## script runs as a background process, started by an initial
## instance of start_rcs.sh.
##
## This script must run in the simulator environment only.
##
## Dependencies:
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
## R2A/1      2014-06-04 erarafo     Stand-alone version.
## R3A/2      2015-02-12 etxarnu     Removed cookie, added dev_patches to PATH
## ----------------------------------------------------------------------

# Specify CXCs needing a PATH refresh here. This applies to CXCs
# that may need a different path after an upgrade. Each entry
# shall have the format
#
#    representant@cxcdir
#
# where 'representant' is an executable file that is unique for
# the CXC, and cxcdir is the CXC directory name.
#

declare -r ModulesNeedingPathLookup="
  te@TRACE-UTILS2_CXC1735093_2 \
  colish@COLI2_CXC1734536_2 \
  start_rcs.sh@SYS2_CXC1733862_2
"


# appendLog string...
#
# Appends a timestamped message to the LogFile.

function appendLog() {
  printf "%s %s %s\n" "`date +'%Y-%m-%d %H-%M-%S'`" "`basename $0` ($$):" "$*" >>${LogFile}
}


# getPath prefix representant
#
# The 'prefix' file tree is searched for a unique file 'representant'.
# The directory in which the file was found is returned.
#

function getPath() {
  local -r prefix=$1; shift
  local -r representant=$1; shift

  if [[ `find -L $prefix -type f  -name $representant | wc -l` < 1 ]]; then
    appendLog "FATAL: could not find a PATH component for executable: $representant"
    echo noPathComponentFor-$representant
  elif [[ `find -L $prefix -type f  -name $representant | wc -l` > 1 ]]; then
    appendLog "FATAL: ambiguous PATH component for executable: $representant"
    echo ambiguousPathComponentFor-$representant
  else
    dirname `find -L $prefix -type f  -name $representant`
  fi
}


# deletePathComponent dirName path
#
# Returns a reduced path where components that include a
# directory that matches the given dirName are dropped.

function deletePathComponent() {
  local result=""
  for c in `echo $2 | tr ':' ' '`; do
    case "$c" in
      */$1/*)
	true;;
      *)
	if [[ -z "$result" ]]; then
	  result="$c"
	else
	  result+=":$c"
	fi
    esac
  done
  echo $result
}


# restart
#
# Spawns another start_rcs.sh instance.

function restart() {
  RestartCount=$((RestartCount + 1))
  appendLog "restart number: $RestartCount"

  local -r prefix=`echo $SysPrivPath | sed -e 's|/software/.*|/software|'`
  local representant
  local loadModule
  local newPath=$PATH
  for u in $ModulesNeedingPathLookup; do
    representant=`echo $u | (IFS='@' read x _ && echo $x)`
    loadModule=`echo $u | (IFS='@' read _ y && echo $y)`
    newPath="`getPath $prefix $representant`:`deletePathComponent $loadModule $newPath`"
  done
  export PATH=${RcsRoot}/home/$USER/dev_patches:$newPath

  appendLog "using modified PATH: $PATH"

  RESTART_HELPER_PID=$$ start_rcs.sh -s ${Sname}  &
  appendLog "spawned: start_rcs.sh -s ${Sname} "
}


declare -r SysPrivPath=$1; shift
declare -r Sname=$1; shift
declare -r LogFile=$1; shift
declare -r RcsRoot=$1; shift

declare RestartCount=0

trap restart USR1

appendLog "started; arguments -"
appendLog "    SysPrivPath: $SysPrivPath"
appendLog "          Sname: $Sname"
appendLog "        LogFile: $LogFile"
appendLog "        RcsRoot: $RcsRoot"


mkdir -p ${RcsRoot}/rcs/run
echo $$ >${RcsRoot}/rcs/run/restart_helper.pid

while true; do
  sleep 3
done

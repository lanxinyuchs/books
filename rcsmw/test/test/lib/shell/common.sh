#! /bin/bash
##
## %CCaseFile:	common.sh %
## %CCaseRev:	/main/R2A/R3A/R4A/R6A/5 %
## %CCaseDate:	2016-08-26 %
## Author: erarafo, rabbe.fogelholm@ericsson.com
##
## Purpose: Common functionality for scripts.
##
## Scripts depending on this file are:
##   nodetool
##   rcs_cliexec
##   cxs_wget
##   simple_upgrade.sh
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
## R2A/1      2014-08-07 erarafo     First version
## R2A/2      2014-08-14 erarafo     Added nodeScp and nodeSsh functions
## R3A/1      2014-11-10 erarafo     Added a function for port lookup
## R3A/2      2015-02-20 erarafo     Added the DevBranch variable
## R3A/3      2015-02-26 erarafo     Comments only
## R3A/4      2015-03-13 erarafo     Switch to 'R3C'
## R3A/5      2015-03-18 erarafo     Switch to 'R3D'
## R3A/6      2015-03-18 erarafo     Switch to 'R3E'
## R3A/7      2015-04-13 erarafo     Switch to 'R3F'
## R3A/8      2015-04-13 erarafo     DevBranch deduced from config spec
## R4A/1      2015-11-30 erarafo     Short timeout when cannot connect
## R6A/1      2016-06-30 erarafo     Occurring du types listed; VRCS support
## R6A/3      2016-08-23 erarafo     getAddress extended to handle VRCS
## R6A/4      2016-08-26 erarafo     getDuType adjusted
## R6A/5      2016-08-26 erarafo     DevBranch replaced by function
## ----------------------------------------------------------------------


declare -r common_LabConfig=/proj/webdocs/rbs-rde-wiki/root/data/Main/LabConfig.txt


# devBranch
#
# Returns the development branch string, e g R6U, as defined by the current
# view. If no view is set the returned string is 'undefined'.

function devBranch() {
  if ! command -v cleartool >/dev/null; then
    echo 'undefined'
  else
    local -r view="`cleartool pwv -short`"
    case "$view" in
      *NONE*)
        echo 'undefined';;
      *)
	local -r result=`cleartool catcs | sed -e '/^#[ \t]*LabelBase[ \t]/!d' -e 's|.*LabelBase[ \t]*||' -e 's|[ \t]*$||'`
	case "$result" in
	  ?*)
	    echo $result;;
	  *)
	    echo 'undefined'
	esac
    esac
  fi
}


# isNode NODE
#
# Returns true if the given node is useful with the getAddress
# function.

function isNode() {
  local -r node="$1"; shift
  local -r result=`getAddress "$node" 2>/dev/null`
  [[ -n "$result" ]]
}



# getAddress NODE
#
# Echoes the IPv4 address of a node. The lookup is based on a config
# file which is assumed to exist in a well-known place. In case the
# lookup fails a message is written to standard error and an empty
# string is echoed.
#
# Special case: if the given node is 'sim' then 'localhost' is echoed.

function getAddress() {
  local -r node="$1"; shift
  case "$node" in
  rcf*)
    local -r configFile=/proj/rcs-tmp/stps/$node/config/stp.cfg
    local -r result=`sed -e '/{erl_dist_ip,/!d' -e 's|.*, *"||' -e 's|".*||' $configFile`
    if [[ -z "$result" ]]; then
      echo "cannot resolve in $configFile: $node" >&2
    else
      echo $result
    fi;;
  sim)
    echo localhost;;
  *)
    local -r result=`cat $common_LabConfig \
    | sed \
        -e "/$node/!d" \
        -e "s:^[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[ \t]*::" \
        -e "s:[ |].*::"`
    if [[ `echo $result | wc --words` != 1 ]]; then
      if [[ `echo $result | wc --words` > 1 ]]; then
        echo "cannot resolve node uniquely in $common_LabConfig: $node" >&2
      else
        echo "cannot resolve in $common_LabConfig: $node" >&2
      fi
    else
      echo $result
    fi
  esac
}



# getDuType NODE
#
# Echoes the DU type of the given node. The lookup is based on a config
# file which is assumed to exist in a well-known place. In case the
# lookup fails a message is written to standard error and an empty
# string is echoed.
#
# Special case: if the given node is 'sim' then 'sim' is echoed.
#
# Known types are (2016-06-30):
#
#  cloud
#  dus
#  dus3101
#  dus3201
#  dus4101
#  dus42
#  dus5201
#  dus53
#  dus5301
#  idu5205
#  idu5209
#  tcu03
#  tcu0401
#
# To list the above, run this oneliner:
# sed -e 's,^|[^|]*|,,' -e 's,|.*,,' -e 's,[ \t]*,,g' -e '/RUS/d' /proj/webdocs/rbs-rde-wiki/root/data/Main/LabConfig.txt | sort -u

function getDuType() {
  local -r node="$1"; shift
  case "$node" in
  vrcs)
    echo vrcs;;
  rcf*)
    echo vrcs;;
  sim|SIM)
    echo sim;;
  *)
    local -r result=`cat $common_LabConfig \
    | sed \
        -e "/$node/!d" \
        -e "s:^[^|]*|[^|]*|[ \t]*::" \
        -e "s:[ \t]*|.*::"`
    if [[ `echo $result | wc --words` != 1 ]]; then
      if [[ `echo $result | wc --words` > 1 ]]; then
        echo "cannot resolve uniquely in $common_LabConfig: $node" >&2
      else
        echo "cannot resolve in $common_LabConfig: $node" >&2
      fi
    else
      echo $result
    fi
  esac
}


# nodeScp SCP_ARGS
#
# Performs an scp command targeting a node. The arguments
# are scanned for prefixes which are resolved to node IP
# addresses. For example,
#
#     nodeScp tcu070:/rcs/erlang/erlang.log.1 .
#
# It is trusted that the given arguments are non-null and
# do not contain embedded whitespace.

function nodeScp() {
    local scpArgs=""
    local scpArg
    local prefix
    local rest
    local success=true
    while [[ $# > 0 ]]; do
      scpArg=$1; shift
      case $scpArg in
      external@?*:?*)
        ipAddr=`echo $scpArg | sed -e 's|.*@||' -e 's|:.*||'`
        rest=`echo $scpArg | sed -e 's|.*:||'`
        scpArgs+=" $ipAddr:$rest";;
      vrcs@?*:?*)
        ipAddr=`echo $scpArg | sed -e 's|.*@||' -e 's|:.*||'`
        rest=`echo $scpArg | sed -e 's|.*:||'`
        scpArgs+=" $ipAddr:$rest";;
      ?*:?*)
        prefix=`echo $scpArg | sed -e 's|:.*||'`
        rest=`echo $scpArg | sed -e 's|.*:||'`
        if ! isNode $prefix; then
          echo "nodeSsh: cannot resolve in $common_LabConfig: $node" >&2
          success=false
          break
        else
          scpArgs+=" `getAddress $prefix`:$rest"
        fi;;
      *)
        scpArgs+=" $scpArg"
      esac
    done
    if [[ $success != true ]]; then
      return 1
    else
      local scpOptions="-o user=root"
      scpOptions+=" -o PubKeyAuthentication=no"
      scpOptions+=" -o StrictHostKeyChecking=no"
      scpOptions+=" -o UserKnownHostsFile=/dev/null"
      scpOptions+=" -o connecttimeout=3"
      cat <<EOF | expect -
spawn scp $scpOptions $scpArgs
expect "password:"
send "root\r"
expect eof
catch wait result
exit [lindex \$result 3]
EOF
      local status=$?
      echo "(scp status: $status)" >&2
      return $status
    fi
}


# nodeSsh vrcs ADDRESS COMMAND...
# nodeSsh NODE COMMAND...
#
# Execute a command on a node. For example,
#
#     nodeSsh tcu070 mkdir /rcs/swm/ug_patches

function nodeSsh() {
# printf "nodeSsh args: '%s' '%s' '%s' '%s' '%s'\n" "$1" "$2" "$3" "$4" "$5"
  local node=$1; shift
  if [[ $node == vrcs ]]; then
    local addr=$1; shift
  elif isNode $node; then
    local addr=`getAddress $node`
  else
    echo "nodeSsh: cannot resolve in $common_LabConfig: $node" >&2
    return 1
  fi
  local sshOptions="-2 -l root"
  sshOptions+=" -o PubKeyAuthentication=no"
  sshOptions+=" -o StrictHostKeyChecking=no"
  sshOptions+=" -o UserKnownHostsFile=/dev/null"
  cat <<EOF | expect -
spawn ssh $sshOptions $addr $*
expect "password:"
send "root\r"
expect eof
catch wait result
exit [lindex \$result 3]
EOF
  local status=$?
  echo "(ssh status: $status)" >&2
  return $status
}



# getPort PORTNAME NODETYPE [INSTDIR]
#
# Returns a port number for the given port name.
# The NODETYPE argument must be 'target' or 'sim'.
# The INSTDIR argument may be given if a non-default
# simulator installation directory is used.

function getPort() {
  local portName="$1"; shift
  local nodeType="$1"; shift
  if [[ $# -gt 0 ]]; then
    local instDir="$1"; shift
  else
    local instDir=/local/scratch/$USER; shift
  fi

  case "$nodeType" in
  target)
    case $portName in
      netconf)
        echo 2022;;
      cli)
        echo 2023;;
      coli)
        echo 4192;;
      sftp)
        echo 2024;;
      www)
        echo 8080
    esac;;
  sim)
    if [[ ! -d $instDir ]]; then
      echo "not a directory: $instDir" >&2
      echo 0
    else
      if [[ ! -r `echo $instDir/RCS_ROOT/home/$USER/releases/R*/port.conf` ]]; then
	echo "cannot read: $instDir/RCS_ROOT/home/$USER/releases/R*/port.conf" >&2
	echo 0
      else
	sed \
		-e "/{[ \t]*${portName}[ \t]*,/!d" \
		-e 's|.*,[ \t]*||' \
		-e 's|[ \t]*}.*||' \
		$instDir/RCS_ROOT/home/$USER/releases/R*/port.conf
      fi
    fi;;
  *)
    echo "unknown node type: $nodeType" >&2
    echo 0
  esac
}

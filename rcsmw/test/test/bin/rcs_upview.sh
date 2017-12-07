#! /bin/bash
##
## %CCaseFile:	rcs_upview.sh %
## %CCaseRev:	/main/R2A/R6A/1 %
## %CCaseDate:	2016-07-01 %
## Author: erarafo, rabbe.fogelholm@ericsson.com
##
## Purpose: Script for launching an HTTP server and an HTTP client
## for easy inspection of an installed RBS CS system.
##
## TODO:
##    A badmatch crash occurs if a load module uses a config file and
##    the file is not present in the file tree (of course this causes
##    errors in the Erlang log as well)
##
## Dependencies: The lab config page, and these files:
##   $RCT_TOP/test/lib/rcs-upview/ebin/*.beam
##   $RCT_TOP/test/lib/rcs-upview/bin/remote_launch_httpd.sh
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
## R2A/4      2013-12-19 erarafo     Documented dependencies
## R2A/5      2014-01-09 erarafo     Remedied SSH annoyances
## R2A/6      2014-01-22 erarafo     Added a TODO
## R6A/1      2016-07-01 erarafo     Eliminated use of /dev/shm
## ----------------------------------------------------------------------


declare -r BrowserDefault=konqueror

declare -r LabConfig=/proj/webdocs/rbs-rde-wiki/root/data/Main/LabConfig.txt

########################################################################

declare -r Script=rcs_upview.sh
declare -r ScriptDir=`dirname $0`

function help() {
  cat <<EOF
Usage is: $Script [OPTIONS]
Options are:
  -d NODE       execute on NODE (e g duw014 or 10.86.148.187)"
  -b BROWSER    specify HTTP browser (defaults to $BrowserDefault)
  -h            this help
  -n            HTTP daemon only; do not start a browser
  -v            verbose

When using rcs_upview.sh towards a node it must be arranged that the
browser does not try to use the corporate HTTP proxy
(www-proxy.ericsson.se:8080). For Konqueror the easiest way is to tick
"Connect to the Internet directly" in
Settings->ConfigureKonqueror->WebBrowsing->Proxy. Alternatively choose
"Use preset proxy environment variables" and click Setup and
AutoDetect.

To get rid of password prompts (three times repeated) it is convenient
to append your public key to the authorized_keys file on the node. See
"man ssh-keygen" on how to create a DSA key. Then do

[on the node]

mkdir -p ~/.ssh
chmod 700 ~/.ssh
ifconfig

[on your local host]

ssh -l root NodeIpAddress <~/.ssh/id_dsa.pub tee -a .ssh/authorized_keys

EOF
}

function die() {
  printf "$Script: FATAL: $1\n" >&2
  exit 1
}

function info() {
  printf "$Script: INFO: $1\n" >&2
}

function verbose() {
  if [[ "$1" == true ]]; then
    printf "$Script: INFO: $2\n" >&2
  fi
}

function warning() {
  printf "$Script: WARNING: $1\n" >&2
}


getAddress() {
  case "$1" in
  *.*.*.*)
    echo $1
  ;;
  *)
    cat $LabConfig \
    | sed \
        -e "/$1/!d" \
        -e "s:^[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[^|]*|[ \t]*::" \
        -e "s:[ |].*::"
  esac
}


########################################################################
# Execution begins here

declare OptionPatterns=""
OptionPatterns+="b:"
OptionPatterns+="d:"
OptionPatterns+="h"
OptionPatterns+="n"
OptionPatterns+="v"

declare Browser=$BrowserDefault
declare DigitalUnit=""
declare Verbose=false
while getopts $OptionPatterns OPT; do
  case "$OPT" in
    b)
      Browser="$OPTARG";;
    d)
      DigitalUnit="$OPTARG";;
    h)
      help
      exit;;
    n)
      Browser=NONE;;
    v)
      Verbose=true;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

if [[ $# -gt 0 ]]; then
  die "arguments not allowed; try -h for help"
fi


if [[ "$Browser" != NONE ]] && ! command -v "$Browser" >/dev/null; then
  die "cannot execute: $Browser"
fi


mkdir -p /dev/shm/$USER
declare -r HostsFile=/dev/shm/$USER/known-hosts-$$
cat /dev/null >$HostsFile
declare -r SecurityOptions="-o StrictHostKeyChecking=no -o UserKnownHostsFile=$HostsFile"

if [[ $Verbose == true ]]; then
  declare -r ScpOptions="-o user=root $SecurityOptions"
else
  declare -r ScpOptions="-q -o user=root $SecurityOptions"
fi


case "$DigitalUnit" in
  "")
    Arguments="
      -noshell \
      -pa $ScriptDir/../lib/rcs-upview/ebin \
      -run main start sim 0 "$Browser" $Verbose $USER \
      -run init stop"
    if [[ $Verbose == true ]]; then
      erl $Arguments
    else
      erl $Arguments >/dev/null
    fi

  ;;
  *)
    info "Viewing UPs on node: $DigitalUnit"

    Address=`getAddress $DigitalUnit`
    info "IP address: $Address"

    # Copy files to the node. It is trusted that /tmp exists
    # and is writable. Overwriting the files with the same
    # content may occur.
    scp $ScpOptions \
      $ScriptDir/../lib/rcs-upview/ebin/*.beam \
      $ScriptDir/../lib/rcs-upview/bin/remote_launch_httpd.sh \
      $Address:/tmp \
    || die "failed to copy files"
    info "copied beams to $Address"

    ssh \
      $SecurityOptions \
      -f \
      -l root \
      $Address \
      /tmp/remote_launch_httpd.sh $USER $Verbose \
    >/dev/null \
    || die "failed to execute script on node: $Address"
    info "launched http daemon on $Address"

    # wait for the port to be reported
    for x in . . .; do printf "." >&2; sleep 1; done; printf "\n" >&2

    # get hold of the port
    mkdir -p /dev/shm/$USER
    declare -r ScriptPid=$$
    verbose $Verbose "script PID: $ScriptPid"

    declare Count=$((0))
    while ! scp $ScpOptions $Address:/tmp/$USER/port.txt \
                            /dev/shm/$USER/port-$ScriptPid.txt; do
      Count=$((Count + 1))
      if [[ $Count -gt 10 ]]; then
        die "failed to pick up HTTPD port number"
      else
	info "waiting for port number, will retry"
	sleep 2
      fi
    done

    declare Port=`cat /dev/shm/$USER/port-$ScriptPid.txt`
    rm /dev/shm/$USER/port-$$.txt
    verbose $Verbose "HTTP port to be used: $Port"

    if [[ $Verbose == true ]]; then
      env no_proxy=$Address $Browser http://$Address:$Port/esi/requestHandler:page
    else
      env no_proxy=$Address $Browser http://$Address:$Port/esi/requestHandler:page >/dev/null
    fi
esac


rm -f $HostsFile

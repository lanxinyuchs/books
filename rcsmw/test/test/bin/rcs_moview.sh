#! /bin/bash
##
## %CCaseFile:	rcs_moview.sh %
## %CCaseRev:	/main/R2A/R3A/R9A/1 %
## %CCaseDate:	2017-01-30 %
## Author: erarafo
##
## Purpose: Offline analysis of the SwmInternal log. This script is DEPRECATED
## since the block of hex code in the SwmInternal log is no longer generated at
## upgrade time.
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
## Rev        Date       Name        What
## -----      -------    --------    ------------------------------------
## R2A/1      2014-06-17 erarafo     First version
## R2A/2      2014-06-17 erarafo     First bugfix
## R2A/3      2014-06-18 erarafo     Version 0.3
## R2A/4      2014-07-03 erarafo     Version 0.4
## R2A/5      2014-08-16 erarafo     Version 0.4, minor fault fixed
## R3A/1      2014-10-18 erarafo     Version 0.5, handles new log format
## R3A/2      2014-10-19 erarafo     Version 0.6, insertion order check
## R3A/3      2014-10-20 erarafo     Version 0.7, cleanup and robustness
## R3A/4      2014-10-22 erarafo     Version 0.7,
## R3A/4      2014-10-31 erarafo     Version 0.8, handle new log format (again)
## R3A/5      2015-02-10 erarafo     Version 0.8, minor bugfix
## R9A/1      2017-01-30 erarafo     Script is deprecated (long since)
## ----------------------------------------------------------------------
declare -r Version="0.8"

declare -r SimLog=/local/scratch/$USER/RCS_ROOT/rcs/log/SwmInternal/SwmInternal.1

declare -r ThisTool=$RCT_TOP/test/lib/rcs-moview


declare -r WinSizeMillisDefault=2000
declare -r NrespDefault=200

# Script name
declare -r Script=rcs_moview.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  cat <<EOF
Usage is: $Script [OPTIONS]

Options are:
  -d          debug
  -f FILE     SwmInternal log file to be analyzed
  -F          short for -f $SimLog
  -h          this help
  -o CLI      generated CLI commands (optional)
  -p PORT     HTTP port number (optional)

  -w WINSIZE  response rate limitation: window size millis (default $WinSizeMillisDefault)
  -r NRESP    limit on number of responses in window (default $NrespDefault)

If the -p option is not given a random port number will
be assigned.

NOTE: The SwmInternal log file must contain no more than
one block of hex strings. If there are several blocks,
as may be the case after several successive upgrades, the
file must be split in smaller pieces before using this
tool.

NOTE: The -o option is experimental. Careful inspection of
the generated commands is recommended.
EOF
}

# Print string to stderr and exit nonzero
function die() {
  printf "$Script: FATAL: $1\n" >&2
  exit 1
}

# Print string to stderr as info message
function info() {
  printf "$Script: INFO: $1\n" >&2
}

# Print string to stderr as warning
function warning() {
  printf "$Script: WARNING: $1\n" >&2
}



########################################################################
# Execution begins here

declare OptionPatterns=""
OptionPatterns+="d"
OptionPatterns+="F"
OptionPatterns+="f:"
OptionPatterns+="h"
OptionPatterns+="p:"
OptionPatterns+="o:"
OptionPatterns+="w:"
OptionPatterns+="r:"


declare SwmInternalLog=NoSuchFile
declare Port=0
declare Debug=false
declare CliScript="(none)"
declare WinSizeMillis=$WinSizeMillisDefault
declare Nresp=$NrespDefault
while getopts $OptionPatterns OPT; do
  case "$OPT" in
    d)
      Debug=true;;
    f)
      SwmInternalLog=$OPTARG;;
    F)
      SwmInternalLog=$SimLog;;
    h)
      help
      exit;;
    p)
      Port=$OPTARG;;
    o)
      CliScript=$OPTARG;;
    w)
      WinSizeMillis=$OPTARG;;
    r)
      Nresp=$OPTARG;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

if [[ $# -gt 0 ]]; then
  die "unknown arguments: $*"
fi

if [[ ! -r "$SwmInternalLog" ]]; then
  die "cannot read: $SwmInternalLog"
fi

if [[ $Debug == true ]]; then
  export MOVIEW_DEBUG=y
fi

export WINDOW_SIZE_MILLIS=$WinSizeMillis
export REQUESTS_IN_WINDOW=$Nresp

exec erl \
  -pa $ThisTool/ebin \
  -run main start \
    version=$Version \
    fqdn=`hostname --fqdn` \
    port=$Port \
    user=$USER \
    swmInternalLog=$SwmInternalLog \
    root=$ThisTool \
    cliScript="$CliScript"

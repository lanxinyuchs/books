#! /bin/bash
##
## %CCaseFile:	cxp_app_info.sh %
## %CCaseRev:	/main/R2A/1 %
## %CCaseDate:	2014-05-20 %
## Author: erarafo
##
## Purpose: Listing the CNX versions that a CXP was built from
## (typically the RBS CS CXP).
##
## Dependencies: cxp_edit, app_info.escript
##
## TODO: Allow the CXP to be defined by an URL as well
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014 All rights reserved.
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
## R2A/1      2014-05-20 erarafo     First version
## ----------------------------------------------------------------------


# Script name
declare -r Script=cxp_app_info.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  cat <<EOF >&2
CXP content is inspected, listing the CNX versions
that the CXP was built from.

Options are:
  -f CXP_FILE
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
OptionPatterns+="h"
OptionPatterns+="f:"

declare CxpFile=""
while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    f)
      CxpFile="$OPTARG";;
    *)
      die "Unknown option, try -h for help"
  esac
done


declare Recursive=false
shift $((OPTIND - 1))
if [[ $# -gt 0 ]]; then
  if [[ $1 == RECURSIVE ]]; then
    Recursive=true; shift
  fi
fi

if [[ $Recursive == false && -z "$CxpFile" ]]; then
  die "no CXP specified, try -h for help"
fi


case $Recursive in
  true)
    app_info.escript `find . -type f -name '*.app'`;;

  false)
    cxp_edit -r -f $CxpFile -e $ScriptDir/$Script RECURSIVE
esac

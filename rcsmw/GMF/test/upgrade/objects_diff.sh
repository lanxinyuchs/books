#! /bin/bash
##
## %CCaseFile:	objects_diff.sh %
## %CCaseRev:	/main/R3A/1 %
## %CCaseDate:	2015-03-06 %
## Author: <name>, <e-mail address>
##
## Purpose: Template for a script with options processing. REPLACE THIS TEXT
## WITH A MEANINGFUL DESCRIPTION.
##
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2015 All rights reserved.
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
##
## ----------------------------------------------------------------------


declare -r Script=objects_diff.sh


function die() {
  printf "$Script: FATAL: $1\n" >&2
  exit 1
}

function warning() {
  printf "$Script: WARNING: $1\n" >&2
}

function info() {
  printf "$Script: INFO: $1\n" >&2
}


########################################################################
# Execution begins here

declare OptionPatterns=""
OptionPatterns+="h"


function help() {
  cat <<EOF
Usage is: $Script REFERENCE NEW
Options are:
  -h          this help

The NEW file is compared to the reference file. Certain filtering
is applied. No listed differences means the files are equivalent.
EOF
}


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



if [[ $# -lt 2 ]]; then
  die "too few arguments, try -h for help"
fi

if [[ ! -f "$1" ]]; then
  die "cannot read: '$1'"
fi

if [[ ! -f "$2" ]]; then
  die "cannot read: '$2'"
fi


declare -r Temp=`mktemp /tmp/$USER-$Script-ref-filtered.XXXXXXXX`


sed \
  -e 's/=[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_//' \
  -e 's/{sshPort,\[[1-9][0-9]*]}/{sshPort,[____]}/' \
  -e 's/{netconfPort,\[[1-9][0-9]*]}/{netconfPort,[____]}/' \
  "$1" \
>$Temp

sed \
  -e 's/=[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_//' \
  -e 's/{sshPort,\[[1-9][0-9]*]}/{sshPort,[____]}/' \
  -e 's/{netconfPort,\[[1-9][0-9]*]}/{netconfPort,[____]}/' \
  "$2" \
| diff $Temp -

rm $Temp

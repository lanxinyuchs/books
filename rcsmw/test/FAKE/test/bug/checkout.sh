#! /bin/bash
##
## %CCaseFile:	checkout.sh %
## %CCaseRev:	/main/R6A/2 %
## %CCaseDate:	2016-05-24 %
## Author: <name>, <e-mail address>
##
## Purpose: Check out files before making model updates. Some of the files may
## not actually need to be updated, if so they can be uncheckedout before
## releasing.
##
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2016 All rights reserved.
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
## R6A/1      2016-05-17 erarafo     First version
## R6A/2      2016-05-24 erarafo     File list extended
## ----------------------------------------------------------------------


declare -r Files="
  test/bug/.project
  test/bug/bug.emx
  appdata/BUG_mim.xml
  appdata/BUG_imm.xml
  etc/BUG_mp.xml
  etc/BUG_immR3_classes.xml
  etc/BUGinstances_immR3_objects.xml
  csrc/noble_icti_dyn.c
"

# Script name
declare -r Script=checkout.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  cat <<EOF

Purpose is: Checking out files before opening the model in DX ECIM Toolchain.

Options are: -h for this help

REMEMBER: Upgrade must be handled in csrc/noble_icti_dyn.c if the IMM schema version was
stepped.
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

cd $RCT_TOP/FAKE/FAKE_CNX*/FAKE_CAX* || die "cannot cd to FAKE CAX directory"



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

help

for f in $Files; do
  if [[ ! -r $f ]]; then
    die "cannot read: $f"
  fi
done

cleartool checkout -unreserved $Files

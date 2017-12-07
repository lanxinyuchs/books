#! /bin/bash
##
## %CCaseFile:	checkout.sh %
## %CCaseRev:	/main/R2A/R3A/R4A/R5A/1 %
## %CCaseDate:	2016-04-05 %
## Author: <name>, <e-mail address>
##
## Purpose: Check out files before modeling work.
##
## Dependencies:
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
## R2A/2      2014-02-06 erarafo     Missing files added
## R2A/3      2014-02-06 erarafo     noble_icti.c added
## R2A/4      2014-02-07 erarafo     improved help
## R3A/2      2015-03-06 erarafo     Adjusted
## R4A/1      2015-06-15 erarafo     Extended
## R4A/2      2015-06-17 erarafo     Removal of the BIRD MOM
## R5A/1      2016-04-05 erarafo     Adjusted
## ----------------------------------------------------------------------


declare -r Files="
  Makefile
  appdata/noble_imm.xml
  appdata/noble_mim.xml
  etc/NOBLE_immR3_classes.xml
  etc/NOBLE_mp.xml
  etc/NOBLEinstances_immR3_objects.xml
  etc/ALKALI_immR3_classes.xml
  etc/ALKALI_mp.xml
  etc/ALKALIinstances_immR3_objects.xml
  test/notif/.project
  test/notif/checkout.sh
  test/notif/ECIM_Top.emx
  test/notif/notif.emx
  test/bird/ECIM_CommonLibrary.emx
  test/bird/ECIM_Top.emx
  test/bird/Bird.emx
  csrc/noble_icti.c
  csrc/noble_icti_dyn.c
"

#   etc/RcsHwIM_mp.xml
#   etc/RcsHwIM_immR3_classes.xml
#   etc/RcsHwIMinstances_immR3_objects.xml



# Script name
declare -r Script=checkout.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  cat <<EOF

Purpose is: Checking out files before opening the model in DX ECIM Toolchain.

Options are: -h for this help

A few of the checked-out files typically stay unchanged and must be uncheckedout
before checking in an updated model. They are:

  test/notif/ECIM_Top.emx test/notif/Output_Models/Notif/mp.dtd test/notif/ECIM_Top.emx

REMEMBER: Upgrade must be handled in noble_icti.c if some IMM schema version was
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


cd $RCT_TOP/FAKE/*CNX*/*CAX* || die "failed to cd to: $RCT_TOP/FAKE/*CNX*/*CAX*"


for f in $Files; do
  if [[ ! -r $f ]]; then
    die "cannot read: $f"
  fi
done

cleartool checkout -unreserved $Files

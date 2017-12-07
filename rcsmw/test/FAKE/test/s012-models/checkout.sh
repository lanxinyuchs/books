#! /bin/bash
##
## %CCaseFile:	checkout.sh %
## %CCaseRev:	/main/R3A/R4A/1 %
## %CCaseDate:	2015-04-27 %
## Author: <name>, <e-mail address>
##
## Purpose: Check out files before modeling work.
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
## R3A/1      2014-11-30 erarafo     First version
## R3A/2      2015-04-23 erarafo     Updates
## ----------------------------------------------------------------------


declare -r Files="
  test/s012-models/checkout.sh
  test/s012-models/ECIM_Top.emx
  test/s012-models/MomFrom.emx
  test/s012-models/MomTo.emx
  etc/DEMO_mp.xml
  etc/DEMO_imm_classes.xml
  etc/DEMOinstances_imm_objects.xml
  etc/S0_mp.xml
  etc/S0_imm_classes.xml
  etc/S0instances_imm_objects.xml
  etc/S1V0_mp.xml
  etc/S1V0_imm_classes.xml
  etc/S1V0instances_imm_objects.xml
  etc/S1V1_mp.xml
  etc/S1V1_imm_classes.xml
  etc/S1V1instances_imm_objects.xml
  etc/S1V2_mp.xml
  etc/S1V2_imm_classes.xml
  etc/S1V2instances_imm_objects.xml
  etc/S1V3_mp.xml
  etc/S1V3_imm_classes.xml
  etc/S1V3instances_imm_objects.xml
  etc/S1V0B_mp.xml
  etc/S1V0B_imm_classes.xml
  test/upgrade/mim_classes/DEMO_mp.xml
  test/upgrade/imm_classes/DEMO_imm_classes.xml
  test/upgrade/imm_objects/DEMOinstances_imm_objects.xml
  test/upgrade/mim_classes/S1V0_mp.xml
  test/upgrade/imm_classes/S1V0_imm_classes.xml
  test/upgrade/imm_objects/S1V0instances_imm_objects.xml
  test/upgrade/mim_classes/S1V1_mp.xml
  test/upgrade/imm_classes/S1V1_imm_classes.xml
  test/upgrade/imm_objects/S1V1instances_imm_objects.xml
  test/upgrade/mim_classes/S1V2_mp.xml
  test/upgrade/imm_classes/S1V2_imm_classes.xml
  test/upgrade/imm_objects/S1V2instances_imm_objects.xml
  test/upgrade/mim_classes/S1V3_mp.xml
  test/upgrade/imm_classes/S1V3_imm_classes.xml
  test/upgrade/imm_objects/S1V3instances_imm_objects.xml
  test/upgrade/mim_classes/S2_mp.xml
  test/upgrade/imm_classes/S2_imm_classes.xml
  test/upgrade/imm_objects/S2instances_imm_objects.xml
  test/upgrade/mim_classes/S2U_mp.xml
  test/upgrade/imm_classes/S2U_imm_classes.xml
  test/upgrade/imm_objects/S2Uinstances_imm_objects.xml
  test/upgrade/mim_classes/S1V0B_mp.xml
  test/upgrade/imm_classes/S1V0B_imm_classes.xml
"


# Script name
declare -r Script=checkout.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  cat <<EOF

Purpose is: Checking out files before opening the model in DX ECIM Toolchain.

Options are: -h for this help

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


cd $RCT_TOP/FAKE/*CNX*/*CAX* || die "failed to cd to: $RCT_TOP/FAKE/*CNX*/*CAX*"


for f in $Files; do
  if [[ ! -r $f ]]; then
    die "cannot read: $f"
  fi
done

cleartool checkout -unreserved $Files

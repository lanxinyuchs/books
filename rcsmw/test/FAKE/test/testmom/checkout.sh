#! /bin/bash
##
## %CCaseFile:	checkout.sh %
## %CCaseRev:	/main/R4A/R5A/1 %
## %CCaseDate:	2016-04-08 %
## Author: <name>, <e-mail address>
##
## Purpose: Check out files before making model updates. Some of the files may
## not actually need to be updated, if so they can be uncheckedout before
## releasing.
##
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
## R4A/1      2015-08-28 erarafo     First version
## R4A/2      2015-08-30 erarafo     Wrong filename corrected
## R4A/3      2015-08-30 erarafo     Do not check out this script itself
## R5A/1      2016-04-07 erarafo     Using TESTMOM_mp_extended.xml
## ----------------------------------------------------------------------





declare -r Files="
  appdata/TESTMOM_appdata_imm.xml
  appdata/TESTMOM_appdata_mim.xml
  csrc/noble_icti_dyn.c
  etc/CoreMW_Profile_Model.emx
  etc/ECIM_Base_Profile_Model.emx
  etc/TESTMOM_imm_classes.xml
  etc/TESTMOM_mp_extended.xml
  etc/testInstances5_imm_objects.xml
  test/testmom/dxet-project/.project
"




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

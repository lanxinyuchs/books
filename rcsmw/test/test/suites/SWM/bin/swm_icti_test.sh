#! /bin/bash
## 
## %CCaseFile:	swm_icti_test.sh %
## %CCaseRev:	/main/R2A/5 %
## %CCaseDate:	2013-08-15 %
## Author: <name>, <e-mail address>
## 
## Purpose: Test of upgrade that includes conversion
##  of MO instances. The steps are as follows:
## 
## Build the C application that handles conversion
## Build a From-version CXS
## Build a To-version CXS
## Compile patched SWM modules (if any)
## Compile patched GMF modules (if any)
## Start the From-version CXS (with patches if any)
## Insert patches for use by To-version (if any)
## Run upgradeprep.sh on the To-version CXS
## Transfer control to swm_icti_SUITE.erl
## 
## swm_icti_SUITE.erl is essentially the same as
## swm_SUITE.erl, with added checks of the To-version
## MO inventory.
## 
## Dependencies:
## 
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013 All rights reserved.
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
## R2A/2      2013-08-07 erarafo     Copyright added
## R2A/3      2013-08-08 erarafo     Patched modules passed on command line
## R2A/4      2013-08-14 erarafo     CXP and CXS created under /local/scratch
## R2A/5      2013-08-15 erarafo     Removed a TODO that was already fixed
## ----------------------------------------------------------------------



set -o nounset


########################################################################
# Set to false if created files need to be examined

declare -r CleanupAfterExecution=true


########################################################################
# Distinct version ids for building the From and To CXSes; these
# numbers are arbitrary

declare -r CxsVerFrom=R3A1
declare -r CxsVerTo=R3A2

declare -r App1CxpVerFrom=R5A11
declare -r App1CxpVerTo=R5A12

declare -r App1CxcVerFrom=R7A81
declare -r App1CxcVerTo=R7A82


declare -r TestSuitesSwmBin=`dirname $0`
declare -r TestSuitesSwm=`readlink -f $TestSuitesSwmBin/..`

declare -r GMF=$RCS_TOP/GMF/GMF_CNX9012719/GMF_CAX1033211
declare -r APPM=$RCS_TOP/APPM/APPM_CNX9012632/APPM_CAX1033073

declare -r RcsRoot=/local/scratch/$USER/RCS_ROOT

declare -r Out=/local/scratch/$USER-ugt/out

function help() {
 printf "Options are:\n"
 printf "      -h          this help\n"
 printf "      -x          RBS CS CXP From-version (e g R2A1474)\n"
 printf "      -y          RBS CS CXP To-version (e g R2A1475)\n"
 printf "      -p MODULE   specify a patched module (may be repeated)\n"
}

function die() {
  echo "FATAL: $*" >&2
  exit 1
}

function info() {
  echo "INFO: $*"
}


# Takes a module name and writes a product directory name
# to standard output

function productDir() {
	local -r module=$1
	case $module in
		appm*)
		 echo $APPM;;
		gmf*)
		 echo $GMF;;
		*)
		 die "cannot find product dir for module: $module"
	esac
}


function insertPatches() {
	local -r patchDir="$1"
	if [[ ! -d "$patchDir" ]]; then
		die "insertPatches: not a directory: '$patchDir'"
	fi
	
	for m in $Patches; do
		cp `productDir $m`/out/$m.beam $patchDir
		info "patched: $m"
	done
}


# Takes a list of modules and writes the product directories
# to standard output

function getProductDirs() {
	mkdir -p tmp
	local -r tmpFile=tmp/productDirs-$$
	for p in $Patches; do
		productDir $p >>$tmpFile
	done
	sort -u $tmpFile
	rm $tmpFile
}


function makePatches() {
	for d in `getProductDirs $Patches`; do
		(cd $d && echo "clearmake in: $d" && clearmake)
	done
}

function getFromRel() {
	echo $CxsVerFrom
}

function waitForHttp() {
  local -r sleepPeriod=5
local -r portConf=$RcsRoot/home/$USER/releases/`getFromRel`/port.conf
  local more=true
  local httpPort

  while [[ $more == true ]]; do  
    if [[ ! -r $portConf ]]; then
    echo "wait for port config ($portConf)"
      sleep $sleepPeriod
    else
      httpPort=`sed -e '/{www,/!d' -e 's|.*{www, *||' -e 's|}.*||' $portConf`
      if ! wget --quiet --output-document=- http://localhost:$httpPort/ >/dev/null; then
        echo wait for HTTP
        sleep $sleepPeriod
      else
        more=false
      fi
    fi
  done
}



function cleanup() {
  if [[ $1 == before || $CleanupAfterExecution == true ]]; then
    (cd $TestSuitesSwm/swm_icti \
     && rm -rf \
          $Out/cxp-app1-From \
          $Out/cxp-app1-To \
          $Out/cxp-cs-From \
          $Out/cxp-cs-To \
          $Out/cxs
    )
  fi
}

########################################################################
# Execution begins here

cleanup before

declare CsCxpVerFrom=""
declare CsCxpVerTo=""
declare Patches=""
while getopts 'hx:y:p:' OPT; do
  case "$OPT" in
    h)
     help
     exit;;
    x)
     CsCxpVerFrom=$OPTARG;;
    y)
     CsCxpVerTo=$OPTARG;;
    p)
     Patches+=" $OPTARG";;
    *)
     die "unknown option, -h for help"
  esac
done

shift $((OPTIND-1))


if [[ -z "$CsCxpVerFrom" ]]; then
	die "missing -x option, -h for help"
fi

if [[ -z "$CsCxpVerTo" ]]; then
	die "missing -y option, -h for help"
fi


########################################################################
# Change directory

cd $TestSuitesSwm/swm_icti


########################################################################
# Build CXSes

make WHEN=From \
  OUT=$Out \
  CXS_VER=$CxsVerFrom \
  CS_CXP_VER=$CsCxpVerFrom \
  APP1_CXP_VER=App1CxpVerFrom \
  APP1_CXC_VER=App1CxcVerFrom \
|| die "could not make the 'From' CXS"

make WHEN=To \
  OUT=$Out \
  CXS_VER=$CxsVerTo \
  CS_CXP_VER=$CsCxpVerTo \
  APP1_CXP_VER=App1CxpVerTo \
  APP1_CXC_VER=App1CxcVerTo \
|| die "could not make the 'To' CXS"




########################################################################
# Insert patches for From-version

declare PatchesOption=""
if [[ -n "$Patches" ]]; then
 makePatches  || die "failed to make patches"
 rm -rf patch
 mkdir patch
 insertPatches patch
 PatchesOption="-P `readlink -f patch`"
fi


########################################################################
# Start the 'From' instance

xterm -geometry 130x60 -e rcssim \
  -c \
  -p $Out/cxs/ugt-From.cxs \
  $PatchesOption \
  &
	
waitForHttp
	
	

########################################################################
# Insert patches for To-version

if [[ -n "$Patches" ]]; then
 mkdir -p $RcsRoot/rcs/swm/ug_patches
 insertPatches $RcsRoot/rcs/swm/ug_patches
fi


########################################################################
# Prepare for upgrade

upgradeprep.sh -sim ${USER}_sim $Out/cxs/ugt-To.cxs



########################################################################
# Change directory to where the test suites are

cd $TestSuitesSwm



########################################################################
# Upgrade and verify correct MOs after upgrade

rct_run.sh -sim ${USER}_sim -suite swm_icti_SUITE



########################################################################
# Clean up

cleanup after

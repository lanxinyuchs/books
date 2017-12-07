#! /bin/bash
##
## %CCaseFile:	simple_upgrade.sh %
## %CCaseRev:	/main/R2A/R3A/R4A/R6A/R8A/2 %
## %CCaseDate:	2017-01-03 %
## Author: erarafo
##
## Purpose: Perform a simple upgrade scenario. This script can be run
## as a sanity check whenever changes are made to the upgrade function.
##
## Only the simulator is supported for now.
##
## Dependencies: cxs_edit, cxp_edit,
## files under $RCT_TOP/FAKE/*CNX*/*CAX*/test/upgrade in the
## metadata, mim_classes, imm_classes and imm_objects directories
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
## R2A/1      2014-03-10 erarafo     Created.
## R2A/2      2014-03-10 erarafo     Bugfix
## R2A/3      2014-06-11 erarafo     Added -U option
## R2A/5      2014-08-16 erarafo     Fault fixed
## R3A/1      2014-10-17 erarafo     Improved version handling
## R3A/2      2014-10-31 erarafo     Option for absolute patches
## R3A/3      2014-11-05 erarafo     Optional removal of patch dir
## R3A/4      2014-11-13 erarafo     Execute on target supported
## R3A/5      2015-02-09 erarafo     Bugfix
## R3A/6      2015-02-26 erarafo     Using common.sh
## R4A/1      2015-06-22 erarafo     Improved resolution of module names
## R4A/2      2015-09-16 erarafo     CXS version string handling
## R6A/1      2016-05-18 erarafo     Support for struct version 2 testing
## R6A/2      2016-08-26 erarafo     Adapted to changes in common.sh
## R8A/1      2016-11-01 erarafo     Quick fix for .zip format in CI web
## R8A/2      2017-01-03 erarafo     Eliminated hard-coded path
## ----------------------------------------------------------------------

set -o nounset

source $RCT_TOP/test/lib/shell/common.sh

declare -r RstateDefault="`devBranch`"    # from common.sh

declare -r TargetDefault=sim
declare -r PatchDir=/repo/$USER/simple-upgrade-patch-$$

declare -r SimRoot=/local/scratch/$USER/RCS_ROOT

# Script name
declare -r Script=simple_upgrade.sh

# Script directory
declare -r ScriptDir=`dirname $0`

# Print string to stderr and exit nonzero
function die() {
  printf "$Script: FATAL: $1\n" >&2
  exit 1
}

# Print string to stderr as info message
function info() {
  printf "$Script: INFO: $1\n" >&2
}

# Print to stderr
function dryRun() {
  printf "dry run: $1\n" >&2
}



# Print string to stderr as warning
function warning() {
  printf "$Script: WARNING: $1\n" >&2
}

# cxsUrl target cxsVersion
#
# Returns a CXS URL for the given target and version.
#
function cxsUrl() {
  local target=$1; shift
  local version=$1; shift

  if [[ $CxsUrl != undefined ]]; then
    echo $CxsUrl
  else

    local qversion
    if echo $version | grep --silent 'R[1-9][0-9]*[A-Z][A-Z]*'; then
      qversion=$version
    else
      qversion=${RstateDefault}$version
    fi


    local prefix=https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1
    case $target in
      sim)
	# echo $prefix/RCP-SIM_CXS101549_2/doc/19010/RCP-SIM_CXS101549_2.cxs@@/CXS101549_2-$qversion;;
	echo $prefix/RCP-SIM_CXS101549_2/doc/19010/RCP-SIM_CXS101549_2.zip@@/CXS101549_2-$qversion;;
      ppc)
	echo $prefix/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs@@/CXS101549_1-$qversion;;
      arm)
	echo $prefix/RCP-ARM_CXS101549_3/doc/19010/RCP-ARM_CXS101549_3.cxs@@/CXS101549_3-$qversion;;
      tcu03)
        echo $prefix/RCP-TCU03_CXS101549_4/doc/19010/RCP-TCU03_CXS101549_4.cxs@@/CXS101549_4-$qversion;;
      dus52|dus2)
        echo $prefix/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.cxs@@/CXS101549_5-$qversion
    esac
  fi
}


# Wait for COM NETCONF access.
#
function waitForCom() {

  # wait for this long at least in case directory removal is in progress
  sleep 5

  local nPortConf
  local portConf=undefined
  while [[ $portConf == undefined ]]; do
    if [[ ! -d $SimRoot/home/$USER/releases ]]; then
      info "waiting for directory: $SimRoot/home/$USER/releases"
      sleep 2
    else
      nPortConf=`find $SimRoot/home/$USER/releases -type f -name port.conf | wc --lines`
      if [[ $nPortConf == 0 ]]; then
        info "waiting for file: port.conf"
        sleep 2
      elif [[ $nPortConf -gt 1 ]]; then
        warning "multiple files: port.conf"
        sleep 10
      else
        portConf=`find $SimRoot/home/$USER/releases -type f -name port.conf`
      fi
    fi
  done

  local netconfPort=0
  while [[ $netconfPort == 0 ]]; do
    if [[ -r $portConf ]]; then
      sleep 10
      netconfPort=`sed -e '/{netconf,/!d' -e 's|^.*,||' -e 's|}.*||' $portConf`
    else
      info "waiting for port config"
      sleep 2
    fi
  done
  info "found NETCONF port number: $netconfPort"

  local listening=""
  while true; do
    listening=`netstat -n --tcp --listening | grep $netconfPort`
    if [[ -n "$listening" ]]; then
      break
    else
      info "waiting for NETCONF service on port: '$netconfPort'"
      sleep 2
    fi
  done
  info "found NETCONF service"
}


# resolvePatch moduleName
#
# Echoes an absolute path for the given module name.

function resolvePatch() {
  local moduleName=$1; shift
  local prefix
  case $moduleName in
    /*)
      echo $moduleName;;
    comsa*|comFm|comSecM|comSnmp|comSysM|comTop)
      prefix=COMSA;;
    comte*)
      prefix=COMTE;;
    appm*)
      prefix=APPM;;
    cert*)
      prefix=CERT;;
    ecoli*)
      prefix=ECOLI;;
    itc*)
      prefix=EITC;;
    *)
      prefix=`echo $moduleName | sed -e 's|^\(...\).*|\1|' -e y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`
  esac
  if [[ ! -r `echo $RCS_TOP/$prefix/*CNX*/*CAX*/out/$moduleName.beam` ]]; then
    warning "cannot read: $RCS_TOP/$prefix/*CNX*/*CAX*/out/$moduleName.beam"
    echo unresolved
  else
    echo $RCS_TOP/$prefix/*CNX*/*CAX*/out/$moduleName.beam
  fi
}


# installFromUp cxsVersion patchDir patches...
#
# Starts a simulator with the given cxs version and patches
#
function installFromUp() {
  local cxsVersion=$1; shift
  local patchDir=$1; shift

  local patches="$*"
  local rcssimOpts;
  if [[ $CxsUrl != undefined ]]; then
    rcssimOpts="-c -p $CxsUrl"
  else
    rcssimOpts="-c -l CXS101549_2-$cxsVersion"
  fi

  if [[ $DryRun == true ]]; then
    if [[ -n "$patches" ]]; then
      dryRun "mkdir -p $patchDir"
      dryRun "cp $patches $patchDir"
      rcssimOpts+=" -P $patchDir"
    fi
    dryRun "rm -rf $SimRoot"
    dryRun "waitForCom"
    dryRun "rcssim $rcssimOpts"
    if [[ -n "$patches" ]]; then
      dryRun "mkdir -p $SimRoot/rcs/swm/ug_patches"
      dryRun "cp $patches $SimRoot/rcs/swm/ug_patches"
    fi
    if [[ $StructVersion2 == true ]]; then
      dryRun "mkdir -p $SimRoot/rcs/swm/ug_patches"
      dryRun "touch $SimRoot/rcs/swm/ug_patches/struct_ver_2"
      dryRun "create one MO instance with struct attributes"
    fi
  else
    if [[ -n "$patches" ]]; then
      mkdir -p $patchDir
      cp $patches $patchDir
      info "using patches: $patches"
      rcssimOpts+=" -P $patchDir"
    fi

    rm -rf $SimRoot
    info "about to execute in a new xterm: rcssim $rcssimOpts"
    xterm -e rcssim $rcssimOpts &

    waitForCom

    if [[ -n "$patches" ]]; then
      mkdir -p $SimRoot/rcs/swm/ug_patches
      cp $patches $SimRoot/rcs/swm/ug_patches
    fi
    if [[ $StructVersion2 == true ]]; then
      info "preparing for 'struct version 2' to be used"
      mkdir -p $SimRoot/rcs/swm/ug_patches
      touch $SimRoot/rcs/swm/ug_patches/struct_ver_2

      info "create one MO instance with struct attributes"
      rcs_exec -m cli -f - sim <<EOF
scriptmode --on
ManagedElement=1,TestRoot=1
configure
TestClassE=2
intC=34
structF,intA=44
structF,intSeqB=[44,55]
structF,strSeqC=["aa","bb"]
structG[@1],intC=45
structG[@1],strF="wef"
structG[@2],intC=46
structG[@2],strF="wefwef"
commit
exit
EOF
      info "create one MO instance with struct attributes - done"
    fi
  fi
}



# createToUp cxsVersion workDir
#
# Creates a To-UP of the given version in the given
# work directory. Any existing content in the directory
# is erased.
#
function createToUp() {
  local cxsVersion=$1; shift
  local workDir=$1; shift

  if [[ $DryRun == true ]]; then
    dryRun "mkdir -p $workDir"
    dryRun "(wipe created work directory)"
    dryRun "(in work directory) wget `cxsUrl $Target $cxsVersion`"
    cxsFileName=SomeCxsFileName
    dryRun "(in work directory) cxs_edit -f $cxsFileName -n DUMMY -e $ScriptDir/$Script replace"
    dryRun "(in work directory) mv $cxsFileName ${cxsFileName}E"
  else
    mkdir -p $workDir
    find $workDir -maxdepth 1 -mindepth 1 -exec rm -rf {} ';'
    cd $workDir

    if ! wget `cxsUrl $Target $cxsVersion`; then
      die "failed to download CXS"
    fi

    cxsFileName=`echo *`
    cxs_edit -f $cxsFileName -n DUMMY -e $ScriptDir/$Script replace
    mv $cxsFileName ${cxsFileName}E

    info "To-UP ready in: $workDir"
  fi
}


# upload workDir sftpSpace
#
# Uploads the CXS from the given working directory to
# the given SFTP space.
#
# The SFTP space name is the name of a directory in
# /proj/rcs-tmp/stps.
#
function uploadToUp() {
  local workDir=$1; shift

  if [[ `find $workDir -maxdepth 1 -mindepth 1 -type f | wc --lines` -ne 1 ]]; then
    die "missing or multiple file in work directory: $workDir"
  fi
  if [[ $Target == sim ]]; then
    upgradeprep.sh -sim ${USER}_sim `readlink -f $workDir/*`
  else
    upgradeprep.sh -stp $Node `readlink -f $workDir/*`
  fi
}


# upgrade TARGET
#
# Performs an upgrade using the To-Up uploaded to the
# SFTP space implied by the given target.
#
function upgrade() {
  local target=$1; shift

  cd $RCT_TOP/test/suites/SWM
  case $target in
    sim)
      rct_run.sh -sim ${USER}_sim -suite swm_SUITE;;
    *)
      rct_run.sh -stp $Node -suite swm_SUITE;;
  esac
}


function help() {
  cat <<EOF
Usage is: $Script

Options are:
  -T TARGET    any of sim, ppc, arm, tcu03, dus52 (defaults to sim)
  -R VERSION   Specify as 3456 for ${RstateDefault}3456, or R7B1234
  -U URL       CXS URL, overrides -R option
  -N NODE      Required unless TARGET is sim
  -P PATCHES   comma-separated list of patch module names
  -Q PATCHES   comma-separated list of patch objects (absolute paths allowed)
  -w WORKDIR   working directory, e g /repo/$USER/upgrade
  -r           remove auto-created patch directory when done
  -2           support for 'struct version 2' testing
  -i           install and run From-UP
  -c           create To-UP
  -u           upload To-UP
  -x           execute upgrade
  -n           dry run, no actions (work in progress, only -i and -c supported)
  -h           this help

Specifying the working directory is optional; a default is
provided. Note however that if the 'create' and 'upload'
actions are run in separate script invocations then the
same working directory must be used.

The -P option is effective only with the 'install' action.
The comma-separated list must not contain white space.
Absolute paths may be used and treated verbatim.

The 'install' action can be run in parallel with the
'create' plus 'upload' actions.

EOF
}



########################################################################
# Execution begins here

case "$RstateDefault" in
  undefined)
    die "could not determine the development branch; no view set?";;
  *)
    true
esac


declare OptionPatterns=""
OptionPatterns+="h"
OptionPatterns+="n"
OptionPatterns+="R:"
OptionPatterns+="U:"
OptionPatterns+="T:"
OptionPatterns+="N:"
OptionPatterns+="P:"
OptionPatterns+="Q:"
OptionPatterns+="w:"
OptionPatterns+="r"
OptionPatterns+="i"
OptionPatterns+="c"
OptionPatterns+="u"
OptionPatterns+="x"
OptionPatterns+="2"

declare Target=$TargetDefault
declare CxsVersion=undefined
declare CxsUrl=undefined
declare Node=undefined
declare Mode=top
declare WorkDirSpec=""
declare PatchesList=""
declare AbsPatchesList=""
declare Install=false
declare Create=false
declare Upload=false
declare Execute=false
declare RemovePatchDir=false
declare DryRun=false
declare StructVersion2=false
while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    n)
      DryRun=true;;
    R)
      CxsVersion=$OPTARG;;
    U)
      CxsUrl=$OPTARG;;
    N)
      Node=$OPTARG;;
    T)
      Target=$OPTARG;;
    P)
      PatchesList=$OPTARG;;
    Q)
      AbsPatchesList=$OPTARG;;
    w)
      WorkDirSpec=$OPTARG;;
    i)
      Install=true;;
    c)
      Create=true;;
    u)
      Upload=true;;
    x)
      Execute=true;;
    r)
      RemovePatchDir=false;;
    2)
      StructVersion2=true;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

if [[ $# -eq 1 ]]; then
  if [[ "$1" == replace ]]; then
    Mode=replace
  fi
fi

if [[ $DryRun == true && $Upload == true ]]; then
  die "cannot combine dry run and upload (please fix)"
fi

if [[ $DryRun == true && $Execute == true ]]; then
  die "cannot combine dry run and execute (please fix)"
fi


case $Mode in
  top)
    # This branch applies when started from the command line
    case "$Target" in
      sim)
	true;;
      ppc|arm|tcu03|dus52|dus2)
        if [[ $Install == true ]]; then
          die "install not yet supported to: $Target"
        else
          true
        fi;;
      *)
	die "unknown target, try -h for help"
    esac

    if [[ "$Target" != sim && $Node == undefined ]]; then
      die "target is $Target, NODE must be specified"
    fi

    if [[ $Install == true || $Create == true ]]; then
      if [[ "$CxsVersion" == undefined && "$CxsUrl" == undefined ]]; then
	die "undefined CXS version, try -h for help"
      fi
    fi

    case $CxsVersion in
    R[1-9]*)
      # branch string already present
      true;;
    *)
      # prepend with development branch string
      CxsVersion="${RstateDefault}${CxsVersion}"
    esac

    declare WorkDir
    if [[ -z "$WorkDirSpec" ]]; then
      WorkDir=/repo/$USER/simple-upgrade-work-$$
    else
      WorkDir=$WorkDirSpec
    fi

    declare AbsPatches=""

    for p in `echo $PatchesList | tr , '\n'`; do
      absPatch=`resolvePatch $p`
      if [[ "$absPatch" == unresolved ]]; then
        die "unresolved module: $p"
      else
        AbsPatches+=" $absPatch"
      fi
    done

    for p in `echo $AbsPatchesList | tr , ' '`; do
      AbsPatches+=" $p"
    done


    [[ $Create == true ]] && createToUp $CxsVersion $WorkDir

    [[ $Install == true ]] && installFromUp $CxsVersion $PatchDir $AbsPatches

    [[ $Upload == true ]] && uploadToUp $WorkDir

    [[ $Execute == true ]] && upgrade $Target

    if [[ $Install == true && -n "$AbsPatches" && $RemovePatchDir == true ]]; then
      info "removing patch directory: $PatchDir"
      rm -rf $PatchDir
    fi
  ;;
  replace)

    declare -r Source=$RCT_TOP/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade

    Sources=`find $Source/imm_classes $Source/imm_objects $Source/metadata $Source/mim_classes -type f`

    for f in $Sources; do
      b=`basename $f`
      g=`find . -type f -name $b`
      if [[ -z "$g" ]]; then
        case "$b" in
          *_imm_classes.xml)
            chmod +w FAKE*/fake-*/priv
            info "insert $b"
            cp $f FAKE*/fake-*/priv;;
          *_imm_objects.xml)
            chmod +w FAKE*/fake-*/priv
            info "insert $b"
            cp $f FAKE*/fake-*/priv;;
          *_mp.xml)
            chmod +w FAKE*/fake-*/priv
            info "insert $b"
            cp $f FAKE*/fake-*/priv;;
          *)
            warning "unresolved: $b ($f)"
        esac
      else
        chmod +w $g `dirname $g`
        info "replace $b: `dirname $g`"
        cp $f $g
      fi
    done;;
  *)
    die "unknown mode: '$Mode'"
esac

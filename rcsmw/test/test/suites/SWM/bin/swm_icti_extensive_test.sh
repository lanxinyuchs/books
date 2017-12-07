#! /bin/bash
##
## %CCaseFile:	swm_icti_extensive_test.sh %
## %CCaseRev:	/main/R2A/24 %
## %CCaseDate:	2013-10-28 %
## Author: <name>, <e-mail address>
##
## Purpose: Test of upgrade that includes conversion
##  of MO instances. The steps are as follows:
##
## Create MO model and instances in 'From' context
## Create MO model and instances in 'To' context
## Build the C applications that handle conversion
## Build a From-version CXS
## Build a To-version CXS
## Compile patched SYS modules (if any)
## Compile patched SWM modules (if any)
## Compile patched GMF modules (if any)
## Start the From-version CXS (with patches if any)
## Insert patches for use by To-version (if any)
## Run upgradeprep.sh on the To-version CXS
## Transfer control to swm_icti_SUITE.erl
## Verify expected conversions
##
## Dependencies:
##    /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/swm_icti/cxsMetaGen.sh
##    /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/swm_icti/cxpMetaGen.sh
##    /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/swm_icti/gmfMetaGen.sh
##    /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/swm_icti/appmMetaGen.sh
##    /vobs/rcs/tools/RDE_LXA119945/bin/upgradeprep.sh
##    swm_SUITE.erl
##    swm_verify_mo_instances_SUITE.erl
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
## R2A/4      2013-08-19 erarafo     Corrected immInfo for S1V1 schema
## R2A/5      2013-08-19 erarafo     Bugfix for -y only invocation
## R2A/6      2013-08-21 erarafo     Script returns exit status from rct_run.sh
## R2A/8      2013-08-22 erarafo     Multiple UE executables in CXC
## R2A/9      2013-08-23 erarafo     Target architecture as parameter
## R2A/10     2013-08-23 erarafo     Verifying MOs by a dedicated suite
## R2A/11     2013-08-26 erarafo     Definitive handling of i386 and i686
## R2A/12     2013-08-26 erarafo     Common functions in shared library
## R2A/13     2013-08-28 erarafo     Added -b option for patches as .beam files
## R2A/14     2013-08-28 erarafo     Reordering: Compile patches early on
## R2A/15     2013-08-29 erarafo     Added a DEMO MOM with 2 classes
## R2A/16     2013-09-09 erarafo     Added handling of patched SYS and SWM modules
## R2A/17     2013-09-11 erarafo     Added schema S2U (unnamed, To-version only).
## R2A/18     2013-09-12 erarafo     Added application ugt_s0
## R2A/19     2013-09-17 erarafo     Explaining the versioning syntax (comments)
## R2A/20     2013-09-18 erarafo     Validate patched module beam files
## R2A/21     2013-09-23 erarafo     Comments corrected
## R2A/22     2013-10-04 erarafo     Introduced a second CXP
## R2A/23     2013-10-21 erarafo     Removed fix of MIM files needed by DTD
## R2A/24     2013-10-28 erarafo     Uplift to ICTI IWD PB1
## ----------------------------------------------------------------------


set -o nounset


########################################################################
# Set to false if created files need to be examined

declare -r CleanupAfterExecution=false


########################################################################
# Schema versioning info for all involved schemas.
#
# The _Ver declarations set "version" and "release" of the schema,
# seprated by a '+'.
#
# The _Fvs declarations set "fromVersion" entries, separated by commas.
# Each entry is one integer (version only) or two integers separated by
# a period (version and release).
#
#
#               FROM                              TO
#               ====                              ==


declare -r DEMO_From_Ver=2+7+NONE;       declare -r DEMO_To_Ver=2+8+NONE
declare -r DEMO_From_Fvs=2,1,0;          declare -r DEMO_To_Fvs=2,1,0

declare -r S0_From_Ver=2+7+NONE
declare -r S0_From_Fvs=2,1

                                         declare -r S1V1_To_Ver=1+0+NONE
                                         declare -r S1V1_To_Fvs=0

declare -r S1V2_From_Ver=2+7+17;         declare -r S1V2_To_Ver=4+0+NONE
declare -r S1V2_From_Fvs=2,1;            declare -r S1V2_To_Fvs=4,3,2.7

declare -r S1V3_From_Ver=2+7+NONE;       declare -r S1V3_To_Ver=2+7+NONE
declare -r S1V3_From_Fvs=2,1;            declare -r S1V3_To_Fvs=2,1

                                         declare -r S2_To_Ver=1+0+NONE
                                         declare -r S2_To_Fvs=NONE


########################################################################
# CXS, CXP and CXC version numbers; not related to anything else.

declare -r     CxsVerFrom=R1A
declare -r       CxsVerTo=R1B

declare -r App1CxpVerFrom=R1A
declare -r   App1CxpVerTo=R1B

declare -r App1CxcVerFrom=R1A
declare -r   App1CxcVerTo=R1B

declare -r App2CxpVerFrom=R1A
declare -r   App2CxpVerTo=R1B

declare -r App2CxcVerFrom=R1A
declare -r   App2CxcVerTo=R1B


########################################################################
# Constants

declare -r TargetDefault=i386

declare -r Postfixes="s0 s1v1 s1v2 s2"

declare -r CsWeb=https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery
declare -r CsArch=$CsWeb/RCP_CSX10179_1/RCP-SIM_CXS101549_2/RCS-SIM_CXP9021221_2/doc/19010/RCS-SIM_CXP9021221_2.cxp@@

declare -r TestSuitesSwmBin=`dirname $0`
declare -r TestSuitesSwm=`readlink -f $TestSuitesSwmBin/..`

declare -r GMF=$RCS_TOP/GMF/GMF_CNX9012719/GMF_CAX1033211
declare -r APPM=$RCS_TOP/APPM/APPM_CNX9012632/APPM_CAX1033073
declare -r SYS=$RCS_TOP/SYS/*CNX*/*CAX*
declare -r SWM=$RCS_TOP/SWM/*CNX*/*CAX*

declare -r RcsRoot=/local/scratch/$USER/RCS_ROOT

declare -r Out=/local/scratch/$USER-ugt/out
declare -r PatchDir=patch

declare Date


function help() {
 printf "Options are:\n"
 printf "      -h          this help\n"
 printf "      -t ARCH     target architecture (i386, powerpc, armhf; defaults to $TargetDefault)\n"
 printf "      -x          RBS CS CXP From-version (e g R2A1474)\n"
 printf "      -y          RBS CS CXP To-version (optional, e g R2A1475)\n"
 printf "      -p MODULE   specify a patched module (may be repeated)\n"
 printf "      -b BEAMFILE specify a .beam file patch (may be repeated)\n"
 printf "\n"
 printf "If the -y option is omitted then no upgrade is attempted.\n"
 printf "If the -x option is omitted then the To-version is scratch-installed.\n"
}

function die() {
  echo "FATAL: $*" >&2
  exit 1
}

function warn() {
  echo "WARNING: $*" >&2
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
                sys*)
                 echo $SYS;;
                swm*)
                 echo $SWM;;
		*)
		 die "cannot find product dir for module: $module"
	esac
}


function insertPatches() {
	local -r patchDir="$1"; shift

	if [[ ! -d "$patchDir" ]]; then
		die "insertPatches: not a directory: '$patchDir'"
	fi

	local file
	for m in $*; do
	        beamFile=`productDir $m`/out/$m.beam
		cp $beamFile $patchDir || die "missing file: $beamFile"
		info "using patched module: $m"
	done
}


function copyBeams() {
	local -r patchDir="$1"; shift

	if [[ ! -d "$patchDir" ]]; then
		die "copyBeams: not a directory: '$patchDir'"
	fi

        for b in $*; do
	    if [[ ! -r $b ]]; then
		die "cannot read: $b"
            fi
	    cp $b $patchDir
	    info "using patched .beam file: $b"
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
  for d in `getProductDirs $*`; do
    (cd $d && echo "clearmake in: $d" && clearmake)
  done
}



# Takes the CXS version as argument

function waitForHttp() {
  local -r cxsVer=$1; shift

  local -r sleepPeriod=5
  local -r portConf=$RcsRoot/home/$USER/releases/$cxsVer/port.conf
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
    rm -rf \
      $Out/cxp-app1-From \
      $Out/cxp-app2-From \
      $Out/cxp-app1-To \
      $Out/cxp-app2-To \
      $Out/cxp-cs-From \
      $Out/cxp-cs-To \
      $Out/cxs
  fi
}

########################################################################
# Execution begins here

cleanup before

declare CsCxpVerFrom=""
declare CsCxpVerTo=""
declare Patches=""
declare Beams=""
declare Target=$TargetDefault
while getopts 'ht:x:y:p:b:' OPT; do
  case "$OPT" in
    h)
     help
     exit;;
    t)
     Target=$OPTARG;;
    x)
     CsCxpVerFrom=$OPTARG;;
    y)
     CsCxpVerTo=$OPTARG;;
    p)
     Patches+=" $OPTARG";;
    b)
     Beams+=" $OPTARG";;
    *)
     die "unknown option, -h for help"
  esac
done

shift $((OPTIND-1))


case $Target in
  i386)
    true;;
  i686)
    warn "deprecated: $Target; consider using i386 instead";;
  powerpc|armhf)
    die "target not yet supported: $Target";;
  *)
    die "unknown target type: $Target";;
esac


if [[ -z "$CsCxpVerFrom" && -z "$CsCxpVerTo" ]]; then
  die "no CS CXP versions specified, -h for help"
fi



if [[ -z "$CsCxpVerFrom" ]]; then
	info "launching To-version ($CsCxpVerTo) only, upgrade will not be performed"
fi

if [[ -z "$CsCxpVerTo" ]]; then
        info "launching From-version ($CsCxpVerFrom) only, upgrade will not be performed"
fi


if [[ -n "$Patches" ]]; then
    makePatches $Patches || die "failed to make patches"
fi



########################################################################
# Change directory

cd $TestSuitesSwm/swm_icti


########################################################################
# Build the upgrade engines
#


make POSTFIX=none out/tgt_i686/bin/upgrade_demo || die "failed to make demo upgrade engine"

for postfix in $Postfixes; do
  make POSTFIX=$postfix out/tgt_i686/bin/ugt_$postfix || die "failed to make upgrade engine: $postfix"
done





########################################################################
# Make scripts in this directory availablefor execution

export PATH=`pwd`:$PATH



########################################################################
# Build the 'From' CXS

if [[ -n "$CsCxpVerFrom" ]]; then

  Date=`date +%Y-%m-%dT%H:%M:%S`

  mkdir -p $Out/cxp-app1-From
  mkdir -p $Out/cxp-app2-From
  for wd in `pwd`; do
    (cd $Out/cxp-app1-From

     mkdir -p squashfs-root/files

     appmOptions=""
     lmCount=$((1))
     appmOptions+="-P local:upgrade_demo:LM00$lmCount:$Target:files/upgrade_demo"
     cp $wd/out/tgt_i686/bin/upgrade_demo               squashfs-root/files
     lmCount=$((lmCount + 1))

     for postfix in $Postfixes; do
       cp $wd/out/tgt_i686/bin/ugt_$postfix             squashfs-root/files
       appmOptions+=" -P local:ugt_$postfix:LM00$lmCount:$Target:files/ugt_$postfix"
       lmCount=$((lmCount + 1))
     done
     cp $wd/out/tgt_i686/bin/libugt_common.so           squashfs-root/files
     appmOptions+=" -P shared_lib:ugt_common:LM00$lmCount:$Target:files/libugt_common.so"
     appmMetaGen.sh $appmOptions | xmllint --format -  >squashfs-root/files/ugt-appm.xml

     cp $wd/MomFrom/DEMO_mp.xml                         squashfs-root/files

     cp $wd/MomFrom/S0_mp.xml                           squashfs-root/files
     cp $wd/MomFrom/S1V[0123]_mp.xml                    squashfs-root/files

     cp $wd/MomFrom/DEMO_imm_classes.xml                squashfs-root/files

     cp $wd/MomFrom/S0_imm_classes.xml                  squashfs-root/files
     cp $wd/MomFrom/S1V[0123]_imm_classes.xml           squashfs-root/files

     cp $wd/MomFrom/DEMOinstances_imm_objects.xml       squashfs-root/files

     cp $wd/MomFrom/S0instances_imm_objects.xml         squashfs-root/files
     cp $wd/MomFrom/S1V[0123]instances_imm_objects.xml  squashfs-root/files

     info "XML files copied"


     gmfMetaGen.sh \
       -T gmfMim \
       -X S0_mp.xml+files+imm \
       -X S1V0_mp.xml+files+imm \
       -X S1V1_mp.xml+files+imm \
       -X S1V2_mp.xml+files+imm \
       -X S1V3_mp.xml+files+imm \
       -X DEMO_mp.xml+files+imm \
       | xmllint --format - \
       >squashfs-root/files/gmfMim.xml

     gmfMetaGen.sh \
       -T gmfImm \
       -Z S0_imm_classes.xml+files+classes+s0+$S0_From_Ver+$S0_From_Fvs \
       -Y S1V0_imm_classes.xml+files+classes \
       -Y S1V1_imm_classes.xml+files+classes \
       -Z S1V2_imm_classes.xml+files+classes+s1v2+$S1V2_From_Ver+$S1V2_From_Fvs \
       -Z S1V3_imm_classes.xml+files+classes+s1v3+$S1V3_From_Ver+$S1V3_From_Fvs \
       -Z DEMO_imm_classes.xml+files+classes+demo+$DEMO_From_Ver+$DEMO_From_Fvs \
       -Y S0instances_imm_objects.xml+files+objects \
       -Y S1V0instances_imm_objects.xml+files+objects \
       -Y S1V1instances_imm_objects.xml+files+objects \
       -Y S1V2instances_imm_objects.xml+files+objects \
       -Y S1V3instances_imm_objects.xml+files+objects \
       -Y DEMOinstances_imm_objects.xml+files+objects \
       | xmllint --format - \
       >squashfs-root/files/gmfImm.xml

      info "MIM and IMM metadata created"


     mkdir -p tmp

     cxpMetaGen.sh \
       -N APP1-SIM \
       -I CXP001 \
       -V $App1CxpVerFrom \
       -D $Date \
       -X "UGT+CXC33333+$App1CxcVerFrom+files,ugt-appm.xml;files,gmfMim.xml;files,gmfImm.xml" \
     | xmllint --format - \
     >tmp/cxp-APP1-SIM.xml

     info "CXP metadata created"


     create_sig.sh \
	       -v \
	       -o APP1-SIM_CXP001.cxp \
	       -f tmp/cxp-APP1-SIM.xml \
	       squashfs-root/ \
     | sed -e 's| from : .*||'

     info "CXP created: $Out/cxp-app1-From/APP1-SIM_CXP001.cxp"


     cd $Out/cxp-app2-From

     mkdir -p squashfs-root/files

     cp $wd/MomFrom/S1V0B_mp.xml                        squashfs-root/files

     cp $wd/MomFrom/S1V0B_imm_classes.xml               squashfs-root/files

     info "XML files copied"


     gmfMetaGen.sh \
       -T gmfMim \
       -X S1V0B_mp.xml+files+imm \
       | xmllint --format - \
       >squashfs-root/files/gmfMim_S1V0B.xml

     gmfMetaGen.sh \
       -T gmfImm \
       -Y S1V0B_imm_classes.xml+files+classes \
       | xmllint --format - \
       >squashfs-root/files/gmfImm_S1V0B.xml

     info "MIM and IMM metadata created"


     mkdir -p tmp

     cxpMetaGen.sh \
       -N APP2-SIM \
       -I CXP002 \
       -V $App2CxpVerFrom \
       -D $Date \
       -X "UGT+CXC33333+$App2CxcVerFrom+files,gmfMim_S1V0B.xml;files,gmfImm_S1V0B.xml" \
     | xmllint --format - \
     >tmp/cxp-APP2-SIM.xml

     info "CXP metadata created"


     create_sig.sh \
	       -v \
	       -o APP2-SIM_CXP002.cxp \
	       -f tmp/cxp-APP2-SIM.xml \
	       squashfs-root/ \
     | sed -e 's| from : .*||'

     info "CXP created: $Out/cxp-app2-From/APP2-SIM_CXP002.cxp"
    )
  done


  mkdir -p $Out/cxp-cs-From
  wget \
    --quiet \
    --no-proxy \
    --output-document=$Out/cxp-cs-From/RCS-SIM_CXP9021221_2.cxp \
    $CsArch/CXP9021221_2-$CsCxpVerFrom
  info "downloaded: RCS-SIM_CXP9021221_2.cxp, version: $CsCxpVerFrom"

  mkdir -p $Out/cxs
  cxsMetaGen.sh \
    -N UGT-SIM \
    -I CXS000 \
    -V $CxsVerFrom \
    -D $Date \
    -X APP1-SIM+CXP001+$App1CxpVerFrom \
    -X APP2-SIM+CXP002+$App2CxpVerFrom \
    -X RCS-SIM+CXP9021221_2+$CsCxpVerFrom \
  | xmllint --format - \
  >$Out/cxs/ugt-From-up.xml

  info "CXS metadata created"


  tar -czf $Out/cxs/ugt-From.cxs \
    -C $Out/cxs ugt-From-up.xml \
    -C $Out/cxp-cs-From RCS-SIM_CXP9021221_2.cxp \
    -C $Out/cxp-app1-From APP1-SIM_CXP001.cxp \
    -C $Out/cxp-app2-From APP2-SIM_CXP002.cxp
  info "CXS created: $Out/cxs/ugt-From.cxs"

fi


########################################################################
# Build the 'To' CXS
#
# Consider doing this in parallel with some From activities!

if [[ -n "$CsCxpVerTo" ]]; then

  Date=`date +%Y-%m-%dT%H:%M:%S`

  mkdir -p $Out/cxp-app1-To
  mkdir -p $Out/cxp-app2-To
  for wd in `pwd`; do
    (cd $Out/cxp-app1-To

     mkdir -p squashfs-root/files

     appmOptions=""
     lmCount=$((1))
     appmOptions+="-P local:upgrade_demo:LM00$lmCount:$Target:files/upgrade_demo"
     cp $wd/out/tgt_i686/bin/upgrade_demo               squashfs-root/files
     lmCount=$((lmCount + 1))

     for postfix in $Postfixes; do
       cp $wd/out/tgt_i686/bin/ugt_$postfix             squashfs-root/files
       appmOptions+=" -P local:ugt_$postfix:LM00$lmCount:$Target:files/ugt_$postfix"
       lmCount=$((lmCount + 1))
     done
     cp $wd/out/tgt_i686/bin/libugt_common.so           squashfs-root/files
     appmOptions+=" -P shared_lib:ugt_common:LM00$lmCount:$Target:files/libugt_common.so"
     appmMetaGen.sh $appmOptions | xmllint --format -  >squashfs-root/files/ugt-appm.xml

     cp $wd/MomTo/DEMO_mp.xml                           squashfs-root/files

     cp $wd/MomTo/S1V[0123]_mp.xml                      squashfs-root/files
     cp $wd/MomTo/S2*_mp.xml                            squashfs-root/files

     cp $wd/MomTo/DEMO_imm_classes.xml                  squashfs-root/files

     cp $wd/MomTo/S1V[0123]_imm_classes.xml             squashfs-root/files
     cp $wd/MomTo/S2*_imm_classes.xml                   squashfs-root/files

     cp $wd/MomTo/DEMOinstances_imm_objects.xml         squashfs-root/files

     cp $wd/MomTo/S1V[0123]instances_imm_objects.xml    squashfs-root/files
     cp $wd/MomTo/S2*instances_imm_objects.xml          squashfs-root/files

     info "XML files copied"


     gmfMetaGen.sh \
       -T gmfMim \
       -X S1V0_mp.xml+files+imm \
       -X S1V1_mp.xml+files+imm \
       -X S1V2_mp.xml+files+imm \
       -X S1V3_mp.xml+files+imm \
       -X S2_mp.xml+files+imm \
       -X S2U_mp.xml+files+imm \
       -X DEMO_mp.xml+files+imm \
       | xmllint --format - \
       >squashfs-root/files/gmfMim.xml

     gmfMetaGen.sh \
       -T gmfImm \
       -Y S1V0_imm_classes.xml+files+classes \
       -Z S1V1_imm_classes.xml+files+classes+s1v1+$S1V1_To_Ver+$S1V1_To_Fvs \
       -Z S1V2_imm_classes.xml+files+classes+s1v2+$S1V2_To_Ver+$S1V2_To_Fvs \
       -Z S1V3_imm_classes.xml+files+classes+s1v3+$S1V3_To_Ver+$S1V3_To_Fvs \
       -Z S2_imm_classes.xml+files+classes+s2+$S2_To_Ver+$S2_To_Fvs \
       -Y S2U_imm_classes.xml+files+classes \
       -Z DEMO_imm_classes.xml+files+classes+demo+$DEMO_To_Ver+$DEMO_To_Fvs \
       -Y S1V0instances_imm_objects.xml+files+objects \
       -Y S1V1instances_imm_objects.xml+files+objects \
       -Y S1V2instances_imm_objects.xml+files+objects \
       -Y S1V3instances_imm_objects.xml+files+objects \
       -Y S2instances_imm_objects.xml+files+objects \
       -Y S2Uinstances_imm_objects.xml+files+objects \
       -Y DEMOinstances_imm_objects.xml+files+objects \
       | xmllint --format - \
       >squashfs-root/files/gmfImm.xml

     info "MIM and IMM metadata created"


     mkdir -p tmp

     cxpMetaGen.sh \
       -N APP1-SIM \
       -I CXP001 \
       -V $App1CxpVerTo \
       -D $Date \
       -X "UGT+CXC33333+$App1CxcVerTo+files,ugt-appm.xml;files,gmfMim.xml;files,gmfImm.xml" \
     | xmllint --format - \
     >tmp/cxp-APP1-SIM.xml

     info "CXP metadata created"


     create_sig.sh \
	       -v \
	       -o APP1-SIM_CXP001.cxp \
	       -f tmp/cxp-APP1-SIM.xml \
	       squashfs-root/ \
     | sed -e 's| from : .*||'

     info "CXP created: $Out/cxp-app1-To/APP1-SIM_CXP001.cxp"



     cd $Out/cxp-app2-To

     mkdir -p squashfs-root/files

     cp $wd/MomTo/S1V0B_mp.xml                          squashfs-root/files

     cp $wd/MomTo/S1V0B_imm_classes.xml                 squashfs-root/files

     info "XML files copied"


     gmfMetaGen.sh \
       -T gmfMim \
       -X S1V0B_mp.xml+files+imm \
       | xmllint --format - \
       >squashfs-root/files/gmfMim_S1V0B.xml

     gmfMetaGen.sh \
       -T gmfImm \
       -Y S1V0B_imm_classes.xml+files+classes \
       | xmllint --format - \
       >squashfs-root/files/gmfImm_S1V0B.xml

     info "MIM and IMM metadata created"


     mkdir -p tmp

     cxpMetaGen.sh \
       -N APP2-SIM \
       -I CXP002 \
       -V $App2CxpVerTo \
       -D $Date \
       -X "UGT+CXC33333+$App2CxcVerTo+files,gmfMim_S1V0B.xml;files,gmfImm_S1V0B.xml" \
     | xmllint --format - \
     >tmp/cxp-APP2-SIM.xml

     info "CXP metadata created"


     create_sig.sh \
	       -v \
	       -o APP2-SIM_CXP002.cxp \
	       -f tmp/cxp-APP2-SIM.xml \
	       squashfs-root/ \
     | sed -e 's| from : .*||'

     info "CXP created: $Out/cxp-app2-To/APP2-SIM_CXP002.cxp"
    )
  done


  mkdir -p $Out/cxp-cs-To
  wget \
    --quiet \
    --no-proxy \
    --output-document=$Out/cxp-cs-To/RCS-SIM_CXP9021221_2.cxp \
    $CsArch/CXP9021221_2-$CsCxpVerTo
  info "downloaded: RCS-SIM_CXP9021221_2.cxp, version: $CsCxpVerTo"

  mkdir -p $Out/cxs
  cxsMetaGen.sh \
    -N UGT-SIM \
    -I CXS000 \
    -V $CxsVerTo \
    -D $Date \
    -X APP1-SIM+CXP001+$App1CxpVerTo \
    -X APP2-SIM+CXP002+$App2CxpVerTo \
    -X RCS-SIM+CXP9021221_2+$CsCxpVerTo \
  | xmllint --format - \
  >$Out/cxs/ugt-To-up.xml

  info "CXS metadata created"


  tar -czf $Out/cxs/ugt-To.cxs \
    -C $Out/cxs ugt-To-up.xml \
    -C $Out/cxp-cs-To RCS-SIM_CXP9021221_2.cxp \
    -C $Out/cxp-app1-To APP1-SIM_CXP001.cxp \
    -C $Out/cxp-app2-To APP2-SIM_CXP002.cxp
  info "CXS created: $Out/cxs/ugt-To.cxs"

fi


########################################################################
# No patches is default

declare PatchesOption=""


########################################################################
# Possibly start the 'From' instance

if [[ -n "$CsCxpVerFrom" ]]; then

  if [[ -n "$Patches" || -n "$Beams" ]]; then
    rm -rf $PatchDir
    mkdir $PatchDir
    PatchesOption="-P `readlink -f $PatchDir`"
  fi

  if [[ -n "$Patches" ]]; then
    insertPatches $PatchDir $Patches
  fi

  if [[ -n "$Beams" ]]; then
    copyBeams $PatchDir $Beams
  fi


  xterm -geometry 130x60 -e rcssim \
    -c \
    -p $Out/cxs/ugt-From.cxs \
    $PatchesOption \
    &

  waitForHttp $CxsVerFrom

  if [[ -z "$CsCxpVerTo" ]]; then
    cleanup after
  fi
fi


########################################################################
# Possibly just start the 'To' instance

if [[ -z "$CsCxpVerFrom" && -n "$CsCxpVerTo" ]]; then

  if [[ -n "$Patches" || -n "$Beams" ]]; then
    rm -rf $PatchDir
    mkdir $PatchDir
    PatchesOption="-P `readlink -f $PatchDir`"
  fi

  if [[ -n "$Patches" ]]; then
    insertPatches $PatchDir $Patches
  fi

  if [[ -n "$Beams" ]]; then
    copyBeams $PatchDir $Beams
  fi


  xterm -geometry 130x60 -e rcssim \
    -c \
    -p $Out/cxs/ugt-To.cxs \
    $PatchesOption \
    &

  waitForHttp  $CxsVerTo

  cleanup after
fi


########################################################################
# Possibly perform upgrade

if [[ -n "$CsCxpVerFrom" && -n "$CsCxpVerTo" ]]; then

  if [[ -n "$Patches" || -n "$Beams" ]]; then
    mkdir -p $RcsRoot/rcs/swm/ug_patches
  fi

  if [[ -n "$Patches" ]]; then
    insertPatches $RcsRoot/rcs/swm/ug_patches $Patches
  fi

  if [[ -n "$Beams" ]]; then
    copyBeams $RcsRoot/rcs/swm/ug_patches $Beams
  fi

  upgradeprep.sh -sim ${USER}_sim $Out/cxs/ugt-To.cxs
  (cd $TestSuitesSwm
   rct_run.sh -sim ${USER}_sim -suite swm_SUITE
   declare -r ResultU=$?
   info "upgrade test suite exited with status: $ResultU"
   if [[ $ResultU -ne 0 ]]; then
     info "no cleanup made"
     exit $ResultU
   else
     rct_run.sh -sim ${USER}_sim -suite swm_verify_mo_instances_SUITE
     declare -r ResultV=$?
     info "verification status: $ResultV"
     if [[ $ResultV -ne 0 ]]; then
       info "no cleanup made"
     else
       cleanup after
     fi
     exit $ResultV
   fi
  )
fi

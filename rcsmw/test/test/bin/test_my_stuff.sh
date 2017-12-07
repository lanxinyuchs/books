#!/bin/bash
## ----------------------------------------------------------
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
#############################################################
##
##                NOTE!!!!!!!
##  The original file is stored in /vobs/rcs/test/RCT_CRX901275/test/bin 
##  Only do changes there and copy to /proj/rcs/bin/ directory.
##
#############################################################
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R2A/1      2013-03-21   etxjovp     Created
## R2A/9      2013-05-23   etxjovp     add support for node ci build
## R2A/10     2013-06-24   etxjovp     add /home/$USER/.test_my_stuff
## R2A/11     2013-09-20   etxjovp     add MODIFY_TYPE
## R2A/12     2013-10-03   etxjovp     add support for tcu
## R2A/13     2013-10-07   etxjovp     add support for du default
## R2A/14     2013-10-15   etxjovp     add support for manipulating the upgrade package
## R2A/15     2013-10-22   etxjovp     handle faulty installation
## R2A/19     2013-10-25   etxjovp     update function create_testspec
## R2A/27     2013-11-26   etxjovp     increase main timeout to 4h
## R2A/29     2013-12-03   etxjovp     remove rbs.. test cases
## R2A/30     2014-01-10   etxjovp     add --no-check-certificate
## R2A/31     2014-01-15   etxjovp     add support for dus5201
## R2A/32     2014-06-04   etxjovp     update function create_testspec
## R2A/33     2014-08-26   etxjovp     add support for tracks
## R3A/1      2015-06-04   etxjovp     bufg fix get LSV_BUILD_NUMBER
######################################################################################
trap 'lock_file' INT

function usage {

        echo -e ""
        echo -e ""
        echo -e "$bold##################################################################################################################$nobold"
        echo -e "${bold}#$nobold"
        echo -e "${bold}# $(basename $0) is a script that helps you run a test specification for a hardware configuration (max 3 hours)$nobold"
        echo -e "${bold}# Mail with a link to the test results sent to the user when the test is complete.$nobold"
        echo -e "${bold}# It is possible to download patches on the node.$nobold"
        echo -e "${bold}# Support for DC test at block level.$nobold"
        echo -e "${bold}# Short time booking of a node is also possible with the script.(max 3 hours)$nobold"
        echo -e "${bold}#$nobold"
        echo -e "$bold##################################################################################################################$nobold"
        echo -e "${bold}Usage:$nobold"
        echo -e "       $(basename $0) ${bold}-T${nobold} track ${bold}-t${nobold} testconfig ${bold}-s${nobold} testspecpath | ${bold}-i${nobold} testspecpath  [${bold}-u${nobold} user] [${bold}-n${nobold} buildnumber] [${bold}-c${nobold} configspec] [${bold}-f${nobold} upgradefrombuildnumber] [${bold}-r${nobold} repeat] [${bold}-p${nobold} patchdirpath] 
       $(basename $0) ${bold}-t${nobold} testconfig  ${bold}-b${nobold} minutes [${bold}-u${nobold} user]
       $(basename $0) ${bold}-t${nobold} testconfig  ${bold}-B${nobold} block [${bold}-n${nobold} buildnumber] [${bold}-c${nobold} configspec ] [${bold}-p${nobold} patchdirpath] 
" 
        echo -e "       ${bold}-u${nobold} testuser               Userid (default = $USER)
"  
        echo -e "       ${bold}-n${nobold} buildnumber            Jenkins ${bold}LSV_BUILD_NUMBER${nobold}  or ${bold}stable${nobold} (default = latest $(ls $CIRESULT/ | sort -n | grep '[0-9]' | tail -1))              
"  
        echo -e "       ${bold}-f${nobold} upgradefrombuildnumber Jenkins ${bold}LSV_BUILD_NUMBER${nobold}  or ${bold}stable${nobold} Build number you upgrade from             
" 
        echo -e "       ${bold}-T${nobold} track                  Track     R2 R3 ....          
" 
        echo -e "       ${bold}-i${nobold} testspec               Testspec path ${bold} NEW!! Interactive generation of test spec ${nobold}            
" 
        echo -e "       ${bold}-r${nobold} number                 Number of repetitions  (default = 1)         
" 
        echo -e "       ${bold}-t${nobold} testconfig             Hwconfig du/dus/duw/dus52/tcu/sim ....   (default = du)           
"   
        echo -e "       ${bold}-c${nobold} configspec             Configspec path (for advanced users! :-) or ${bold}latest${nobold}  (default It included in the LSV build)         
"           echo -e "       ${bold}-f${nobold} upgradefrombuildnumber Jenkins ${bold}LSV_BUILD_NUMBER${nobold}  or ${bold}stable${nobold} Build number you upgrade from             
" 
        echo -e "       ${bold}-b${nobold} minutes                Short time booking of a node (max 180 min) "
        echo -e "                                 You will receive an email when a node is available.          
"   
        echo -e "       ${bold}-p${nobold} patchdir              patchdir path          
"   
        echo -e "       ${bold}-m${nobold} dc or hc              Node CI build         ${bold} NOTE!! Can not be combined with upgrade ${nobold}  
"                       
        echo -e "       ${bold}-M${nobold} momconv               Scrip for manipulating the upgrade package  (upgrade package that manipulated defined by ${bold}-n${nobold} flag) 
"  
        echo -e "       ${bold}-B${nobold} block                 Block that you want to run a delivery check on ${bold}($(echo $(ls $RCS_TOP/ | head -10 )) ....)${nobold}
"  
        echo -e "       ${bold}-e${nobold} build number          Emergency stopping last run ${bold}Build number${nobold} can be found in the mail from test_my_stuff.                                                                
"                                          
        echo -e "${bold}Example:$nobold"
        echo -e "       $(basename $0)  ${bold}-T${nobold} R2 ${bold}-u${nobold} nisse ${bold}-n${nobold} 12076 ${bold}-s${nobold} /home/nisse/nisse.ts ${bold}-c${nobold} /home/nisse/nisse.cs  ${bold}-t${nobold} dus"
        echo -e "       $(basename $0)  ${bold}-T${nobold} R2 ${bold}-u${nobold} nisse ${bold}-n${nobold} 12076 ${bold}-s${nobold} /home/nisse/nisse.ts  ${bold}-t${nobold} dus"
        echo -e "       $(basename $0)  ${bold}-T${nobold} R2 ${bold}-u${nobold} nisse ${bold}-n${nobold} 12076 ${bold}-s${nobold} /home/nisse/nisse.ts  ${bold}-t${nobold} dus ${bold}-p${nobold} /home/nisse/nisse_patch_dir"
        echo -e "       $(basename $0) ${bold}-T${nobold} R2 ${bold}-s${nobold} /home/nisse/nisse.ts  ${bold}-c${nobold} latest  ${bold}-t${nobold} dus" 
        echo -e "       $(basename $0) ${bold}-T${nobold} R2  ${bold}-s${nobold} /home/nisse/nisse.ts  ${bold}-t${nobold} dus" 
        echo -e "       $(basename $0)  ${bold}-T${nobold} R2  ${bold}-B${nobold} ALH  ${bold}-t${nobold} du ${bold}-p${nobold} /home/nisse/nisse_patch_dir" 
        echo -e "       $(basename $0)  ${bold}-T${nobold} R2 ${bold}-s${nobold} /home/nisse/nisse.ts -M /home/nisse/nisse.sh" 
        echo -e "       $(basename $0) ${bold}-T${nobold} R2  ${bold}-b${nobold} 60  ${bold}-t${nobold} dus" 
       echo -e ""

}

######################################################################################
function lock_file {

if [ -f /home/$USER/.$KEY ]; then
rm -f /home/$USER/.$KEY*
exit 0
fi
}

######################################################################################
function create_testspec {

 
  rm -f $TMP_SPEC
  touch $TMP_SPEC

  CASESLIST1=$(cat $RCT_TOP/test/jenkins/testsuites_list_long | grep -v "#" | grep "${BLOCK}_" | cut -f5- -d\| )
  CASESLIST2=$(cat $RCT_TOP/test/jenkins/testsuites_list_qualification| grep -v "#" | grep "${BLOCK}_" | cut -f5- -d\| )

  CASESLIST="$CASESLIST1 $CASESLIST2" 
  echo $CASESLIST | sed 's/}./}.\n/g' | sort -u >> $TMP_SPEC

  echo "% Test spec for delivery check of :$BLOCK " > $SPEC
  echo "{merge_tests, false}." >> $SPEC
  echo "{define, 'Top', \"/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test\"}." >> $SPEC
  echo " {define, 'T1', \"'Top'/suites/install\"}. " >> $SPEC
  echo " {define, 'T2', \"'Top'/suites/install\"}. " >> $SPEC
  echo " {define, 'T3',  \"'Top'/suites/install\"}. " >> $SPEC
  echo " {define, 'T4', \"'Top'/suites/install\"}. " >> $SPEC
 


  echo " {cases, 'T2', basic_tests_SUITE, [ check_test_app, cli, coli, nc_get_config, nc_get, nc_getset_config, ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ] }." >> $SPEC
  cat $TMP_SPEC >> $SPEC 
  echo " {cases, 'T1', basic_tests_SUITE, [ power_cycle ] }." >> $SPEC
  echo " {cases, 'T2', basic_tests_SUITE, [ check_test_app, cli, coli, nc_get_config, nc_get, nc_getset_config, ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ] }." >> $SPEC

  cat $TMP_SPEC >> $SPEC 
  echo " {cases, 'T2', basic_tests_SUITE, [ reboot ]}." >> $SPEC
  echo " {cases, 'T3', basic_tests_SUITE, [ check_test_app, cli, coli, nc_get_config, nc_get,  nc_getset_config, ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ] }." >> $SPEC

  cat $TMP_SPEC >> $SPEC 
  echo " {cases, 'T3', basic_tests_SUITE, [ init_restart ]}." >> $SPEC
  echo " {cases, 'T4', basic_tests_SUITE, [ check_test_app, cli, coli, nc_get_config, nc_get,  nc_getset_config, ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ] }." >> $SPEC

  cat $TMP_SPEC >> $SPEC 

 
}
######################################################################################
function run_test_spec {
  SPEC=$NEWLOGDIR/dc.ts
  #$RCT_RUN -stp $NODE -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_3.${REPEAT}.xml\"}]"
  $RCT_RUN -stp $NODE -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   
  if [ "$?" = "0" ] && [ -f /home/$USER/.$KEY ]; then
   echo "passed" > /home/$USER/.$KEY
  elif [ "$?" != "0" ] && [ -f /home/$USER/.$KEY ]; then
    echo "error" > /home/$USER/.$KEY
  else
    echo "TIMEOUT !!!!!!!!!!!!!" 
  fi
}
######################################################################################
function stop {
CONFIG=$(cat /home/$USER/.test_my_stuff)
wget --quiet --no-proxy --no-check-certificate --output-document=/tmp/${USER}_$(basename $0)  https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/stop_test_my_stuff/buildWithParameters\?token=nisse\&NUMBER=${BUILD_NUMBER}\&CONFIG=${CONFIG}\&TESTUSER=${USER}
exit
}
######################################################################################

######################################################################################
RCT_RUN="$RCT_TOP/test/bin/rct_run.sh"
GET_SPEC_INT="$RCT_TOP/test/bin/get_spec_interactive.sh"
CIRESULT="/proj/webdocs/rbs-rde-ci/root/ciresults"
LOGDIR="/proj/rcs-tmp/stps"
SPECDIR="$RCT_TOP/test/testspec"
SUITEDIR="$RCT_TOP/test/suites"
LSV_BUILD_NUMBER=""
FROM_LSV_BUILD_NUMBER=""
MIN=""
PATCHDIR=""
TRACK=""
NODE_CI_BUILD=""
MODIFY_TYPE=""
AUTO_CONFIG_FILE=""
CONFIGSPEC="no"
CONFIG="du"
REPEAT="1"
bold='\033[1m'
nobold='\033[0m'
normal='\E[0m'

######################################################################################
#   MAIN
######################################################################################


while getopts u:n:f:s:r:t:T:c:b:B:p:m:M:e:i:ht: option
do
	case "$option"
	in
                u) USER=$OPTARG
                   ;;
		n) LSV_BUILD_NUMBER=$OPTARG
                   ;;
		f) FROM_LSV_BUILD_NUMBER=$OPTARG
                   ;;
		s) TESTSPEC=$OPTARG
                   ;;
		t) CONFIG=$OPTARG
                   ;;
                T) TRACK=$OPTARG
                   ;;
                r) REPEAT=$OPTARG
                   ;;
                c) CONFIGSPEC=$OPTARG
                   ;;
                b) MIN=$OPTARG
                   ;;
                B) BLOCK=$OPTARG
                   ;;
                p) PATCHDIR=$OPTARG
                   ;;
                m) NODE_CI_BUILD=$OPTARG
                   ;;
                e) BUILD_NUMBER=$OPTARG
                   stop
                   ;;
                M) MODIFY_TYPE=$OPTARG
                   ;;
                i) GEN_SPEC_NAME=$OPTARG  
                   ;;
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done
CIRESULT="/proj/webdocs/rbs-rde-ci/root/ciresults_branch/$TRACK"
if [ -f /proj/rcs-tmp/.test_my_stuff/log ]; then
echo "$(date)	$USER 	$0	$@" >> /proj/rcs-tmp/.test_my_stuff/log
fi
if [ "$#" = "0" ]; then
  usage
  exit
fi 
if [ "$TRACK" = "" ]; then
  usage
  exit
fi 
if [ "$MODIFY_TYPE" != "" ] ; then
 if [ ! -f $MODIFY_TYPE  ]; then
  echo -e "${bold}NOTE!! modify type $MODIFY_TYPE not exist ${nobold} "
  exit
 fi 
fi 
if [ "$PATCHDIR" != "" ] && [ ! -d $PATCHDIR ]; then
 echo -e "${bold}NOTE!! Patch dir is not exist ${nobold} "
  exit
fi 
if [ "$CONFIG" != "dus" ] && [ "$CONFIG" != "duw" ] && [ "$CONFIG" != "sim" ] && [ "$CONFIG" != "dus_long" ] && [ "$CONFIG" != "tcu" ] && [ "$CONFIG" != "du" ] && [ "$CONFIG" != "dus52" ]; then
  echo "CONFIG ${CONFIG} not exist "
  usage
  exit
fi 

touch /home/$USER/.test_my_stuff
chmod 666 /home/$USER/.test_my_stuff
if [ "$MIN" != "" ]; then
  
  wget --no-proxy --no-check-certificate --output-document=/tmp/${USER}_$(basename $0)  https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/book_${CONFIG}/buildWithParameters\?token=nisse\&TESTUSER=${USER}\&MIN=${MIN}
echo "
------------------------------------------------------------------------------
You are added to the queue for access to the ${CONFIG} for $MIN minutes.
An email will be sent to you when there is a ${CONFIG} available for you.
10 minutes before your reservation ends, 
you will receive an email asking you to quit.
------------------------------------------------------------------------------
"
  exit
fi

if [ ! -f $CONFIGSPEC ] && [ "$CONFIGSPEC" != "no" ]; then
  if [ "$CONFIGSPEC" != "latest" ]; then
     echo "CONFIGSPEC ${CONFIGSPEC} not exist "
     exit
  fi
fi 
if [ "$LSV_BUILD_NUMBER" = "stable" ]; then
   LSV_BUILD_NUMBER=$(ls -lrt $CIRESULT/*/lsv_status | tail -1 | cut -f8 -d/ )
fi
if [ "$LSV_BUILD_NUMBER" = "" ]; then
  LSV_BUILD_NUMBER=$(basename $(dirname $(egrep blue $CIRESULT/*/lsv_No_new | tail -1 | cut -f1 -d:)))
  #LSV_BUILD_NUMBER=$(ls $CIRESULT/ | sort -n | grep '[0-9]' | tail -1) 
fi

if [ ! -d ${CIRESULT}/${LSV_BUILD_NUMBER} ]; then
  echo "LSV_BUILD_NUMBER ${LSV_BUILD_NUMBER} not exist "
  exit
fi

if [ "${NODE_CI_BUILD}" != "" ] && [ ! -f ${CIRESULT}/${LSV_BUILD_NUMBER}/node_ci_${NODE_CI_BUILD}\_du_container ]  ; then
  echo "${NODE_CI_BUILD} build  not exist for ${LSV_BUILD_NUMBER}"
  exit 
fi
if [ "${PATCHDIR}" != "" ]; then
   if [ ! -d ${PATCHDIR} ]; then
     echo "PATCHDIR ${PATCHDIR} not exist "
     exit
   fi
fi


if [  -f ${MODIFY_TYPE} ] && [ "${MODIFY_TYPE}" != "" ]; then
  if [ "$(basename ${MODIFY_TYPE})" = "${MODIFY_TYPE}" ]; then
      MODIFY_TYPE=${PWD}/${MODIFY_TYPE}
  fi
  if [  -f $(pwd)/${MODIFY_TYPE} ]; then
     MODIFY_TYPE=$(pwd)/${MODIFY_TYPE}
  fi
fi


if [ "${GEN_SPEC_NAME}" != "" ]; then

  $GET_SPEC_INT -o ${GEN_SPEC_NAME}
  TESTSPEC=${GEN_SPEC_NAME}
fi
if [  -f ${TESTSPEC} ] && [ "${TESTSPEC}" != "" ]; then
  if [ "$(basename ${TESTSPEC})" = "${TESTSPEC}" ]; then
      TESTSPEC=${PWD}/${TESTSPEC}
  fi
  if [  -f $(pwd)/${TESTSPEC} ]; then
     TESTSPEC=$(pwd)/${TESTSPEC}
  fi
elif [ "${TESTSPEC}" = "" ] &&  [ "${BLOCK}" != "" ]; then

   echo " "
else
   echo "TESTSPEC ${TESTSPEC} not exist "
   exit
fi
if [ "$FROM_LSV_BUILD_NUMBER" = "stable" ]; then
   FROM_LSV_BUILD_NUMBER=$(ls -lrt $CIRESULT/*/lsv_status | tail -1 | cut -f7 -d/ )
fi 
if [ "$FROM_LSV_BUILD_NUMBER" != "" ]; then
  if [ "$LSV_BUILD_NUMBER" -le "$FROM_LSV_BUILD_NUMBER" ]; then
   echo "LSV_BUILD_NUMBER:$LSV_BUILD_NUMBER <= FROM_LSV_BUILD_NUMBER:$FROM_LSV_BUILD_NUMBER" 
   exit
  fi
else
   FROM_LSV_BUILD_NUMBER=$LSV_BUILD_NUMBER
fi
KEY=$(date +"%Y%m%d%H%M%S%N")
if [ "$BLOCK" = "" ]; then
echo "${CONFIG}" > /home/$USER/.test_my_stuff
cat /home/$USER/.test_my_stuff
wget --no-proxy --no-check-certificate --output-document=/tmp/${USER}_$(basename $0)  https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/test_my_stuff_${CONFIG}/buildWithParameters\?token=nisse\&LSV_BUILD_NUMBER=${LSV_BUILD_NUMBER}\&TESTSPEC=${TESTSPEC}\&TESTUSER=${USER}\&KEY=${KEY}\&REPEAT=${REPEAT}\&CONFIGSPEC=${CONFIGSPEC}\&FROM_LSV_BUILD_NUMBER=${FROM_LSV_BUILD_NUMBER}\&PATCHDIR=${PATCHDIR}\&NODE_CI_BUILD=${NODE_CI_BUILD}\&MODIFY_TYPE=${MODIFY_TYPE}

echo "Send to Jenkins test_my_stuff_${CONFIG}
---------------------------------------------------"

if [ "$LSV_BUILD_NUMBER" != "$FROM_LSV_BUILD_NUMBER" ]; then
  echo "FROM_LSV_BUILD_NUMBER	:${FROM_LSV_BUILD_NUMBER}"
fi
echo "LSV_BUILD_NUMBER	:${LSV_BUILD_NUMBER}"

echo "CONFIGSPEC		:${CONFIGSPEC}
TESTSPEC		:${TESTSPEC}
TESTUSER		:${USER}
REPEAT			:${REPEAT}
PATCHDIR		:${PATCHDIR}
NODE_CI_BUILD		:${NODE_CI_BUILD}
"
elif [  -d $RCS_TOP/$BLOCK ]; then
SPEC=/home/$USER/.$KEY.dc.ts
TMP_SPEC=/home/$USER/.$KEY.tmp_dc.ts
create_testspec
echo "The following test spec will run
---------------------------------------------------
"
cat /home/$USER/.$KEY.dc.ts
echo "
You have the option to cancel, do: control C within 10 seconds.
---------------------------------------------------"
sleep 10

echo "a b waiting for an available node" > /home/$USER/.$KEY
sync
sleep 1
V=$(touch /home/$USER/.$KEY;chmod 666 /home/$USER/.$KEY ;sleep 14400 ; rm -f /home/$USER/.$KEY*) &
echo "${CONFIG}" > /home/$USER/.test_my_stuff
cat /home/$USER/.test_my_stuff
#CONFIG="test"
wget  --quiet --no-proxy --no-check-certificate --output-document=/tmp/${USER}_$(basename $0)  https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/test_my_stuff_${CONFIG}/buildWithParameters\?token=nisse\&LSV_BUILD_NUMBER=${LSV_BUILD_NUMBER}\&TESTSPEC=no\&TESTUSER=${USER}\&KEY=${KEY}\&REPEAT=${REPEAT}\&CONFIGSPEC=${CONFIGSPEC}\&FROM_LSV_BUILD_NUMBER=${FROM_LSV_BUILD_NUMBER}\&PATCHDIR=${PATCHDIR}\&NODE_CI_BUILD=${NODE_CI_BUILD}\&MODIFY_TYPE=${MODIFY_TYPE}
INSTALL_STATE="waiting for an available node"
TMP_INSTALL_STATE=""

############################################################################################
while [ "$INSTALL_STATE" = "installation in progress" ] || [ "$INSTALL_STATE" = "waiting for an available node" ]
do
if [ "$TMP_INSTALL_STATE" != "$INSTALL_STATE" ]; then
echo  ""
echo  "$(date) $INSTALL_STATE  $NODE "
echo  "================================================================================"
echo  ""
TMP_INSTALL_STATE=$INSTALL_STATE
fi
echo -n "."
sleep 10
if [ -f /home/$USER/.$KEY ]; then
INSTALL_STATE=$(cat /home/$USER/.$KEY  | cut -f3- -d" ")
NODE=$(cat /home/$USER/.$KEY  | cut -f1 -d" ")
REV=$(cat /home/$USER/.$KEY  | cut -f2 -d" ")
else
INSTALL_STATE="timeout"

fi
#echo ">$INSTALL_STATE<"
done
if [ "$INSTALL_STATE" != "installation is complete" ]; then
  echo "Install failed exit !!!!!"
  exit 1
fi

############################################################################################




echo  ""
echo  "$(date) $INSTALL_STATE  $NODE"
echo  "==============================================================================="
echo  ""

NEWLOGDIR=$LOGDIR/$NODE/$REV/TESTUSER/$USER/no
#create_testspec
mv $SPEC $NEWLOGDIR/dc.ts

echo "running" > /home/$USER/.$KEY
sync
sleep 1
run_test_spec
sleep 40
############################################################################################
while [ -f /home/$USER/.$KEY ] && [ "$INSTALL_STATE" != "ok" ]
do
INSTALL_STATE=$(cat /home/$USER/.$KEY )
sleep 10
done
############################################################################################

else
 echo "$RCS_TOP/$BLOCK not exist!!!"

fi
lock_file

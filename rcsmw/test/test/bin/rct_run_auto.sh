#!/bin/bash
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2012 - 2014 All rights reserved.
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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2012-12-01   etxjovp     Created
## R1A/2      2012-12-19   etxjovp     Add handling of exit code
## R1A/8      2013-01-14   etxjovp     Add flag -f testconfig
## R2A/17     2013-03-19   etxjovp     Changed the handling of the exit code
## R2A/26     2013-04-09   etxjovp     Add to using upgradeprep.sh
## R2A/32     2013-04-23   etxjovp     Add support for AUTOCONFIGFILE
## R2A/34     2013-05-27   etxjovp     Commented out the check to not install
## R2A/35     2013-09-20   etxjovp     Add function for manipulating the upgrade package
## R2A/38     2013-10-20   etxjovp     Add support for no testspec
## R2A/47     2013-10-29   etxjovp     Update create_log_dir_user()
## R2A/50     2013-12-11   etxjovp     Add rm -f /vobs/rcs/dev/RCP_CSX10179/OS_CRX901265/TESTBOX/TESTBOX_CXA1105670/rct/suites/*beam
## R2A/51     2014-03-28   etxjovp     Add support for the cover tool
## R2A/57     2014-03-28   etxjovp     bug fix!!
## R2A/58     2014-03-31   etxjovp     Add -pa /proj/rcs/rct_dev_patches/CT for cover
## R2A/61     2014-04-03   etxjovp     Add INSTALL_SIM_SPEC
## R2A/62     2014-04-03   etxjovp     bux fix
## R2A/63     2014-06-10   etxjovp     Change the handling of patches
## R2A/64     2014-07-22   etxarnu     Create and write protect tftpboot dir if not exist
## R2A/69     2014-09-30   etxjovp     Add support for UPGRADE_FROM_URL
## ----------------------------------------------------------

######################################################################################
usage()
{
echo -e ""
echo -e "$bold================================================================================================$nobold" 
 echo -e "${bold}The script install or upgrade an STP and then run the tests included in the test specification.$nobold"
 echo -e "${bold}Test logs are placed in the directory $LOGDIR/<STP>/<CXS_FOR_TEST>/(basename <SPEC>)/. $nobold"
 echo -e "${bold}Results files report_<n>.xml created for presentation of results in Jenkins.$nobold"  
 echo -e "$bold================================================================================================$nobold"

echo -e ""
echo -e "${bold}Usage:$nobold $0 ${bold}-s$nobold STP ${bold}-t$nobold SPEC ${bold}-c$nobold BUILD_CXS ${bold}-c$nobold CXS_FOR_TEST [${bold}-g$nobold URL] [${bold}-f$nobold TESTCONFIG] [${bold}-r$nobold REPEAT ${bold}-u$nobold CXS_UPGRADE_FROM] [${bold}-a$nobold AUTOCONFIGFILE]"
echo -e ""
echo -e "	${bold}-s$nobold STP               STP name "
echo -e "	${bold}-t$nobold SPEC              Test specification"
echo -e "	${bold}-f$nobold TESTCONFIG        Test config"
echo -e "	${bold}-r$nobold REPEAT            Repeat the test specification REPEAT of times"
echo -e "	${bold}-b$nobold BUILD_CXS         Build label. "
echo -e "	${bold}-c$nobold CXS_FOR_TEST      CXS_FOR_TEST version as the test specification will run on. "
echo -e "	${bold}-g$nobold URL               If URL is given, UP is downloaded from URL, ex node CI. "
echo -e "	${bold}-G$nobold UPGRADE_FROM_URL  UPGRADE_FROM_URL is version you are upgrading from"
echo -e "	${bold}-a$nobold AUTOCONFIGFILE    CT config file. "
echo -e "	${bold}-p$nobold PATCHDIR           "
echo -e "	${bold}-m$nobold MODIFY TYPE        modify  script         "
echo -e "	${bold}-C$nobold COVERSPEC          cover spec         "
echo -e "	${bold}-u$nobold CXS_UPGRADE_FROM  If you want an upgraded system for testing, CXS_UPGRADE_FROM is"
echo -e "	                     version you are upgrading from."
echo -e ""
echo -e "${bold}Example:$nobold $0 ${bold}-s$nobold duw005 ${bold}-t$nobold /home/nisse/test.ts ${bold}-b$nobold CXS101553-R2A392  ${bold}-c$nobold  CXS101549_1-R2A281"
echo -e "${bold}Example:$nobold $0 ${bold}-s$nobold duw005 ${bold}-t$nobold /home/nisse/test.ts ${bold}-b$nobold CXS101553-R2A392  ${bold}-c$nobold  CXS101549_1-R2A281 -g https://rbs-g2-ci.rnd.ki.sw.ericsson.se:443/job/DC-BuildUP/159//artifact/DC-CXP9021221_1.tgz"
echo -e "${bold}Example:$nobold $0 ${bold}-s$nobold duw005 ${bold}-t$nobold /home/nisse/test.ts ${bold}-b$nobold CXS101553-R2A392  ${bold}-c$nobold  CXS101549_1-R2A281 ${bold}-u$nobold CXS101549_1-R2A260"
echo -e ""




}
######################################################################################
create_log_dir()
{
   REV=$BUILD_CXS
   mkdir -p $LOGDIR
   mkdir -p $LOGDIR/$NODE
   mkdir -p $LOGDIR/$NODE/$REV
   SPECBASENAME=$(basename $SPEC)
   mkdir -p $LOGDIR/$NODE/$REV/$SPECBASENAME
if [ "$TESTCONFIG" = "" ]; then
   NEWLOGDIR=$LOGDIR/$NODE/$REV/$SPECBASENAME
else
   mkdir -p $LOGDIR/$NODE/$REV/$SPECBASENAME/$TESTCONFIG
   NEWLOGDIR=$LOGDIR/$NODE/$REV/$SPECBASENAME/$TESTCONFIG
fi
}
create_log_dir_user()
{
   REV=$BUILD_CXS
   mkdir -p $LOGDIR
   mkdir -p $LOGDIR/$NODE
   mkdir -p $LOGDIR/$NODE/$REV
   mkdir -p $LOGDIR/$NODE/$REV/TESTUSER
   mkdir -p $LOGDIR/$NODE/$REV/TESTUSER/$TESTUSER
   SPECBASENAME=$(basename $SPEC)
   mkdir -p $LOGDIR/$NODE/$REV/TESTUSER/$TESTUSER/$SPECBASENAME
   if [ "$TESTCONFIG" = "" ]; then
     NEWLOGDIR=$LOGDIR/$NODE/$REV/TESTUSER/$TESTUSER/$SPECBASENAME
   else
     mkdir -p $LOGDIR/$NODE/$REV/TESTUSER/$TESTUSER/$SPECBASENAME/$TESTCONFIG
     NEWLOGDIR=$LOGDIR/$NODE/$REV/TESTUSER/$TESTUSER/$SPECBASENAME/$TESTCONFIG
   fi
}
check_if_exit()
{
  LINE=$(grep -n install_dus_SUITE $TEST_SPEC_INDEX_FILE | cut -f1 -d:)
  OK_LINE=$(expr $LINE + 3 )
  if [ "$(head -${OK_LINE} $TEST_SPEC_INDEX_FILE | tail -1 | cut -f2 -d\> | cut -f1 -d\<)" = "2" ]; then
    echo " It's ok to continue driving even though it has gone wrong! "
  else
    exit 1
  fi
  
}
######################################################################################
BOOKING_DIR="/proj/webdocs/rbs-rde/etc/host-book"
RCT_RUN="$RCT_TOP/test/bin/rct_run.sh"
RCS_SCP="$RCT_TOP/test/bin/rcs_scp.exp"
RCS_SSH="$RCT_TOP/test/bin/rcs_ssh.exp"
RCS_UPMOD4UPGRADE="$RCT_TOP/test/bin/rcs_upmod4upgrade.sh"
RCT_PREP="rcstprep.sh"
RCT_UPGRADE_PREP="upgradeprep.sh"
LOGDIR="/proj/rcs-tmp/stps"
TFTPBOOTDIR="/proj/rcs-tmp/tftpboot"
SPECSDIR="$RCT_TOP/test/testspecs"
INSTALL_SPEC="$SPECSDIR/install"
INSTALL_SIM_SPEC="$SPECSDIR/install_sim"
CHECK_AFTER_INSTALL_SPEC="$SPECSDIR/check_after_install"
INSTALL_BEFORE_UPGRADE_SPEC="$SPECSDIR/install_before_upgrade"
UPGRADE_SPEC="$SPECSDIR/upgrade"
UPGRADE_SPEC_SIM="$SPECSDIR/upgrade_sim"
BASIC_SUITE="$RCT_TOP/test/suites/install/basic_tests_SUITE.erl"
AUTO_CONFIG_FILE=""
UPGRADE_FROM_CXS=""
REPEAT=1 
TESTUSER=""
TESTCONFIG=""
PATCHDIR=""
COVERSPEC=""
URL=""
MODIFY_TYPE=""
INSTALL="yes"
bold='\033[1m'
nobold='\033[0m'



######################################################################################
#   MAIN
######################################################################################
while getopts s:t:f:c:b:u:r:e:p:g:a:m:C:G: option
do
	case "$option"
	in
                s) NODE=$OPTARG
                   ;;
                t) SPEC=$OPTARG
                   ;;
                f) TESTCONFIG=$OPTARG
                   ;;
                b) BUILD_CXS=$OPTARG
                   ;;
                c) CXS=$OPTARG
                   ;;
                u) UPGRADE_FROM_CXS=$OPTARG
                   ;;
                r) REPEAT=$OPTARG
                   ;;
                e) TESTUSER=$OPTARG
                   ;;
                p) PATCHDIR=$OPTARG
                   ;;
                g) URL=$OPTARG
                   ;;
                G) UPGRADE_FROM_URL=$OPTARG
                   ;;
                a) AUTO_CONFIG_FILE=$OPTARG
                   ;;
                m) MODIFY_TYPE=$OPTARG
                   ;;
                C) COVERSPEC=$OPTARG
                   ;;
		\?) usage
		   exit 1;; 
	esac
done 
######################################################################################
if [ "$NODE" = "" ] || [ "$SPEC" = "" ]; then
  usage
  exit
fi
if [ "$TESTUSER" != "" ]; then
  create_log_dir_user
else
  create_log_dir
fi
if [ ! -d $LOGDIR/$NODE ]; then 
  echo "$LOGDIR/$NODE not exist"
  usage
  exit 1
fi
if [ ! -f $SPEC ] && [ "$SPEC" != "no" ]; then 
  echo "$SPEC not exist"
  usage
  exit 1
fi

if [ "$MODIFY_TYPE" != "" ] ; then
   if [ ! -f $MODIFY_TYPE ]; then
    echo "$MODIFY_TYPE not exist"
    usage
    exit 1
   fi

fi
NODE_TYPE=$(echo $NODE | sed 's/[0-9]//g')
echo "NODE_TYPE:$NODE_TYPE"
XL=$( echo $(basename $SPEC) | cut -c -2 )
###################################################################################
echo "sleep ........"
sleep $[ ( $RANDOM % 120 )  + 1 ]
###################################################################################
rm -f $PWD/auto_config.cfg
if [ "$AUTO_CONFIG_FILE" = "" ] ; then
   touch $PWD/auto_config.cfg  
else
    if [ ! -f $AUTO_CONFIG_FILE ]; then 
     echo "$AUTO_CONFIG_FILE not exist"
     usage
     exit 1
    fi
   cp $AUTO_CONFIG_FILE $PWD/auto_config.cfg
fi
AUTO_CONFIG_FILE="$PWD/auto_config.cfg"

echo "############################################################################"
echo "# $CXS "
echo "############################################################################"
echo "rm -f /vobs/rcs/test/RCT_CRX901275/test/suites/*/*beam"
rm -f /vobs/rcs/test/RCT_CRX901275/test/suites/*/*beam
echo "rm -f /vobs/rcs/dev/RCP_CSX10179/OS_CRX901265/TESTBOX/TESTBOX_CXA1105670/rct/suites/*beam"
rm -f /vobs/rcs/dev/RCP_CSX10179/OS_CRX901265/TESTBOX/TESTBOX_CXA1105670/rct/suites/*beam
echo "rm -f /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/*/*_CNX*/test/suites/*.beam"
rm -f /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/*/*_CNX*/test/suites/*.beam
echo "############################################################################"
if [ "$(echo $NODE | grep sim)" = "" ] ; then
   for Hw in $(cat $LOGDIR/$NODE/config/stp.hws)
   do
   if [ "$(cat $BOOKING_DIR/$Hw)" != "$USER" ] ; then
      echo "$USER has not booked the $Hw"
      echo "The script is stopped! exit 1"
      exit 1
   fi
   done
   if [ ! -d $TFTPBOOTDIR/$NODE ]; then 
     echo "$TFTPBOOTDIR/$NODE not exist, creating it"
     mkdir -p $TFTPBOOTDIR/$NODE
   fi
chmod 777 $TFTPBOOTDIR/$NODE   
echo " START TEST $(date)  "


# Can not be used. Must be fixed!
##################################
if [ "$TESTUSER" != "" ] && [ -f $TFTPBOOTDIR/$NODE/cxslabel_${CXS} ] && [ "$UPGRADE_FROM_CXS" = "" ] && [ "upgrade" = "noupgrade" ]; then
   echo "${CXS}  is already loaded on the node"
   INSTALL="no"
##################################
else
   if [ "$UPGRADE_FROM_CXS" != "" ] && [ "$UPGRADE_FROM_URL" = "" ]; then
      $RCT_PREP $NODE $UPGRADE_FROM_CXS
      if [ "$?" != "0" ] ; then
          exit 1
      fi
      cp ${WORKSPACE}/upgradeFromPrep4TestInfo.txt ${LOGDIR}/${NODE}/prep4TestInfo.txt
   elif [ "$UPGRADE_FROM_CXS" != "" ] && [ "$UPGRADE_FROM_URL" != "" ]; then
      $RCT_PREP $NODE $UPGRADE_FROM_URL
      if [ "$?" != "0" ] ; then
          exit 1
      fi
      cp ${WORKSPACE}/upgradeFromPrep4TestInfo.txt ${LOGDIR}/${NODE}/prep4TestInfo.txt
   else
      if [ "$URL" = "" ] || [ "$URL" = "no" ] ; then
          if [ "$MODIFY_TYPE" = "" ] ; then
             $RCT_PREP $NODE $CXS
             if [ "$?" != "0" ] ; then
               echo " EXIT $(date)  "
               exit 1
             fi
          else
             echo "$RCS_UPMOD4UPGRADE  -r $CXS -t $NODE_TYPE -m $MODIFY_TYPE"
             $RCS_UPMOD4UPGRADE  -r $CXS -t $NODE_TYPE -m $MODIFY_TYPE
             ls -l $PWD/$CXS
             $RCT_PREP $NODE $PWD/$CXS
             
             if [ "$?" != "0" ] ; then
               rm -f $PWD/$CXS
               echo " EXIT $(date)  "
               exit 1
             else
               rm -f $PWD/$CXS
             fi

          fi
   
      else
        $RCT_PREP $NODE $URL
        if [ "$?" != "0" ] ; then
          echo " EXIT $(date)  "
          exit 1
        fi
      fi
      cp ${WORKSPACE}/prep4TestInfo.txt ${LOGDIR}/${NODE}/prep4TestInfo.txt
   fi
 

   if [ "$UPGRADE_FROM_CXS" != "" ] ; then
      $RCT_RUN -stp $NODE -spec $INSTALL_BEFORE_UPGRADE_SPEC  -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]"
      if [ "$?" != "0" ] ; then
          TEST_SPEC_INDEX_FILE=${NEWLOGDIR}/index.html
          check_if_exit
          #exit 1
      fi
      if [ "$MODIFY_TYPE" = "" ] && [ "$UPGRADE_FROM_URL" = "" ] ; then
      $RCT_UPGRADE_PREP -stp $NODE $CXS
      if [ "$?" != "0" ] ; then
          exit 1
      fi
      elif [ "$MODIFY_TYPE" = "" ] && [ "$UPGRADE_FROM_URL" != "" ] ; then
      $RCT_UPGRADE_PREP -stp $NODE $URL
      if [ "$?" != "0" ] ; then
          exit 1
      fi
      else
      echo "$RCS_UPMOD4UPGRADE  -r $CXS -t $NODE_TYPE -m $MODIFY_TYPE"
      $RCS_UPMOD4UPGRADE  -r $CXS -t $NODE_TYPE -m $MODIFY_TYPE
      ls -l $PWD/$CXS
      echo "$RCT_UPGRADE_PREP -stp $NODE $PWD/$CXS"
      $RCT_UPGRADE_PREP -stp $NODE $PWD/$CXS
      if [ "$?" != "0" ] ; then
               rm -f $PWD/$CXS
               echo " EXIT $(date)  "
               exit 1
             else
               rm -f $PWD/$CXS
             fi
      fi
      $RCT_RUN -stp $NODE -spec $UPGRADE_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_2.xml\"}]"
      if [ "$?" != "0" ] ; then
          exit 1
      fi
   else
      $RCT_RUN -stp $NODE -spec $INSTALL_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]"
      if [ "$?" != "0" ] ; then
          TEST_SPEC_INDEX_FILE=${NEWLOGDIR}/index.html
          if [ "$XL" != "xl" ] ; then
             check_if_exit
          else
               $RCT_RUN -stp $NODE -spec $INSTALL_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]"
               if [ "$?" != "0" ] ; then
                  TEST_SPEC_INDEX_FILE=${NEWLOGDIR}/index.html
                   if [ "$XL" != "xl" ] ; then
                      check_if_exit
                   else
                      $RCT_RUN -stp $NODE -spec $INSTALL_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]"
                      TEST_SPEC_INDEX_FILE=${NEWLOGDIR}/index.html
                      if [ "$?" != "0" ] ; then
                        check_if_exit
                      fi
                   fi
               fi
          fi
          #exit 1
      fi
      $RCT_RUN -stp $NODE -spec $CHECK_AFTER_INSTALL_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.1.xml\"}]"
   fi

fi
if [ "$TESTUSER" != "" ] && [ "$PATCHDIR" != "" ]; then
   echo "PATCHDIR >$PATCHDIR<" 
   $RCS_SSH $NODE "\rm -f /home/sirpa/dev_patches/*"
   PATCHES=$(ls $PATCHDIR)
   for P in $PATCHES
   do
     $RCS_SCP $NODE $PATCHDIR/$P /home/sirpa/dev_patches/
   done
   $RCS_SSH $NODE "ls -l  /home/sirpa/dev_patches/"
  # $RCS_SSH $NODE "\rm -f /home/sirpa/install_complete"
  # $RCS_SSH $NODE "mv  /rcs/networkloader/config_initial.netconf_loaded_ok /rcs/networkloader/config_initial.netconf"
  # $RCS_SSH $NODE "ls -l  /rcs/networkloader/"
   $RCT_RUN -stp $NODE -suite $BASIC_SUITE -case reinstall -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_4.xml\"}]"
  # $RCT_RUN -stp $NODE -suite $BASIC_SUITE -case reboot -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_4.xml\"}]"
elif [ "$TESTUSER" != "" ] && [ "$PATCHDIR" = "" ] && [ "$INSTALL" = "no" ]; then
   $RCS_SSH $NODE "\rm -f /home/sirpa/dev_patches/*"
  # $RCS_SSH $NODE "\rm -f /home/sirpa/install_complete"
  # $RCS_SSH $NODE "mv  /rcs/networkloader/config_initial.netconf_loaded_ok /rcs/networkloader/config_initial.netconf"
  # $RCS_SSH $NODE "ls -l  /rcs/networkloader/"
   $RCT_RUN -stp $NODE -suite $BASIC_SUITE -case reinstall -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_4.xml\"}]"
  # $RCT_RUN -stp $NODE -suite $BASIC_SUITE -case reboot -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_4.xml\"}]"
fi
  if [ "$SPEC" != "no" ] ; then
   while [ $REPEAT -gt 0 ]; do
      $RCT_RUN -stp $NODE -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_3.${REPEAT}.xml\"}]"
      REPEAT=$(expr $REPEAT - 1  )
   done 
  fi
else 
###################################################################################
#   SIM
###################################################################################
 echo "SIM"
  if [ "$UPGRADE_FROM_CXS" != "" ] ; then
     $RCT_UPGRADE_PREP -sim $NODE $CXS
      if [ "$?" != "0" ] ; then
          exit 1
      fi
     $RCT_RUN -sim $NODE -spec $UPGRADE_SPEC_SIM -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_2.xml\"}]"
      if [ "$?" != "0" ] ; then
          exit 1
      fi  
  fi
    if [ "$COVERSPEC" = "" ] ; then
     $RCT_RUN -sim $NODE -spec $INSTALL_SIM_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]"
     if [ "$?" != "0" ] ; then
          exit 1
      fi  
    while [ $REPEAT -gt 0 ]; do
      
       $RCT_RUN -sim $NODE -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_3.${REPEAT}.xml\"}]"
       REPEAT=$(expr $REPEAT - 1  )
   done
   else
    echo "$RCT_RUN -sim $NODE -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_3.${REPEAT}.xml\"}]" -cover $COVERSPEC -pa ${PWD}/dev_patches/ -cover_stop false"
    $RCT_RUN -sim $NODE -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR   -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_3.${REPEAT}.xml\"}]" -cover $COVERSPEC -pa ${PWD}/dev_patches/ -cover_stop false -pa /proj/rcs/rct_dev_patches/CT
   fi  

fi
echo " END TEST $(date)  "


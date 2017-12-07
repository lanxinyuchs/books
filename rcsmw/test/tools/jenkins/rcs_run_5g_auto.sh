#!/bin/bash
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2012 - 2016 All rights reserved.
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
## -          2016-05-26   etxjovp     Created
## main/23    2016-10-24   etxjovp     Add add GIT support
## ----------------------------------------------------------


trap "clean_up ; exit 1 "  1 
trap "clean_up ; exit  "  0

######################################################################################
clean_up()
{
if [ -d ${WORKSPACE} ]; then
  echo "clean_up"
  rm -f ${WORKSPACE}/*.tgz
  rm -f ${WORKSPACE}/*.zip
  rm -f ${WORKSPACE}/*.cxp
  rm -fr ${WORKSPACE}/ct_tmp
fi
if [ "$(ps -eaf | grep  "/repo/${NODE}/RCS_ROOT/home/rcsci1/software" | grep -v grep )" != "" ]; then
      ps -eaf | grep  "/repo/${NODE}/RCS_ROOT/home/rcsci1/software" | grep -v grep | awk '{print $2}' | wc -l
      pkill -9 -f "/repo/${NODE}/RCS_ROOT/home/rcsci1/software"
fi
}

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
echo -e "	${bold}-q$nobold    Will NOT compile RCT libs or ALL testuites, i.e. only the specified testsuite(s)
                                    will be compiled if necessary. "
echo -e ""
echo -e "${bold}Example:$nobold $0 ${bold}-s$nobold duw005 ${bold}-t$nobold /home/nisse/test.ts ${bold}-b$nobold CXS101553-R2A392  ${bold}-c$nobold  CXS101549_1-R2A281"
echo -e "${bold}Example:$nobold $0 ${bold}-s$nobold duw005 ${bold}-t$nobold /home/nisse/test.ts ${bold}-b$nobold CXS101553-R2A392  ${bold}-c$nobold  CXS101549_1-R2A281 -g https://rbs-g2-ci.rnd.ki.sw.ericsson.se:443/job/DC-BuildUP/159//artifact/DC-CXP9021221_1.tgz"
echo -e "${bold}Example:$nobold $0 ${bold}-s$nobold duw005 ${bold}-t$nobold /home/nisse/test.ts ${bold}-b$nobold CXS101553-R2A392  ${bold}-c$nobold  CXS101549_1-R2A281 ${bold}-u$nobold CXS101549_1-R2A260"
echo -e ""




}
######################################################################################
run_0()
{
  echo ""
}
######################################################################################
run_1()
{
   $RCT_RUN -stp $NODE -spec $INSTALL_BEFORE_UPGRADE_SPEC  -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR $NO_COMPILE  -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]"  
    if [ "$?" != "0" ] ; then
       TEST_SPEC_INDEX_FILE=${NEWLOGDIR}/index.html
       check_if_exit
    fi
    if [ "$PRE_DC" != "" ] ; then
        $RCT_RUN -stp $NODE -spec $PRE_DC_NODECI_CONFIG_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR $NO_COMPILE  -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.2.xml\"}]" 
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
    $RCT_RUN -stp $NODE -spec $UPGRADE_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR $NO_COMPILE  -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_2.xml\"}]" 
    if [ "$?" != "0" ] ; then
       exit 1
    fi  
}
######################################################################################
run_2()
{
  $RCT_RUN -stp $NODE -spec $INSTALL_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR  $NO_COMPILE -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.xml\"}]" 
  if [ "$?" != "0" ] ; then
     TEST_SPEC_INDEX_FILE=${NEWLOGDIR}/index.html
     check_if_exit
  fi
  $RCT_RUN -stp $NODE -spec $CHECK_AFTER_INSTALL_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR $NO_COMPILE  -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.1.xml\"}]" 
  if [ "$PRE_DC" != "" ] [ "$(echo $STP_LABEL | grep dual)" = "" ]; then
     $RCT_RUN -stp $NODE -spec $PRE_DC_NODECI_CONFIG_SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR $NO_COMPILE  -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_1.2.xml\"}]" 
  fi
}
######################################################################################
run_3()
{

if [ "$SPEC" != "no" ] ; then
      while [ $REPEAT -gt 0 ]; do
         $RCT_RUN -stp $NODE ${CLUSTER} -spec $SPEC -config $AUTO_CONFIG_FILE -create_priv_dir auto_per_tc -logdir $NEWLOGDIR $NO_COMPILE  -noshell -ct_hooks cth_surefire "[{path,\"${PWD}/report_3.${REPEAT}.xml\"}]" 
         if [ "$?" != "0" ] ; then
            exit 1
         fi
         REPEAT=$(expr $REPEAT - 1  )
      done 
fi
}
######################################################################################



create_log_dir()
{
 if [ "$STP" = "" ]; then
   REV=$BUILD_CXS
   mkdir -p $LOGDIR
   mkdir -p $LOGDIR/$NODE
   mkdir -p $LOGDIR/$NODE/$REV
   echo "TESTCONFIG, ${TESTCONFIG}"
   #SPECBASENAME=$(basename $SPEC)
   SPECBASENAME=$(echo $TESTCONFIG | cut -f2- -d_)
   echo "SPECBASENAME, ${SPECBASENAME}"
   mkdir -p $LOGDIR/$NODE/$REV/$SPECBASENAME
if [ "$TESTCONFIG" = "" ]; then
   NEWLOGDIR=$LOGDIR/$NODE/$REV/$SPECBASENAME
else
   mkdir -p $LOGDIR/$NODE/$REV/$SPECBASENAME/$TESTCONFIG
   NEWLOGDIR=$LOGDIR/$NODE/$REV/$SPECBASENAME/$TESTCONFIG
fi
fi
    echo "STP=$STP NODE=$NODE"

 if [ "$STP" != "" ]; then
 REV=$BUILD_CXS
   mkdir -p $LOGDIR
   mkdir -p $LOGDIR/$STP
   mkdir -p $LOGDIR/$STP/$REV
   echo "TESTCONFIG, ${TESTCONFIG}"
   #SPECBASENAME=$(basename $SPEC)
   SPECBASENAME=$(echo $TESTCONFIG | cut -f2- -d_)
   echo "SPECBASENAME, ${SPECBASENAME}"
   mkdir -p $LOGDIR/$STP/$REV/$SPECBASENAME
if [ "$TESTCONFIG" = "" ]; then
   NEWLOGDIR=$LOGDIR/$STP/$REV/$SPECBASENAME
else
   mkdir -p $LOGDIR/$STP/$REV/$SPECBASENAME/$TESTCONFIG
   NEWLOGDIR=$LOGDIR/$STP/$REV/$SPECBASENAME/$TESTCONFIG
fi

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
   echo "TESTCONFIG, ${TESTCONFIG}"
   #SPECBASENAME=$(basename $SPEC)
   SPECBASENAME=$(echo $TESTCONFIG | cut -f2- -d_)
   echo "SPECBASENAME, ${SPECBASENAME}"
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

AUTO_CONFIG_FILE=""
UPGRADE_FROM_CXS=""
REPEAT=1 
TESTUSER=""
TESTCONFIG=""
PATCHDIR=""
COVERSPEC=""
URL=""
MODIFY_TYPE=""
WRAPPER=""
CLUSTER=""
OAMAP=""
INSTALL="yes"
bold='\033[1m'
nobold='\033[0m'

######################################################################################
#   MAIN
######################################################################################
while getopts s:t:f:c:b:u:r:e:p:g:a:m:C:G:n:k:L:X:S:q  option
do
	case "$option"
	in
                s) NODE=$OPTARG
                   ;;
                S) STP=$OPTARG
                   ;;
                q) NO_COMPILE=-no_compile
		   ;;
                t) SPEC=$OPTARG
                   ;;
                f) TESTCONFIG=$OPTARG
                   ;;
                b) BUILD_CXS=$OPTARG
                   ;;
                c) CXS=$OPTARG
                   ;;
                X) CXS_2=$OPTARG
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
                n) SNAME=$OPTARG
                   ;;
                k) COOKIE=$OPTARG
                   ;;
                L) LOGDIR=$OPTARG
                   ;;
		\?) usage
		   exit 1;; 
	esac
done 
######################################################################################
GIT=no
if [ -f info.txt ]; then
  if [ "$(cat info.txt | grep "GIT=yes" )" != ""  ]; then
   GIT=yes
   COMMIT=$(cat $PWD/properties_$(basename $SPEC) | grep "CXS=" | cut -f2- -d= )
   git checkout -f $COMMIT
   RDE_TOP_CC=$RDE_TOP

   source env-i586.sh
ln -s  test/test/suites .
   RDE_TOP=$RDE_TOP_CC
   echo "##############################################################################"
   echo "
RCT_TOP		$RCT_TOP
RCS_TOP		$RCS_TOP
RDE_TOP		$RDE_TOP

"
  fi
fi

BOOKING_DIR="/proj/webdocs/rbs-rde/etc/host-book"
RCT_PREP="rcstprep.sh"
RCT_UPGRADE_PREP="upgradeprep.sh"
LOGDIR="/proj/rcs-tmp/stps"
TFTPBOOTDIR="/proj/rcs-tmp/tftpboot"
RCT_RUN="$RCT_TOP/test/bin/rct_run.sh"
RCS_SCP="$RCT_TOP/test/bin/rcs_scp.exp"
RCS_SSH="$RCT_TOP/test/bin/rcs_ssh.exp"
RCS_UPMOD4UPGRADE="$RCT_TOP/test/bin/rcs_upmod4upgrade.sh"
SPECSDIR="$RCT_TOP/test/testspecs"
BASIC_SUITE="$RCT_TOP/test/suites/install/basic_tests_SUITE.erl"
INSTALL_SPEC="$SPECSDIR/install"
INSTALL_SIM_SPEC="$SPECSDIR/install_sim"
CHECK_AFTER_INSTALL_SPEC="$SPECSDIR/check_after_install"
INSTALL_BEFORE_UPGRADE_SPEC="$SPECSDIR/install_before_upgrade"
PRE_DC_NODECI_CONFIG_SPEC="$SPECSDIR/pre_dc_nodeci_config_spec"
UPGRADE_SPEC="$SPECSDIR/upgrade"
UPGRADE_SPEC_SIM="$SPECSDIR/upgrade_sim"







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
PRE_DC=$( echo $(basename $SPEC) | grep "pre_dc" )
###################################################################################
if [ "$HOST" != "sekilx736" ]; then
#TMP etxjovp 151123 ip6
#MP=$($RDE_TOP/tools/jenkins/rcs_testspecs_query.sh -t $(basename $SPEC) -d cluster_du )
MP=""
echo $MP
if [ "$MP" != "" ]; then
  CLUSTER=" -mp $MP "
  echo "cluster ${CLUSTER} "
fi
fi
###################################################################################
if [ "$NODE_TYPE" = "rcf" ] && [ "$( echo ${TEST_TYPE} | grep GIT)" != "" ]; then
  CXS="${CXS}-${BRANCH}_${TEST_TYPE}"
fi
###################################################################################
if [ "$(echo $NODE_TYPE | cut -f1 -d_)" != "sim" ]; then
  echo "sleep ........"
  #sleep $[ ( $RANDOM % 120 )  + 1 ]
fi



###################################################################################
rm -f $PWD/auto_${NODE}_config.cfg
if [ "$AUTO_CONFIG_FILE" = "" ] ; then
   touch $PWD/auto_${NODE}_config.cfg  
else
    if [ ! -f $AUTO_CONFIG_FILE ]; then 
     echo "$AUTO_CONFIG_FILE not exist"
     usage
     exit 1
    fi
   cp $AUTO_CONFIG_FILE $PWD/auto_${NODE}_config.cfg
fi
chmod 666 $PWD/auto_${NODE}_config.cfg
AUTO_CONFIG_FILE="$PWD/auto_${NODE}_config.cfg"
if [ "$SNAME" != "" ] && [ "$COOKIE" != "" ]; then
  WRAPPER=" -sname $SNAME  -cookie $COOKIE"
  WRAPPER=" -cookie $NODE -rcssim_sname $NODE"

  echo "############################################################################"
  echo "Update $PWD/auto_${NODE}_config.cfg!!!!!"
  echo "{rcssim_sname, [$NODE]}." >> $PWD/auto_${NODE}_config.cfg
  cat $PWD/auto_${NODE}_config.cfg
fi
if [ -f ${WORKSPACE}/sdc_sim.cfg ]; then 
   cat ${WORKSPACE}/sdc_sim.cfg | grep -v "%%" >> $PWD/auto_${NODE}_config.cfg
fi 
if [  -f ${WORKSPACE}/properties_$(basename $SPEC) ]; then 
   if [  -f ${WORKSPACE}/info.txt ]; then 
     LL=$(cat ${WORKSPACE}/info.txt )
     PRE_BDC_CONFIG=$(cat ${WORKSPACE}/info.txt  | grep PRE_BDC_CONFIG | cut -f2- -d= )
   fi
   L=$(cat ${WORKSPACE}/properties_$(basename $SPEC) )
   
   if [ "$PRE_BDC_CONFIG" != "" ] && [ "$(cat $PWD/auto_${NODE}_config.cfg | grep node_type )" != "" ]; then
     cat $PWD/auto_${NODE}_config.cfg | grep -v node_type > $PWD/auto_${NODE}_config.cfg_tmp
     mv -f $PWD/auto_${NODE}_config.cfg_tmp $PWD/auto_${NODE}_config.cfg
    
   fi
   if [ "$PRE_BDC_CONFIG" != "" ]; then
      U=$PRE_BDC_CONFIG
   else
      U=dummy
   fi
   echo "{jenkins_config,[ " >> $PWD/auto_${NODE}_config.cfg
   for i in $LL
   do
     C=$(echo $i | cut -f1 -d= | sed 's/\(.*\)/\L\1/') 
     V=$(echo $i | cut -f2- -d= ) 
     echo "{$C ,\"$V\"}," >> $PWD/auto_${NODE}_config.cfg
   done
   for i in $L
   do
     C=$(echo $i | cut -f1 -d= | sed 's/\(.*\)/\L\1/') 
     V=$(echo $i | cut -f2- -d= ) 
      if [ "$V" != "[]" ]; then
         echo "{$C ,\"$V\"}," >> $PWD/auto_${NODE}_config.cfg
      else
         echo "{$C ,$V}," >> $PWD/auto_${NODE}_config.cfg
      fi 
   done
   echo "{workspace ,\"$WORKSPACE\" }," >> $PWD/auto_${NODE}_config.cfg
   echo "{installed_type ,$U }," >> $PWD/auto_${NODE}_config.cfg
   echo "{up_type ,$U }" >> $PWD/auto_${NODE}_config.cfg
   echo " ]}." >> $PWD/auto_${NODE}_config.cfg

fi
if [ "$(cat $PWD/auto_${NODE}_config.cfg | grep oamap_ipv6)" != "" ]; then
  OAMAP="-oamap_ipv6"
elif [ "$(cat $PWD/auto_${NODE}_config.cfg | grep no_oamap)" != "" ]; then
  OAMAP=""
elif [ "$(cat $PWD/auto_${NODE}_config.cfg | grep oamap_ipv4)" != "" ]; then
   OAMAP="-oamap_ipv4"
else
  OAMAP=""
fi
echo "############################################################################"
echo "OAMAP = $OAMAP"
echo "############################################################################"
echo $AUTO_CONFIG_FILE
echo "############################################################################"
  cat $AUTO_CONFIG_FILE
echo "############################################################################"
echo "# $CXS "
echo "# $CXS_2"
echo "############################################################################"
echo "############################################################################"
if [ "$(echo $NODE | grep sim)" = "" ] ; then
   for Hw in $(cat $LOGDIR/$NODE/config/stp.hws)
   do
if [ "$HOST" != "sekilx736" ]; then
      if [ "$(cat $BOOKING_DIR/$Hw)" != "$USER" ] ; then
         echo "$USER has not booked the $Hw"
         echo "The script is stopped! exit 1"
         exit 1
      fi
fi
   done
   if [ ! -d $TFTPBOOTDIR/$NODE ]; then 
     echo "$TFTPBOOTDIR/$NODE not exist, creating it"
     mkdir -p $TFTPBOOTDIR/$NODE
   fi
   chmod 777 $TFTPBOOTDIR/$NODE   
   echo " START TEST $(date)  "
##################################
# Can not be used. Must be fixed!
##################################
  if [ "$TESTUSER" != "" ] && [ -f $TFTPBOOTDIR/$NODE/cxslabel_${CXS} ] && [ "$UPGRADE_FROM_CXS" = "" ] && [ "upgrade" = "noupgrade" ]; then
      echo "${CXS}  is already loaded on the node"
      INSTALL="no"
##################################
  elif [ "$(echo $(basename $SPEC) | grep "_no_install_nl_")" != "" ]; then
      echo "take care of the installation"
      INSTALL="no"
  elif [ "$CXS_2" = "" ] && [ "$STP" != "" ]; then
        echo "UP  is already loaded on the node $STP"
        run_3 
  else
      if [ "$UPGRADE_FROM_CXS" = "trueNotIns" ] || [ "$UPGRADE_FROM_URL" = "trueNotIns" ]; then
         NOTINS="trueNotIns"
      else
         NOTINS=""
      fi
      if [ "$UPGRADE_FROM_CXS" != "" ]  && [ "$UPGRADE_FROM_URL" = "" ] && [ "$NOTINS" = "" ]; then
         $RCT_PREP $NODE $UPGRADE_FROM_CXS 
         if [ "$?" != "0" ] ; then
           exit 1
         fi

      elif [ "$UPGRADE_FROM_CXS" != "" ] && [ "$UPGRADE_FROM_URL" != "" ] && [ "$NOTINS" = "" ]; then
         $RCT_PREP $NODE $UPGRADE_FROM_URL $OAMAP
         if [ "$?" != "0" ] ; then
           exit 1
         fi

      else
         if [ "$URL" = "" ] || [ "$URL" = "no" ] ; then
            if [ "$MODIFY_TYPE" = "" ]; then
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
           $RCT_PREP $NODE $URL $OAMAP
           if [ "$?" != "0" ] ; then
             echo " EXIT $(date)  "
             exit 1
           fi
        fi

     fi

  if [ "$UPGRADE_FROM_CXS" != "" ] && [ "$NOTINS" = "" ]; then
    run_1
  elif [ "$CXS_2" != "" ] && [ "$STP" != "" ]; then
    run_2
  elif [ "$CXS_2" = "" ] && [ "$STP" != "" ]; then
    run_3     
  fi 
fi
fi
echo " END TEST $(date)  "


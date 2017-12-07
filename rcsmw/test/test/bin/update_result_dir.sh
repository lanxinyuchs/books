#!/bin/bash 
## ----------------------------------------------------------
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
## -          2012-12-01   etxjovp     Created
## R2A/3      2013-01-14   etxjovp     Update URL path for testlog
## R2A/21     2013-11-25   etxjovp     Add status indicator for create_lsv
## ----------------------------------------------------------

##############################################################################################################
create_lsv()
{
if [ "$ARG" = "START" ]; then 
   mkdir -p ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}
   chmod 775 ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}
   echo "<a href=\"${JENKINS_URL}/job/create_lsv/${BUILD_NUMBER}\"> <font color=black> #${BUILD_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/lsv_No_new
   date +"%Y %b %d %H:%M" > ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/date_new
elif [ "$ARG" = "FAILED" ]; then 
   echo "<a href=\"${JENKINS_URL}/job/create_lsv/${ARG1}\"> <font color=red> #${ARG1}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${ARG1}/lsv_No_new
   #date +"%Y %b %d %H:%M" > ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/date_new
elif [ "$ARG" = "UNSTABLE" ]; then 
   echo "<a href=\"${JENKINS_URL}/job/create_lsv/${ARG1}\"> <font color=orange> #${ARG1}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${ARG1}/lsv_No_new
   #date +"%Y %b %d %H:%M" > ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/date_new
else
   mkdir -p ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}
   chmod 775 ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}
   echo "<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${BUILD_NUMBER}\"> <font color=blue> #${BUILD_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/lsv_No_new
   if [ ! -f ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/date_new ]; then
      date +"%Y %b %d %H:%M" > ${JENKINS_HOME}/../root/ciresults/${BUILD_NUMBER}/date_new
   fi
fi
}
##############################################################################################################
test_activity()
{
date +"%Y %b %d %H:%M" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/date_new
echo "   ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${JOB_NAME}.start "
mkdir -p ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}
mkdir -p ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${JOB_NAME}
color="black"
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>#${BUILD_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${JOB_NAME}.state
date +"%s" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${JOB_NAME}.start 
}
##############################################################################################################
test_spec()
{
#TEST_CONFIG=config_template_spec_duw
mkdir -p ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}
color="black"
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>#${BUILD_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}.state
ln -sf ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}.state ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${JOB_NAME}.state
if [ "$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/config.xml |  grep "<disabled>false</disabled>")" != "" ]; then
echo "<a href=\"${JENKINS_URL}/job/${TEST_CONFIG}\"> <font color=$color>#</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}/${TEST_CONFIG}
echo "<font color=black>${TEST_CONFIG}</font>" >  ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}/${TEST_CONFIG}
echo "0 0 0" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.result
fi
}
##############################################################################################################
test_config()
{
date +"%s" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${JOB_NAME}.start
echo "<a href=\"${COMMON_TEST_URL}/${NODE_NAME}/${buildlabel}/$TEST_SPEC/${JOB_NAME}/\"> <font color=blue>${JOB_NAME} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font  color=blue> #${BUILD_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${JOB_NAME}

}

##############################################################################################################
test_logging()
{
if [ $RESULT = "SUCCESS" ]
  then color="green"
elif [ $RESULT = "UNSTABLE" ]
  then color="orange"
elif [ $RESULT = "FAILED" ];then 
   color="red"
   ERROR=0
   BLACK_LIST=" ${JENKINS_HOME}/../root/ciresults/black_list"
   if [ -f $BLACK_LIST ] && [ -f ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/junitResult.xml ]; then
       ErrorTestCaseList=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/junitResult.xml  | grep "<errorDetails>" | awk '{print $4":"$2"()"}' | sort -u )
       for i in $ErrorTestCaseList
       do
          if [ "$(cat $BLACK_LIST | grep $i | grep -v "#" )" = "" ];then
          ERROR=$( expr $ERROR + 1 )
          echo $i
          fi
       done 
     if [ "$ERROR" = "0" ] && [ "$ErrorTestCaseList" != "" ];then
       color="orange"
     fi
   fi
else
  color="black"
fi
label=$CXS
echo " ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.stop "
date +"%s" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.stop 
echo "<a href=\"${COMMON_TEST_URL}/${STP}/${label}/$TEST_SPEC/${TEST_CONFIG}/\"> <font color=$color>${TEST_CONFIG} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${TEST_CONFIG}/${TEST_CONFIG_NUMBER}/aggregatedTestReport\"> <font  color=$color> #${TEST_CONFIG_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG}



ln -sf ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG}  ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.state
###################################################################################################################################
if [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=black")" != "" ]
  then config_color="black"
elif [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=blue")" != "" ]
  then config_color="blue"
elif [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=red")" != "" ]
  then config_color="red"
elif [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=orange")" != "" ]
  then config_color="orange"
else
  config_color="green"
fi
echo "<a href=\"${JENKINS_URL}/job/${TEST_SPEC}/${TEST_SPEC_NUMBER}/aggregatedTestReport\"> <font color=$config_color>#${TEST_SPEC_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}.state
ln -sf ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}.state ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_SPEC}.state
###################################################################################################################################
if [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=black")" != "" ]
  then spec_color="black"
elif [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=blue")" != "" ]
  then spec_color="blue"
elif [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=red")" != "" ]
  then spec_color="red"
elif [ "$(cat ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=orange")" != "" ]
  then spec_color="orange"
else
  spec_color="green"
fi
echo "<a href=\"${JENKINS_URL}/job/${TEST_ACTIVITY}/${TEST_ACTIVITY_NUMBER}/aggregatedTestReport\"> <font color=$spec_color>#${TEST_ACTIVITY_NUMBER}</font></a>" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.state

ls -l ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log
###################################################################################################################################
TOTALLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f3- -df | awk '{print $1}' )
#PASSEDLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | awk '{print $1}')
FAILEDLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f2- -d, | awk '{print $1}')
SKIPPEDLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f3- -d, | awk '{print $1}')




TOTAL=0
for i in $TOTALLIST
do
  if [ "$i" != "0" ]; then
    TOTAL=$(expr $TOTAL + $i)
  fi
done
FAILED=0

for j in $FAILEDLIST
do
  if [ "$j" != "0" ]; then
    FAILED=$(expr $FAILED + $j)
  fi
done

SKIPPED=0
for k in $SKIPPEDLIST
do
  if [ "$k" != "0" ]; then
   SKIPPED=$(expr $SKIPPED + $k)
  fi
done

echo " $TOTAL $FAILED $SKIPPED"
echo " $TOTAL $FAILED $SKIPPED" > ${JENKINS_HOME}/../root/ciresults/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.result

echo "LSV_BUILD_NUMBER: ${LSV_BUILD_NUMBER} TEST_CONFIG: ${TEST_CONFIG} TOTAL: $TOTAL FAILED: $FAILED SKIPPED. $SKIPPED TEST_CONFIG: ${TEST_CONFIG}"


}


##############################################################################################################
COMMON_TEST_URL="https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/stps"

CMD=$1
ARG=$2
ARG1=$3
if [ "$CMD" = "" ]; then
  exit 1

elif [ "$CMD" = "create_lsv" ]; then 
   track=$ARG
   create_lsv

elif [ "$CMD" = "test_activity" ] && [ "$ARG" != "" ]; then 
   LSV_BUILD_NUMBER=$ARG
   track=$ARG1
   test_activity

elif [ "$CMD" = "test_spec" ] && [ "$ARG" != "" ]; then 
   TEST_CONFIG=$ARG
   track=$ARG1
   test_spec

elif [ "$CMD" = "test_config" ] && [ "$ARG" != "" ]; then 
   buildlabel=$ARG
   track=$ARG1
   test_config

elif [ "$CMD" = "test_logging" ]; then 
   track=$ARG
   test_logging
else 

   echo "$CMD not exist"
fi









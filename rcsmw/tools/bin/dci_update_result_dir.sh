#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2015 All rights reserved.
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
##  The original file is stored in/vobs/rcs/tools/RDE_LXA119945/tools/jenkins   
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
## R2A/22     2014-02-04   etxjovp     Add support for CI 3.0
## R2A/25     2014-08-25   etxjovp     Add TRACK_TOP dir
## R2A/25     2015-08-31   etxjovp     Add start and stop build time
## main/26    2015-09-25   etxjovp     Add .new_state file
## ----------------------------------------------------------

##############################################################################################################
create_lsv()
{
if [ "$ARG" = "START" ]; then 
   mkdir -p ${CIRESULTS_BRANCH} 
   chmod 775 ${CIRESULTS_BRANCH}
   mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}
   chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}
   mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}
   chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}
   mkdir -p ${CIRESULTS}
   chmod 775 ${CIRESULTS}
   ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER} ${CIRESULTS}/
   TRACK_TOP=$(echo $(echo $BRANCH_1 | sed 's/[0-9]/ /g' | awk '{print $1}')$(echo $BRANCH_1 | sed 's/[A-Z]/ /g' | awk '{print $1}'))
   mkdir -p ${CIRESULTS_BRANCH}/${TRACK_TOP}
   chmod 775 ${CIRESULTS_BRANCH}/${TRACK_TOP}
   ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER} ${CIRESULTS_BRANCH}/${TRACK_TOP}/
   echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${BUILD_NUMBER}\"> <font color=black> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/lsv_No_new
   date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new
   date +"%S" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/.build_start_time
elif [ "$ARG" = "FAILED" ]; then 
   #echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${ARG1}\"> <font color=red> #${ARG1}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/lsv_No_new
   echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${ARG1}\"> <font color=red> #${ARG1}</font></a>" > ${CIRESULTS}/${ARG1}/lsv_No_new
   #date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new
elif [ "$ARG" = "UNSTABLE" ]; then 
   #echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${ARG1}\"> <font color=red> #${ARG1}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/lsv_No_new
   echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${ARG1}\"> <font color=orange> #${ARG1}</font></a>" > ${CIRESULTS}/${ARG1}/lsv_No_new
   #date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new

else
   #mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}
   #chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}
   #ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER} ${CIRESULTS}/
   
 if [ "$TEST_TYPE" = "no" ] || [ "$TEST_TYPE" = "" ] || [ "$TEST_TYPE" = "A" ]; then
   echo "<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${BUILD_NUMBER}\"> <font color=blue> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/lsv_No_new
   if [ ! -f ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new ]; then
      date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new
   fi
   date +"%S" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/.build_stop_time
 elif [ "$TEST_TYPE" != "" ] ; then
   echo "<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${ARG1}+${TEST_TYPE}\"> <font color=blue> #${ARG1}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/lsv_No_new
   if [ ! -f ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/date_new ]; then
      date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/date_new
   fi
   date +"%S" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/.build_stop_time
 fi
   
fi
}
create_lsv_CI_3.0()
{
   
if [ "$ARG" = "START" ]; then 
   mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}
   chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}
   mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}
   chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}
   ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER} ${CIRESULTS}/${BUILD_NUMBER}
   echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${BUILD_NUMBER}\"> <font color=black> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/lsv_No_new
   date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new
elif [ "$ARG" = "FAILED" ]; then 

   echo "<a href=\"${JENKINS_URL}/job/${CREATE_LSV}/${ARG1}\"> <font color=red> #${ARG1}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${ARG1}/lsv_No_new
   #date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${BUILD_NUMBER}/date_new
else
   mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}
   chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}
   mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}
   chmod 775 ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}
   if [ "$ARG1" = "no" ] || [ "$ARG1" = "" ]; then
     echo "ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER} ${CIRESULTS}"
     ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER} ${CIRESULTS}
   else
     ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER} ${CIRESULTS}
     #cp ${CIRESULTS_BRANCH}/${ARG}/${LSV_BUILD_NUMBER}/T* ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/
   fi
   #echo "<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult_CI_3.0.pl?${BRANCH_1}/${LSV_BUILD_NUMBER}\"> <font color=blue> #${LSV_BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/lsv_No_new
   if [ ! -f ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/date_new ]; then
      date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/date_new
   fi
fi
}
##############################################################################################################
test_activity()
{
if [ "$ARG1" = "" ]; then 
date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/date_new
echo "   ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.start "
mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}
mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}
color="black"
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>#${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.state
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>ong</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.new_state
date +"%s" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.start 
else
date +"%Y %b %d %H:%M" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/date_new
echo "   ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${ARG1}.start "
mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}
mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${ARG1}
color="black"
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>#${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${ARG1}.state
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>ong</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${ARG1}.new_state
date +"%s" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${ARG1}.start 

fi
}

##############################################################################################################
no_upgrade()
{

color="yellow"

echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>-</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.new_state
echo "na" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.proc
}
##############################################################################################################
test_spec()
{
if [ "$TEST_SPEC_NAME" = "" ]; then
mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}
color="black"
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font color=$color>#${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}.state
ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}.state ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.state
if [ "$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/config.xml |  grep "<disabled>false</disabled>")" != "" ]; then
echo "<a href=\"${JENKINS_URL}/job/${TEST_CONFIG}\"> <font color=$color>#</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}/${TEST_CONFIG}
echo "<font color=black>${TEST_CONFIG}</font>" >  ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${JOB_NAME}/${TEST_CONFIG}
echo "0 0 0" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.result
fi
else
mkdir -p ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC_NAME}
color="black"
echo "<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}\"> <font color=$color>#${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC_NAME}.state
ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC_NAME}.state ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_SPEC_NAME}.state
echo "<font color=black>${TEST_CONFIG}</font>" >  ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC_NAME}/${TEST_CONFIG}
echo "0 0 0" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.result
fi
}
##############################################################################################################
test_config()
{
if [ "$TEST_CONFIG_NAME" = "" ]; then
date +"%s" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${JOB_NAME}.start
   if [ "$ARG4" = "" ]; then
     echo "<a href=\"${COMMON_TEST_URL}/${NODE_NAME}/${buildlabel}/$TEST_SPEC/${JOB_NAME}/\"> <font color=blue>${JOB_NAME} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font  color=blue> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${JOB_NAME}
   else
     echo "<a href=\"${COMMON_TEST_URL}/${ARG4}/${buildlabel}/$TEST_SPEC/${JOB_NAME}/\"> <font color=blue>${JOB_NAME} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}/aggregatedTestReport\"> <font  color=blue> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${JOB_NAME}
   fi
else
date +"%s" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG_NAME}.start
   if [ "$ARG4" = "" ]; then
     echo "<a href=\"${COMMON_TEST_URL}/${NODE_NAME}/${buildlabel}/$TEST_SPEC/${TEST_CONFIG_NAME}/\"> <font color=blue>${TEST_CONFIG_NAME} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}\"> <font  color=blue> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG_NAME}
   else
     echo "<a href=\"${COMMON_TEST_URL}/${ARG4}/${buildlabel}/$TEST_SPEC/${TEST_CONFIG_NAME}/\"> <font color=blue>${TEST_CONFIG_NAME} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${JOB_NAME}/${BUILD_NUMBER}\"> <font  color=blue> #${BUILD_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG_NAME}
   fi
fi
}

##############################################################################################################
##############################################################################################################
test_logging()
{
if [ "$TEST_CONFIG_JOB_NAME" = "" ]; then
if [ $RESULT = "SUCCESS" ]
  then color="green"
elif [ $RESULT = "UNSTABLE" ]
  then color="orange"
elif [ $RESULT = "FAILED" ];then 
   color="red"
   ERROR=0
   BLACK_LIST=" ${CIRESULTS_BRANCH}/${BRANCH_1}/black_list"
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
echo " ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.stop "
date +"%s" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.stop 
echo "<a href=\"${COMMON_TEST_URL}/${STP}/${label}/$TEST_SPEC/${TEST_CONFIG}/\"> <font color=$color>${TEST_CONFIG} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${TEST_CONFIG}/${TEST_CONFIG_NUMBER}/aggregatedTestReport\"> <font  color=$color> #${TEST_CONFIG_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG}

##############################################################################################################
ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG}  ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.state
###################################################################################################################################
if [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=black")" != "" ]
  then config_color="black"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=blue")" != "" ]
  then config_color="blue"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=red")" != "" ]
  then config_color="red"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=orange")" != "" ]
  then config_color="orange"
else
  config_color="green"
fi
echo "<a href=\"${JENKINS_URL}/job/${TEST_SPEC}/${TEST_SPEC_NUMBER}/aggregatedTestReport\"> <font color=$config_color>#${TEST_SPEC_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}.state
ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}.state ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_SPEC}.state
###################################################################################################################################
if [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=black")" != "" ]
  then spec_color="black"
  TEST_ACTIVITY_STATE="ong"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=blue")" != "" ]
  then spec_color="blue"
  TEST_ACTIVITY_STATE="ong"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=red")" != "" ]
  then spec_color="red"
  TEST_ACTIVITY_STATE="nok"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=orange")" != "" ]
  then spec_color="orange"
  TEST_ACTIVITY_STATE="ok"
else
  spec_color="green"
  TEST_ACTIVITY_STATE="ok"
fi
#if [ -f ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc ]; then
# TEST_ACTIVITY_STATE=$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc )
#fi
#TEST_ACTIVITY_STATE=$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc )
###################################################################################################################################
FAILED="0"
SKIPPED="0"
OK="0"

echo "###### NEW PROC   ###########################################"
for i in $(ls  ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/*_spec_*/* )
do
  echo $i
  if [ -f $i ]; then
    P=$(cat $i | cut -f4- -d/ | cut -f1 -d\" )
    echo $P
    if [ -f /${P}index.html ]; then
      R=$(cat /${P}index.html | grep -A6 "<td><b>Total</b></td>" | grep "align=right>" | sed 's/<b>//' | sed 's/<\/b>/ /' | cut -f2- -d\> | awk '{print $1}' | sed 's/\\n/ /g')
      echo $R
      FAILED=$( expr $(echo $R | awk '{print $3}') + $FAILED )
      SKIPPED=$( expr $(echo $R | awk '{print $2}') + $SKIPPED )
      OK=$( expr $(echo $R | awk '{print $1}') + $OK )
    fi
 fi
done

PROC="0"
TOTAL=$( expr $OK + $SKIPPED + $FAILED )
#FFF=$( expr $TOTAL - $SKIPPED - $FAILED )
if [ "$OK" != "0" ] && [ "$TOTAL" != "0" ]; then
GGG=$(echo "scale = 4;$OK/$TOTAL" | bc -l)
PROC1=$(echo "scale = 4;$GGG*100" | bc -l)
PROC=$(echo $PROC1 | cut -f1 -d.)
if [ "$PROC" = "" ]; then
PROC="0"
fi
else
PROC="0"
fi
echo " $PROC % $OK - $SKIPPED - $FAILED"
TEST_ACTIVITY_STATE=${PROC}
echo ${PROC} > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc

###################################################################################################################################

if [ "$RCS_CI_JENKINS_VER" != "" ]; then
echo "<a href=\"${JENKINS_URL}/job/start_activity/${TEST_ACTIVITY_NUMBER}/aggregatedTestReport\"> <font color=$spec_color>#${TEST_ACTIVITY_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.state
echo "<a href=\"${JENKINS_URL}/job/start_activity/${TEST_ACTIVITY_NUMBER}/aggregatedTestReport\"> <font color=$spec_color>${TEST_ACTIVITY_STATE}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state
else

echo "<a href=\"${JENKINS_URL}/job/${TEST_ACTIVITY}/${TEST_ACTIVITY_NUMBER}/aggregatedTestReport\"> <font color=$spec_color>#${TEST_ACTIVITY_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.state
echo "<a href=\"${JENKINS_URL}/job/${TEST_ACTIVITY}/${TEST_ACTIVITY_NUMBER}/aggregatedTestReport\"> <font color=$spec_color>${TEST_ACTIVITY_STATE}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state
if [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state | wc -l | awk '{print $1}' )" != "1" ]; then
   cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state | head -1 > nisse_tmp
  cat nisse_tmp > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state

fi
fi
ls -l ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log
###################################################################################################################################
TOTALLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f3- -df | awk '{print $1}' )
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
echo " $TOTAL $FAILED $SKIPPED" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.result

echo "LSV_BUILD_NUMBER: ${LSV_BUILD_NUMBER} TEST_CONFIG: ${TEST_CONFIG} TOTAL: $TOTAL FAILED: $FAILED SKIPPED. $SKIPPED TEST_CONFIG: ${TEST_CONFIG}"
else
##############################################################################################################
##############################################################################################################
##############################################################################################################
label=$CXS
if [ $RESULT = "SUCCESS" ]
  then color="green"
elif [ $RESULT = "UNSTABLE" ]
  then color="orange"
elif [ $RESULT = "FAILED" ];then 
   color="red"
   ERROR=0
   BLACK_LIST=" ${CIRESULTS_BRANCH}/${BRANCH_1}/black_list"
   echo ">>>>>$BLACK_LIST "
  # if [ -f $BLACK_LIST ] && [ -f ${JENKINS_HOME}/jobs/${TEST_CONFIG_JOB_NAME}/builds/${TEST_CONFIG_NUMBER}/junitResult.xml ]; then
   if [ -f $BLACK_LIST ]; then
       echo "/proj/rcs-tmp/stps/${STP}/${label}/$TEST_SPEC/${TEST_CONFIG}/ct_run.*/*logs/run*/suite.log "


       ErrorTestCaseList=$(cat /proj/rcs-tmp/stps/${STP}/${label}/$TEST_SPEC/${TEST_CONFIG}/ct_run.*/*logs/run*/suite.log | grep -B5 "=result        failed:" | grep "=case" | grep -v failed | cut -f2- -d: )
       echo "ErrorTestCaseList $ErrorTestCaseList"
       #ErrorTestCaseList=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG_JOB_NAME}/builds/${TEST_CONFIG_NUMBER}/junitResult.xml  | grep "<errorDetails>" | awk '{print $4":"$2"()"}' | sort -u )
       for i in $ErrorTestCaseList
       do
          echo ">>>>>>>>>>>>>>$i"
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

echo " ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.stop "
date +"%s" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.stop 
echo "<a href=\"${COMMON_TEST_URL}/${STP}/${label}/$TEST_SPEC/${TEST_CONFIG}/\"> <font color=$color>${TEST_CONFIG} </font></a>&nbsp&nbsp<a href=\"${JENKINS_URL}/job/${TEST_CONFIG_JOB_NAME}/${TEST_CONFIG_NUMBER}\"> <font  color=$color> #${TEST_CONFIG_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG}

##############################################################################################################
ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}/${TEST_CONFIG}  ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.state
###################################################################################################################################
if [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=black")" != "" ]
  then config_color="black"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=blue")" != "" ]
  then config_color="blue"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=red")" != "" ]
  then config_color="red"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/${TEST_SPEC}/* | grep "=orange")" != "" ]
  then config_color="orange"
else
  config_color="green"
fi
echo " <a href=\"${JENKINS_URL}/job/${TEST_SPEC_JOB}/${TEST_SPEC_NUMBER}\"> <font color=$config_color>#${TEST_SPEC_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}.state
ln -sf ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/${TEST_SPEC}.state ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_SPEC}.state
###################################################################################################################################
if [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=black")" != "" ]
  then spec_color="black"
  TEST_ACTIVITY_STATE="ong"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=blue")" != "" ]
  then spec_color="blue"
  TEST_ACTIVITY_STATE="ong"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=red")" != "" ]
  then spec_color="red"
  TEST_ACTIVITY_STATE="nok"
elif [ "$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY}/*.state | grep "=orange")" != "" ]
  then spec_color="orange"
  TEST_ACTIVITY_STATE="ok"
else
  spec_color="green"
  TEST_ACTIVITY_STATE="ok"
fi
#if [ -f ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc ]; then
 #TEST_ACTIVITY_STATE=$(cat ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc )
#fi

###################################################################################################################################
FAILED="0"
SKIPPED="0"
OK="0"

echo "###### NEW PROC   ###########################################"
for i in $(ls  ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY/*_spec_*/* )
do
  echo $i
  if [ -f $i ]; then
    P=$(cat $i | cut -f4- -d/ | cut -f1 -d\" )
    echo $P
    if [ -f /${P}index.html ]; then
      R=$(cat /${P}index.html | grep -A6 "<td><b>Total</b></td>" | grep "align=right>" | sed 's/<b>//' | sed 's/<\/b>/ /' | cut -f2- -d\> | awk '{print $1}' | sed 's/\\n/ /g')
      echo $R
      FAILED=$( expr $(echo $R | awk '{print $3}') + $FAILED )
      SKIPPED=$( expr $(echo $R | awk '{print $2}') + $SKIPPED )
      OK=$( expr $(echo $R | awk '{print $1}') + $OK )
    fi
 fi
done
PROC="0"
TOTAL=$( expr $OK + $SKIPPED + $FAILED )
#FFF=$( expr $TOTAL - $SKIPPED - $FAILED )
if [ "$OK" != "0" ] && [ "$TOTAL" != "0" ]; then
GGG=$(echo "scale = 4;$OK/$TOTAL" | bc -l)
PROC1=$(echo "scale = 4;$GGG*100" | bc -l)
PROC=$(echo $PROC1 | cut -f1 -d.)
if [ "$PROC" = "" ]; then
PROC="0"
fi
else
PROC="0"
fi
echo " $PROC % $OK - $SKIPPED - $FAILED"
TEST_ACTIVITY_STATE=${PROC}
echo ${PROC} > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.proc

###################################################################################################################################


if [ "$RCS_CI_JENKINS_VER" != "" ]; then
echo "<a href=\"${JENKINS_URL}/job/start_activity/${TEST_ACTIVITY_NUMBER}\">  <font color=$spec_color>#${TEST_ACTIVITY_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.state
echo "<a href=\"${JENKINS_URL}/job/start_activity/${TEST_ACTIVITY_NUMBER}\">  <font color=$spec_color>${TEST_ACTIVITY_STATE}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state
else
echo "<a href=\"${JENKINS_URL}/job/${TEST_ACTIVITY}/${TEST_ACTIVITY_NUMBER}\">  <font color=$spec_color>#${TEST_ACTIVITY_NUMBER}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.state
echo "<a href=\"${JENKINS_URL}/job/${TEST_ACTIVITY}/${TEST_ACTIVITY_NUMBER}\">  <font color=$spec_color>${TEST_ACTIVITY_STATE}</font></a>" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/$TEST_ACTIVITY.new_state

fi
ls -l ${JENKINS_HOME}/jobs/${TEST_CONFIG_JOB_NAME}/builds/${TEST_CONFIG_NUMBER}/log
###################################################################################################################################
TOTALLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG_JOB_NAME}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f3- -df | awk '{print $1}' )
FAILEDLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG_JOB_NAME}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f2- -d, | awk '{print $1}')
SKIPPEDLIST=$(cat ${JENKINS_HOME}/jobs/${TEST_CONFIG_JOB_NAME}/builds/${TEST_CONFIG_NUMBER}/log | grep "TEST COMPLETE," | cut -f2- -d, | cut -f3- -d, | awk '{print $1}')




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
echo " $TOTAL $FAILED $SKIPPED" > ${CIRESULTS_BRANCH}/${BRANCH_1}/${LSV_BUILD_NUMBER}/${TEST_CONFIG}.result

echo "LSV_BUILD_NUMBER: ${LSV_BUILD_NUMBER} TEST_CONFIG: ${TEST_CONFIG} TOTAL: $TOTAL FAILED: $FAILED SKIPPED. $SKIPPED TEST_CONFIG: ${TEST_CONFIG}"

##############################################################################################################
##############################################################################################################
##############################################################################################################
fi

}


##############################################################################################################
COMMON_TEST_URL="https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/stps"

CMD=$1
ARG=$2
ARG1=$3
ARG4=$4
 if [ "$TEST_TYPE" = "no" ] || [ "$TEST_TYPE" = "" ] || [ "$TEST_TYPE" = "A" ]; then
      BRANCH_1=${BRANCH}
      JENKINS_HOME_MASTER=${JENKINS_HOME}
      CIRESULTS="${JENKINS_HOME_MASTER}/../root/ciresults"
      CIRESULTS_BRANCH="${JENKINS_HOME_MASTER}/../root/ciresults_branch"
      CREATE_LSV="create_lsv"
   elif [ "$TEST_TYPE" != "" ] ; then
      JENKINS_HOME_MASTER="/proj/webdocs/rbs-rde-ci/jenkinsdata/"
      BRANCH_1="${BRANCH}"
      CIRESULTS="${JENKINS_HOME_MASTER}/../root/${TEST_TYPE}_ciresults"
      CIRESULTS_BRANCH="${JENKINS_HOME_MASTER}/../root/${TEST_TYPE}_ciresults_branch"
      CREATE_LSV="create_lsv_${TEST_TYPE}"
   fi


if [ "$CMD" = "" ]; then
  exit 1

elif [ "$CMD" = "create_lsv" ]; then 
   track=$ARG
   create_lsv
elif [ "$CMD" = "create_lsv_CI_3.0" ]; then 
  if [ "$ARG1" = "no" ] || [ "$ARG1" = "" ]; then
      BRANCH_1=${ARG}
   else
      BRANCH_1=${ARG}
   fi
   create_lsv_CI_3.0
elif [ "$CMD" = "test_activity" ] && [ "$ARG" != "" ]; then 
  # LSV_BUILD_NUMBER=$ARG
   track=$ARG1
   
   test_activity
elif [ "$CMD" = "no_upgrade" ] && [ "$ARG" != "" ]; then 
  # LSV_BUILD_NUMBER=$ARG
   track=$ARG1
   
   no_upgrade
elif [ "$CMD" = "test_spec" ] && [ "$ARG" != "" ]; then 
   TEST_CONFIG=$ARG
   TEST_SPEC_NAME=$ARG1
   
   test_spec

elif [ "$CMD" = "test_config" ] && [ "$ARG" != "" ]; then 
   buildlabel=$ARG
   TEST_CONFIG_NAME=$ARG1
   
   test_config

elif [ "$CMD" = "test_logging" ]; then 
   TEST_CONFIG_JOB_NAME=$ARG
   
   test_logging
else 

   echo "$CMD not exist"
fi









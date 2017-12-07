#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2014 - 2015 All rights reserved.
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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2014-01-28   etxjovp     Created
## /main/50   2014-11-25   etxjovp     Update for CI 4.0

NODE_TYPE=$1
MAXTIME_MIN=$2
TEST_ACTIVITY=$3
UPGRADE=$4
CONFIG1=$5
TARGET_TYPE=$6
ST_LIST=$7
TEST_TIMEOUT=$8
CONTAINER=$9
UPGRADE_CONTAINER=${10}

EX_TIME_START=$(date +"%Y:%m:%d_%H:%M:%S")

echo "${ST_LIST}"
if [ -f $ST_LIST ] || [ "${ST_LIST}" = "def" ]; then
########################################################################################
# CREATE TESTSPECS FILE
########################################################################################

#CREATE_PROP="./dci_create_properties.sh"

CREATE_PROP="gitWrapper.sh -P tools/bin/dci_create_properties.sh -D . "


CONFIG2=""
TMP_CONFIG=$(echo $CONFIG1 | sed 's/:/ /' | awk '{print $2}' | sed 's/:/\|\|/g')
CONFIG1=$(echo $CONFIG1 | cut -f1 -d:)
if  [ "$CONFIG1" = "ai" ]; then
  CONFIG2=$CONFIG1
  CONFIG1="no"
elif  [ "$CONFIG1" = "ip6" ] || [ "$CONFIG1" = "lab" ] ; then
  CONFIG2=$CONFIG1
  CONFIG1="no"
fi
if [ "$(echo $TEST_ACTIVITY | grep ":" )" != "" ]; then
  TEST_SPEC_NAME=$(echo $TEST_ACTIVITY | cut -f2- -d: )
  TEST_ACTIVITY=$(echo $TEST_ACTIVITY | cut -f1 -d: )
elif [ "$(echo $TEST_ACTIVITY | grep "#" )" != "" ]; then
  TEST_ACTIVITY_TEXT=$(echo $TEST_ACTIVITY | cut -f2- -d# )
  TEST_ACTIVITY=$(echo $TEST_ACTIVITY | cut -f1 -d# )
fi
if  [ "$CONFIG2" != "" ]; then
  TEST_SPEC_NAME="${TEST_SPEC_NAME}$CONFIG2"
fi
echo "--------------------------------------------------------------------
NODE_TYPE		:$NODE_TYPE
MAXTIME_MIN		:$MAXTIME_MIN
TEST_ACTIVITY		:$TEST_ACTIVITY
TEST_SPEC_NAME		:$TEST_SPEC_NAME
UPGRADE			:$UPGRADE	
CONFIG1			:$CONFIG1	
TARGET_TYPE		:$TARGET_TYPE
ST_LIST			:$ST_LIST
TEST_TIMEOUT		:$TEST_TIMEOUT
CONTAINER		:$CONTAINER
UPGRADE_CONTAINER	:$UPGRADE_CONTAINER
--------------------------------------------------------------------"


if [ "$UPGRADE" = "false" ] && [ "$TEST_ACTIVITY" = "delivery_check" ] && [ "$CONFIG1" = "no" ] && [ "$TARGET_TYPE" = "Sim" ]; then
  cat $ST_LIST | grep -v "#" | grep -v "/SAF" | grep ":${CONFIG1}"  | grep ":cover" | grep -v ":onlyupgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE=""

elif [ "$UPGRADE" = "false" ] && [ "$MAXTIME_MIN" = "cover" ] && [ "$CONFIG1" = "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${CONFIG1}"  | grep ":cover" | grep -v ":onlyupgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE=""
elif [ "$UPGRADE" = "false" ] && [ "$MAXTIME_MIN" = "all" ] && [ "$CONFIG1" = "no" ] && [ "$TARGET_TYPE" != "Sim" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${CONFIG1}"  | grep -v ":onlyupgrade" | grep -v "buti_c_SUITE" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE=""
elif [ "$UPGRADE" = "true" ] && [ "$MAXTIME_MIN" = "all" ] && [ "$CONFIG1" = "no" ] && [ "$TARGET_TYPE" != "Sim" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${CONFIG1}"  | grep  ":upgrade" | grep -v "buti_c_SUITE" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_upgrade"
elif [ "$UPGRADE" = "false" ] && [ "$CONFIG1" = "no" ] && [ "$MAXTIME_MIN" = "random" ] && [ "$TARGET_TYPE" != "Sim" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${CONFIG1}" | grep -v ":onlyupgrade" | grep -v "buti_c_SUITE"| sort -k1 -n -r > tmp_list
  CONFIG_TYPE=""
elif [ "$UPGRADE" = "true" ] && [ "$CONFIG1" = "no" ] && [ "$MAXTIME_MIN" = "random" ] && [ "$TARGET_TYPE" != "Sim" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${CONFIG1}" | grep  ":upgrade" | grep -v "buti_c_SUITE"| sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_upgrade"
elif [ "$UPGRADE" = "false" ] && [ "$CONFIG1" = "no" ]; then

  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" | grep ":${CONFIG1}" | grep -v ":onlyupgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE=""

elif [ "$UPGRADE" = "false" ] && [ "$CONFIG1" != "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" | grep ":${CONFIG1}" | grep -v ":onlyupgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_$(echo $CONFIG1 | sed 's/\&\&/_/g' | sed 's'/\|\|'/_/g' )"



elif [ "$UPGRADE" = "true" ] && [ "$CONFIG1" = "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" | grep ":${CONFIG1}"| grep ":upgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_upgrade"

elif [ "$UPGRADE" = "true" ] && [ "$CONFIG1" != "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" |  grep ":${CONFIG1}" | grep ":upgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_$(echo $CONFIG1 | sed 's/\&\&/_/g' | sed 's'/\|\|'/_/g' )_upgrade"

elif [ "$UPGRADE" = "true:short" ] && [ "$CONFIG1" = "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" | grep ":${CONFIG1}"| grep ":upgrade:short" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_upgrade"

elif [ "$UPGRADE" = "true:short" ] && [ "$CONFIG1" != "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" |  grep ":${CONFIG1}" | grep ":upgrade:short" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_$(echo $CONFIG1 | sed 's/\&\&/_/g' | sed 's'/\|\|'/_/g' )_upgrade"

elif [ "$UPGRADE" = "trueNotIns" ] && [ "$CONFIG1" != "no" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}[0-9]*:\|:${NODE_TYPE}[0-9]* \|:${NODE_TYPE}[0-9]*|\|:${NODE_TYPE}:\|:${NODE_TYPE} \|:${NODE_TYPE}|" |  grep ":${CONFIG1}" | grep ":upgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="_$(echo $CONFIG1 | sed 's/\&\&/_/g' | sed 's'/\|\|'/_/g' )_upgrade"
fi

########################################################################################
UPGRADE=$( echo $UPGRADE | cut -f1 -d: )
echo "--------------------------------------------------------------------
UPGRADE			:$UPGRADE	
TEST_TYPE		:$TEST_TYPE	
CONFIG_TYPE		:$CONFIG_TYPE	
--------------------------------------------------------------------"


if  [ "$(echo $TEST_TYPE | grep CXS )" != "" ] ; then
  echo $TEST_TYPE
elif  [ "$TEST_TYPE" = "no" ] ; then
   CONFIG_TYPE=$CONFIG_TYPE
else
  if [ "$CONFIG_TYPE" != "" ] ; then
     CONFIG_TYPE="${CONFIG_TYPE}_${TEST_TYPE}"
  else
     CONFIG_TYPE="_${TEST_TYPE}"
  fi
fi

if [ "$CONFIG_TYPE" = "_ip6_" ] || [ "$CONFIG_TYPE" = "_lab_" ]; then
     CONFIG_TYPE="${CONFIG_TYPE}_${TEST_TYPE}"
fi
echo "--------------------------------------------------------------------
CONFIG_TYPE		:$CONFIG_TYPE	
--------------------------------------------------------------------"

if [ "$TEST_SPEC_NAME" = "" ]; then
  
    if [ "$( echo $TEST_ACTIVITY | grep sbc )" != "" ] || [ "$( echo $TEST_ACTIVITY | grep sdc )" != "" ]; then
      if [ "$TEST_ACTIVITY_TEXT" = "" ]; then
            TEST_SPEC="${TEST_ACTIVITY}_${NODE_TYPE}${CONFIG_TYPE}_spec_"
      else
            TEST_SPEC="${TEST_ACTIVITY}_${TEST_ACTIVITY_TEXT}_${NODE_TYPE}${CONFIG_TYPE}_spec_"
      fi
    else
    TEST_SPEC="${TEST_ACTIVITY}_${NODE_TYPE}${CONFIG_TYPE}_spec_"
    fi


else
  TEST_SPEC="${TEST_ACTIVITY}_${TEST_SPEC_NAME}_spec_"
fi
echo "--------------------------------------------------------------------
TEST_SPEC		:$TEST_SPEC	
--------------------------------------------------------------------"

echo " ${TEST_SPEC}"
rm -f ${TEST_SPEC}*
########################################################################################
echo "# START preparations for adaptive creation of test spec"
########################################################################################
ciresults="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults"
NEWDTIMELIST="${ciresults}/${LSV_BUILD_NUMBER_UPGRADE}/${TEST_SPEC}time.txt"
if [ ! -f $NEWDTIMELIST ]; then
 touch new_time_list
  NEWDTIMELIST=new_time_list
fi
echo "NEWDTIMELIST=$NEWDTIMELIST"
if [ -f $NEWDTIMELIST ]; then
rm -f tmp_list_new
LINE=$(wc -l tmp_list | awk '{print $1}')

while [ $LINE -gt 0 ]
do
    CASE_LIST=""
    ROW=$(tail -$LINE tmp_list | head -1 )
    OLDTIME=$(echo $ROW | awk '{print $1}')
    SUITE=$(echo $ROW | cut -f5- -d\| | cut -f3 -d,)
    GROUP=$(echo $ROW | cut -f5- -d\| | cut -f4 -d, | cut -f1 -d\} | grep -v "]")
    if [ "$( echo $ROW | grep  "\[" )" != "" ]; then  
       CASE_LIST=$( echo $ROW | cut -f2- -d\[ | cut -f1 -d\] )
       
    fi  
    
    if [ "$CASE_LIST" != "" ]; then
      NEWTIME=$(cat $NEWDTIMELIST | grep $SUITE  | grep "${CASE_LIST}" | awk '{print $2}')
      if [ "$NEWTIME" = "" ]; then
         NEWTIME=$(cat $NEWDTIMELIST | grep $SUITE  | grep -v "group" | awk '{print $2}')
      fi
      echo "OLDTIME $OLDTIME	 NEWTIME $NEWTIME	 $NEWDTIMELIST   $SUITE     "
    elif [ "$GROUP" = "" ]; then

      NEWTIME=$(cat $NEWDTIMELIST | grep $SUITE  | awk '{print $2}')
      echo "OLDTIME $OLDTIME	 NEWTIME $NEWTIME	 $NEWDTIMELIST   $SUITE     "
    else 

      GROUP1=$(echo "SUITE.${GROUP}." | sed 's/ //' )

      NEWTIME=$(cat $NEWDTIMELIST | grep $SUITE | grep "${GROUP1}" | awk '{print $2}')
      echo "OLDTIME $OLDTIME	 NEWTIME $NEWTIME	 $NEWDTIMELIST   $SUITE   $GROUP  "
    fi
      #echo "OLDTIME $OLDTIME NEWTIME $NEWTIME"
      if [ "$NEWTIME" = "" ] || [ "$NEWTIME" = "0" ]; then
         echo $ROW >> tmp_tmp
      else
         echo $ROW | sed 's/'${OLDTIME}'/'${NEWTIME}'/' >> tmp_tmp 

      fi

    LINE=$( expr $LINE - 1 )
done
cat tmp_tmp | sort -k1 -n -r > tmp_list_new
echo "#####################################################################"
cat tmp_list
echo "#####################################################################"
cat tmp_list_new
echo "#####################################################################"
rm -f tmp_tmp
########################################################################################
# activate adaptive creation of test spec 
########################################################################################
echo "#####################################################################"
echo " Activate adaptive creation of test spec   ! "
echo "#####################################################################"
cat tmp_list_new > tmp_list
########################################################################################
rm -f tmp_list_new

########################################################################################
TOT_TIME=0
LINE=$(wc -l tmp_list | awk '{print $1}')
echo $LINE
########################################################################################
# CREATE TESTSPECS 
########################################################################################
if [ "$LINE" != "0" ]; then


if [ "$MAXTIME_MIN" = "random" ]; then
  NUMBER_OF_TESTSPECS="1"
  N=$NUMBER_OF_TESTSPECS
 echo "%%########################		TESTSPEC ${TEST_SPEC}${N}			######################## " > ${TEST_SPEC}${N}
 echo "{merge_tests, false}." >> ${TEST_SPEC}${N}
  cat tmp_list | grep -v "#" | cut -f5- -d\| | sort -R | grep -v "restartable_programs_SUITE" | grep -v "measure_restart_SUITE" >> ${TEST_SPEC}${N}
  echo "{cases, \"/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test/suites/install\",fetch_esi_SUITE , [all]}." >> ${TEST_SPEC}${N}
elif [ "$MAXTIME_MIN" = "all" ]; then
  NUMBER_OF_TESTSPECS="1"
  N=$NUMBER_OF_TESTSPECS
 echo "%%########################		TESTSPEC ${TEST_SPEC}${N}			######################## " > ${TEST_SPEC}${N}
 echo "{merge_tests, false}." >> ${TEST_SPEC}${N}
  cat tmp_list | grep -v "#" | cut -f5- -d\|  | grep -v "restartable_programs_SUITE" | grep -v "measure_restart_SUITE" >> ${TEST_SPEC}${N}
  echo "{cases, \"/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test/suites/install\",fetch_esi_SUITE , [all]}." >> ${TEST_SPEC}${N}
elif [ "$MAXTIME_MIN" = "cover" ]; then
  NUMBER_OF_TESTSPECS="1"
  N=$NUMBER_OF_TESTSPECS
 echo "%%########################		TESTSPEC ${TEST_SPEC}${N}			######################## " > ${TEST_SPEC}${N}
 echo "{merge_tests, false}." >> ${TEST_SPEC}${N}
  cat tmp_list | grep -v "#" | cut -f5- -d\|  | grep -v "restartable_programs_SUITE" | grep -v "measure_restart_SUITE" >> ${TEST_SPEC}${N}
  echo "{cases, \"/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test/suites/install\",fetch_esi_SUITE , [all]}." >> ${TEST_SPEC}${N}
else
#if  [ "$MAXTIME_MIN" != "random" ] || [ "$MAXTIME_MIN" != "cover" ]; then
MAXTIME=$( expr $MAXTIME_MIN \* 60 )
for i in $(cat tmp_list | cut -f1 -d\| )
do
  TOT_TIME=$(echo "scale = 4;$TOT_TIME+$i" | bc -l)
done
if [ "$(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l)" = "1" ]; then
   #NUMBER_OF_TESTSPECS=$(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l)
   NUMBER_OF_TESTSPECS=$(expr $(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l) + 1 )
else
   NUMBER_OF_TESTSPECS=$(expr $(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l) + 1 )
fi
N=$NUMBER_OF_TESTSPECS
echo "NUMBER_OF_TESTSPECS:$N TOT_TIME:$TOT_TIME MAXTIME:$MAXTIME"

while [ $N -gt 0 ]
do
  echo " 0 " > ${N}_TESTSPECS_TIME

 echo "%%########################		TESTSPEC ${TEST_SPEC}${N}			######################## " > ${TEST_SPEC}${N}
 echo "{merge_tests, false}." >> ${TEST_SPEC}${N}
  N=$( expr $N - 1 )
done
NN=$NUMBER_OF_TESTSPECS

if [ "$NUMBER_OF_TESTSPECS" != "1" ]; then 
while [ $LINE -gt 0 ]
do
  ROW=$(cat tmp_list | tail -$LINE | head -1)
  TESTSUITETIME=$(echo $ROW | cut -f1 -d\| )
  SUITE=$(echo $ROW | cut -f5- -d\|  )

  TESTSPECTIME=$(egrep ""  *_TESTSPECS_TIME |  sort -k2 -n -r  | tail -1 | cut -f2- -d: )
  K=$(egrep ""  *_TESTSPECS_TIME |  sort -k2 -n -r  | tail -1 | cut -f1 -d: )
  N=$(egrep ""  *_TESTSPECS_TIME |  sort -k2 -n -r  | tail -1 | cut -f1 -d: | cut -f1 -d_)
  echo " $N	K $K 	TESTSPECTIME $TESTSPECTIME TESTSUITETIME $TESTSUITETIME	SUITE $SUITE "
 # echo "${TEST_ACTIVITY}_${NODE_TYPE}_spec_${N}"
  echo $SUITE >> ${TEST_SPEC}${N}

  echo " $(echo "scale = 4;$TESTSUITETIME+$TESTSPECTIME" | bc -l) " > $K
LINE=$( expr $LINE - 1 )
done
else

 cat tmp_list | cut -f5- -d\|   >> ${TEST_SPEC}1
fi
while [ $NN -gt 0 ]
do
 echo "{cases, \"/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/test/suites/install\",fetch_esi_SUITE , [all]}." >> ${TEST_SPEC}${NN}
  NN=$( expr $NN - 1 )
done
cat ${TEST_SPEC}*
rm -f *_TESTSPECS_TIME

rm -f tmp_list

#el
fi

########################################################################################
# CREATE PROP FILE
########################################################################################
DYNAMIC="true"
N=$NUMBER_OF_TESTSPECS

if  [ "$CONFIG2" != "" ]; then
  CONFIG1=$CONFIG2
fi

if [ "$CONFIG1" = "no" ] && [ "$TMP_CONFIG" != "" ]; then 
  STP_LABEL="${NODE_TYPE}&&${TMP_CONFIG}"

elif [ "$CONFIG1" = "no" ]; then 
  STP_LABEL="${NODE_TYPE}"

elif [ "$CONFIG1" != "no" ] && [ "$TMP_CONFIG" != "" ]; then 
  STP_LABEL="${NODE_TYPE}&&${CONFIG1}&&${TMP_CONFIG}"
else
  STP_LABEL="${NODE_TYPE}&&${CONFIG1}"
fi
rm -f spec_all
while [ $N -gt 0 ]
do
  cat ${TEST_SPEC}${N} >> spec_all
  echo "$CREATE_PROP $N  ${TEST_SPEC}${N} ${TARGET_TYPE} ${STP_LABEL} ${UPGRADE} ${DYNAMIC} ${TEST_TIMEOUT} ${CONTAINER} ${UPGRADE_CONTAINER}  "
  $CREATE_PROP $N  ${TEST_SPEC}${N} ${TARGET_TYPE} ${STP_LABEL} ${UPGRADE} ${DYNAMIC} ${TEST_TIMEOUT} ${CONTAINER} ${UPGRADE_CONTAINER}  
  N=$( expr $N - 1 )
done



########################################################################################
fi
fi
fi

EX_TIME_STOP=$(date +"%Y:%m:%d_%H:%M:%S")
echo ">>>>>>>>>>>>>>>> START	$EX_TIME_START"
echo ">>>>>>>>>>>>>>>> STOP	$EX_TIME_STOP"
echo ">>>>>>>>>>>>>>>> END $0 <<<<<<<<<<<<<<<<<<<<<<<<<<<"

#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2014 - 2016 All rights reserved.
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
## -          2016-04-28   etxjovp     Created




######################################################################################
#   MAIN
######################################################################################
while getopts s:m:a:u:c:t:S:C:U:T:F:I: option
do
	case "$option"
	in
                s) NODE_TYPE=$OPTARG
                   ;;
                m) MAXTIME_MIN=$OPTARG
                   ;;
                a) TEST_ACTIVITY=$OPTARG
                   ;;
                u) UPGRADE=$OPTARG
                   ;;
                c) CONFIG1=$OPTARG
                   ;;
                t) TARGET_TYPE=$OPTARG
                   ;;
                S) ST_LIST=$OPTARG
                   ;;
                C) CONTAINER=$OPTARG
                   ;;
                U) UPGRADE_CONTAINER=$OPTARG
                   ;;  
                T) TEST_TIMEOUT=$OPTARG
                   ;;  
                I) INSTANCES=$OPTARG
                   ;;  
                F) FILE=$OPTARG
                   ;;           
		\?) usage
		   exit 1;; 
	esac
done 


EX_TIME_START=$(date +"%Y:%m:%d_%H:%M:%S")
echo "${ST_LIST}"
if [ -f $ST_LIST ] || [ "${ST_LIST}" = "def" ]; then
########################################################################################
# CREATE TESTSPECS FILE
########################################################################################

CREATE_PROP="$RDE_TOP/test/tools/jenkins/rcs_create_properties.sh"

echo "--------------------------------------------------------------------
NODE_TYPE		:$NODE_TYPE
MAXTIME_MIN		:$MAXTIME_MIN
TEST_ACTIVITY		:$TEST_ACTIVITY
UPGRADE			:$UPGRADE	
CONFIG1			:$CONFIG1	
TARGET_TYPE		:$TARGET_TYPE
ST_LIST			:$ST_LIST
TEST_TIMEOUT		:$TEST_TIMEOUT
CONTAINER		:$CONTAINER
UPGRADE_CONTAINER	:$UPGRADE_CONTAINER
--------------------------------------------------------------------"
rm -f tmp_list
if [ "$UPGRADE" = "" ]; then
  UPGRADE="false"
fi
if [ "$UPGRADE" = "false" ] || [ "$UPGRADE" = "" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}"  | grep -v ":onlyupgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE=""
elif [ "$UPGRADE" = "true" ] ; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}" | grep  ":upgrade" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="upgrade"
elif [ "$UPGRADE" = "true:short" ]; then
  cat $ST_LIST | grep -v "#" | grep ":${NODE_TYPE}"| grep ":upgrade:short" | sort -k1 -n -r > tmp_list
  CONFIG_TYPE="upgrade"
else
 exit 1
fi
ls -l tmp_list
if [ "$(cat tmp_list | wc -l | awk '{print $1}' )" = "0" ]; then
exit
fi
########################################################################################


if [ "$CONFIG_TYPE" != "" ] && [ "$CONFIG1" = "" ] ; then
    TEST_SPEC="${TEST_ACTIVITY}_${NODE_TYPE}_${CONFIG_TYPE}_spec_"
elif [ "$CONFIG_TYPE" != "" ] && [ "$CONFIG1" != "" ] ; then
    TEST_SPEC="${TEST_ACTIVITY}_${NODE_TYPE}_${CONFIG1}_${CONFIG_TYPE}_spec_"
elif [ "$CONFIG_TYPE" = "" ] && [ "$CONFIG1" != "" ] ; then
    TEST_SPEC="${TEST_ACTIVITY}_${NODE_TYPE}_${CONFIG1}_spec_"  
else
    TEST_SPEC="${TEST_ACTIVITY}_${NODE_TYPE}_spec_"  
fi



echo "--------------------------------------------------------------------
TEST_SPEC		:$TEST_SPEC	
--------------------------------------------------------------------"

echo " ${TEST_SPEC}"
rm -f ${TEST_SPEC}*
########################################################################################
echo "# START preparations for adaptive creation of test spec"
########################################################################################


NEWDTIMELIST=$FILE

echo "NEWDTIMELIST=$NEWDTIMELIST"
if [ -f tmp_list ] && [ -f $NEWDTIMELIST ] && [ "$NEWDTIMELIST" != "" ]; then
echo "####"
rm -f tmp_list_new
LINE=$(wc -l tmp_list | awk '{print $1}')

while [ $LINE -gt 0 ]
do
    CASE_LIST=""
    ROW=$(tail -$LINE tmp_list | head -1 )
    OLDTIME=$(echo $ROW | awk '{print $1}')
    SUITE=$(echo $ROW | cut -f5- -d\| | cut -f3 -d,)
    GROUP=$(echo $ROW | cut -f5- -d\| | cut -f4 -d, | cut -f1 -d\} | grep -v "]" |  awk '{print $1}')
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
if [ ! -f tmp_tmp ]; then
  exit 1
fi
ls -l tmp_tmp

cat tmp_tmp | sort -k1 -n -r > tmp_list_new
ls -l tmp_list_new
rm -f tmp_tmp
########################################################################################
# activate adaptive creation of test spec 
########################################################################################
echo "#####################################################################"
echo " Activate adaptive creation of test spec   ! "
echo "#####################################################################"
cat tmp_list_new > tmp_list
ls -l tmp_list

########################################################################################
rm -f tmp_list_new
fi

echo "#####################################################################"
cat tmp_list
echo "#####################################################################"
########################################################################################
TOT_TIME=0
LINE=$(wc -l tmp_list | awk '{print $1}')
echo $LINE
########################################################################################
# CREATE TESTSPECS 
########################################################################################
if [ "$LINE" != "0" ]; then
if [ "$INSTANCES" = "" ]; then

MAXTIME=$( expr $MAXTIME_MIN \* 60 )
for i in $(cat tmp_list | cut -f1 -d\| )
do
  TOT_TIME=$(echo "scale = 4;$TOT_TIME+$i" | bc -l)
done
if [ "$(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l)" = "1" ]; then
   NUMBER_OF_TESTSPECS=$(expr $(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l) + 1 )
else
   NUMBER_OF_TESTSPECS=$(expr $(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l) + 1 )
fi
else
   echo " FIXT NUMBER OF TESTSPECS $INSTANCES   "
   NUMBER_OF_TESTSPECS=$INSTANCES
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

  echo $SUITE >> ${TEST_SPEC}${N}

  echo " $(echo "scale = 4;$TESTSUITETIME+$TESTSPECTIME" | bc -l) " > $K
LINE=$( expr $LINE - 1 )
done
else

 cat tmp_list | cut -f5- -d\|   >> ${TEST_SPEC}1
fi
while [ $NN -gt 0 ]
do
 echo "{cases, \"test/test/suites/install\",fetch_esi_SUITE , [all]}." >> ${TEST_SPEC}${NN}
  NN=$( expr $NN - 1 )
done
cat ${TEST_SPEC}*
rm -f *_TESTSPECS_TIME

rm -f tmp_list


fi




########################################################################################
# CREATE PROP FILE
########################################################################################
DYNAMIC="true"
N=$NUMBER_OF_TESTSPECS

if [ "$CONFIG1" = "no" ] || [ "$CONFIG1" = "" ]; then 
  STP_LABEL="${NODE_TYPE}"
else
  STP_LABEL="${NODE_TYPE}&&${CONFIG1}"
fi

while [ $N -gt 0 ]
do
  echo "$CREATE_PROP $N  ${TEST_SPEC}${N} ${TARGET_TYPE} ${STP_LABEL} ${UPGRADE} ${DYNAMIC} ${TEST_TIMEOUT} ${CONTAINER} ${UPGRADE_CONTAINER}  "
  $CREATE_PROP $N  ${TEST_SPEC}${N} ${TARGET_TYPE} ${STP_LABEL} ${UPGRADE} ${DYNAMIC} ${TEST_TIMEOUT} ${CONTAINER} ${UPGRADE_CONTAINER}  
  N=$( expr $N - 1 )
done



########################################################################################
fi



EX_TIME_STOP=$(date +"%Y:%m:%d_%H:%M:%S")
echo ">>>>>>>>>>>>>>>> START	$EX_TIME_START"
echo ">>>>>>>>>>>>>>>> STOP	$EX_TIME_STOP"
echo ">>>>>>>>>>>>>>>> END $0 <<<<<<<<<<<<<<<<<<<<<<<<<<<"

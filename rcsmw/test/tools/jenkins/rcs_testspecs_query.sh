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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2015-06-10   etxjovp     Created


######################################################################################
#   MAIN
######################################################################################


while getopts d:a:g:s:b:c:t:m:T: option
do
	case "$option"
	in
                d) DO=$OPTARG
                   ;;
                a) ACTIVITY=$OPTARG
                   ;;
		g) NEXT=$OPTARG                  
                   ;;
                s) SUITE_PATH=$OPTARG
                   ;;
                b) BRANCH=$OPTARG
                   ;;
		c) CXS=$OPTARG                  
                   ;;
                t) TEST_SPEC_1=$OPTARG
                   ;;
                m) MAXTIME_MIN=$OPTARG
                   ;;
                T) TARGET=$OPTARG
                   ;;
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done

if [ "$USER" = "rcsci1" ] || [ "$DO" = "get_test_suite_time" ] || [ "$DO" = "save_test_suite_time" ]; then
  TARGET1=$( echo $TARGET | sed 's/[0-9]/ /g' | awk '{print $1}'  | sed 's/_//g')
  mkdir -p /proj/rcs/rbs_cs_ci_statistics/db/spec_time/$BRANCH/${TARGET1}
  TEST_TIME_FILE=/proj/rcs/rbs_cs_ci_statistics/db/spec_time/$BRANCH/${TARGET1}/test_time.txt
else
  TEST_TIME_FILE=$PWD/${BRANCH}${TARGET1}_test_time.txt
fi
  TEST_TIME_FILE_TMP=$PWD/test_time_tmp.txt
  touch $TEST_TIME_FILE

#echo $TEST_TIME_FILE
   \rm -f $TEST_TIME_FILE_TMP
##########################################################################################################################
##########################################################################################################################
if [ "$DO" = "create_testspec" ]; then
   MAXTIME=$( expr $MAXTIME_MIN \* 60 )
   TEST_SPEC=$TEST_SPEC_1
   NUMBER_OF_TESTSPECS="0"
  echo " "
  rm -f ${TEST_SPEC}*
  rm -f .suites_list*
  touch .suites_list
  $0 -a $ACTIVITY -d get_test_suites -b $BRANCH  -T $TARGET -s "$SUITE_PATH" -g "$NEXT"  > .suites_list 
rm -f tmp_list
  if [ -f tmp_list ]; then
    cat tmp_list | cut -f5- -d\| > tmp_list_new
    echo "####################################################"
    cat .suites_list 
    echo "####################################################"
    LINE1=$(wc -l tmp_list_new | awk '{print $1}') 
    if [ "$LINE1" != "0" ]; then
      while [ $LINE1 -gt 0 ]
      do
        ROW1=$(tail -$LINE1 tmp_list_new | head -1 )
        #echo $ROW1 | awk '{print $2}' | sed 's/,//g' | sed 's/\"//g'
        SUITE1=$(echo $ROW1 | awk '{print $3}' | sed 's/,//g')
        if [ "$(cat .suites_list | grep $SUITE1 )" = "" ]; then
         echo $ROW1 >> .suites_list 

        fi
        LINE1=$( expr $LINE1 - 1 )
      done
    fi
  fi
   # cat .suites_list 
  LINE=$(wc -l .suites_list | awk '{print $1}') 
  if [ "$LINE" != "0" ]; then
  while [ $LINE -gt 0 ]
  do
    ROW=$(tail -$LINE .suites_list | head -1 )
    SG=$(echo $ROW | cut -f3- -d, | cut -f1 -d\} | sed 's/,/./' | sed 's/ //g')
    echo "$0 -d get_test_suite_time -b $BRANCH -T $(echo $TARGET | cut -f2- -d_) -s $SG "
    TT=$($0 -d get_test_suite_time -b $BRANCH -T $(echo $TARGET | cut -f2- -d_) -s $SG )
echo "$TT $SG "
    echo "$TT $SG " >> .suites_list_time
    LINE=$( expr $LINE - 1 )
  done
  cat .suites_list_time | sort -k1 -n -r > .suites_list_time_new
  echo " "
 # cat .suites_list_time_new

 TOT_TIME=0
 for i in $(cat .suites_list_time_new | awk '{print $1}' )
 do
   TOT_TIME=$(echo "scale = 4;$TOT_TIME+$i" | bc -l)
 done

 if [ "$(echo "scale = 0;$TOT_TIME/$MAXTIME" | bc -l)" = "1" ]; then
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
  LINE=$(wc -l .suites_list_time_new | awk '{print $1}') 
while [ $LINE -gt 0 ]
do
  ROW=$(cat .suites_list_time_new | tail -$LINE | head -1)
  S=$(echo $ROW | awk '{print $2}' | cut -f1 -d.)
  G=$(echo $ROW | awk '{print $2}' | cut -f2- -d.)
  TESTSUITETIME=$(echo $ROW | awk '{print $1}' )
#echo $S $G
#echo "#####"
  #cat .suites_list | grep $S | grep $G
  SUITE=$(cat .suites_list | grep $S | grep $G )

  TESTSPECTIME=$(egrep ""  *_TESTSPECS_TIME |  sort -k2 -n -r  | tail -1 | cut -f2- -d: )
  K=$(egrep ""  *_TESTSPECS_TIME |  sort -k2 -n -r  | tail -1 | cut -f1 -d: )
  N=$(egrep ""  *_TESTSPECS_TIME |  sort -k2 -n -r  | tail -1 | cut -f1 -d: | cut -f1 -d_)
  echo " $N	K $K 	TESTSPECTIME $TESTSPECTIME TESTSUITETIME $TESTSUITETIME	SUITE $SUITE "
  echo $SUITE >> ${TEST_SPEC}${N}

  echo " $(echo "scale = 4;$TESTSUITETIME+$TESTSPECTIME" | bc -l) " > $K
LINE=$( expr $LINE - 1 )
done
else

 cat .suites_list    >> ${TEST_SPEC}1
fi
while [ $NN -gt 0 ]
do
 echo "{cases, \"${RCT_TOP}/test/suites/install\",fetch_esi_SUITE , [all]}." >> ${TEST_SPEC}${NN}
  NN=$( expr $NN - 1 )
done
echo "############################################################################################################"
cat ${TEST_SPEC}*
fi
rm -f *_TESTSPECS_TIME

rm -f .suites_list 

echo ${TEST_SPEC} > .t
echo $NUMBER_OF_TESTSPECS > .n
##########################################################################################################################
##########################################################################################################################
elif [ "$DO" = "get_test_suites" ]; then
LIST=""
   for j in $SUITE_PATH
    do
       LIST="$LIST $(ls $j)"
    done
GREP_PATH=""
   if [ "$NEXT" != "" ]; then
     GREP_PATH="__${NEXT}__"
     #echo $GREP_PATH
     #echo "${ACTIVITY}__   ${GREP_PATH}   __${TARGET}__"
     for k in $LIST
     do
       GROUP_LIST=$(cat $k | grep -v "%" | grep "${ACTIVITY}__" | grep "${GREP_PATH}" | grep "__${TARGET}__"| sed 's/{//' | sed 's/,/ /' | awk '{print $1}')
       if [ "$GROUP_LIST" = "" ]; then
          GROUP_LIST=$(cat $k | grep -v "%" | grep "${ACTIVITY}__" | grep "${GREP_PATH}" | grep "__all__" | sed 's/{//' | sed 's/,/ /' | awk '{print $1}') 
       fi
    if [ "$GROUP_LIST" != "" ]; then
      for j in $GROUP_LIST
       do
        #echo $k:$j
        if [ "$(cat  $k | grep $j | cut -f3- -d\[ | sed 's/ //g' | cut -f1 -d\] )" != "" ]; then
        echo "{groups, \"$(dirname $k)\", $(basename $(echo $k | cut -f1 -d.)), $j}."
        fi
      done
    fi
   done
  fi

##########################################################################################################################
##########################################################################################################################
elif [ "$DO" = "save_test_suite_time" ]; then
   TEST_SPEC=$TEST_SPEC_1
echo $TEST_TIME_FILE
   #TEST_SPEC=$(echo $TEST_SPEC_1 | sed 's/spec/spec  /' | awk '{print $1}')
   echo $TEST_SPEC

   if [ "$ACTIVITY" = "sdc" ]; then
      SUITE=$(ls /proj/rcs-tmp/stps/*/${CXS}/TESTUSER/rcsci1/${TEST_SPEC}/*_*/ct_*/*_SUITE*ogs/run*/suite.log  | grep ${TEST_SPEC}  | grep -v install_dus_SUITE | grep -v check_after_install_SUITE ) 
   else
      SUITE=$(ls /proj/rcs-tmp/stps/*/${CXS}/${TEST_SPEC}/config_*/ct_*/*_SUITE*ogs/run*/suite.log  | grep ${TEST_SPEC}  | grep -v install_dus_SUITE | grep -v check_after_install_SUITE ) 
   fi
   #echo $SUITE
   echo "################################################################################################"
   
   TOT="0"
   for i in $SUITE 
   do
      #echo $i
      S=$(echo $i | cut -f6- -d. | cut -f1 -d\/)
      #echo $S
      GT_LIST=$(cat $i | grep group_time | awk '{print $2}' | cut -f1 -ds | tail -1)
      echo ">> $GT_LIST	$S"
      T="0"
      for j in $GT_LIST 
      do
         T=$(echo "scale = 4;$T+$j" | bc -l)
      done
      if [ "$(cat $i| tail -4 | grep -v "=successful" | awk '{print $2}' | sort -u )" = "0" ]; then
      cat $TEST_TIME_FILE | grep -v $S > $TEST_TIME_FILE_TMP
      echo $S $T >> $TEST_TIME_FILE_TMP
      cat $TEST_TIME_FILE_TMP > $TEST_TIME_FILE
      TOT=$(echo "scale = 4;$TOT+$T" | bc -l)
      else

        echo "################################################################################################"
        echo "FAILED $i"
        echo "################################################################################################"
      fi
    done
    echo $TOT
    M=$(echo "scale = 4;$TOT/60" | bc -l)
    echo "################################################################################################"
    echo TOT $M min
    echo "################################################################################################"
##########################################################################################################################
##########################################################################################################################
elif [ "$DO" = "get_test_suite_time" ]; then
   if [ "$NEXT" = "" ]; then
      NEXT=".logs"
   fi 
  TEST_SUITE_TIME=$(cat $TEST_TIME_FILE | grep $SUITE_PATH | grep $NEXT | awk '{print $2}')
  if [ "$TEST_SUITE_TIME" = "" ]; then
      N=$(cat $TEST_TIME_FILE | awk '{print $2}' | wc -l | awk '{print $1}')
      T_LIST=$(cat $TEST_TIME_FILE |  grep -v "XL." |  grep -v ".xl" | awk '{print $2}')
       T_S=0
       for i in $T_LIST
       do
         T_S=$(echo "scale = 4;$T_S+$i" | bc -l)

       done
       M_T=$(echo "scale = 4;$T_S/$N" | bc -l)
       echo $M_T
      
  else
     echo $TEST_SUITE_TIME
  fi 

##########################################################################################################################
##########################################################################################################################

elif [ "$DO" = "cluster_du" ]; then
   CLUSTER=$(echo $TEST_SPEC_1 | grep cluster | sed 's/cluster_/ /' | sed 's/_spec/ /' | awk '{print $2 }' )
   N=$(echo $CLUSTER | cut -f3 -d_ | sed 's/[a-z]//g')
   if [ "$CLUSTER" != "" ] && [ "$N" != "" ]; then
     echo "du${N}"

   fi
   #sbc_cluster_dual_sim_2_spec_1


fi





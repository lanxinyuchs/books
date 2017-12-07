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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2014-02-17   etxjovp     Created
BRANCH=$1
TEST_SPEC=$2
CXS=$3
STP=$4
NODE_TYPE=$5
if [ "$USER" = "rcsci1" ]; then
  # Make STATISTICS_PATH parameterizable, otherwise the JobDSL
  # regression tests may overwrite the test time files, if it is set to run this chunk of code.
  STATISTICS_PATH=${STATISTICS_PATH-/proj/5G_rcs/rbs_cs_ci_statistics/spec_time}
else
  STATISTICS_PATH=$HOME/spec_time
  mkdir -p $STATISTICS_PATH
fi
# Work with a copy of test_time.txt before it's completely ready.
# Add CXS as unique identifier at end of file name
# In this way we can have several jobs that modifies it in parallel
TEST_TIME_FILE=$STATISTICS_PATH/$BRANCH/${NODE_TYPE}/test_time.txt
TEST_TIME_FILE_COPY=${TEST_TIME_FILE}.${CXS}
mkdir -p $STATISTICS_PATH/$BRANCH
mkdir -p $STATISTICS_PATH/$BRANCH/${NODE_TYPE}
TEST_TIME_FILE_TMP=$PWD/test_time_tmp.txt
rm -f $TEST_TIME_FILE_TMP
cp $TEST_TIME_FILE $TEST_TIME_FILE_COPY
echo $TEST_TIME_FILE_COPY
echo $TEST_SPEC
cp $TEST_TIME_FILE_COPY test_time.txt
#/proj/rcs-tmp/stps/rcf008/123456/SDC_rcf_no_spec_4/config_SDC_rcf_no_spec_4/ct_run.rcf008@esekilxv7152.2016-04-29_10.06.31/index.html  
SUITE=$(ls /proj/rcs-tmp/stps/${STP}/${CXS}/${TEST_SPEC}/config_*/ct_*/*_SUITE*ogs/run*/suite.log  | grep ${TEST_SPEC}  | grep -v install_dus_SUITE | grep -v check_after_install_SUITE )
echo $SUITE
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
      cat $TEST_TIME_FILE_COPY | grep -v $S > $TEST_TIME_FILE_TMP
      echo $S $T >> $TEST_TIME_FILE_TMP
      cat $TEST_TIME_FILE_TMP > $TEST_TIME_FILE_COPY
      TOT=$(echo "scale = 4;$TOT+$T" | bc -l)
      else

        echo "################################################################################################"
        echo "FAILED $i"
        echo "################################################################################################"
      fi
done
echo $TOT
M=$(echo "scale = 4;$TOT/60" | bc -l)
diff  $TEST_TIME_FILE_COPY test_time.txt
cat $TEST_TIME_FILE_COPY | wc -l
echo "################################################################################################"
echo TOT $M min
echo "################################################################################################"

# Swap over to new version of test time file with atomic operation
mv ${TEST_TIME_FILE_COPY} ${TEST_TIME_FILE}

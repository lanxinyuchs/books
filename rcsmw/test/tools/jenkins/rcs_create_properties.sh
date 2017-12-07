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
## -          2014-01-28   etxjovp     Created
## /main/20   2014-11-25   etxjovp     Update for CI 4.0
N=$1
TEST_SPEC=$2
TARGET_TYPE=$3
STP_LABEL=$4
UPGRADE=$5
DYNAMIC=$6
TEST_TIMEOUT=$7
CONTAINER=$8
UPGRADE_CONTAINER=$9


echo "
	$0
____________________________________________________________________________________
N			:$N
TEST_SPEC		:$TEST_SPEC
TARGET_TYPE		:$TARGET_TYPE
STP_LABEL		:$STP_LABEL
UPGRADE			:$UPGRADE
DYNAMIC			:$DYNAMIC
TEST_TIMEOUT		:$TEST_TIMEOUT
CONTAINER		:$CONTAINER
UPGRADE_CONTAINER	:$UPGRADE_CONTAINER
TEST_TYPE		:$TEST_TYPE"

if [ "$USER" =  "etxjovp" ]; then
WORKSPACE="$PWD"
fi
PROPERTIES_FILE=properties_$(basename $TEST_SPEC)_${N}

if [ "$DYNAMIC" != "true" ]; then
  PROPERTIES_FILE=properties_$(basename $TEST_SPEC)_${N}
  rm -f ${WORKSPACE}/$(basename $TEST_SPEC)_${N}
  cp $TEST_SPEC ${WORKSPACE}/$(basename $TEST_SPEC)_${N} 
  chmod 666 ${WORKSPACE}/$(basename $TEST_SPEC)_${N}
  TEST_SPEC=$(basename $TEST_SPEC)_${N}
else

   PROPERTIES_FILE=properties_$(basename $TEST_SPEC)
fi

echo "PROPERTIES_FILE		:$PROPERTIES_FILE
TEST_SPEC		:$TEST_SPEC
"

echo "____________________________________________________________________________________"
LABEL1=$(echo $STP_LABEL | sed 's/\&\&/ /g' | sed 's'/\|\|'/ /g'  | awk '{print $2}' )
LABEL2=$(echo $STP_LABEL | sed 's/\&\&/ /g' | sed 's'/\|\|'/ /g'  | awk '{print $3}' )
STP_LABEL=$(echo $STP_LABEL | sed 's/\&\&/ /g' | sed 's'/\|\|'/ /g'  | awk '{print $1}')
if [ "$LABEL1" = "" ] || [ "$LABEL1" = "no" ]; then
LABEL1="l"
fi
if [ "$LABEL2" = "" ]; then
   if [ "$TEST_TIMEOUT" = "9000" ] ; then
      LABEL2="xxl" 
   elif [ "$TEST_TIMEOUT" = "60" ] || [ "$TEST_TIMEOUT" = "30" ] || [ "$TEST_TIMEOUT" = "61" ]; then
      LABEL2="l" 
   else
      LABEL2="xl" 
   fi
fi
if [ "$LABEL1" = "wrat" ] || [ "$LABEL1" = "grat" ] || [ "$LABEL1" = "lrat" ]; then
LABEL2="ru" 
fi
echo "LSV_BUILD_NUMBER=${LSV_BUILD_NUMBER}" >  ${WORKSPACE}/$PROPERTIES_FILE
echo "LSV_BUILD_NUMBER_UPGRADE=${LSV_BUILD_NUMBER_UPGRADE}" >>  ${WORKSPACE}/$PROPERTIES_FILE
if [ "$TEST_ACTIVITY" = "" ]; then
echo "TEST_ACTIVITY=${JOB_NAME}" >>  ${WORKSPACE}/$PROPERTIES_FILE
else
echo "TEST_ACTIVITY=${TEST_ACTIVITY}" >>  ${WORKSPACE}/$PROPERTIES_FILE
fi
echo "TEST_ACTIVITY_NUMBER=${BUILD_NUMBER}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "TEST_SPEC=$(basename $TEST_SPEC)" >>  ${WORKSPACE}/$PROPERTIES_FILE		
echo "STP_LABEL=${STP_LABEL}" >>  ${WORKSPACE}/$PROPERTIES_FILE	
echo "LABEL1=${LABEL1}" >>  ${WORKSPACE}/$PROPERTIES_FILE	
echo "LABEL2=${LABEL2}" >>  ${WORKSPACE}/$PROPERTIES_FILE	
echo "BRANCH=${BRANCH}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "TEST_TYPE=${TEST_TYPE}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "CXS=${CXS}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "CXP=${CXP}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "UPGRADE_FROM_CXP=${UPGRADE_FROM_CXP}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "UPGRADE=${UPGRADE}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "TEST_TIMEOUT=${TEST_TIMEOUT}" >>  ${WORKSPACE}/$PROPERTIES_FILE
echo "CONTAINER=${CONTAINER}" >>  ${WORKSPACE}/$PROPERTIES_FILE
if [ "$UPGRADE_CONTAINER" != "" ]; then
echo "UPGRADE_FROM_CONTAINER=${UPGRADE_CONTAINER}" >>  ${WORKSPACE}/$PROPERTIES_FILE
fi
cat ${WORKSPACE}/$PROPERTIES_FILE
#ls -l 



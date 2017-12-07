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
CXS=$2
TEST_SPEC_1=$3
LSV_NO=$4

#CIRESULTS="/home/etxjovp/CI_3.0/ciresults_branch/${BRANCH}"
if [ "$TEST_TYPE" = "no" ] || [ "$TEST_TYPE" = "" ] || [ "$TEST_TYPE" = "A" ]; then
CIRESULTS="/proj/webdocs/rbs-rde-ci/root/ciresults_branch/${BRANCH}"
else
CIRESULTS="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults_branch/${BRANCH}"
fi
TEST_SPEC=$(echo $TEST_SPEC_1 | sed 's/spec/spec  /' | awk '{print $1}')
echo $TEST_SPEC
#SUITE=$(ls /proj/rcs-tmp/stps/*/${CXS}/${TEST_SPEC}_*/config_*/ct_*/*_SUITE*ogs/run*/suite.log  | grep ${TEST_SPEC} | grep -v upgrade_spec | grep -v install_dus_SUITE | grep -v check_after_install_SUITE ) 
SUITE=$(ls /proj/rcs-tmp/stps/*/${CXS}/${TEST_SPEC}_*/config_*/ct_*/*_SUITE*ogs/run*/suite.log  | grep ${TEST_SPEC}  | grep -v install_dus_SUITE | grep -v check_after_install_SUITE ) 
echo $SUITE
N=$TESTSPECS

#for jk in $SUITE 
#do
#echo $jk
#done
echo "################################################################################################"

\rm -f $PWD/$USER$$
TOT="0"
for i in $SUITE 
do
S=$(echo $i | cut -f6- -d. | cut -f1 -d\/)
GT_LIST=$(cat $i | grep group_time | awk '{print $2}' | cut -f1 -ds | tail -1)

T="0"
#echo ">-----------------------------------------------------------------------------------------"
#echo $GT_LIST
#echo "<-----------------------------------------------------------------------------------------"
for j in $GT_LIST 
do
 T=$(echo "scale = 4;$T+$j" | bc -l)
done
#echo "> $S $T"
echo $S $T >> $PWD/$USER$$
TOT=$(echo "scale = 4;$TOT+$T" | bc -l)

done
cat $PWD/${USER}$$ | sort -nk2,2 | sort -k1,1 -u
if [ "$USER" = "rcsci1" ]; then
echo "${CIRESULTS}/${LSV_NO}/${TEST_SPEC}_time.txt"
ls -l $PWD/${USER}$$
 cat $PWD/${USER}$$ | sort -nk2,2 | sort -k1,1 -u > ${CIRESULTS}/${LSV_NO}/${TEST_SPEC}_time.txt
fi
echo $TOT
M=$(echo "scale = 4;$TOT/60" | bc -l)
echo "################################################################################################"
echo TOT $M min
echo "################################################################################################"

\rm -f $PWD/$USER*$$

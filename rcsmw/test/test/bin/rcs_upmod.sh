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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R2A/1      2013-09-13   etxjovp     Created
## R2A/2      2013-09-13   etxjovp     add rm 
## R2A/4      2014-01-10   etxjovp     add --no-check-certificate
## ----------------------------------------------------------
usage()
{
echo "
                -u dc hc 
                   
                -m rbs lrat wrat grat tcu_03 .......
                   
		-c R2A2066
                   
		-h) help

Ex:
rcs_upmod.sh -u dc -m tcu_03    
rcs_upmod.sh -u dc -m lrat 
rcs_upmod.sh -u hc -m lrat -c R2A2066

"
}
progname="`basename $0`"
CIRESULT="/proj/webdocs/rbs-rde-ci/root/ciresults"
LATEST_RBS_HC_URL="https://rbs-g2-ci.rnd.ki.sw.ericsson.se/job/HourlyCheckFlow/lastSuccessfulBuild/parameters/"
LATEST_TCU_03_HC_URL="https://rbs-g2-ci.rnd.ki.sw.ericsson.se/job/HourlyCheckTCU03/lastSuccessfulBuild/parameters/"
RCS_CONTAINER_URL="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/RCS_CXP9021221_1/doc/19010/RCS_CXP9021221_1.cxp@@/CXP9021221_1-"
######################################################################################
#   MAIN
######################################################################################
while getopts ht:u:m:c: option
do
	case "$option"
	in
                u) UP_TYPE=$OPTARG # dc hc 
                   ;;
                m) MODIFY=$OPTARG  # rbs lrat wrat grat tcu_03 .......
                   ;;
		c) RCS_REV=$OPTARG # R2A2066
                   ;;
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done

if [ "$#" = "0" ]; then
  usage
exit
fi
if [ "$MODIFY" != "rbs" ] && [ "$MODIFY" != "lrat" ] && [ "$MODIFY" != "grat" ] && [ "$MODIFY" != "wrat" ] && [ "$MODIFY" != "tcu_03" ]; then
   echo " EXIT $MODIFY not exist"
   exit
fi



if [ "$UP_TYPE" = "hc" ]; then

    if [ "$MODIFY" = "tcu_03" ]; then

      echo "${progname}: Fetching latest HC UP URL from $LATEST_TCU_03_HC_URL"
      NODE_CI_CONTAINER=$(curl --silent $LATEST_TCU_03_HC_URL | grep tcu_03 | grep up | cut -f6 -d\")
    else
      echo "${progname}: Fetching latest HC UP URL from $LATEST_RBS_HC_URL"
      NODE_CI_CONTAINER=$(curl --silent $LATEST_RBS_HC_URL | grep ms_rbs | grep up | cut -f6 -d\")
    fi
elif [ "$UP_TYPE" = "dc" ]; then
    if [ "$MODIFY" = "tcu_03" ]; then
      NODE_TYP="rbs"
    else
      NODE_TYP="tcu"
    fi
    LATES_PATH=$(egrep DC-BuildUP $CIRESULT/*/node_ci_dc_${NODE_TYP}_container | tail -1 )
    echo "${progname}: Fetching latest DC UP URL from $(echo $LATES_PATH | cut -f1 -d: )"
    NODE_CI_CONTAINER=$(echo $LATES_PATH | cut -f2- -d: )

else
   echo " EXIT $UP_TYPE not exist"
   usage
   exit

fi


if [ "$USER" = "rcsci1" ]; then
   TMPDIR="/tmp/$pwd/rcs_mod"
else
   mkdir -p /tmp/$USER/
   TMPDIR="/tmp/$USER/rcs_mod"
fi
mkdir -p $TMPDIR
\rm -rf $TMPDIR/*
mkdir -p $TMPDIR/tmpdir
UP_NAME="${UP_TYPE}_${MODIFY}.tgz"
cd $TMPDIR
echo "${progname}: Fetching UP from  $NODE_CI_CONTAINER  to ${PWD}/tmp.tgz"

wget -nd -q --no-proxy --no-check-certificate --output-document tmp.tgz $NODE_CI_CONTAINER  -t 5 -T 120
cd $TMPDIR/tmpdir

tar -xzf $TMPDIR/tmp.tgz  
\rm -rf $TMPDIR/tmp.tgz
XML_FILE=$(ls *.xml | grep up )
OLD_RCS_REV=$(cat $XML_FILE | grep '\"RCS' | cut -f4 -d= | cut -f2 -d\")
if [ "$MODIFY" = "rbs" ] || [ "$MODIFY" = "tcu_03" ]; then
    cat $XML_FILE  > $TMPDIR/tmp.xml
elif [ "$MODIFY" = "lrat" ]; then
   echo "${progname}: Remove grat and wrat lm from the UP. Modify $XML_FILE  "
   cat $XML_FILE | grep -v 'GRAT\|WRAT' > $TMPDIR/tmp.xml
   rm -f GRAT*
   rm -f WRAT*
elif [ "$MODIFY" = "wrat" ]; then  
   echo "${progname}: Remove grat and lrat lm from the UP. Modify $XML_FILE  " 
   cat $XML_FILE | grep -v 'GRAT\|LRAT' > $TMPDIR/tmp.xml
   rm -f GRAT*
   rm -f LRAT*
elif [ "$MODIFY" = "grat" ]; then
   echo "${progname}: Remove wrat and lrat lm from the UP. Modify $XML_FILE  " 
   cat $XML_FILE | grep -v 'LRAT\|WRAT' > $TMPDIR/tmp.xml
   rm -f WRAT*
   rm -f LRAT*
else
   echo " EXIT $MODIFY not exist"
   usage
   exit
fi 

if [ "$RCS_REV" != "" ]; then
  RCS_CONTAINER=${RCS_CONTAINER_URL}${RCS_REV}
  echo "${progname}: Fetching RCS from $RCS_CONTAINER"
  
  wget -nd -q --no-proxy --no-check-certificate --output-document RCS_CXP9021221_1.cxp $RCS_CONTAINER  -t 5 -T 120
  OLD_RCS_REV=$(cat $XML_FILE | grep '\"RCS' | cut -f4 -d= | cut -f2 -d\")
  
  echo "${progname}: Replaced RCS $OLD_RCS_REV to $RCS_REV in the UP. Modify $XML_FILE" 
  sed "s/$OLD_RCS_REV/$RCS_REV/" $TMPDIR/tmp.xml  > $TMPDIR/tmp1.xml
  mv $TMPDIR/tmp1.xml $TMPDIR/tmp.xml

fi
mv $TMPDIR/tmp.xml $XML_FILE


echo "${progname}: Building a new UP $TMPDIR/$UP_NAME" 

tar -czf $TMPDIR/$UP_NAME  $(ls)

cd $TMPDIR

 \rm -rf $TMPDIR/tmpdir

echo $TMPDIR/$UP_NAME


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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2016-08-24               Created
set -u
set -x

RCS=$1
REV=$2
REVOC=$3
OUTPUTCXS=$4
R=$(basename $RCS) 
H=$(pwd)

if [ "$USER" = "rcsci1" ]; then
      cd ${WORKSPACE}
      TMPDIR="$PWD/rcs_mod"
      mkdir -p  $PWD/rcs_mod 
      rm -fr $PWD/rcs_mod/*
                              
      TMPDIR="$PWD/rcs_mod"
else
      mkdir -p /tmp/$USER/
      mkdir -p /tmp/$USER/rcs_mod
      rm -fr /tmp/$USER/rcs_mod/* 
     
     TMPDIR="/tmp/$USER/rcs_mod"      
fi

################################################################################################
cd $TMPDIR
RCS_CXP=$(basename $RCS) 
echo "${0}: Download packages"
if [ -f $RCS ]; then
  cp  $RCS .
else
   echo "${0}: wget -nd -q --no-proxy --output-document $RCS_CXP $RCS -t 5 -T 120"
   echo "PWD"
   wget -nd -q --no-proxy --output-document $RCS_CXP $RCS  -t 5 -T 120
fi
#S=$(tar -tvf $RCS_CXP | grep cxs | grep up.xml)
################################################################################################
#if [ "$S" != "" ]; then
cd $TMPDIR
echo "pwd"

source /vobs/rcs/tools/RDE_LXA119945/bin/shModuleInit.sh;
module list&&
module rm openssl&& 
module add openssl/1.1.0d&& 
module list


RCS_CXS=$(basename $RCS) 

cp $RCS_CXS UP1.zip
unzip -p UP1.zip RCS-DUS2_CXP9031275_4.cxp | gunzip -dc - | tee RCS-DUS2_CXP9031275_4.tar | tar -Oxf - RCSEE-DUS2_CXP9025851_3.cxp | gunzip -dc - | tee RCSEE-DUS2_CXP9025851_3.tar | tar xf - RCSEE4_CXC1737262_4

##Copy crl
if [ $REVOC = "revoc" ]; then
	#cp /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/certTestDir/certTestRevoked/certs-test-crl+revoked/* RCSEE4_CXC1737262_4/rcsee4/certs
	cp /home/eransbn/cs/revo/certTestDir/certTestRevoked/certs-test-crl+revoked/* RCSEE4_CXC1737262_4/rcsee4/certs
else
	#cp /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/certTestDir/certTest/certs-test-crl/* RCSEE4_CXC1737262_4/rcsee4/certs
	 cp /home/eransbn/cs/revo/certTestDir/certTest/certs-test-crl/* RCSEE4_CXC1737262_4/rcsee4/certs
fi

tar --delete -f RCSEE-DUS2_CXP9025851_3.tar RCSEE4_CXC1737262_4
tar --update -f RCSEE-DUS2_CXP9025851_3.tar RCSEE4_CXC1737262_4
gzip RCSEE-DUS2_CXP9025851_3.tar
mv RCSEE-DUS2_CXP9025851_3.tar.gz RCSEE-DUS2_CXP9025851_3.cxp
tar --delete -f RCS-DUS2_CXP9031275_4.tar RCSEE-DUS2_CXP9025851_3.cxp
tar --update -f RCS-DUS2_CXP9031275_4.tar RCSEE-DUS2_CXP9025851_3.cxp
gzip RCS-DUS2_CXP9031275_4.tar
mv RCS-DUS2_CXP9031275_4.tar.gz RCS-DUS2_CXP9031275_4.cxp
zip -d UP1.zip RCS-DUS2_CXP9031275_4.cxp
zip UP1.zip RCS-DUS2_CXP9031275_4.cxp
echo $REVOC
if [ $REVOC = "revoc" ]; then
   # /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/certTestDir/cxsremake -o $OUTPUTCXS -c /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/certTestDir/certTestRevoked/create_sig-crl-test2.sh -r $REV UP1.zip
   /home/eransbn/cs/revo/certTestDir/cxsremake -o $OUTPUTCXS -c /home/eransbn/cs/revo/certTestDir/certTestRevoked/create_sig-crl-test2.sh -r $REV UP1.zip
else
    #/vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/certTestDir/cxsremake -o $OUTPUTCXS -c /vobs/rcs/test/RCT_CRX901275/test/suites/SWM/bin/certTestDir/certTest/create_sig-crl-test.sh -r $REV UP1.zip
    /home/eransbn/cs/revo/certTestDir/cxsremake -o $OUTPUTCXS -c /home/eransbn/cs/revo/certTestDir/certTest/create_sig-crl-test.sh -r $REV UP1.zip
fi


################################################################################################
RESULT=$TMPDIR/$OUTPUTCXS

cd $H 
if [ -f $RESULT ]; then
    mv $RESULT .
    ls $OUTPUTCXS
else
    echo "ERROR !!!"
    exit 1
fi











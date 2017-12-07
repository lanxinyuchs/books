#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	rcsapp.sh %
# %CCaseRev:	/main/R2A/R3A/R6A/R7A/R10A/R12A/3 %
# %CCaseDate:	2017-12-01 %
# Author:	etxarnu/etxbjca
#
# Short description:  Wrapper script to start a program with same anvironment
#                     as if started from MW.
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2013-2017 All rights reserved.
# 
# The information in this document is the property of Ericsson.
# 
# Except as specifically authorized in writing by Ericsson, the 
# receiver of this document shall keep the information contained 
# herein confidential and shall protect the same in whole or in 
# part from disclosure and dissemination to third parties.
# 
# Disclosure and disseminations to the receivers employees shall 
# only be made on a strict need to know basis.
# %CCaseCopyrightEnd%
#
#----------------------------------------------------------------------
# #1.    REVISION LOG
#----------------------------------------------------------------------
# Rev      Date       Name        What
# -----    -------    --------    -------------------------------------
# R2A/1  2013-04-05 etxarnu     First version in SYS
# R2A/4  2013-11-21 etxarnu     Corrected for arm
# R2A/5  2014-06-09 etxarnu     Corrected for arm-wr6
# R2A/6  2014-06-11 etxarnu     Take first otp_root if multiple found
# R2A/7  2014-07-15 etxarnu     Now handles tgt_arm/arm-wr6
# R2A/9  2014-10-06 etxarnu     Fix for removed tgt_arm/
# R2A/10 2014-10-13 etxarnu     Changed arm to armv7l
# R3A/1  2015-02-11 etxarnu     Removed sim as target
# R7A/1  2016-08-19 etxarnu     Support for aarch64
# R7A/2  2016-10-03 etxarnu     Support for vrcs (i686)
# R10A/1 2017-06-21 etxarnu     Support for vrcs64 (x86_64)
# R12A/2 2017-12-01 etxarnu     Make it work on all targets
#
#----------------------------------------------------------------------
# Wrapper script used for setting environment for RBS CS application programs.
# If no argument is given, show all environment variables
# else call the program given as argument with environment variables set as if started
# from RBS CS middleware. CXP_PATH will not be correct for the application though.
#
#this OTP_ROOT is only used to find a working erl_call
OTP_ROOT=$(readlink -f /software/*MW*/*/otp-*/priv/tgt_*/ | head -1)
COOKIE=rcs
ENV_FILE=/home/sirpa/env.sh

IsSirpa=$(ps -ef | grep "sname sirpa" |grep -v grep | wc -c)

if [  "${IsSirpa}" -gt 0  ]; then
    SNAME=sirpa
else
    SNAME=root
fi

export ERL_CALL=`readlink -f ${OTP_ROOT}/lib/erl_interface-*/bin/erl_call`

RCS_COOKIE=${COOKIE}
RCS_ROOT=`$ERL_CALL -c ${COOKIE}  -sname ${SNAME} -a 'os getenv ["RCS_ROOT"]' |tr -d '\"'`
OTP_ROOT=`$ERL_CALL -c ${COOKIE}  -sname ${SNAME} -a 'os getenv ["OTP_ROOT"]' |tr -d '\"'`


echo "export RCS_COOKIE=${COOKIE}" > ${ENV_FILE}
echo "export RCS_ROOT=$RCS_ROOT " >> ${ENV_FILE}
echo "export OTP_ROOT=$OTP_ROOT" >> ${ENV_FILE}
echo "export SNAME=$SNAME" >> ${ENV_FILE}


${ERL_CALL} -c ${COOKIE}  -sname ${SNAME} -a 'appmServer get_rcs_env ["unknown"]' | tr -d '[' | tr -d ']' | sed "s/},/}\n/g" | tr -d '{' | tr -d '}' | sed "s/, /,/g" | awk -F, '{print "export "$1"="$2 }' | tr -d '"' >> ${ENV_FILE}
PTH=`${ERL_CALL} -c ${COOKIE}  -sname ${SNAME} -a 'os getenv ["PATH"]' |tr -d '\"'`
echo "export PATH=$PTH:$PATH"  >> ${ENV_FILE}
if [ "$#" -eq 0  ] 
then
    echo
    echo "#===== RBS CS application environment variables ========"
    echo
    cat ${ENV_FILE}
    echo

else
    . ${ENV_FILE}; $*
fi

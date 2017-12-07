#!/bin/sh
## -----------------------------------------------------------------------------
## %CCaseFile:	rcs_erl_call.sh %
## %CCaseRev:	/main/R5A/R6A/R7A/R12A/1 %
## %CCaseDate:	2017-11-20 %
## %CCaseDocNo:  %
##
## Author:      etxarnu
## Description:
##  Script to call the  erlang shell from the unix shell
##
##  Example of call
##  rcs_erl_call  appmServer start_lm '"test_app"'
##  will restart the application test_app
##
## -----------------------------------------------------------------------------
##
## ----------------------------------------------------------
## %CCaseTemplateFile:   %
## %CCaseTemplateId: %
##
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
## %CCaseCopyrightEnd%
##
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev     Date         Name        What
## ------- --------     --------    ------------------------
## R5A/1   2014-05-07   etxarnu     First version in sys


Usage="Usage: $0 [-i installDir] Mod Func [Args]"

if [ $# -eq 0 ]; then
    echo $Usage
    exit 1

fi

case $1 in
    -h) 
	echo $Usage
	exit 1
	;;
    -i) 
	installDir=$2
	shift
	shift
	    ;;
    *)
	if [ ! -n "${installDir}" ]; then
	    if [ -d "/local/scratch/" ]; then
		installDir=/local/scratch/${USER}
	    elif [ -d "/repo/" ]; then
		installDir=/repo/${USER}
	    else
		installDir=/tmp/${USER}
	    fi
	fi
esac

ARCH=`arch`
if [  "${ARCH}" = x86_64 -a ! -d "/home/sirpa" ]; then
    OTP_ROOT=${installDir}/OTP
    COOKIE=`grep COOKIE ${installDir}/RCS_ROOT/sourceMe.csh | awk '{ print $3 }'`
    SNAME=${SNAME:=$USER}
elif [  "${ARCH}" = "armv7l" -o "${ARCH}" = "aarch64" ]; then
    OTP_ROOT=`readlink -f /home/sirpa/software/*RCS*MW*/*/otp-*/priv/tgt_*/ | head -1`
    COOKIE=rcs
    SNAME=sirpa
else
    OTP_ROOT=`readlink -f /home/sirpa/software/*RCS*MW*/*/otp-*/priv/tgt_*/ | head -1`
    COOKIE=rcs
    SNAME=root
fi

ERL_CALL=`readlink -f ${OTP_ROOT}/lib/erl_interface-*/bin/erl_call`


if [ -x $ERL_CALL ]
then
    Mod=$1
    Fun=$2
    shift
    shift
    Args=$*
    ${ERL_CALL}  -c ${COOKIE} -sname $SNAME -a "${Mod} ${Fun} [$Args] " 

else
    echo "Cannot find erl_call"
fi

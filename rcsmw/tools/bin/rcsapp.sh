#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	rcsapp.sh %
# %CCaseRev:	/main/16 %
# %CCaseDate:	2015-02-12 %
# Author:	etxarnu/etxbjca
#
# Short description:  Wrapper script to start a program with same anvironment
#                     as if started from MW.
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
#
#----------------------------------------------------------------------
# Wrapper script used for setting environment for RBS CS application programs.
# If no argument is given, show all environment variables
# else call the program given as argument with environment variables set as if started
# from RBS CS middleware. CXP_PATH will not be correct for the application though.
#
Usage="Usage: $0 [-h] [-i installDir] [program args]"

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
esac

ARCH=`arch`
#this OTP_ROOT is only used to find a working erl_call
if [ "${ARCH}" = ppc -o "${ARCH}" = armv7l ]; then
    echo
    echo "#===== This version is for SIM only ======#"
    echo "#===== Use /home/sirpa/bin/rcsapp.sh for target ====#"
    echo
    exit 1
else
    if [ ! -n "${installDir}" ]; then
	if [ -d "/local/scratch/" ]; then
	    installDir=/local/scratch/${USER}
	elif [ -d "/repo/" ]; then
	    installDir=/repo/${USER}
	else
	    installDir=/tmp/${USER}
	fi
    fi
    OTP_ROOT=${installDir}/OTP
    COOKIE=`grep COOKIE ${installDir}/RCS_ROOT/sourceMe.csh | awk '{ print $3 }'`
    PRIV_COOKIE=`cat $HOME/.erlang.cookie`
    if [ "${COOKIE}" == "${PRIV_COOKIE}" ]; then
	COOKIE_OPT=""
	COOKIE_ROW=""
    else
	COOKIE_OPT="-c ${COOKIE}"
	COOKIE_ROW="export RCS_COOKIE=${COOKIE}"
    fi
    SNAME=${SNAME:=$USER}
fi

if [ -e ${OTP_ROOT}/lib/erl_interface-0/bin/erl_call ]; then
    export ERL_CALL=${OTP_ROOT}/lib/erl_interface-0/bin/erl_call
else
    export ERL_CALL=`readlink -f ${OTP_ROOT}/lib/erl_interface-*/bin/erl_call`
fi

RCS_COOKIE=${COOKIE}

${ERL_CALL} ${COOKIE_OPT}  -sname ${SNAME} -a 'appmServer get_apps ' > /dev/null 2>&1 
if [  $? -eq 0 ];then 

    RCS_ROOT=`$ERL_CALL ${COOKIE_OPT}  -sname ${SNAME} -a 'os getenv ["RCS_ROOT"]' |tr -d '\"'`
    OTP_ROOT=`$ERL_CALL ${COOKIE_OPT}  -sname ${SNAME} -a 'os getenv ["OTP_ROOT"]' |tr -d '\"'`
    
    if [ "${ARCH}" = ppc -o "${ARCH}" = arm ]; then
	ENV_FILE=/home/sirpa/env.sh
    else
	ENV_FILE=$RCS_ROOT/env.sh
    fi
    
    echo "export RCS_ROOT=$RCS_ROOT " > ${ENV_FILE}
    chmod 700 ${ENV_FILE}
    [  "${COOKIE_ROW}" != "" ] && echo ${COOKIE_ROW} >> ${ENV_FILE}
    echo "export OTP_ROOT=$OTP_ROOT" >> ${ENV_FILE}
    echo "export SNAME=$SNAME" >> ${ENV_FILE}
    
    
    ${ERL_CALL} ${COOKIE_OPT}  -sname ${SNAME} -a 'appmServer get_rcs_env ["unknown"]' | tr -d '[' | tr -d ']' | sed "s/},/}\n/g" | tr -d '{' | tr -d '}' | sed "s/, /,/g" | awk -F, '{print "export "$1"="$2 }' | tr -d '"' >> ${ENV_FILE}
    PTH=`${ERL_CALL} ${COOKIE_OPT}  -sname ${SNAME} -a 'os getenv ["PATH"]' |tr -d '\"'`
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
else
 	echo
	echo "#===== rcsapp.sh failed, can not connect to Erlang node ========"
	exit 1
fi   

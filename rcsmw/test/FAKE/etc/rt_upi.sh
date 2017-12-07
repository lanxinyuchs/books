#!/bin/sh
#set -x
#----------------------------------------------------------------------
# %CCaseFile:	rt_upi.sh %
# %CCaseRev:	/main/R2A/R11A/R12A/1 %
# %CCaseDate:	2017-11-16 %
#----------------------------------------------------------------------
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
#----------------------------------------------------------------------
# #1.    REVISION LOG
#----------------------------------------------------------------------
# Rev    Date       Name        What
# -----  -------    --------    -------------------------------------
# R2A/1  2013-03-15 etxarnu     First version 
# R2A/2  2013-05-15 etxarnu     Added logfile
# R2A/3  2013-05-15 etxjotj     Added checks for correct parameters
#                               with help from RAFO
# R2A/4  2013-05-15 etxjotj     Further refinements; Added header
# R2A/5  2013-05-15 etxjotj     Bring header in synch
# R11A/1 2017-10-13 etxpejn     Added triggers configurationStop, configurationStart
#                               trafficStart and trafficStop
# R12A/1 2017-11-16 etxpejn     Added triggers restoreStart & restoreStop
#----------------------------------------------------------------------
#

log () {
	D=`date`
	echo $D: $* >> ${LOG_FILE}
}
app=`basename $0`
LOG_FILE=${LOG_DIR}/rt_upi.log
log " $app called with args: $* "

if [[ $# -eq 1 ]]; then 
    if [[ $1 == "verifyPreconditions" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1
    elif [[ $1 == "activateStart" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1
    elif [[ $1 == "preload" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1
    elif [[ $1 == "activate" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1
    elif [[ $1 == "commit" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1
    elif [[ $1 == "configurationStop" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1	
    elif [[ $1 == "configurationStart" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1	
    elif [[ $1 == "trafficStart" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1	
    elif [[ $1 == "trafficStop" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1	
    elif [[ $1 == "restoreStart" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1	
    elif [[ $1 == "restoreStop" ]]; then 
	sleep .5
	echo "OK: $app $* succeded" >&1	
    else
	sleep .5
	echo "ERROR: $app $* unknown" >&1
    fi
elif [[ $# -eq 2 && $1 == "verifyUpgrade" ]]; then 
    sleep .5
    echo "OK: $app $* succeded" >&1
elif [[ $# -eq 0 ]]; then
    sleep .5
    echo "ERROR: $app without arguments not allowed" >&1
else
    sleep .5
    echo "ERROR: $app $* unknown" >&1
fi
    
    

#!/bin/sh
#set -x
#----------------------------------------------------------------------
# %CCaseFile:	consul.sh %
# %CCaseRev:	/main/R2A/2
# %CCaseDate:	2017-06-20 %
#----------------------------------------------------------------------
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2017 All rights reserved.
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
# Rev     Date       Name        What
# -----   -------    --------    -------------------------------------
# R9A/1   2017-03-29 etxarnu     Created
# R10A/1  2017-05-17 etxarnu     Change exit 1 to exit 0
# R10A/2  2017-06-20 qostjoa     Trap SIGHUP and send SIGINT to Consul
#                                to allow for a clean shutdown of all
#                                child processes.
#----------------------------------------------------------------------
#
# called as consul.sh LogFile RestOfCommand....
#

LOG=$1
trap 'echo "Received SIGHUP, terminating Consul" >> ${LOG} && kill -INT $(jobs -p) && sleep 9999' SIGHUP
shift
echo "Output from:  '$*'" > ${LOG}
$* >> ${LOG} 2>&1 &
wait -n $!
Res=$?
[ $Res -eq 1 ] && exit 0
exit $Res


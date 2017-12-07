#!/bin/bash
#set -x
#----------------------------------------------------------------------
# %CCaseFile:	pg_app1.sh %
# %CCaseRev:	/main/R2A/2
# %CCaseDate:	2014-02-17 %
#----------------------------------------------------------------------
#
#  A program to test escalation from program to program group etc.
#
#  Multiple instances of this program is handled by having symlinks in the CXC
#  for pg_app2.sh and pg_app3.sh
#  The only one copy needs to be edited.
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2013-2014 All rights reserved.
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
# R2A/1   2013-11-13 etxarnu     Created
#----------------------------------------------------------------------
#

trap "cleanup" 2 3 4 5 6 7

cleanup () {
    log "cleanup"
    exit 1
}

log () {
    D=`date`
    echo $D: $* >>$LOG_FILE
}
PRG=`basename $0`
LOG_FILE=$LOG_DIR/$PRG.log
touch $LOG_FILE 
log "Starting $PRG"
cnt=1
while true
do 
    sleep .5
    cnt=$[$cnt + 1]
    [ $[$cnt % 300] -eq 0 ] &&  log "$cnt - looping "
done

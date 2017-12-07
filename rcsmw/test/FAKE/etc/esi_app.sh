#!/bin/sh
#set -x
#----------------------------------------------------------------------
# %CCaseFile:	esi_app.sh %
# %CCaseRev:	/main/R2A/1
# %CCaseDate:	2015-03-11 %
#----------------------------------------------------------------------
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
# R2A/1   2014-04-16 etxarnu     Created
# R3A/1   2015-03-11 etxarnu     Copy erlang logs for oi_testapp to LOG_DIR
#----------------------------------------------------------------------
#


log () {
    D=`date +%Y-%m-%dT%T`
    echo $D: esi_app.sh: $* >> $LOG_FILE
}

LOG_FILE=${LOG_DIR}/esi_app.log
mkdir -p  ${LOG_DIR}/oi_testapp 
cp -r ${RCS_ROOT}/tmp/oi_testapp/* ${LOG_DIR}/oi_testapp
log "Generate esi_dump"
sleep 2
log "Generate esi_dump ready"

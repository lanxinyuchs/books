#!/bin/sh
#set -x
#----------------------------------------------------------------------
# %CCaseFile:	esi_local_app.sh %
# %CCaseRev:	/main/R4A/1
# %CCaseDate:	2015-04-29 %
#----------------------------------------------------------------------
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2015 All rights reserved.
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
# R4A/1   2015-04-27 etxpejn     Created
# R4A/2   2015-04-29 etxpejn     Removed code with oi_local_testapp
#----------------------------------------------------------------------
#


log () {
    D=`date +%Y-%m-%dT%T`
    echo $D: esi_local_app.sh: $* >> $LOG_FILE
}

LOG_FILE=${LOG_DIR}/esi_local_app.log
log "Generate esi_local_dump"
sleep 2
log "Generate esi_local_dump ready"

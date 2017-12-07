#!/bin/sh
#set -x
#----------------------------------------------------------------------
# %CCaseFile:	rt_app.sh %
# %CCaseRev:	/main/R2A/2
# %CCaseDate:	2013-05-15 %
#----------------------------------------------------------------------
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2013 All rights reserved.
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
# R2A/2   2013-05-15 etxjotj     Added header
#----------------------------------------------------------------------
#


log () {
    if [ "${RT_DBG}" = "true" ]; then
	echo rt_app.sh: $* >&2
    fi
}

log "Starting rt_app"
log "RRR = ${RRR}"
sleep 5
while true
do 
    log "looping"
    sleep 5
done

#!/bin/bash
## -----------------------------------------------------------------------------
## %CCaseFile:	restart_hook.sh %
## %CCaseRev:	/main/R5A/2 %
## %CCaseDate:	2016-03-30 %
## %CCaseDocNo: %
##
## Author: Tomas Ryden
## Description: This script is called by heart when a node is restarted.
## -----------------------------------------------------------------------------
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2016 All rights reserved.
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
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R5A/1      2016-03-29   etxtory     Saving /rcs/erlang to /rcs/erlang_disk/
## R5A/2      2016-03-30   etxarnu     Send ouput of tar to /dev/null
## -----------------------------------------------------------------------------

ERLANG_LOG_DIR=/rcs/erlang
FILENAME=/rcs/erlang_disk/erlang.tar.gz

LAST=`ls -t1 $FILENAME.* 2>/dev/null| head -1`

if [ -z "$LAST" ]; then
    NO=1
else 
    NO=`echo "${LAST##*.}"`
    if [ "$NO" -gt 4 ]; then
	NO=1
    else
	NO=$((NO + 1))
    fi
fi

cd $ERLANG_LOG_DIR;
tar cfz $FILENAME.$NO *  > /dev/null  2>&1

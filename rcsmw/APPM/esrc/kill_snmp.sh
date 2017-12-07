#!/bin/bash
## -----------------------------------------------------------------------------
## %CCaseFile:	kill_snmp.sh %
## %CCaseRev:	/main/R5A/2 %
## %CCaseDate:	2016-07-16 %
##
## Description:
##    Wrapper script to kill snmpd 
##    and ignore any exits from pkill due to that snmpd has already died
##    Called from appmServer:comea_snmp(stop_from_aic) via PGH
##
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
### R5A/1     2016-07-16   etxarnu   Created
### R5A/2     2016-07-16   etxarnu   Used basename for program name in logging
## --------- ---------------------------------------------------- column 80 --->

pid=$(pgrep snmpd)
if [ $? -eq 0 ]; then
    
    logger "$(basename $0): killing snmpd $pid"
    pkill snmpd > /dev/null 2>&1
fi
exit 0

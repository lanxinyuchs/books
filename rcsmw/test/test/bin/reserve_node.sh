#!/bin/bash
## ----------------------------------------------------------
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
#############################################################
##
##                NOTE!!!!!!!
##  The original file is stored in /vobs/rcs/test/RCT_CRX901275/test/bin 
##  Only do changes there and copy to /proj/rcs/bin/ directory.
##
#############################################################
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R2A/1      2013-09-04   etxjovp     Created
## R2A/4      2013-10-01   etxjovp     add /home/$USER/.test_my_stuff
## R2A/8      2014-01-13   etxjovp     curl handle calls without certificate
## R2A/9      2014-01-20   etxjovp     curl handle calls without certificate
## R2A/10     2014-01-21   etxjovp     curl handle calls without certificate
## R2A/10     2014-04-21   erarafo     Copyright complaint fixed
## R4A/10     2017-10-04   etxjovp     move script to git repo rcsmw
#
######################################################################################


######################################################################################
#   MAIN
######################################################################################
# Init variables
tmpDir="`mktemp -d`"
prog="tools/bin/dci_reserve_node.sh"

# trap handler: cleanup function
function trap_handler()
{
    LASTERR=$2
    \rm -rf "${tmpDir}"
    exit $LASTERR
}

# Clean on abort/exit
trap 'trap_handler ${LINENO} $?' 0 2 3 15

set -e

# Fetch real program and start to execute
( \cd "${tmpDir}" && git archive --remote=ssh://gerritmirror-ha.rnd.ki.sw.ericsson.se:29418/rcsmw/rcsmw.git HEAD ${prog} | tar -x )
\chmod 755 "${tmpDir}/${prog}"
${tmpDir}/${prog} $@


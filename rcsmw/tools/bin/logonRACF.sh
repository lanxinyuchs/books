#!/bin/sh
# ----------------------------------------------------------------------
# Author      : etxbjca
#
# Short description: 
# Script that logon to RACF by PDI.
#
# **********************************************************************
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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

# **********************************************************************
#

#
# Define variables
#
progname="`basename $0`"
USAGE="\n\t${progname}\n"
skelUser=rcsci1
skelDir=/home/${skelUser}/private
racfFile=${skelUser}.RACF_info
ssoFile=${skelUser}.SSO_info
DOLOGIN=''

#
# Init PDI
#
. /app/modules/0/init/sh
module add pdi/LZN90113983-R17A

if [ -z "${PRIVATE_D}" ]
then
    PRIVATE_D=${HOME}/private
fi

if [ -z "${RACF_USER}" ]
then
    RACF_USER=${USER}
fi

# Override default 5 min value if arg exist.
if [ -n "$1" ]
then
    timeout=$1
else
    timeout=5
fi

#
# Init
#
if [ ! -d "${PRIVATE_D}" ]
then
    mkdir ${PRIVATE_D} && chmod 700 ${PRIVATE_D}
fi
if [ "${USER}" != "${skelUser}" ]
then
    if ! ( sudo -u ${skelUser} /bin/cat ${skelDir}/${racfFile} ) > ${PRIVATE_D}/${racfFile} && chmod 0600 ${PRIVATE_D}/${racfFile}
    then
	echo "${progname}: Error: sudo command as user [${skelUser}] for RACF-file update failed, aborting!" >&2
	exit 3
    fi
    if ! ( sudo -u ${skelUser} /bin/cat ${skelDir}/${ssoFile} ) > ${PRIVATE_D}/${ssoFile} && chmod 0600 ${PRIVATE_D}/${ssoFile}
    then
	echo "${progname}: Error: sudo command as user [${skelUser}] for SSO-file update failed, aborting!" >&2
	exit 3
    fi
fi
if [ ! -s "${PRIVATE_D}/${racfFile}" ]
then
    echo "${progname}: Missing RACF-file, aborting" >&2
    exit 3
fi

if [ ! -s "${PRIVATE_D}/${ssoFile}" ]
then
    echo "${progname}: Missing SSO-file, aborting" >&2
    exit 3
fi

#
# Check/Login
#
info=`pdi login -info | awk '{if($1 == "Status:"){if(NF == 2){print "0"} else {print $4}}}'` 2> /dev/null
pdiCookie=`pdi login -info -src SDA | grep Cookie`
if [ "${info}" -le "${timeout}" -o -z "${info}" ]
then
    tmp=`mktemp`
    pdi login -logout > /dev/null 2>&1
    \rm -f ${pdiCookie}
    if ! pdi login -env Prod -force > ${tmp} 2>&1
    then
	if grep -i "can't connect to" ${tmp}
	then
	    mailx -r "bjorn.carlsson@ericsson.com" -s "RBS CS, Automatic Error Notification: pdi login cant access server" 'pdi-discuss@mailman.lmera.ericsson.se' < ${tmp}
	fi
	echo "${progname}: Error: RACF login failed!" >&2
	exit 2
    fi
    \rm -r ${tmp} > /dev/null 2>&1
fi

exit 0


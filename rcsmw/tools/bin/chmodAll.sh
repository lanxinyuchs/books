#!/bin/sh
# ----------------------------------------------------------------------
# Short description:  Deploy management of 3PPs (external source)
#
# ----------------------------------------------------------------------
#
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
#
#----------------------------------------------------------------------
# #1.    REVISION LOG
#----------------------------------------------------------------------
# Rev      Date       Name        What
# -----    -------    --------    -------------------------------------
# main/1   2017-03-02 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
debug=''
topDir=''
USAGE="\n\t${progname} -p <path> [-d]\n\n"
Exit=0

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    local exit_line=$1
    local exit_code=$2
    if [ "${exit_code}" = "0" ]
    then
	exit 0
    else
	echo "Exit [${exit_code}] triggered by line [${exit_line}]"
	exit ${exit_code}
    fi
}

# Catch exit's
trap 'trap_handler ${LINENO} $?' 0 1 2 3 15

set -e

#
# Define Echo Procedure
#
Echo()
{
    printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Info $1\n" >&2
}

#
# Define Error Procedure
#
Error()
{
    printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Error: $1\n" >&2
    exit 1
}

#
# Define Warning Procedure
#
Warn()
{
    printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Warning: $1\n" >&2
}


#
# Get options
#
while getopts p:d option
do
  case ${option} in
    p)  topDir="${OPTARG}";;
    d)  debug="-${option}";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1` 

#
# Check number of options
#
OptNo="3"
if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi

if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -n "${topDir}" -a ! -d "${topDir}" ]
then
  Error "${topDir} is missing!"
fi


#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi

# Step 1 - whole structure "rwx" for user, "rx" for all other
chmod -R 0755 ${topDir}

# Step 2 - all files "rw" for user, "r" for all other 
find ${topDir} -type f -exec chmod 0644 {} \;

# Step 3 - all binaries/scripts "rwx" for user, "rx" for all other
find ${topDir} -type f -print | xargs file | egrep 'executable|script' | cut -f 1 -d : | xargs chmod ugo+x 2> /dev/null || true

# Step 4 - remove "w" on all dir's for all except "user" 
chmod -R go-w ${topDir}
#!/bin/sh
# ----------------------------------------------------------------------
# Short description: Report build R-states to Baseline file 
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2016 All rights reserved.
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
# main/1   2017-01-27 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
tmpFile=`mktemp`
debug=''
topDir=''
USAGE="\n\t${progname} -i <input file>  -f <file (output)> -o <outdir> [-d]\n\n"
Exit=0

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    local exit_line=$1
    local exit_code=$2
    #\rm -f ${tmpFile}* > /dev/null 2>&1
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
while getopts i:f:o:d option
do
  case ${option} in
    i)  infile="${OPTARG}";;
    f)  file="${OPTARG}";;
    o)  outdir="${OPTARG}";;
    d)  debug="-${option}";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1`

#
# Check number of options
#
OptNo="7"
if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi
if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -z "${infile}" ]
then
    Error "Missing entries in File [${infile}]!"
fi

#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi

for entry in `cat ${infile}|tr ' ' '|'`
do
   prodNo=`echo ${entry} | awk -F\| '{print $1}' | tr -d ' ' | tr '_' '/'`
   Rstate=`echo ${entry} | awk -F\| '{print $2}' | tr -d ' ' | tr '_' '/'`
   id=`ci_data.sh -f ${file} -a read -p 2,1 | grep '|'"${prodNo}"'$' | head -1 | awk -F\| '{print $1}'`
   if [ -n "${id}" ]
   then
       CL0=`ci_data.sh -f ${file} -a write -i ${id} -c ${Rstate} -p 3`
       prodNo=`echo ${prodNo} | tr '/' '_'`
       prodName=`ci_data.sh -f ${file} -i ${id} -a read -p 7 | tr -d '[:space:]'`
       prodExt=`ci_data.sh -f ${file} -i ${id} -a read -p 15 | tr -d '[:space:]'`
       prodFile=${outdir}/${prodName}_${prodNo}.${prodExt}
       if [ -s "${prodFile}" ]
       then
	   FILE=`ci_data.sh -f ${file} -a write -i ${id} -c ${prodFile} -p 13`
       fi
   else
       Warn "Unknown product [${prodNo}]"
   fi
done

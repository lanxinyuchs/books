#!/bin/sh
# ----------------------------------------------------------------------
# Short description: Manage Product PY files
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
# main/1   2017-01-17 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
tmpFile=`mktemp`
debug=''
topDir=''
USAGE="\n\t${progname} -i '<input skeleton file>' -s <skeleton file> -o <output file> [-d]\n\n"
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
while getopts i:s:o:d option
do
  case ${option} in
    i)  idfile="${OPTARG}";;
    s)  skelfile="${OPTARG}";;
    o)  outfile="${OPTARG}";;
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

if [ -z "${idfile}" ]
then
    Error "Missing arguments in ID File!"
fi

if [ ! -s "${skelfile}" ]
then
    Error "Missing or empty skeleton file [${skelfile}]!"
fi

#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi


# Variables
idList=`cat ${skelfile} | tr "'" '"' | tr ',"' '\n' | sed 's/ : /\n/g' | grep '^<' | sed 's/.*\(<[0-9,:,<,>]*>\).*/\1/g'`



#
# Execution start
#
#Echo "Extracting/refreshing 3PP Cache/info for ID: [${idList}]"
if [ -n "${idList}" ]
then
    for entry in ${idList}
    do
      data1=`echo ${entry} | tr -d '<>' | awk -F: '{print $1}'`
      data2=`echo ${entry} | tr -d '<>' | awk -F: '{print $2}'`
      data3=`echo ${entry} | tr -d '<>' | awk -F: '{print $3}'`
      data4=`echo ${entry} | tr -d '<>' | awk -F: '{print $4}'`
      if [ -n "${data3}" -a -n "${data4}" ]
      then
	  id1=`ci_data.sh -f ${idfile} -a read -i ${data1} -p ${data2}`
	  id=`ci_data.sh -f ${idfile} -a read -i ${id1} -p ${data3}`
	  pos=${data4}
      elif [ -n "${data3}" -a ! -n "${data4}" ]
      then
	  id=`ci_data.sh -f ${idfile} -a read -i ${data1} -p ${data2}`
	  pos=${data3}
      else
	  id=${data1}
	  pos=${data2}
      fi
      data=`ci_data.sh -f ${idfile} -a read -i ${id} -p ${pos} | tr '/' '_'`
      if echo ${data} | grep '^_' > /dev/null 2>&1
      then
	  data=`ci_data.sh -f ${idfile} -a read -i ${id} -p ${pos}`
      fi
      echo "s¤${entry}¤${data}¤g" >> ${tmpFile}
    done
fi

sed -f ${tmpFile} ${skelfile} > ${outfile}


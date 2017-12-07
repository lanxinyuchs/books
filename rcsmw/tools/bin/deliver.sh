#!/bin/sh
# ----------------------------------------------------------------------
# Short description: Extract Product data from CI file & run budit
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
# -        2017-11-20 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
debug=''
information=''
buditcfg=''
buditdebuglevel='--debuglevel 2'

USAGE="\n\t${progname} -a <allcs id> [-b <budit cfg>] -r '<ranbl id> [<ranbl id2> <ranbl idn>]' [-f <flow context>] -i <id file> -c <confidence level> [-u <information url>] [-d]\n\n"
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
while getopts a:b:i:f:r:c:u:d option
do
  case ${option} in
    a)  allcsid="${OPTARG}";;
    b)  buditcfg="${OPTARG}";;
    r)  ranblids="${OPTARG}";;
    i)  idfile="${OPTARG}";;
    f)  flowcontext="${OPTARG}";;
    c)  conflevel="${OPTARG}";;
    u)  url="${OPTARG}";;
    d)  debug="-${option}";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1` 

#
# Check number of options
#
OptNo="9"
if [ -n "${url}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    information="--information ${url}"
else
    ciinfo=`ci_data.sh -f ${idfile} -a read -i ${allcsid} -p 12`
    if [ -n "${ciinfo}" ]
    then
	information="--information ${ciinfo%.*}.html"
    fi
fi

if [ -n "${buditcfg}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    buditcfg="--configurationfile ${buditcfg}"
fi

if [ -n "${flowcontext}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi

if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi

if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ ! -s "${idfile}" ]
then
    Error "Non readable ID file [${idfile}]!"
fi

case ${conflevel} in
    0|1) true;;
    *)   Error "Non allowed confidence level argument [${conflevel}]";;
esac

#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi

# Variables
allcsno=`ci_data.sh -f ${idfile} -a read -i ${allcsid} -p 2 | tr '/' '_'`
allcsrev=`ci_data.sh -f ${idfile} -a read -i ${allcsid} -p 3`
connectedto="--connectedto ${allcsno}-${allcsrev}"

#
# Init Modules
#
. /app/modules/0/init/sh
module add jdk/1.7.0_55

# Create file
for blid in ${ranblids}
do
  blno="`ci_data.sh -f ${idfile} -a read -i ${blid} -p 2 | tr '/' '_'`"
  blname="`ci_data.sh -f ${idfile} -a read -i ${blid} -p 7`"
  blcontent="`ci_data.sh -f ${idfile} -a read -i ${blid} -p 8`"
  if [ -n "${flowcontext}" ]
  then
      blflowcontext=${flowcontext}
  else
      blflowcontext=`ci_data.sh -f ${idfile} -a read -i ${blid} -p 11`
  fi
  providediflist=''
  for contentid in ${blcontent}
  do
    contenturl=`ci_data.sh -f ${idfile} -a read -i ${contentid} -p 12`
    if [ -n "${contenturl}" ]; then
	dbgurl="--debugfile ${contenturl%.*}.dbg"
	contenturl="--url ${contenturl}"
    else
	Error "Missing Content URL for ID [${contentid}] in IDFILE [${idfile}]"
    fi
    contentptype=`ci_data.sh -f ${idfile} -a read -i ${contentid} -p 16`
    if [ -n "${contentptype}" ]; then
	contentptype="--packagetype ${contentptype}"
    else
	Error "Missing Content Product Type for ID [${contentid}] in IDFILE [${idfile}]"
    fi
    providedifids=`ci_data.sh -f ${idfile} -a read -i ${contentid} -p 17`
    providedifurls=''
    for providedif in ${providedifids}
    do
      providedifurl=`ci_data.sh -f ${idfile} -a read -i ${providedif} -p 12`
      if [ -n "${providedifurl}" ]; then
        providedifurls="${providedifurls} --providesif ${providedifurl}"
      else
	  Error "Missing IF URL for ID [${providedif}] in IDFILE [${idfile}]"
      fi
    done
    /env/rbsg2/bin/budit.pl ${buditcfg} ${contentptype} ${contenturl} ${dbgurl} ${providedifurls} ${connectedto} --confidencelevel ${conflevel} ${information} ${buditdebuglevel}
  done
  /env/rbsg2/bin/budit.pl ${buditcfg} --commit --noxmlvalidation --flowContext ${blflowcontext} --noerlookup
done

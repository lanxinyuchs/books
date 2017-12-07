#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	updatePRIM.sh %
# %CCaseRev:	/main/42 %
# %CCaseDate:	2016-11-30 %
# %CCaseDocNo:	511/190 55-LXA 119 334 Ux %
# Author      : Björn Carlsson (etxbjca)
#
# Short description: Calculate "next" R-State for a given product and registrate it in PRIM.
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
# main/1   2017-06-16 etxbjca     Created
#----------------------------------------------------------------------
#


#
# Set default variables
#
progname="`basename $0`"
progdir="`dirname $0`"
debug=''
tmp="`mktemp`"
tmpFile="${tmp}.${progname}"
warnings=${tmpFile}.warnings
pdiopt=''
IS='1095-'
USAGE="\n\t${progname} -p <Product> -r <R-state base> [-b <baseline file>] [-L|-D|-P] [-d]\n\n"
Exit=0

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    local exit_line=$1
    local exit_code=$2
    if [ -z "${debug}" ]
    then
	\rm -rf ${tmp} ${tmp}* > /dev/null 2>&1 || true
    fi
    if [ "${exit_code}" = "0" ]
    then
	exit 0
    else
	Echo "Exit [${exit_code}] triggered by line [${exit_line}]"
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


set -e

# Source common procedures for PDI support
. ${progdir}/2source/pdiSupport.sh

#
# Get options
#
while getopts b:p:r:DLPd option
do
  case ${option} in
    b)  baselineFile="${OPTARG}";;
    p)  product=`echo "${OPTARG}" | tr -d '[ 	]' | tr '[:lower:]' '[:upper:]' | tr '_' '/'`;;
    r)  rBase=`echo "${OPTARG}" | tr -d '[ 	]' | tr '[:lower:]' '[:upper:]' | tr '_' '/'`;;
    d)  debug="yes";;
    D)  digitStep="yes";;
    L)  letterStep="yes";;
    P)  prelStep="yes";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1` 

#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi

#
# Check number of options
#
OptNo="4"
if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi
if [ -n "${baselineFile}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    if [ ! -s "${baselineFile}" ]
    then
	Error "Missing or empty baseline file [${baselinefile}]\n"
    fi
fi
if [ -z "${digitStep}" -a -z "${letterStep}" -a -z "${prelStep}" ]
then
    OptNo="`expr ${OptNo} + 1`"
elif [ -n "${digitStep}" -a -z "${letterStep}" -a -z "${prelStep}" ] ||
     [ -z "${digitStep}" -a -n "${letterStep}" -a -z "${prelStep}" ] ||
     [ -z "${digitStep}" -a -z "${letterStep}" -a -n "${prelStep}" ]
then
    OptNo="`expr ${OptNo} + 2`"
else
    Error "Wrong argument combination!\n${USAGE}\n"
fi
if [ "${OPTIND}" != "${OptNo}" ]
then 
    Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -n "${product}" ]
then
    if ! echo "${product}" | sed 's,^\(.*[A-Z][A-Z][A-Z]\)\([0-9][0-9][0-9]\)\(.*\)$,\1 \2 \3,' | ChProdNo > /dev/null 2>&1
    then
	Error "Faulty Product [${product}] argument (not valid ABC-class)!"
    fi
else
    Error "Missing Product argument!"
fi

if [ -n "${rBase}" ]
then
    if ! echo "${rBase}" | ChRState > /dev/null 2>&1
    then
	Error "Faulty R-State base argument [${rBase}] (not valid R-state syntax)!"
    fi
else
    Error "Missing R-State base argument!"
fi

# Use R-state from Baseline File (if existing)
if [ -n "${baselineFile}" ]
then
    CID=`awk -F\| '{if ($2=="'${product}'") {print $1}}' ${baselineFile} | head -1`
    if [ -n "${CID}" ]
    then
	RSTATE=`ci_data.sh -f ${baselineFile} -a read -i ${CID} -p 3`
    else
	Error "Missing Product id in baseline file [${baselineFile}]!"
    fi
    if [ -n "${RSTATE}" ]
    then
	# Log data in console
	Echo "Version selected from baseline file [${baselineFile}]: ${product} ${RSTATE}"

	# Return data
	echo ${RSTATE}
	exit 0
    else
	Echo "Defining R-state for CID [${CID}] in baseline file [${baselineFile}]..."
    fi
fi
# Hardcode "prel" version to Pxx01
if [ -n "${prelStep}" ]
then
    RSTATE=`echo "${rBase}" | sed 's,^R,P,'`01
    Echo "Private build version (fixed): ${product} ${RSTATE}"
else
  # Logon on RACF to make pdi commands to work...
  logonRACF.sh

  if ! highestAvailRstate=`runPDIcmd pdi lsprod -pno "${product}" -csv_noheader -csv_hd_flds 'RState' -skp_tag retrieveInformationCode.output.row | grep "${rBase}" | head -1`
  then
      Error "PRIM look up failed!"
  fi

  # Manage "first" R-state
  if [ -z "${highestAvailRstate}" ]
  then
	if [ -n "${digitStep}" ]
	then
	    nextRstate=`nextRev.escript "${rBase}01" | awk -F: '{print $1}'`
	elif [ -n "${letterStep}" ]
	then
	    nextRstate=`nextRev.escript "${rBase}01" | awk -F: '{print $2}'`
	elif [ -n "${prelStep}" ]
	then
	    nextRstate=`echo "${rBase}" | sed 's,^R,P,'`01
	else
	    nextRstate=${rBase}01
	fi
  else
    if calcRstate=`nextRev.escript "${highestAvailRstate}"`
    then
	if [ -n "${digitStep}" ]
	then
	    nextRstate=`echo "${calcRstate}" | awk -F: '{print $1}'`
	elif [ -n "${letterStep}" ]
	then
	    nextRstate=`echo "${calcRstate}" | awk -F: '{print $2}'`
	elif [ -n "${prelStep}" ]
	then
	    nextRstate=`echo "${calcRstate}" | awk -F: '{print $4}'`
	else
	    nextRstate=`echo "${calcRstate}" | awk -F: '{print $3}'`
	fi
    else
	Error "Calculation of next R-State failed!"
    fi
  fi

  if [ -n "${nextRstate}" ]
  then
    # Create new R-state
    if ! RSTATE=`runPDIcmd pdi mkrstate -prodno "${product}" -rstate "${nextRstate}" -csv_hd_flds RState -csv_noheader`
    then
      Error "PRIM R-state registration [${product}, ${nextRstate}] failed!"
    fi
    # Check that PRIM was updated
    if ! RSTATE=`runPDIcmd pdi lsprod -prodno "${product}" -rstate "${nextRstate}" -csv_hd_flds RState -csv_noheader -skp_tag retrieveInformationCode.output.row | tr -d '" '`
    then
      Error "PRIM Product Info read [${product}, ${nextRstate}] failed!"
    fi
  else
    Error "Missing \"next\" R-State value!"
  fi

  # Log data in console
  Echo "Version registred in PRIM: ${product} ${RSTATE}"
fi

# Use update R-state in Baseline File (if existing)
if [ -n "${baselineFile}" ]
then
    CIDRSTATE=`ci_data.sh -f ${baselineFile} -a write -i ${CID} -c ${RSTATE} -p 3`
    if [ -n "${RSTATE}" ]
    then
	RSTATE=${CIDRSTATE}
    else
	Error "Failed to update baseline file [${baselineFile}] with R-state [${RSTATE}]!"
    fi
fi

# Return data
echo ${RSTATE}

#!/bin/sh
# ----------------------------------------------------------------------
# Short description:  Deploy management of 3PPs (external source)
#
# ----------------------------------------------------------------------
#
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
#
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
debug=''
topDir=''
buditcfg=''
USAGE="\n\t${progname} [-b <budit cfg>] -i '< Id list>' -f <baseline file> -p <path to cache>|<path to file> [-e] -u <url (base) for binary archive> -C <Config file (nexus)> -c <confidence level> [-d]\n\n"
Exit=0
tmp="`mktemp`"

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
	echo "Exit [${exit_code}] triggered by line [${exit_line}]"
	exit ${exit_code}
    fi
}

# Catch exit's
trap 'trap_handler ${LINENO} $?' 0 1 2 3 15

set -e

# Create managed structures writable for all in unix group 
umask 002

#
# Init Modules
#
. /app/modules/0/init/sh
module add lynx

#
# Define Echo Procedure
#
Echo()
{
    printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Info: $1\n" >&2
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
while getopts b:c:C:i:f:p:u:ed option
do
  case ${option} in
    b)  buditcfg="${OPTARG}";;
    e)  EXTERNAL="yes";;
    c)  CLEVEL="${OPTARG}";;
    C)  CONFIG="${OPTARG}";;
    i)  idList="${OPTARG}";;
    f)  baselinefile="${OPTARG}";;
    p)  topDir="${OPTARG}";;
    u)  baseurl="${OPTARG}";;
    d)  debug="-${option}";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1` 

#
# Check number of options
#
OptNo="13"
if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi

if [ -n "${buditcfg}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    buditcfg="--cfg ${buditcfg}"
fi

if [ -n "${EXTERNAL}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi

if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -z "${idList}" ]
then
    Error "Missing arguments in ID list!"
else
    IDLIST=''
    for entry in ${idList}
    do
      if echo "${entry}" | grep ':' > /dev/null 2>&1
      then
	  id=`echo "${entry}" | cut -f1 -d:`
	  listPos=`echo "${entry}" | cut -f2 -d:`
	  IDLIST="${IDLIST} `ci_data.sh -f ${baselinefile} -a read -i ${id} -p ${listPos}`"
      else
	  IDLIST="${IDLIST} ${entry}"
      fi
    done
    idList=${IDLIST}
fi

if [ -z "${baseurl}" ]
then
    Error "Missing url!"
fi

if [ ! -s "${baselinefile}" ]
then
    Error "Missing or empty baselinefile [${baselinefile}]!"
fi

if [ -n "${topDir}" ]
then
    if [ -f "${topDir}" -a -s "${topDir}" ]
    then
	FILE2STORE=${topDir}
    elif [ ! -d "${topDir}" ]
    then
      mkdir -p ${topDir}
    fi
fi


#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi


store2nexus()
  {
      FROM=$1
      FROMDIR=`dirname ${FROM}`
      FROMFILE=`basename ${FROM}`
      ERRFILE=${tmp}.store2nexus.$$
      MIMETYPE='application/x-tar'

      Echo "Storing [${FROM}] ID: [${ID}]: ${NAME}, ${PROD} ${REV} to Nexus..."
      RESULT=$( cd ${FROMDIR} && sudo -u "${NEXUS_USER}" cat "${CONFIG}" | curl -sS --config /dev/stdin --request PUT --data-binary @${FROMFILE} --header "Content-Type:${MIMETYPE}" "${URL}/${FROMFILE}" 2> ${ERRFILE} | lynx -dump -stdin | sed 's/^[[:space:]]*//g' )
      if [ -s "${ERRFILE}" ]
      then
	  if grep 'SSL write: error -5961' ${ERRFILE} > /dev/null
	  then
	      Echo "File [${FROMFILE}] already stored with URL: [${URL}/${FROMFILE}]!"
	      return
	  else
	      cat ${ERRFILE} >&2
	  fi
      fi
      if [ -n "${RESULT}" ]
      then
	  Error "...storage failed with curl result: [${RESULT}]!"
      else
	  Echo "...storage succeded with URL: [${URL}/${FROMFILE}]!"
      fi
}

deploy()
  {
      PROD=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 2 | tr '/' '_' | tr -d '[:space:]'`
      REV=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 6 | tr -d '[:space:]'`
      if [ -z "${REV}" ]
      then
	  REV=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 3 | tr -d '[:space:]'`
	  ci_data.sh -f "${baselinefile}" -a write -i "${ID}" -p 6 -c "${REV}"  > /dev/null 2>&1 # Update HA Rstate (highest availabe) with CL0 R-state 
      fi
      NAME=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 7 | tr -d '[:space:]'`
      SUFFIX=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 15 | tr -d '[:space:]'`
      TYPE=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 16 | tr -d '[:space:]'`
      CACHE=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 13 | tr -d '[:space:]'`
      if [ -z "${TYPE}" ]
      then
	  case ${PROD} in
	      	CXS*)	TYPE="SWC";;
		CXP*)	TYPE="LMC";;
		CXC*)	TYPE="LM";;
		CXA*) TYPE="IFC";;
	  	CA*) TYPE="SWU";;
	  	*)    TYPE="UNKNOWN";;
	  esac
      fi
      URL="${baseurl}/${NAME}_${PROD}/${REV}"
      if [ -n "${EXTERNAL}" ]
      then
	  DIR="${topDir}/${REV}/${NAME}_${PROD}"
	  FILE="${NAME}_${PROD}-${REV}.zip"
      elif [ -n "${CACHE}" -a -s "${CACHE}" ]
      then
	  DIR="`dirname ${CACHE}`"
	  FILE="`basename ${CACHE}`"
      else
	  DIR="${topDir}"
	  FILE="${NAME}_${PROD}.${SUFFIX}"
      fi
      if [ -n "${FILE2STORE}" ]
      then
	  DIR="`dirname ${FILE2STORE}`"
	  FILE="`basename ${FILE2STORE}`"
      else
	  DBGFILE="${NAME}_${PROD}.dbg"
      fi
      FLOW=`ci_data.sh -f "${baselinefile}" -a read -i "${ID}" -p 11 | tr -d '[:space:]'`
      PROJECT=`echo "${FLOW}" | awk -F/ '{print $2}'`
      TRACK=`echo "${FLOW}" | awk -F/ '{print $3}'`
      NEXUS_USER=`echo "${CONFIG}" | awk -F/ '{print $3}'`
      CURDIR=`pwd`
      ci_data.sh -f "${baselinefile}" -a write -i "${ID}" -p 3 -c "${REV}"  > /dev/null 2>&1 # R-state update
      if [ -n "${EXTERNAL}" ]
      then
	  ci_data.sh -f "${baselinefile}" -a write -i "${ID}" -p 13 -c "${DIR}" > /dev/null 2>&1 # Cache path update
	  Echo "Creating zipfile..."
	  test -d ${DIR} || mkdir -p ${DIR}
	  ( cd `dirname "${DIR}"` && zip -q -r "${CURDIR}/${FILE}" `basename "${DIR}"` )
	  DIR="${CURDIR}"
      fi

      #
      # Store to Nexus
      #
      if [ -n "${DIR}" -a -n "${FILE}" -a -s "${DIR}/${FILE}" ]
      then
	  store2nexus "${DIR}/${FILE}"
      else
	  Error "Missing/empty file to store [${DIR}/${FILE}]"
      fi
      if [ -n "${DIR}" -a -n "${DBGFILE}" -a -s "${DIR}/${DBGFILE}" ]
      then
	  store2nexus "${DIR}/${DBGFILE}"
      fi

      ci_data.sh -f "${baselinefile}" -a write -i "${ID}" -p 12 -c "${URL}/${FILE}" # Cache url update
      if update_ib.pl ${buditcfg} -set_rstate "${REV}" -product "${PROD}" | grep 'ERROR:' > /dev/null 2>&1
      then
	  update_ib.pl ${buditcfg} -new_product "${PROD}"| grep 'ERROR:' && false
	  update_ib.pl ${buditcfg} -set_productname "${NAME}" -product "${PROD}" | grep 'ERROR:' && false
	  update_ib.pl ${buditcfg} -set_producttype "${TYPE}" -product "${PROD}" | grep 'ERROR:' && false
	  update_ib.pl ${buditcfg} -set_rstate "${REV}" -product "${PROD}"| grep 'ERROR:' && false
      fi
      update_ib.pl ${buditcfg} -set_urlpath "${URL}/${FILE}" -product "${PROD}" -rstate "${REV}" | grep 'ERROR:' && false
      if [ -n "${DIR}" ]
      then
	  update_ib.pl ${buditcfg} -set_property build_cache -value "${DIR}" -product "${PROD}" -rstate "${REV}" | grep 'ERROR:' && false
      fi
      update_ib.pl ${buditcfg} -set_cl "${CLEVEL}" -verdict "SUCCESS" -project "${PROJECT}" -track "${TRACK}" -product "${PROD}" -rstate "${REV}"| grep 'ERROR:' && false
      Echo "Deployed data for ID: [${ID}]: ${NAME}, ${PROD} ${REV}, CL=${CLEVEL}\n"
}

#
# Execution start
#
Echo "Deploying data for ID: [${idList}]"
idPid=''
for ID in ${idList}
do
  Echo "Deploy of ID: [${ID}] started..."
  ( deploy > ${tmp}.${ID} 2>&1 || echo $? > ${tmp}.${ID}.error
    if [ -n "${EXTERNAL}" ]
    then
      \rm -f "${DIR}/${FILE}" > /dev/null 2>&1
    fi )&
  idPid="${idPid} $!"
  sleep 1
done
wait ${idPid}
for ID in ${idList}
do
  cat ${tmp}.${ID} >&2
  if [ -s "${tmp}.${ID}.error" ]
  then
      exit `cat ${tmp}.${ID}.error`
  fi
done

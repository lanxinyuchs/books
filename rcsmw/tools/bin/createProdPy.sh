#!/bin/sh
# ----------------------------------------------------------------------
# Short description: Create Product PY files
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
# -        2017-09-28 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
tmpFile=`mktemp`
tmpFile2=${tmpFile}2
debug=''
topDir=''
USAGE="\n\t${progname} -i '<input data file>' -s <skeleton file> -a '<app dir list>:<testapp dir list>' -o <output file> -u '<upid>[ <upid2> ...<upidnn>]' [-d]\n\n"
Exit=0

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    local exit_line=$1
    local exit_code=$2
    \rm -f ${tmpFile}* > /dev/null 2>&1
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
while getopts a:i:o:u:d option
do
  case ${option} in
    a)  appdirlist="${OPTARG}";;
    i)  idfile="${OPTARG}";;
    o)  outfile="${OPTARG}";;
    u)  upids="${OPTARG}";;
    d)  debug="-${option}";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1` 

#
# Check number of options
#
OptNo="9"
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

if [ ! -n "${appdirlist}" ]
then
    Error "Missing application directory list [${appdirlist}]!"
else
    dirlist=${appdirlist//[:]/ }
    for appdir in ${dirlist}
    do
      if [ ! -d "${appdir}" ]
      then
	  Error "Missing application directory [${appdir}]!"
      fi
    done
fi

#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi

# Variables
appdirs=( `echo ${appdirlist} | awk -F: '{print $1}'` )
tstappdirs=( `echo ${appdirlist} | awk -F: '{print $2}'` )
appsrclist=( ${appdirs[@]/%//wscript_build} )
tstappsrclist=( ${tstappdirs[@]/%//wscript_build} )
applist=( `grep 'cxaid=' ${appsrclist[@]} | awk -F, '{for (i=1; i<=NF; i++) {if($i~/cxaid=/){cxaid=$i}};{print $1":"cxaid} }' | tr -d ' ")' | sed 's/^.*(//g'` )
tstapplist=( `grep 'cxaid=' ${tstappsrclist[@]} | awk -F, '{for (i=1; i<=NF; i++) {if($i~/cxaid=/){cxaid=$i}};{print $1":"cxaid} }' | tr -d ' ")' | sed 's/^.*(//g'` )

# Create file
echo "BASELINE = { }" > ${outfile}
echo "BUNDLED_LMC = { }" >> ${outfile}
echo "EXT_LMC = { }" >> ${outfile}
printf "\nUP =\t{\n" >> ${outfile}
for upid in ${upids}
do
  upname=`ci_data.sh -f ${idfile} -a read -i ${upid} -p 7`
  upno=`ci_data.sh -f ${idfile} -a read -i ${upid} -p 2 | tr '/' '_'`
  uptype=`ci_data.sh -f ${idfile} -a read -i ${upid} -p 16`
  case ${uptype} in
	*RadioTNode)	uptype="BASEBAND-T"
			gupmaker="True";;
	*RadioNode)	uptype="BASEBAND"
			gupmaker="True";;
	*)		uptype="BASEBAND"
			gupmaker="False";;
  esac
  lmclnames=''
  lmcids=`ci_data.sh -f ${idfile} -a read -i ${upid} -p 8`
  for lmcid in ${lmcids}
  do
    lmcname=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 7`
    lmclname=`echo ${lmcname} | tr '[:upper:]' '[:lower:]'`
    lmcno=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 2 | tr '/' '_'`
    lmclnames="\"${lmclname}\" ${lmclnames}"
    lmcrev=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 6`
    lmcdir=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 13`
    lmcinfo=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 14`
    lmcprovideifid=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 17`
    if [ -n "${lmcprovideifid}" ]
    then
	lmcprovideif=`ci_data.sh -f ${idfile} -a read -i ${lmcprovideifid} -p 2 | tr '/' '_'`
    fi
    if [ -n "${lmcdir}" ]
    then
	lmcurl="file://${lmcdir}"
    else
	lmcurl=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 12`
    fi
    if [ "${lmcinfo}" = "external" ]
    then
	elmc="\t\"${lmclname}\":\t(\"${lmcname}\", \"${lmcno}\", \"${lmcrev}\", \"${lmcurl}\"),\n"
	printf "${elmc}" >> ${tmpFile2}
    else
	lmidlist=`ci_data.sh -f ${idfile} -a read -i ${lmcid} -p 8`
	for lmid in ${lmidlist}
	do
	  lmname=''
	  lmno=''
	  swuid=''
	  swuno=''
	  if [ -n "${lmid}" ]
	  then
	      lmname=`ci_data.sh -f ${idfile} -a read -i ${lmid} -p 7`
	      lmno=`ci_data.sh -f ${idfile} -a read -i ${lmid} -p 2| tr '/' '_' `
	      swuid=`ci_data.sh -f ${idfile} -a read -i ${lmid} -p 8`
	      if [ -n "${swuid}" ]
	      then
		  swuno=`ci_data.sh -f ${idfile} -a read -i ${swuid} -p 2 | tr '/' '_'`
	      fi
	  fi
	  lmc="\t\"${lmclname}\":\t(\"${lmcname}\", \"${lmcno}\", \"${lmname}\", \"${lmno}\", \"${swuno}\"),\n"
	  case ${lmcname} in
	      *DUMMY*)	dummylmclname=${lmclname}
			dummylmname=${lmname}
			dummylmno=${lmno}
			printf "${lmc}" >> ${tmpFile};;
	      *MW*)	mwlmclname=${lmclname}
			mwlmcprovideif=${lmcprovideif}
			mwlmname=${lmname}
			mwlmno=${lmno}
			printf "${lmc}" >> ${tmpFile};;
	  esac
	done
    fi
  done
  printf "\t\"${upname}\":  (\"${upno}\", [${lmclnames// /, }], \"${uptype}\"),\n" >> ${outfile}
done
echo "}" >> ${outfile}

# Gupmaker control
echo "USE_GUPMAKER = ${gupmaker}" >> ${outfile}

# Ext LMCs
printf "\nEXT2_LMC =\t{\n" >> ${outfile}
sort -u ${tmpFile2} >> ${outfile}
echo "}">> ${outfile}

# LMCs
printf "\nLMC =\t{\n" >> ${outfile}
sort -u ${tmpFile} >> ${outfile}
echo "}">> ${outfile}

# APPs
printf "\nAPPS = {\n" >> ${outfile}
for app in ${applist[@]}
do
  appname=`echo ${app} | awk -F: '{print $1}'`
  eval `echo ${app} | awk -F: '{print $2}'`
  ifno=''
  if [ -n "${cxaid}" ]
  then
      ifno=`ci_data.sh -f ${idfile} -a read -i ${cxaid} -p 2| tr '/' '_' `
  fi
  echo "	\"${appname}\":	(\"${mwlmclname}\", \"${ifno}\", \"${mwlmname}_${mwlmno}\" )," >> ${outfile}
done
for app in ${tstapplist[@]}
do
  appname=`echo ${app} | awk -F: '{print $1}'`
  eval `echo ${app} | awk -F: '{print $2}'`
  ifno=''
  if [ -n "${cxaid}" ]
  then
      ifno=`ci_data.sh -f ${idfile} -a read -i ${cxaid} -p 2| tr '/' '_' `
  fi
  echo "	\"${appname}\":	(\"${dummylmclname}\", \"${ifno}\", \"${dummylmname}_${dummylmno}\" )," >> ${outfile}
done
echo "}" >> ${outfile}

# SRC dirs
printf "\nSRCDIR = [\n" >> ${outfile}
for dir in ${dirlist}
do \
  echo "	'${dir}'," >> ${outfile}
done
echo " ]" >> ${outfile}

#!/bin/sh
# ----------------------------------------------------------------------
# %File:	xmlCxUpdate.sh %
# %Date:	2017-09-05 %
# Author:       etxbjca
#
# Short description: A tool that modifies cxpnnnnn.xmlSrc files.
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
# Date       Name        What
# -------    --------    -------------------------------------
# 2017-10-05 etxbjca     Created
#----------------------------------------------------------------------
#
#
# Set default variables
#
progname="`basename $0`"
debug=''
topDir=''
USAGE="\n\t${progname} -F '<FrameWork Name>:<FrameWork No>:<FrameWork Rev>' -S '<System Name>:<System No>:<System Rev>' -i <inputdata file> -o <output file> [-d]\n\n"
Exit=0
tmp=`mktemp`

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    local exit_line=$1
    local exit_code=$2
    if [ -z "${debug}" ]
    then
	\rm -f ${tmp}* > /dev/null 2>&1
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

# Get options
while getopts i:o:rF:S:nd option
do
  case $option in
    i) idfile="${OPTARG}";;
    o) outfile="${OPTARG}";;
    s) skelfile="${OPTARG}";;
    F) frameWork="${OPTARG}";;
    r) realRstate='yes';;
    S) system="${OPTARG}";;
    d) debug="-${option}";;
   \?) Error "${USAGE}\n";;
  esac
done
shift `expr $OPTIND - 1`

#
# Check number of options
#
OptNo="9"
if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
    set -x
fi
if [ -n "${realRstate}" ]
then
    prelOpt=''
    OptNo="`expr ${OptNo} + 1`"
else
    prelOpt='-P'
fi
if [ -n "${erlystart}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ ! -s "${idfile}" ]
then
    Error "Missing or empty inputdata file [${idfile}]!"
fi

# Fetch R-state base for nextRev
rstatebase=`ci_data.sh -f ${idfile} -a read -i 0 -p 2 || Error "Missing R-state base for id [0] in skeleton file [${skelfile}]!"`

# R-state calculation
nextrev="nextRev.sh ${prelOpt} -r ${rstatebase} -b ${idfile} -p"

# Identify skeleton/prodNo file
case "${outfile}" in
    *up.xml_tmpl)   	prodNo=`basename "${outfile}" | sed 's/-up\.xml.*$//' | tr '[:lower:]' '[:upper:]' | tr '_' '/'`
    			skelfile=skeleton/UP.xml.skel;;
    *cxs*.xml_tmpl)	prodNo=`basename "${outfile}" | sed 's/\.xml.*$//' | tr '[:lower:]' '[:upper:]' | tr '_' '/'`
    			skelfile=skeleton/CXS.xml.skel;;
    *cxp*.xml_tmpl) 	prodNo=`basename "${outfile}" | sed 's/\.xml.*$//' | tr '[:lower:]' '[:upper:]' | tr '_' '/'`
    			skelfile=skeleton/CXP.xml.skel;;
    *) 			Error "Unknown type of out file [${outfile}]!";;
esac

# Variables
appdirs=( `\ls -d1 [A-Z][A-Z]*/wscript_build test/[A-Z][A-Z]*/wscript_build` )
applist=( `grep 'cxcid=' ${appdirs[@]} | awk -F, '{for (i=1; i<=NF; i++) {if($i~/cxcid=/){cxcid=$i}};{print $1":"cxcid} }' | tr -d ' ")' | sed 's/^.*(//g'` )
frameWorkName=`echo "${frameWork}" | awk -F: '{print $1}'`
frameWorkId=`echo "${frameWork}" | awk -F: '{print $2}' | tr '/' '_'`
frameWorkVersion=`echo "${frameWork}" | awk -F: '{print $3}'`
systemName=`echo "${system}" | awk -F: '{print $1}' | tr '/' '_'`
systemId=`echo "${system}" | awk -F: '{print $2}'`
systemVersion=`echo "${system}" | awk -F: '{print $3}'`
prodId=`ci_data.sh -f ${idfile} -a read -p 1,2 | grep "${prodNo}" | head -1 | awk -F\| '{print $1}'`
prodName=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 7 || Error "Missing product name for id [${prodId}] in skeleton file [${skelfile}]!"`
prodType=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 14 || Error "Missing product type for id [${prodId}] in skeleton file [${skelfile}]!"`
if [ "${prodType}" = "external" ]
then
    prodVersion=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 6 || Error "Missing product version for id [${entry}] in skeleton file [${skelfile}]!"`
    Echo "External product version selected from baseline file [${idfile}]: ${prodName} ${prodNo} ${prodVersion}"
else
    prodVersion=`${nextrev} ${prodNo}`
fi
prodNo=`echo "${prodNo}" | tr '/' '_'`
prodType=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 16 | awk -F, '{print $1}' || Error "Missing product type for id [${prodId}] in skeleton file [${skelfile}]!"`
nodeType=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 16 | awk -F, '{print $2}' || Error "Missing node type for id [${prodId}] in skeleton file [${skelfile}]!"`
entryList=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 8 || Error "Missing content list for id [${prodId}] in skeleton file [${skelfile}]!"`
outDir=`dirname ${outfile}`
skelfileBase=`basename $skelfile`
date=`date '+%Y-%m-%dT%T'`
gitHash=`git rev-parse HEAD`
prodArch=`ci_data.sh -f ${idfile} -a read -i ${prodId} -p 18 | awk -F= '{print $2}'`

if [ ! -s "${skelfile}" ]
then
    Error "Missing or empty skeleton file [${skelfile}]!"
fi

#
# Handle Check/Fetch System metadata with proper syntax.
#
CheckInfo()
{
    for entry in ${metadataList}
    do
      if fgrep '&'"${entry}"'&' ${skelfile} > /dev/null 2>&1
      then
	  entryValue='echo ${'"${entry}"'}'
	  preFormatedValue="`eval ${entryValue} | tr -d '[:space:]' | tr '[:lower:]' '[:upper:]'`"
	  syntaxOk=""
	  while [ -z "${syntaxOk}" -o "$?" != "0" ]
	  do
	    case ${entry} in
		    	frameWorkId)		promptinfo="Framework Product Identity (ABCnnn)";;
  			systemId)		promptinfo="System Product Identity (CSXnnn)";;
			frameWorkVersion)	promptinfo="Framework R-State";;
			systemVersion)		promptinfo="System R-State";;
			frameWorkName)		promptinfo="Framework Short Name?";;
  			systemName)       	promptinfo="System Short Name";;
            esac
	    if [ -z "${preFormatedValue}" ]
	    then
		Error "Missing ${promptinfo}, aborted!"
	    fi
	    formatedValue=`echo "${preFormatedValue}" | tr '_' '/' | sed -e 's%^\([A-Z]*\)\([0-9][0-9][0-9]\)\([0-9,/]*\)$%\1 \2 \3%g' -e 's%^\([0-9,/]*\)\([A-Z]*\)\([0-9][0-9][0-9]\)\([0-9,/]*\)$%\1\2 \3 \4%g'`
	    case ${entry} in
		       *Id)		
		       		if echo "${formatedValue}" | ChProdNo > /dev/null
				then
				    eval `echo "${entry}='${preFormatedValue}'"`
				    syntaxOk="yes"
				else
				    ErrMsg="Syntax Error in Product Identity: [${formatedValue}]"
				    syntaxOk=""
				    preFormatedValue=""
				    echo "${progname}: Error: ${ErrMsg}" >&2
				fi
				;;
		       *Version)	
		       		if echo "${formatedValue}" | tr 'P' 'R' | ChRState > /dev/null
				then
				    eval `echo "${entry}='${preFormatedValue}'"`
				    syntaxOk="yes"
				else
				    ErrMsg="Syntax Error in R-state: [${formatedValue}]"
				    syntaxOk=""
				    preFormatedValue=""
				    Error "${ErrMsg}"
				fi
				;;
		       *)      	
		       		if [ -n "${formatedValue}" ]
				then
				    eval `echo "${entry}='${preFormatedValue}'"`
				    syntaxOk="yes"
				else
				    ErrMsg="Product Short Name is missing!"
				    syntaxOk=""
				    preFormatedValue=""
				    Error "${ErrMsg}"
				fi
				;;
	    esac
	  done
      fi
    done
}


#
# Handle appdata xml files
#
HandleAppdataFile()
  {
    if [ -n "${noAppData}" ]
    then
	return 0
    fi

    appdataDir=`find ${cxfindpath} -type d -name appdata -print | sort -u`
    if [ -n "${appdataDir}" ]
    then
	appdataFileList=`find ${appdataDir} -type f -name '*.xml' -print | sort -u `

      case "${prodArch}" in
	  sim)	  appdataFileList=`echo ${appdataFileList} | tr ' ' '\n' | egrep -v 'pms.*KMT.xml|_tgt.xml|_arm.*\.xml|_vrcs\.xml|_i[36]86\.xml|_vrcs64\.xml|_x86_64\.xml'`;;
	  arm)	  appdataFileList=`echo ${appdataFileList} | tr ' ' '\n' | egrep -v 'pms.*KMT.xml|_sim\.xml|_vrcs\.xml|_i[36]86\.xml|_vrcs64\.xml|_x86_64\.xml'`;;
	  vrcs)	  appdataFileList=`echo ${appdataFileList} | tr ' ' '\n' | egrep -v 'pms.*KMT.xml|_tgt.xml|_sim\.xml|_arm.*\.xml|_vrcs64\.xml|_x86_64\.xml'`;;
	  vrcs64) appdataFileList=`echo ${appdataFileList} | tr ' ' '\n' | egrep -v 'pms.*KMT.xml|_tgt.xml|_sim\.xml|_arm.*\.xml|_vrcs\.xml|_i[36]86\.xml'`;;
      esac
    else
	appdataFileList=''
    fi
    if [ -n "${appdataFileList}" ]
    then
	echo '' > $tmp3
	for appdataFile in ${appdataFileList}
	do
	  appSrcPath=`echo ${appdataFile} | sed 's,/appdata/.*,,g'`
	  appName=`grep 'appdataroot=' ${appSrcPath}/wscript_build 2> /dev/null | awk -F, '{for (i=1; i<=NF; i++) {if($i~/appdataroot=/){appdataroot=$i}};{print $1} }' | tr -d ' ")' | sed 's/^.*(//g'`
	  appNameVsn="${appName}-${cxvsn}"
	  baseAppdataFile=`basename ${appdataFile}`
	  relpath="${cxname}_${cxprod}/${appNameVsn}/priv/appdata"
	  echo "        <appdata relpath=\"${relpath}\" file=\"${baseAppdataFile}\"/>" >> $tmp3	
	done
    fi
}

#
# Handle earlystart script registration
#
HandleStartScript()
  {
      case "${prodArch}" in
	  arm)		erlystart='rcs_start';;
	  vrcs|vrcs64)	erlystart='start_vrcs.sh';;
	  sim)		erlystart='start_g2sim_mw';;
	      *)	erlystart='';;
      esac
      if [ -n "${erlystart}" ]
      then
	  startScript=`find ${cxfindpath} -type f -name "${erlystart}" -print | sort -u | head -1`
	  if [ -z "${startScript}" ]
	  then
	      Error "Cant find erlystart script [${erlystart}] below dir/... [${cxfindpath}]"
	  fi
	  
	  startConfig=`find ${cxfindpath} -type f -name "${erlystart}-cfg.xml" -print | sort -u | head -1`
	  if [ -z "${startConfig}" ]
	  then
	      Error "Cant find erlystart config file [${erlystart}-cfg.xml] below dir/... [${cxfindpath}]"
	  fi
      else
	  Error "Missing erlystart value!"
      fi
      if [ -n "${startScript}" ]
      then
	  echo '' > $tmp5
	  appSrcPath=`echo ${startScript} | awk -F/ '{print $1}'`
	  appName=`grep 'appdataroot=' ${appSrcPath}/wscript_build 2> /dev/null | awk -F, '{for (i=1; i<=NF; i++) {if($i~/appdataroot=/){appdataroot=$i}};{print $1} }' | tr -d ' ")' | sed 's/^.*(//g'`
	  appNameVsn="${appName}-${cxvsn}"
	  baseStartScript=`basename ${startScript}`
	  baseStartConfig=`basename ${startConfig}`
	  relpath="${cxname}_${cxprod}/${appNameVsn}/priv/bin"
	  echo "        <earlystart filepath=\"${relpath}/${baseStartScript}\"  config=\"${relpath}/${baseStartConfig}\"/>" >> $tmp5
      fi
}

# Disable appdata for CXS's
case `basename ${prodNo}` in 
    CXS*)	noAppData=yes;;
esac

#
# Make tmpfile with list of entrie's
#

pid=''
( for entry in ${entryList}
do
    tmp3=${tmp}.3.$$
    tmp5=${tmp}.5.$$
    cxfile=''
    #  Evaluate cxvsn/cxid/cxname
    cxprod=`ci_data.sh -f ${idfile} -a read -i ${entry} -p 2 | tr '/' '_'|| Error "Missing product number for id [${entry}] in skeleton file [${skelfile}]!"`
    cxid=`echo ${cxprod} | tr '/' '_'`
    cxname=`ci_data.sh -f ${idfile} -a read -i ${entry} -p 7 || Error "Missing product name for id [${entry}] in skeleton file [${skelfile}]!"`
    cxtype=`ci_data.sh -f ${idfile} -a read -i ${entry} -p 14 || Error "Missing product type for id [${entry}] in skeleton file [${skelfile}]!"`
    if [ "${cxtype}" = "external" ]
    then
	cxvsn=`ci_data.sh -f ${idfile} -a read -i ${entry} -p 6 || Error "Missing product version for id [${entry}] in skeleton file [${skelfile}]!"`
	Echo "External subproduct version selected from baseline file [${idfile}]: ${cxname} ${cxprod} ${cxvsn}"
    else
	cxvsn=`${nextrev} ${cxprod}`
    fi
    cxbase=``
    case ${cxprod} in 
	*CXP*)		# Load Module Container (LMC)
			cxfile="${cxname}_${cxid}.cxp"
			;;
	*CXC*)		# Load Module
			cxshortprod=`echo "${cxprod}" | sed 's,_[0-Z][0-Z]*$,,'`
			cxmatchpattern=`ci_data.sh -f ${idfile} -a read -p 1,2 | grep "${cxshortprod}" | awk -F\| '{print "cxcid=\""$1"\""}' | tr '[:space:]' '|' | sed 's/|$//'`
			cxfindpath=`egrep ${cxmatchpattern} test/[A-Z][A-Z]*/wscript_build [A-Z][A-Z]*/wscript_build 2> /dev/null | awk -F: '{print $1}' | awk -F/ '{print $1}' | sort -u`
			if [ -n "${cxfindpath}" ]
			then
			    HandleAppdataFile
			    if [ "${cxfindpath}" != "test" ]
			    then
				HandleStartScript
			    fi
			fi
			;;
    esac
    if [ -s "${tmp3}" -o -s "${tmp5}" ]
    then
	echo '      <product name="'"${cxname}"'" id="'"${cxid}"'" version="'"${cxvsn}"'">' >> $tmp
	if [ -s "${tmp3}" ]
	then
	    grep 'appdata' ${tmp3} >> $tmp
	fi
	if [ -s "${tmp5}" ]
	then
	    grep 'earlystart' ${tmp5} >> $tmp
	fi
	echo '      </product>' >> $tmp
	\rm -f ${tmp3} ${tmp5} > /dev/null 2>&1
    else
	if [ -n "${cxfile}" ]
	then
	    echo '      <product name="'"${cxname}"'" id="'"${cxid}"'" version="'"${cxvsn}"'" filename="'"${cxfile}"'"/>' >> $tmp
	else
	    echo '      <product name="'"${cxname}"'" id="'"${cxid}"'" version="'"${cxvsn}"'"/>' >> $tmp
	fi

    fi
done )&
pid="${pid} $!"
wait ${pid} > /dev/null 2>&1

#
# Check Vaules for metadata
#
metadataList="frameWorkName frameWorkId frameWorkVersion systemName systemId systemVersion"
CheckInfo


#
# mwinfo should only be defined in MW-CXP
#
if ! grep earlystart $tmp > /dev/null 2>&1
then
    sedString='-e /mwinfo/d'
fi

#
# Create result file
#
sed \
 ${sedString} \
 -e s¤'&gitHash&'¤"${gitHash}"¤g \
 -e s¤'&prodType&'¤"${prodType}"¤g \
 -e s¤'&nodeType&'¤"${nodeType}"¤g \
 -e s¤'&frameWorkName&'¤"${frameWorkName}"¤g \
 -e s¤'&frameWorkId&'¤"${frameWorkId}"¤g \
 -e s¤'&frameWorkVersion&'¤"${frameWorkVersion}"¤g \
 -e s¤'&systemName&'¤"${systemName}"¤g \
 -e s¤'&systemId&'¤"${systemId}"¤g \
 -e s¤'&systemVersion&'¤"${systemVersion}"¤g \
 -e s¤'&prodName&'¤"${prodName}"¤g \
 -e s¤'&prodId&'¤"${prodNo}"¤g \
 -e s¤'&date&'¤"${date}"¤g \
 -e s¤'&prodVersion&'¤"${prodVersion}"¤g \
 -e /'&contentName&'/r\ $tmp \
 -e /'<product name="&contentName&"'/D \
 $skelfile > ${outfile}

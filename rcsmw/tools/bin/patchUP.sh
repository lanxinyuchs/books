#!/bin/sh
# ----------------------------------------------------------------------
# Short description: Patch Full UP with Build Result, controlled by BL file
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
# main/1   2017-02-02 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Set default variables
#
progname="`basename $0`"
debug=''
outdir=''
realRstate=''
USAGE="\n\t${progname} -i '< Id list>' -f <file> -o <outdir> [-d]\n\n"
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
while getopts i:f:o:t:dt: option
do
  case ${option} in
    i)  idList="${OPTARG}";;
    f)  file="${OPTARG}";;
    o)  outdir="${OPTARG}";;
    d)  debug="-${option}";;
    t)  type="${OPTARG}";;
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
if [ -n "${type}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi

if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -z "${idList}" ]
then
    Error "Missing arguments in ID list!"
fi

if [ ! -s "${file}" ]
then
    Error "Missing or empty file [${file}]!"
fi

if [ ! -d "${outdir}" ]
then
    mkdir -p "${outdir}"
fi


#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi


read_1()
  {
      if [ -n "${3}" ]
      then
	  data=`ci_data.sh -f ${file} -a read -i ${1} -p ${2} | tr '/' '_' | tr ',' '\n' | grep "${3}" | awk -F= '{print $2}'`
      else
	  data=`ci_data.sh -f ${file} -a read -i ${1} -p ${2}`
      fi
      if [ -n "${data}" ]
      then
	  echo "${data}"
      else
	  Error "Empty value read from baseline file:[${file}], Id:[${1}], Pos:[${2}]!"
      fi
}


patchUP()
  {
      TOADD=`read_1 ${ID} 18 toadd`
      MODOPT=''
      TMPMODLIST=''
      UPID=`read_1 ${ID} 18 upbase | tr -d '[:space:]'`
      TOREMOVE=`read_1 ${UPID} 18 toremove`
      TMPTOREMOVE=`echo $TOREMOVE | sed 's/ /,/g'`
      N=1
      for i in ${TOADD}
      do
	MODNO=`read_1 ${i} 2 | tr '/' '_' | tr -d '[:space:]'`
	MODNAME=`read_1 ${i} 7 | tr -d '[:space:]'`
	MODEXT=`read_1 ${i} 15 | tr -d '[:space:]'`
	MODOPTS="${MODOPTS} -module ${outdir}/${MODNAME}_${MODNO}.${MODEXT}"
        TMPREM=`echo $TMPTOREMOVE | cut -f${N} -d, `
        N=` expr ${N} + 1 `
        TMPMODLIST="${TMPMODLIST} ${TMPREM}|${MODNO}|${outdir}/${MODNAME}_${MODNO}.${MODEXT}"
      done
      NO=`read_1 ${UPID} 2 | tr '/' '_' | tr -d '[:space:]'`
      UP=`read_1 ${UPID} 13 | tr -d '[:space:]'`
      UPREV=`read_1 ${UPID} 6 | tr -d '[:space:]'`
      TMPUP=${outdir}/`basename ${UP}`
      PUPNAME=`read_1 ${ID} 7 | tr -d '[:space:]'`
      PUPNO=`read_1 ${ID} 2 | tr '/' '_' | tr -d '[:space:]'`
      PUPREV=${UPREV}
      PUPFILE=${outdir}/${PUPNAME}_${PUPNO}-${PUPREV}.zip
      NODETYPE=`ci_data.sh -f ${file} -a read -i ${ID} -p 16`
      NRANDOM=$(mktemp -u | cut -f2- -d.)
      PUPIMGFILE=${outdir}/${PUPNAME}_${PUPNO}-${NRANDOM}.qcow2
      mkdir ${outdir}/${NRANDOM}
#####################################################################################################
#Temporary solution
if [ "$type" = "" ]; then
( cd ${outdir}/${NRANDOM}
\temporaryPatchUP
)
else
exit 1
fi

ci_data.sh -f ${file} -a write -i ${ID} -p 3 -c ${PUPREV} > /dev/null 
if [ "$NODETYPE" = "qcow2" ]; then
      ci_data.sh -f ${file} -a write -i ${ID} -p 13 -c ${PUPIMGFILE} > /dev/null
else
      ci_data.sh -f ${file} -a write -i ${ID} -p 13 -c ${PUPFILE} > /dev/null
fi
}


temporaryPatchUP()
{
  echo temporaryPatchUP
  cp ${UP} ${TMPUP}
  rm -rf ${outdir}/${NRANDOM}/temporaryPatchUP 
  mkdir -p temporaryPatchUP
  cd temporaryPatchUP
  unzip ${TMPUP}
  ls | wc -w
  XML=$(basename $(ls ${outdir}/${NRANDOM}/temporaryPatchUP/*.xml | grep -v nmsinfo ) )
  cp -f ${outdir}/${NRANDOM}/temporaryPatchUP/$XML ${outdir}/${NRANDOM}/$XML
  cd ../
for i in $TMPMODLIST
do
  FROMNAME=$(echo $i | cut -f1 -d\| )
  TONAME=$(echo $i | cut -f2 -d\| )
  TOCXP=$(echo $i | cut -f3 -d\| )
  TOFILENAME=$( basename $TOCXP)
  get_rev

  cat $XML | grep $FROMNAME   > tmp_list
  while read p; do 
    FROMREV=$(echo $p  | sed 's/version=/#/' | cut -f2- -d# | cut -f2 -d\")
    FROMNAME=$(echo $p  | sed 's/id=/#/' | cut -f2- -d# | cut -f2 -d\")
    FROMFILENAME=$(echo $p  | sed 's/filename=/#/' | cut -f2- -d# | cut -f2 -d\")
    rm -f ${outdir}/${NRANDOM}/temporaryPatchUP/$FROMFILENAME
    cp -f $TOCXP ${outdir}/${NRANDOM}/temporaryPatchUP/
    np=$(echo $p | sed 's/'${FROMFILENAME}'/'${TOFILENAME}'/' | sed 's/'${FROMREV}'/'${TOREV}'/' | sed 's/'${FROMNAME}'/'${TONAME}'/')
    sed  -i "s|${p}|${np}|g" $XML 
    sed  -i "s|${NO}|${PUPNO}|g" $XML
  done < tmp_list
  echo "Remove	REV=$FROMREV	NAME=$FROMNAME	FILENAME=$FROMFILENAME"
  echo "Add	REV=$TOREV	NAME=$TONAME	FILENAME=$TOFILENAME	"
done
rm -f tmp_list


rm ${outdir}/${NRANDOM}/temporaryPatchUP/$XML
mv -f  ${outdir}/${NRANDOM}/$XML ${outdir}/${NRANDOM}/temporaryPatchUP/${PUPNO}-up.xml
build
rm -rf ${outdir}/${NRANDOM}/temporaryPatchUP  

}
build()
{

cd ${outdir}/${NRANDOM}/temporaryPatchUP/
ls -l
 zip ${outdir}/${NRANDOM}/nodeContainer $(ls)
  ls | wc -w
 cd ${outdir}/${NRANDOM}
 #rm -f ${TMPUP}
  echo "NODETYPE=${NODETYPE} $REPO_HOME"
if [ "$NODETYPE" = "LMC" ]; then
  mv -f nodeContainer.zip ${PUPFILE}
elif [ "$NODETYPE" = "qcow2" ]; then
 rm -rf ${outdir}/${NRANDOM}/temporaryPatchUP/*
 cd ${outdir}/${NRANDOM}/temporaryPatchUP/
 mv -f ${outdir}/${NRANDOM}/nodeContainer.zip ${outdir}/${NRANDOM}/temporaryPatchUP/nodeContainer.zip
 echo "/app/rcs-ee/vrcs/tools/rcs-ee_make_vm.sh -U nodeContainer.zip -i -o ${PUPIMGFILE}"
 /app/rcs-ee/vrcs/tools/rcs-ee_make_vm.sh -U nodeContainer.zip -i -o ${PUPIMGFILE}
 cd ${outdir}
else
  rm -rf ${outdir}/${NRANDOM}/temporaryPatchUP/*
  cd ${outdir}/${NRANDOM}/temporaryPatchUP/
   mv -f ${outdir}/${NRANDOM}/nodeContainer.zip ${outdir}/${NRANDOM}/temporaryPatchUP/nodeContainer.zip
  echo "/app/rcs-ee/vrcs/tools/rcs-ee_make_vm.sh -U nodeContainer.zip -i -o ${PUPIMGFILE}"
  /app/rcs-ee/vrcs/tools/rcs-ee_make_vm.sh -U nodeContainer.zip -i -o ${PUPIMGFILE}
  TEMP=$(echo ${NODETYPE} | tr '[:upper:]' '[:lower:]')
  echo "source /proj/platform_cs/users/eprzrze/python_virtenv/bin/activate"
  source /proj/platform_cs/users/eprzrze/python_virtenv/bin/activate
  echo "/app/rcs-ee/vrcs/tools/create_vnf_package.py ${REPO_HOME}/SVNFMD/${TEMP}_template.yaml  ${PUPNO} ${PUPREV} --lcm_scripts ${REPO_HOME}/SVNFMD/lcm_scripts/ --image \"${NODETYPE} ${PUPIMGFILE} $(basename ${PUPIMGFILE})\"  --user_data \"${NODETYPE} ${REPO_HOME}/SVNFMD/userdata/userdata.file userdata.file\"  --output  ${PUPFILE}"

  /app/rcs-ee/vrcs/tools/create_vnf_package.py ${REPO_HOME}/SVNFMD/${TEMP}_template.yaml  ${PUPNO} ${PUPREV} --lcm_scripts ${REPO_HOME}/SVNFMD/lcm_scripts/ --image "${NODETYPE} ${PUPIMGFILE} $(basename ${PUPIMGFILE})"  --user_data "${NODETYPE} ${REPO_HOME}/SVNFMD/userdata/userdata.file userdata.file"  --output  ${PUPFILE}
cd ${outdir}
fi

}
get_rev()
{
  mkdir t$$
  cd t$$
  tar -xzf  $TOCXP
  TOREV=$(cat *.xml | grep "id=\"${TONAME}\"" | grep "version="  | sed 's/version=/#/' | cut -f2- -d# | cut -f2 -d\")
  cd ..
  rm -fr t$$
}
#
# Execution start
#
REPO_HOME=$(pwd)
Echo "Updating UP for ID: [${idList}]"
idPid=''
for ID in ${idList}
do
  Echo "UP update of ID: [${ID}] started..."
  ( patchUP > ${tmp}.${ID} 2>&1 || echo $? > ${tmp}.${ID}.error )&
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

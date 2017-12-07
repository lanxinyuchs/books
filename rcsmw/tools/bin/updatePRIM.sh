#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	updatePRIM.sh %
# %CCaseRev:	/main/42 %
# %CCaseDate:	2016-11-30 %
# %CCaseDocNo:	511/190 55-LXA 119 334 Ux %
# Author      : Björn Carlsson (etxbjca)
#
# Short description: refresh R-states and PSD's in PRIM for all products in RBS CS
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
# main/1   2014-09-11 etxbjca     Created
# main/17  2015-05-11 etxbjca     Added support for SUBPROD's (CXC's in CXC's)
#----------------------------------------------------------------------
#

if [ -w $0 -o -n "${PDI_DBG}" ]
then
    set -x
fi

#
# Set default variables
#
progname="`basename $0`"
debug=''
tmp="`mktemp`"
tmpFile="${tmp}.${progname}"
warnings=${tmpFile}.warnings
pdiopt=''
IS='1095-'
USAGE="\n\t${progname} -p <Product Level List> [-d] [-s|-r]\n\n"
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
while getopts p:D:P:drs option
do
  case ${option} in
    p)  prodLevelList="${OPTARG}";;
    d)  debug="-${option}";;
    D)  DSCODE="-${option} ${OPTARG}";;
    P)  PRCODE="-${option} ${OPTARG}";;
    r)  relIS="-${option}";;
    s)  syncPSD="-${option}";;
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
if [ -n "${syncPSD}" -a -z "${relIS}" -a -z "${DSCODE}" -a -z "${PRCODE}" ]
then
    OptNo="`expr ${OptNo} + 1`"
    pdiopt=${syncPSD}
fi
if [ -z "${syncPSD}" -a -n "${relIS}" ]
then
    OptNo="`expr ${OptNo} + 1`"
    pdiopt=${relIS}
fi
if [ -z "${syncPSD}" -a -n "${DSCODE}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -z "${syncPSD}" -a -n "${PRCODE}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -z "${prodLevelList}" ]
then
    Warn "Missing arguments in Product Level List!"
fi


#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi


getRstate()
  {
    CHECKDIR=$1
    CHECKRSTATE=''
    if [ -d "$CHECKDIR" ]
    then
      # Check if selecet by label rule
      CHECKRSTATE=`cleartool ls -d $CHECKDIR | awk '{print $3}' | awk -F\- '{print $2}' | head -1`
      if ! echo $CHECKRSTATE | ChRState > /dev/null 2>&1
      then
	CHECKRSTATE=`nextRev -C -d $CHECKDIR`
      fi
    fi
    echo $CHECKRSTATE
}


#
# Execution start
#
Echo "Starting RBS CS PRIM Refresh..."

for prodLevel in ${prodLevelList}
do
    if [ ! -r "${prodLevel}" ]
    then
	continue
    fi
    if [ -h "${prodLevel}" ]
    then
	prodLevel=`( cd ${prodLevel} && pwd )`
    fi
    cd ${prodLevel} || Error "Failed to change to directory [${prodLevel}], aborting!"
    
    PROD=`nextRev -p -d ${prodLevel}|tr '_' '/'`
    if nextRev -R -d ${prodLevel}
    then
	case ${PROD} in
	    CXA*)	
	    		LBTYPE=`nextRev -L -d ${prodLevel}/../*CNX*`
			FULLRSTATE=`nextRev -C -d ${prodLevel}/../*CNX* | sed 's/[0-9]*$//'`;;
	    *)	 	
	    		LBTYPE=`nextRev -L -d ${prodLevel}`
			FULLRSTATE=`nextRev -C -d ${prodLevel} | sed 's/[0-9]*$//'`;;
	esac
    else
	case ${PROD} in
	    CSX101*)	LBTYPE=`nextRev -L -d ${prodLevel}`;;
	    CXA*)	LBTYPE=`cleartool ls -d ${prodLevel}/../*CNX* | awk '{print $3}'`;;
	    *)	 	LBTYPE=`cleartool ls -d ${prodLevel} | awk '{print $3}'`;;
	esac
	FULLRSTATE=`echo ${LBTYPE} | awk -F\- '{print $2}' | sed 's/[0-9]*$//'`
	if [ -z "${LBTYPE}" -o -z "${FULLRSTATE}" ]
	then
	    Warn "Missing label rule for product directory [${prodLevel}], skipping..."
	    if [ "${prodLevel}" != "`echo ${prodLevelList} | awk '{print $NF}'`" ]
	    then
	      continue
	    else
	      exit 0
	    fi
	fi
    fi
    if [ -n "${relIS}" -o -n "${DSCODE}" -o -n "${PRCODE}" ]
    then
	if [ -n "${PROD}" -a -n "${FULLRSTATE}" ]
	then
	    echo "${IS},${PROD},${FULLRSTATE}" >> ${tmpFile}
	else
	    Warn "No product data found for dir [${prodLevel}], product skipped!"
	fi
    else
	PRODLIST=''
	CCA_REV_OPT=''
	CCA_ARCHIVE_ID=`cleartool describe -fmt '%[cca_archive_id]Sa\n' vob:${prodLevel} | tr -d '"()' | tr -d '[:space:]'`
	CCA_REV_STATE=`cleartool describe -fmt '%[cca_rev_state]Sa\n' lbtype:${LBTYPE} | tr -d '"()'| tr -d '[:space:]'`
	if [ -n "${CCA_REV_STATE}" ]
	then
	    CCA_REV_OPT="-c ${CCA_REV_STATE}"
	fi
	case ${PROD} in
	    CSX[0-9]*/[0-9]*)
	    		CODEDOC=''
			src_prod=`echo ${PROD} | awk -F/ '{print $1}'`
			src_prod_dir=`( cd /vobs/rcs/dev/*_${src_prod} && pwd )`
	    		for cca_prod in ${PROD},${prodLevel} ${src_prod},${src_prod_dir} `nextRev -p -d ${RDE_TOP}`,${RDE_TOP}
			do
			  loop_prod=`echo ${cca_prod} | awk -F, '{print $1}'`
			  loop_prod_dir=`echo ${cca_prod} | awk -F, '{print $2}'`
			  lbtype=`nextRev -L -d ${loop_prod_dir}`
			  cca_rev_state=`cleartool describe -fmt '%[cca_rev_state]Sa\n' lbtype:${lbtype}@${loop_prod_dir} | tr -d '"()'| tr -d '[:space:]'`
			  CODEDOC="${CODEDOC} 19011-${loop_prod},${cca_rev_state}"
			done
  			dirList="`\ls -d1 *[CL][RX][A-Z]*[0-9,_]* 2> /dev/null || true`";;
	    CSX*)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
  			dirList="`\ls -d1 *[CL][RX][A-Z]*[0-9,_]* 2> /dev/null || true`";;
            CXA*)	prodLevelTop=`dirname ${prodLevel}`
	    		prodTop=`basename ${prodLevelTop}`
	    		CAXDIR=`\ls -d1 ${prodLevel}/*CA[A-Z]*[0-9,_]* 2> /dev/null | grep "/${prodTop}/" | head -1 || true`
			if [ -h "${CAXDIR}" -a -d "${CAXDIR}" ]
			then
			    CAXDIR=`( cd ${CAXDIR} && pwd )`
			fi
	    		CODEDOC=19011-`nextRev -p -d $(dirname $(dirname ${CAXDIR}))/*CNX*`,${CCA_REV_STATE}
			dirList="`\ls -d1 *CA[A-Z]*[0-9,_]* 2> /dev/null || true`";;
            CX[SP]*)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 *CX[A-Z]*[0-9,_]* 2> /dev/null || true`";;
	    CNX9013327)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 ../*_CXC*[0-9,_]* *CA[A-Z]*[0-9,_]* 2> /dev/null || true`"
			subProdDirList="`\ls -d1 ../*_C[A-Z]*[0-9,_]*/*_CXC*[0-9,_]* 2> /dev/null || true`";;
	    CNX9012629)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 ../*_C[A-Z]*[0-9,_]* *CA[A-Z]*[0-9,_]* 2> /dev/null || true`"
			subProdDirList="`\ls -d1 ../*_C[A-Z]*[0-9,_]*/*_CX[AC]*[0-9,_]* | egrep -v 'CXA11445|CXC1737262_3' 2> /dev/null || true`";;
	    CNX*)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 ../*_C[A-Z]*[0-9,_]* *CA[A-Z]*[0-9,_]* 2> /dev/null || true`"
			subProdDirList="`\ls -d1 ../*_C[A-Z]*[0-9,_]*/*_CXC*[0-9,_]* 2> /dev/null || true`";;
	    CRX901265)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 RCSEE/*CN[A-Z]*[0-9,_]* NL/*CN[A-Z]*[0-9,_]* 2> /dev/null || true`";;
	    CRX*)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 */*CN[A-Z]*[0-9,_]* 2> /dev/null || true`";;
	    LXA*)	CODEDOC=19011-${PROD},${CCA_REV_STATE}
			dirList="`\ls -d1 *[CL][RX][A-Z]*[0-9,_]* 2> /dev/null || true`";;
	    *)		Error "Dont know how to manage product type [${PROD}]";;
        esac

	if [ ! -n "${dirList}" ]
	then
	    Echo "No substructure for content directory [${prodLevel}], skipping to next..."
	    continue   
	fi
	for dir in ${dirList}
	do
	  if [ ! -r "${dir}" ]
          then
	      Warn "Skipping product, directory [${dir}] is not readable"
	      continue
	  fi
	  if [ -h "${dir}" -a -d "${dir}" ]
	  then
	      dir=`( cd ${dir} 2> /dev/null && cleartool pwd )`
	  fi

	  DIRPROD=`nextRev -p -d ${dir}|tr '_' '/'`

	  case ${DIRPROD} in
	      CXP102*)	PRODRSTATE=`getRstate ${dir}`;;
	      CXA*)	PRODRSTATE=`getRstate ${dir}/../*CNX* | sed 's/[0-9]*$//'`;;
	      *)	PRODRSTATE=`getRstate ${dir} | sed 's/[0-9]*$//'`;;
	  esac

	  if [ -z "${PRODRSTATE}" ]
	  then
	      Warn "Missing label rule for content directory [${dir}], skipping to next..."
	      continue
	  fi

	  PRODLIST="${PRODLIST} ${DIRPROD},${PRODRSTATE}"
	done
	if [ -n "${subProdDirList}" ]
	then
	    for subProdDir in ${subProdDirList}
	    do
	      if [ ! -r "${subProdDir}" ]
	      then
		  continue
	      fi
	      SUBPRODDIR=`dirname ${subProdDir}`
	      SUBPROD=`nextRev -p -d ${SUBPRODDIR}`
	      if [ -h "${subProdDir}" ]
	      then
		  dir=`( cd ${subProdDir} && cleartool pwd )`
	      fi
	      if ! nextRev -R -d ${subProdDir}
	      then
		  Warn "Missing label rule for content directory [${subProdDir}], skipping to next..."
		  continue
	      fi
	      SUBPRODRSTATE=`getRstate ${subProdDir} | sed 's/[0-9]*$//'`
	      SUBPRODLIST="${SUBPRODLIST} ${SUBPROD},`nextRev -p -d ${subProdDir}|tr '_' '/'`,${SUBPRODRSTATE}"
	    done
	fi
	if [ -n "${CCA_ARCHIVE_ID}" -a -n "${PROD}" -a -n "${FULLRSTATE}" -a -n "${CODEDOC}" ]
	then
	    if [ -z "${PRODLIST}" ]
	    then
		Warn "Empty product list, skipping update!"
		continue
	    fi
	
	    if [ -z "${pdiopt}" ]
            then
	        # Update IS if there is an active label lock on $PROD
		if cleartool catcs | egrep -v '\-none|AUTORELEASE' | grep '^element' | grep "${PROD}" > /dev/null 2>&1
		then
		    Echo "Label lock active on product: [`basename ${prodLevel}`, ${FULLRSTATE}], additional Info Structure (1095-) refresh will be executed." 
		    pdiopt='-s'
		fi
	    fi
	    if [ -n "${SUBPRODLIST}" ]
	    then
		Echo "Working with product: [`basename ${prodLevel}`, ${FULLRSTATE}] with content: [${PRODLIST}] and subcontent: [${SUBPRODLIST}]"
		Echo "Running command: [runPDI.sh ${debug} -a ${CCA_ARCHIVE_ID} -B ${PROD} -R ${FULLRSTATE} -p \"${PRODLIST}\" -S \"${SUBPRODLIST}\" -C \"${CODEDOC}\" ${CCA_REV_OPT} ${pdiopt}]"
		runPDI.sh ${debug} -a ${CCA_ARCHIVE_ID} -B ${PROD} -R ${FULLRSTATE} -p "${PRODLIST}" -S "${SUBPRODLIST}" -C "${CODEDOC}" ${CCA_REV_OPT} ${pdiopt} || Error "PRIM update failed for product: [`basename ${prodLevel}`, ${FULLRSTATE}] with content: [${PRODLIST}]!"
	    else
		Echo "Working with product: [`basename ${prodLevel}`, ${FULLRSTATE}] with content: [${PRODLIST}]"
		Echo "Running command: [runPDI.sh ${debug} -a ${CCA_ARCHIVE_ID} -B ${PROD} -R ${FULLRSTATE} -p \"${PRODLIST}\" -C \"${CODEDOC}\" ${CCA_REV_OPT} ${pdiopt}]"
		runPDI.sh ${debug} -a ${CCA_ARCHIVE_ID} -B ${PROD} -R ${FULLRSTATE} -p "${PRODLIST}" -C "${CODEDOC}" ${CCA_REV_OPT} ${pdiopt} || Error "PRIM update failed for product: [`basename ${prodLevel}`, ${FULLRSTATE}] with content: [${PRODLIST}]!"
	    fi
	else
	    Error "Missing parameters to update PRIM, aborting!"
	fi
    fi
done

if [ -n "${relIS}" -o -n "${DSCODE}" -o -n "${PRCODE}" ]
then
    if [ -s "${tmpFile}" ]
    then
	Echo "Running command: [runPDI.sh -p \"`cat ${tmpFile}`\" ${relIS} ${DSCODE} ${PRCODE} ${debug}]"
	runPDI.sh -p "`cat ${tmpFile}`" ${relIS} ${DSCODE} ${PRCODE} ${debug} || Error "PRIM Update faild for:\n `cat ${tmpFile}`" 
    else
	Error "Missing parameters to update PRIM, aborting!"
    fi
fi

#exit $?

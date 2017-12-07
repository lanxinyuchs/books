#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	runPDI.sh %
# %CCaseRev:	/main/144 %
# %CCaseDate:	2017-05-04 %
# %CCaseDocNo:	511/190 55-LXA 119 334 Ux %
# Author      : Björn Carlsson (etxbjca)
#
# Short description: Support PRIM administration during "clearmake release"
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
# main/1   2014-09-04 etxbjca     Created
# main/46  2015-02-03 etxbjca     Proper 1901x- management introduced
# main/49  2015-02-10 etxbjca     Now supporting Substructures of CXC PSD's
# main/56  2015-03-10 etxbjca     "setprcod" replaced by "chrstate -src cpws"
# main/94  2015-05-18 etxbjca     Missing PSD entry bug fixed (prod does not exist in "current" R-state)
# main/104 2015-08-11 etxbjca     Bugs fixed for updatePSD
# main/107 2015-08-20 etxbjca     Updated DEFAULTIGNORELIST
# main/110 2015-09-03 etxbjca     Improved DESRESP/RELRESP management
# main/136 2016-08-23 etxbjca     Updated with new organization
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
progdir="`dirname $0`"
debug=''
ldapServer="ecd.internal.ericsson.com"
ldapFlags='-b ou=users,ou=internal,o=ericsson -p 389 -D uid=rcsci1,ou=users,ou=internal,o=ericsson -w Ericsson666 -s sub -a always -z 1000 -x -LLL'
tmp="`mktemp`"
tmpFile="${tmp}.${progname}"
DOCREV=''
USAGE="\n\t${progname}[-I '<Ignore List>'] [ -p '<Product List>' [-S '<Sub Product List>'] [-r] [-D <DS code>] [-P <PR Code>] ] | [ -a <CCA Archive Id>  -B <Base Product>  -p <Product List> -C <Code Document Class> -R <R-state> [-s]] [-d]\n\n"
Exit=0
# Ignore sourced products (IELL, UBOOT, TAIPAN, COBRA, KATLA, RCS-TCU)
DEFAULTIGNORELIST='CXP102|CXP9023113|CXP9023384|CXP9031274/3|CXC1736593|CXC1736595|CXC1739049'
DEFAULTCONTACT='UABHAME'
DEFAULTDESRESP='BNEWIHLG'

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    LASTERR=$2
    if [ -z "${debug}" ]
    then
	\rm -rf ${tmp} ${tmp}* > /dev/null 2>&1 || true
    fi
    exit $LASTERR
}

# Clean on abort
trap 'trap_handler ${LINENO} $?' 0 2 3 15

set -e

# Source common procedures for PDI support
. ${progdir}/2source/pdiSupport.sh

#
# Get options
#
while getopts a:p:c:o:C:D:I:P:B:R:S:O:drs option
do
  case ${option} in
    a)  CCA_ARCHIVE_ID="${OPTARG}";;
    p)  PRODLIST="`echo ${OPTARG} | tr '_' '/'`";;
    c)	CCA_DOCREV="${OPTARG}";;
    o)	OWNER="${OPTARG}";;
    O)	OWNORG="${OPTARG}";;
    C)	CODEDOC="${OPTARG}";;
    D)	DSCODE="${OPTARG}";;
    I)	IGNORELIST="${OPTARG}";;
    P)	PRCODE="${OPTARG}";;
    B)  PROD2="`echo ${OPTARG} | tr '_' '/'`";;
    R)  FULLRSTATE="${OPTARG}";;
    S)  SUBPRODLIST="`echo ${OPTARG} | tr '_' '/'`";;
    d)  debug="yes";;
    r)  relIS="yes";;
    s)  syncPSD="yes";;
    \?)	printf "${USAGE}\n";exit 1;;
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
if [ -n "${CCA_DOCREV}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${CCA_ARCHIVE_ID}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${DSCODE}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${IGNORELIST}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    IGNORELIST="${DEFAULTIGNORELIST}`echo ${IGNORELIST} | tr ' ' '|'`"
else
    IGNORELIST="${DEFAULTIGNORELIST}"
fi
if [ -n "${PRCODE}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${FULLRSTATE}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${CODEDOC}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${PROD2}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${SUBPRODLIST}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi
if [ -n "${syncPSD}" -a -z "${relIS}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi
if [ -z "${syncPSD}" -a -n "${relIS}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi
if [ -n "${OWNER}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    DEFAULTCONTACT="${OWNER}"
fi
if [ -n "${OWNORG}" ]
then
    OptNo="`expr ${OptNo} + 2`"
    DEFAULTDESRESP="${OWNORG}"
fi
if [ "${OPTIND}" != "${OptNo}" ]
then 
  Error "Wrong number of arguments!\n${USAGE}\n"
fi

if [ -z "${PRODLIST}" ]
then
    Warn "Missing arguments in PRODLIST!"
fi


#
# Define PSD Update procedure
#
updatePSD() {
    # Input: <PSD> <PROD> <LIST>
    PSD=$1
    psdProd=$2
    reuse=$3
    shift 3
    entryList=$*
    newPsdFile=${tmpFile}.newPSD
    curPsdFile=${tmpFile}.curPSD
    tmpPsdFile=${tmpFile}.tmpPSD
    shortRstate=`echo "${FULLRSTATE}" | sed 's/[A-Z,/]*$//'`
    designcode=`runPDIcmd pdi lsprod -pno ${psdProd} -rst ${FULLRSTATE} -csv_noheader -csv_hd_flds 'DesignCode' | head -1`
    if [ "${PSD}" = "13132-" ]
    then
	PSDOPT='PSD1'
    else
	PSDOPT='PSD3'
    fi
    if [ -n "${designcode}" ]
    then
	designcode=`echo ${designcode} | awk '{print $1}'`
	if [ "${designcode}" != "DS-" ]
        then
	    Echo "PRIM PSD [${PSD}${psdProd}] not needed to modify, ${psdProd} ${FULLRSTATE} is already released [${designcode}]"
	    return
	fi
    fi
    if [ "${reuse}" = "no" -o "${designcode}" = "DS-" ]
    then
	echo '' > ${newPsdFile}.tmp
	for entry in ${entryList}
	  do
	  entryProd=`echo "${entry}" | awk -F, '{print $1}'`
	  entryRstate=`echo "${entry}" | awk -F, '{print $2}'`
	  entryFullRstate=`echo ${entryRstate} | sed 's/[0-9]*$//'`
	  subentry=''
	  if pdi lsprod -pno ${entryProd} -rst ${entryFullRstate} > /dev/null 2>&1
	  then
	      subentry="${entryProd},${entryFullRstate}"
	  else
	      if pdi lsprod -pno ${entryProd} -rst ${entryRstate} > /dev/null 2>&1
	      then
		  subentry="${entryProd},${entryRstate}"
	      else
		  highestEntryRstate=`runPDIcmd pdi lsprod -pno ${entryProd} -csv_noheader -csv_hd_flds 'RState' | grep -v 'R999A' | head -1` 2> /dev/null
		  if [ -n "${highestEntryRstate}" ]
		  then
		      subentry="${entryProd},${highestEntryRstate}"
		  else
		      Warn "R-state [${entryRstate}] for product [${entryProd}] does not exist!"
		  fi
	      fi
	  fi
	  if [ -n "${subentry}" ]
	  then
	      echo "${subentry}" >> ${newPsdFile}.tmp
	  fi
	done
	if [ "${PSDOPT}" = "PSD1" ]
	then
	    cat ${newPsdFile}.tmp | uniq | sed 's/,\(.*\)/,=\1,1/g' > ${newPsdFile}
	else
	    cat ${newPsdFile}.tmp | uniq | sed 's/,\(.*\)/,\1,1/g' > ${newPsdFile}
	fi
    else
	Echo "PRIM PSD [${PSD}${psdProd}] not modified, ${psdProd} ${FULLRSTATE} is marked for reuse"
	return
    fi
    
    # Add latest registred 3PP products to new PSD
    echo '' > ${tmpPsdFile}
    if runPDIcmd pdi lspsd -pno ${psdProd} -dc ${PSD} -csv_noheader -csv_hd_flds PrNoE,PlSel,RState,Quantity -outf ${tmpPsdFile}
    then
	if [ -s "${tmpPsdFile}" ]
	then
	    cat ${tmpPsdFile} | tr -d ' ' | egrep 'CAX105|CXS102' > ${tmpPsdFile}.2 || true
	    if [ -s "${tmpPsdFile}.2" ]
	    then
		cat ${tmpPsdFile}.2 | tr -d '"' | sed -e 's/=,/=/g' -e 's/,,/,/g' | awk -F. '{print $1}' | sort >> ${newPsdFile}
		Echo "Added 3PP prodcts to PSD [${PSD}] for product [${psdProd} ${FULLRSTATE}]"
	    fi
	fi
    fi

    # Get current PSD from PRIM
    if runPDIcmd pdi consist -pno ${psdProd} -pst1 ${PSDOPT} -appl FP -rst ${shortRstate} -sort S -filtertyp S -csv_noheader -lev 1 -csv_hd_flds PrNoESub,PLSelSub1,PrRevESub,Quantity -outf ${curPsdFile}
    then
	if [ -s "${curPsdFile}" ]
	then
	    cat ${curPsdFile} | tr -d '" ' | sed -e 's/=,/=/g' -e 's/,,/,/g' | awk -F. '{print $1}' | sort > ${curPsdFile}.srt
	else
	    Warn "PRIM PSD [${PSD}] read for product [${psdProd} ${FULLRSTATE}] not possible!"
	fi
    else
	Warn "Failed to extract content for current PSD [${PSD}${psdProd}]!"
    fi

    # Validate new/curretnt PSD content, update PSD if needed
    sort -n ${newPsdFile} | grep -v '^$' > ${newPsdFile}.srt
    
    if cmp -s ${newPsdFile}.srt ${curPsdFile}.srt
    then
	Echo "PRIM PSD [${PSD}${psdProd}] not modified, already up to date for ${FULLRSTATE}"
    else
	if [ ! -s "${curPsdFile}" ]
	then
	    Echo "PSD [${PSD}] does not exist or is not readable for product [${psdProd} ${FULLRSTATE}], starting PSD creation..."
	else
	    Echo "Updating PSD [${PSD}${psdProd} ${FULLRSTATE}]..."
	fi
	if runPDIcmd pdi mkpsd -hdpno ${psdProd} -hdrst ${FULLRSTATE} -psdc ${PSD} -head pno,rst,qte -rows_f ${newPsdFile}.srt > ${newPsdFile}.result 2>&1
	then
	    Echo "PRIM PSD [${PSD}${psdProd}] successfully created for ${FULLRSTATE}"
	else
	    if [ "$?" = "2" ]
	    then
		cat ${newPsdFile}.result >&2
	    else
		cat ${newPsdFile}.result ${newPsdFile}.srt >&2
		Error "PRIM PSD [${PSD}${psdProd}] update for ${FULLRSTATE} failed!"
	    fi
	fi
    fi
}


#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi

#
# Execution start
#
Echo "Starting PRIM update, please be patient..."
#
# Login to RACF to enshure there is a valid login session when needed (minimum valid for 30 min)
#
logonRACF.sh 30 || Warn "Faild to prepare a valid RACF session!"
 
#
# IS Release 
#
if [ -n "${relIS}" -o -n "${DSCODE}" -o -n "${PRCODE}" ]
then
    for entry in `echo ${PRODLIST} | tr ' ' '\n' | egrep -v ${IGNORELIST}`
    do
      isdc="`echo ${entry} | awk -F, '{print $1}'`"
      pno="`echo ${entry} | awk -F, '{print $2}'`"
      rst="`echo ${entry} | awk -F, '{print $3}'`"
      if [ -n "${relIS}" ]
      then
	  curPR=`runPDIcmd pdi lsprod -pno ${pno} -rst ${rst} -csv_hd_flds ReleaseCode -csv_noheader -skp_tag retrieveInformationCode.output.row | tr -d '" '`
	  if [ "${curPR}" = "PR-" ]
	  then
	      chkisResult=`runPDIcmd pdi lsis -isdc ${isdc} -prodno ${pno} -rst ${rst} -srvc ISRstCol -csv_hd_flds InfoStrucStatus,DocProdNumber -csv_noheader 2> /dev/null | tr -d '[ ]' | grep "${isdc}${pno}"`
	      if [ -n "${chkisResult}" ]
	      then
		  if echo "${chkisResult}" | grep 'PREL' > /dev/null
	          then
		      if runPDIcmd pdi relis -head isdc,pno,rst -row ${isdc},${pno},${rst} -csv_hd_flds ProductNumber,RState -csv_noheader > /dev/null
		      then
			  Echo "PRIM IS [${isdc}${pno},${rst}] successfully released!"
		      else
			  Warn "PRIM IS [${isdc}${pno},${rst}] release failed!"
		      fi
		  elif echo "${chkisResult}" | grep 'FREE' > /dev/null
		  then
		      Echo "PRIM IS [${isdc}${pno},${rst}] already released!"
		  else
		      Warn "PRIM IS [${isdc}${pno},${rst}] management failed!"
		  fi
	      else
		  Warn "PRIM IS [${isdc}${pno},${rst}] is missing, check failed!"
	      fi
	  elif [ -n "${curPR}" ]
	  then
	      Echo "PRIM IS [${isdc}${pno},${rst}] already released (PR-code [${curPR}] exist)!"
	  else
	      Error "PRIM IS [${isdc}${pno},${rst}] release failed! PR-code is missing!"
	  fi
      fi
      if [ -n "${DSCODE}" ]
      then
	  relset=''
	  curDS=`runPDIcmd pdi lsprod -pno ${pno} -rst ${rst} -csv_hd_flds DesignCode,DesignInd -csv_noheader -skp_tag retrieveInformationCode.output.row | tr -d '" '| awk -F, '{ if ($2=="") print $1; else print $1"/"$2}'`
	  if [ "${curDS}" != "${DSCODE}" ]
	  then
	      case ${DSCODE} in
		  DS3|DS4)	relset='-relset Y';;
	      esac
	      if runPDIcmd pdi chrstate -src cpws ${relset} -pno ${pno} -rst ${rst} -dsc ${DSCODE} -csv_hd_flds ProductNumber,RState -csv_noheader > /dev/null
	      then
		  Echo "PRIM DS Code [${DSCODE}] on [${pno},${rst}] successfully set!"
	      else
		  Warn "PRIM DS Code [${DSCODE}] setting on [${pno},${rst}] failed!"
	      fi
	  else
	      Echo "PRIM DS Code [${DSCODE}] on [${pno},${rst}] already set!"
	  fi
      fi
      if [ -n "${PRCODE}" ]
      then
	  curPR=`runPDIcmd pdi lsprod -pno ${pno} -rst ${rst} -csv_hd_flds ReleaseCode -csv_noheader -skp_tag retrieveInformationCode.output.row | tr -d '" '`
	  if [ "${curPR}" != "${PRCODE}" ]
	  then
	      if runPDIcmd pdi chrstate -src cpws -pno ${pno} -rst ${rst} -prc ${PRCODE} -relset Y -csv_hd_flds ProductNumber,RState -csv_noheader > /dev/null
              then
		  Echo "PRIM PR Code [${PRCODE}] on [${pno},${rst}] successfully set!"
	      else
		  Warn "PRIM PR Code [${PRCODE}] setting on [${pno},${rst}]failed!"
	      fi
	  else
	      Echo "PRIM PR Code [${PRCODE}] on [${pno},${rst}] already set!"
	  fi
      fi
    done
    exit 0
fi

if ! echo "${PRODLIST}" | grep "${PROD2}" > /dev/null
then
    PRODLIST="${PROD2} ${PRODLIST}"
fi

#
# PSD rule logic
#

prodList=''
case ${PROD2} in
    *CS[HX]*/*)	select='CXS|LXA';subList="${PROD2}";;
    *CS[HX]*)	select='CRX';subList="${PROD2}";;
    *CR[HX]*)	select='CNX';subList="${PROD2}";;
    *CXA*)	select='CAX';subList="${PRODLIST}";;
    *CXC*)	select='CAX';subList="${PRODLIST}";;
    *CN[HX]*)	select='CAX';subList="${PRODLIST}";;
    *CXS*)	select='CXP|CXA';subList="${PROD2}";;
    *CXP903*)	select='CXP902';subList="${PROD2}";;
    *CXP202*)	select='CXP902';subList="${PROD2}";;
    *CXP902*)	select='CXC';subList="${PROD2}";;
    *CXP201*)	select='CXC';subList="${PROD2}";;
    *LXA*)	select='*';subList="${PROD2}";;
    *)		Error "Dont know how to manage product [${PROD2}]";;
esac

for prod in ${PRODLIST}
do
  if echo ${prod} | grep -v ${PROD2} | egrep "${select}" > /dev/null 2>&1
  then
      prodList="${prodList} ${prod}"
  fi
done

#
# Create new R-state when needed + update RelResp and Contact
#
for prod in ${subList}
do
  prod2=`echo ${prod} | awk -F, '{print $1}'`
  rstate2=`echo ${prod} | awk -F, '{print $2}'`
  if [ -n "${rstate2}" ]
  then
      R_STATE=${rstate2}
  else
      R_STATE=${FULLRSTATE}
  fi
  # Check R-State/DesResp
  if PRODINFO=`runPDIcmd pdi lsprod -prodno "${prod2}" -rstate ${R_STATE} -csv_hd_flds RState,DesignResponsible,ReleaseResponsible,ContactId -csv_noheader -skp_tag retrieveInformationCode.output.row | tr -d '" '`
  then
      if [ -z "${PRODINFO}" ]
      then

	  
          # Create new R-state
	  if ! RSTATE=`runPDIcmd pdi mkrstate -prodno "${prod2}" -rstate "${R_STATE}" -csv_hd_flds RState -csv_noheader`
	  then
	      Error "PRIM R-state registration [${prod2}, ${R_STATE}] failed!"
	  fi
      	  # Get updated R-State/DesResp
	  if ! PRODINFO=`runPDIcmd pdi lsprod -prodno "${prod2}" -rstate ${R_STATE} -csv_hd_flds RState,DesignResponsible,ReleaseResponsible,ContactId -csv_noheader -skp_tag retrieveInformationCode.output.row | tr -d '" '`
	  then
	      Error "PRIM Product Info extraction [${prod2}, ${R_STATE}] failed!"
	  fi
      fi
  else
      Error "PRIM Product Data extraction [${prod2}, ${R_STATE}] failed!"
  fi
  CHECKRSTATE=`echo "${PRODINFO}" | awk -F, '{print $1}'`
  DESRESP=`echo "${PRODINFO}" | awk -F, '{print $2}'`
  RELRESP=`echo "${PRODINFO}" | awk -F, '{print $3}'`
  CONTACT=`echo "${PRODINFO}" | awk -F, '{print $4}'`
  if [ "${DESRESP}" != "${DEFAULTDESRESP}" -o "${RELRESP}" != "${DEFAULTDESRESP}" -o "${CONTACT}" != "${DEFAULTCONTACT}" ]
  then
      CONTACT=${DEFAULTCONTACT}
      DESRESP=${DEFAULTDESRESP}
      RELRESP=${DEFAULTDESRESP}
      PRODINFO2=`runPDIcmd pdi chrstate -csv_hd_flds desResp,relResp,contactId -csv_noheader -doRead -prodNo "${prod2}" -rState "${R_STATE}" -desResp "${DESRESP}" -relResp "${RELRESP}" -contact "${CONTACT}"`
      if [ "${PRODINFO2}" != "${DESRESP},${RELRESP},${CONTACT}" ]
      then
	  Warn "PRIM update of DesResp [${DESRESP}], RelResp [${RELRESP}] and Contact [${CONTACT}] for ${prod2}, ${R_STATE} failed!"
      fi
  fi
done

if [ -z "${syncPSD}" -a -z "${relIS}" ]
then
    if [ -n "${CCA_DOCREV}" ]
    then
	DOCREV=${CCA_DOCREV}
    else
        # Remove current 1901x- version from IS on releated products
	for prod in ${subList}
	do
	  prod2=`echo ${prod} | awk -F, '{print $1}'`
	  rstate2=`echo ${prod} | awk -F, '{print $2}'`
	  if [ -n "${rstate2}" ]
	  then
	      R_STATE=${rstate2}
	  else
	      R_STATE=${FULLRSTATE}
	  fi
	  if [ -z "${syncPSD}" ]
	  then
	      tmpIsFile=${tmpFile}.IS
	      tmpIsFile2=${tmpFile}.IS.2
	      if ! pdi lsis -isdc 1095- -prodno "${prod2}" -rst "${R_STATE}" -ofmt NUL 2> /dev/null
	      then
		  if pdi lsis -isdc 1095- -prodno "${prod2}" -status FREE -csv_hd_flds ChildNoE,ChildSelAttre -csv_noheader 2> /dev/null | egrep -v '131 61-|131 32-' > ${tmpIsFile2}
		  then
		      if [ -s ${tmpIsFile2} ]
		      then
			  grep -v '190 1[01]-' ${tmpIsFile2} | sed 's/,$/,\//g' > ${tmpIsFile}
			  grep '190 1[01]-' ${tmpIsFile2} | sed 's/,$/, /g' >> ${tmpIsFile}
			  if [ -s ${tmpIsFile} ]
		          then
			      if pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -head dno,rev -rows_f ${tmpIsFile} > /dev/null
			      then
				  Echo "Info Structure [1095-${prod2}, ${R_STATE}] successfully copied in PRIM!"
			      else
				  Error "Info Structure copy [1095-${prod2}, ${R_STATE}] in PRIM failed!"
			      fi
			  fi
		      fi
		  else
		      Warn "Reading FREE Info Structure [1095-${prod2}] in PRIM failed!"
		  fi
	      fi
	      if ! pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -object "${CODEDOC}" -objver " " > /dev/null 2>&1
	      then
	          # 2nd try...
		  Warn "Command unsuccessful [pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -object "${CODEDOC}" -objver " "], retrying..."
		  sleep 5
		  if ! pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -object "${CODEDOC}" -objver " " > /dev/null 2>&1
		  then
		      # 3rd try...
		      if pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -object "${CODEDOC}" -objver "/" > /dev/null
		      then
			  if ! pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -object "${CODEDOC}" -objver " " > /dev/null
			  then
			      Warn "Failed to clean obosolete Code Document Revision away from IS [${CODEDOC}] for R-state [${R_STATE}] on product [${prod2}]!"
			  fi
		      fi
		  fi
	      fi
	  fi
	done

        # Create new Rev of 1901x- document
	DOCREV=`runPDIcmd perl ${pdiPATH}/utils/pdi/lib/cpws.pm mkcoderev -arch ${CCA_ARCHIVE_ID} -head pno,rst,dno -row ${PROD2},${FULLRSTATE},${CODEDOC} -DocumentFormat DIRECTORYSTRUCTURE -ADPArchive ${CCA_ARCHIVE_ID} | grep '<DocumentRevision>'| awk -F\> '{print $2}' | awk -F\< '{print $1}'`

	if [ -n "${DOCREV}" ]
	then
	    # Set 1901x- to FREE
	    if ! runPDIcmd pdi chdoc -src cpws -dno ${CODEDOC} -rev ${DOCREV} -lang X -docStatus FREE > /dev/null
	    then
		Error "Release of Code Document [${CODEDOC}] Revision [${DOCREV}] in PRIM failed, aborting!"
	    fi
	    # Update docResp, ignore if failing
	    runPDIcmd pdi chdoc -src rest -dno ${CODEDOC} -rev ${DOCREV} -lang X -docResp ${DEFAULTDESRESP} > /dev/null 2>&1 || true
	else
	    Error "Missing/faulty Code Document Revision [${CODEDOC} Rev=${DOCREV}] for R-state [${FULLRSTATE}], aborting!"
	fi
    fi
fi

# Update IS on related products with new Rev-state of 1901x-
for prod in ${subList}
do
  prod2=`echo ${prod} | awk -F, '{print $1}'`
  rstate2=`echo ${prod} | awk -F, '{print $2}'`
  if [ -n "${rstate2}" ]
  then
      R_STATE=${rstate2}
  else
      R_STATE=${FULLRSTATE}
  fi

  if [ -z "${syncPSD}" ]
  then
      if echo ${CODEDOC} | grep -v ',' > /dev/null 2>&1
      then
	  CODEDOC="${CODEDOC},${DOCREV}"
      fi
      for codedoc in ${CODEDOC}
      do
        cca_docrev=`echo ${codedoc} | awk -F, '{print $2}'`
        cca_doc=`echo ${codedoc} | awk -F, '{print $1}'`
        # Updste IS
	if runPDIcmd pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -object "${cca_doc}" -objver "${cca_docrev}" -acode 2 > /dev/null
        then
	  # Slash all empty Rev's (if any)
	  tmpIsFile3=${tmpFile}.IS3
	  if pdi lsis -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -status PREL -csv_hd_flds ChildNoE,ChildSelAttre -csv_noheader 2> /dev/null | grep ',$' | sed 's/,$/,\//g' > ${tmpIsFile3}
	  then
	      if [ -s ${tmpIsFile3} ]
	      then
		  if pdi add2is -isdc 1095- -prodno "${prod2}" -rstate "${R_STATE}" -head dno,rev -rows_f ${tmpIsFile3} > /dev/null 2>&1
		  then
		      Echo "\"Slashing\" empty Rev(s) in Info Structure [1095-${prod2}, ${R_STATE}] succeded!"
		  else
		      Warn "\"Slashing\" empty Rev(s) in Info Structure [1095-${prod2}, ${R_STATE}] not possible!"
		  fi
	      fi
	  fi
	  Echo "Info Structure [1095-${prod2},${R_STATE}] updated with Code Document [${cca_doc} Rev=${cca_docrev}]"
        else
	  Warn "Failed to update Info Structure [1095-${prod2},${R_STATE}] with Code Document [${cca_doc} Rev=${cca_docrev}]!"
        fi
      done
  fi
  if [ -n "${prodList}" ]
  then
      for entry in ${prodList}
      do
	entryProd=`echo ${entry} | awk -F, '{print $1}'`
	if [ "${entryProd}" = "${prod2}" ]
	then
	    continue
	fi
	SUBPRODLIST="${SUBPRODLIST} ${prod2},${entry}"
      done
  fi
done


# Update PSD's on related products with proper R-states
if [ -n "${SUBPRODLIST}" ]
then
    lastProdList=''
    subProdTopList=`echo ${SUBPRODLIST} | tr ' ' '\n' | awk -F, '{print $1}' | uniq`
    for prod in ${subProdTopList} 
    do
      prod2=`echo ${prod} | awk -F, '{print $1}'`

      prodList=`echo ${SUBPRODLIST} | tr ' ' '\n' | grep '^'"${prod}" | uniq | awk -F, '{print $2","$3}'`
      reuse=no
      if [ "${prodList}" = "${lastProdList}" ]
      then
	  reuse=yes
      fi
      Echo "Analyzing PSD for product [${prod2}]..."
      case ${prod2} in
	  *C[NRS]X*)		updatePSD 13161- ${prod2} ${reuse} ${prodList};;
	  *[LC]X[ACSP]*)	updatePSD 13132- ${prod2} ${reuse} ${prodList};;
      esac
      lastProdList=${prodList}
    done
fi

# Return proper DOCREV
if [ -n "${DOCREV}" -a -z "${CCA_DOCREV}" ]
then
      echo ${DOCREV}
fi
exit 0

#!/bin/sh
# ----------------------------------------------------------------------
# Author:	Björn Carlsson (etxbjca)
#
# Short description: 
# Script that archive/update GASK2/PRIM from Design Environment
#
# In this script PDI, (LZN 901 1398/3) is used as PRIM/GASK interface
#
# Document is stored and then the document information is updated on the
# 1095 of the product that the document is connected to.
# Some special algorithms is used to detect and update more than the
# "own" 1095 for "109 21", "190 89" and "190 99" to minimize manual adjustments
# in PRIM afterwards this script is exeuted.
# 
# Following A-codes is selected by this program and used in PRIM:
#
# A-code	Document type
# ======================================================================
# 0		109 21, 190 95 (own product)
# 1		All CPI documents
# 2		190 89, 190 99 & prom-files (own product)
# 3		Default (own product)
# 4		non-own product dokuments example: 1/190 99-CXP 
#		at 1095-CXS
#
# Access Code   Info
# ======================================================================
# G (Group)     Default
# O (Office)    Files with decclass "190 10" when SW-Gateway is used
# W (World)     All files with A-code "0" or "1"      
#                
# **********************************************************************
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
# **********************************************************************
#

# **********************************************************************

# Define variables
progname="`basename $0`"
progdir="`dirname $0`"
USAGE="\n\t${progname} [-n] [-f] [-d] [-D <DocNo>] [-r <revision>|-s] [-u] [-I <InfoStructure to update in PRIM> -R <R-State>] <file2archive>\n"
LAST=`echo "$*" | awk '{ print $NF }'`
PRODDIR=`dirname $LAST`
LAST=`basename $LAST`

Default=""
ForcePrel=""
Individual=""
infostruct=""
FORMCHG=N
tmpFile=`mktemp`
tmpFile2=${tmpFile}2

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    LASTERR=$2
    \rm -f ${tmpFile}*
    exit $LASTERR
}

# Clean on abort
trap 'trap_handler ${LINENO} $?' 0 2 3 15

set -e

# Source common procedures for PDI support
. ${progdir}/2source/pdiSupport.sh

#
# Define "ask" procedure for "Responsible"
#
get_OwnOrg() {

    #
    # Get DesignResponsible for ${Individual}
    #
    OrgProd=`echo "${Individual}" | tr -d ' ' | tr '_' '/'`
    OwnOrg="`runPDIcmd pdi lsprod -pno ${OrgProd} -rst ${prodrev} -csv_hd_flds DesignResponsible -csv_noheader  -skp_tag retrieveInformationCode.output.row`"
    if [ -z "${OwnOrg}" ]
    then
	Error "Extraction of Design Responsible from PRIM for product [${OrgProd}] and R-state [${prodrev}] failed, aborting!"
    fi
}

#
# Define Bar Graph procedure
#
barGraph() {
    case $* in
	start)	( while :
                do 
		  printf '.' >&2
		  sleep 5
		done )&
		idleproc=$!;;
	stop)	kill ${idleproc}
	        trap 'kill $!' SIGTERM
		printf '\n' >&2;;
    esac
}

#
# Define procedure that check syntax of "DocNo" (InfoStruct)
#
Check4Err()
  {
    
    eval `echo $OPTARG | tr -d " " | tr "[:lower:]" "[:upper:]" | cut -dU -f1 | gawk -F- ' {
	i=split($1 ,a,"/");
	if ( i == 1 )
	  printf("DocTyp=\"%s\";",a[1]);
	else
	  printf("DocTyp=\"%s\";",a[2]);
	  printf("DocNr=\"%s\"; ProdNr=\"%s\"\n",$1,$2); }'`

    DocTyp=`echo "$DocTyp" | sed -n -e '/^[0-9]\{4,4\}$/p' -e 's/^\([0-9][0-9][0-9]\)\([0-9][0-9]\)$/\1 \2/p'`

    if [ -n "${ProdNr}" ]
    then
	DocSuf=`echo "$DocNr" | awk -F/ '{if (NF >= 2){print $1"/"}}'`
	ProdNr=`echo "$ProdNr" | sed 's/^\([A-Z]*\)\([0-9][0-9][0-9]\)\(.*\)/\1 \2 \3/'`
	ErrMsg=""
	
	if ! echo "$ProdNr" | ChProdNo > /dev/null
	then
	    ErrMsg="Syntax Error in Product Number: $ProdNr"
	    return 1
	fi
    
	if ! echo "${DocSuf}${DocTyp}" | ChDocNo > /dev/null
	then
	    ErrMsg="Syntax Error in Document Number: $DocNr"
	    return 1
	fi
    fi
    OPTARG="${DocSuf}${DocTyp}-${ProdNr}"
} 

#
# Define procedure that check syntax of option arguments
#
Checkparam()
  {
    case $option in
      D)
        if [ -n "$OPTARG" ]
	then
	  OPTARG=`echo "$OPTARG" | tr '_' '/'`
	  Default=""
	  DocNo=$OPTARG
	elif [ -n "$NO_INTERACTIVE" ]
	then
	  Error 'Missing DocNo, aborting!'
	else
	  Default=""
	  ErrMsg="Missing DocNo!"
	  OPTARG=""
	  clearprompt proceed -type error -mask proceed -prompt "${progname}: Error: ${ErrMsg}" -prefer_gui
	fi
	;;
      R) 
	OPTARG=`echo "$OPTARG" | tr -d " " | tr "[:lower:]" "[:upper:]" | tr '_' '/'`
	if echo "$OPTARG" | ChRState > /dev/null
	then
	  Default=""
	  prodrev=$OPTARG
	elif [ -n "$NO_INTERACTIVE" ]
	then
	  Error "Syntax Error in R-state: $OPTARG, aborting!"
	else
	  Default="$OPTARG"
	  ErrMsg="Syntax Error in R-state: $OPTARG"
	  OPTARG=""
	  clearprompt proceed -type error -mask proceed -prompt "${progname}: Error: ${ErrMsg}" -prefer_gui
	fi
	;;
      r) 
	OPTARG=`echo "$OPTARG" | tr -d " " | tr "[:lower:]" "[:upper:]" | tr '_' '/'`
	if echo "$OPTARG" | ChRev > /dev/null
	then
	  Default=""
	  rev=$OPTARG
	elif [ -n "$NO_INTERACTIVE" ]
	then
	  Error "Syntax Error in Revision: $OPTARG, aborting!"
	else
	  Default="$OPTARG"
	  ErrMsg="Syntax Error in Revision: $OPTARG"
	  OPTARG=""
	  clearprompt proceed -type error -mask proceed -prompt "${progname}: Error: ${ErrMsg}" -prefer_gui
	fi
	;;
      I)	
	if Check4Err
	then
	  Default=""
	  infostruct=$OPTARG
	elif [ -n "$NO_INTERACTIVE" ]
	then
	  Error "Missing Info Struct data, aborting"
	else
	  Default="$OPTARG"
	  OPTARG=""
	  clearprompt proceed -type error -mask proceed -prompt "${progname}: Error: ${ErrMsg}" -prefer_gui
	fi
	;;
    esac
}

#
# Define procedure that check option arguments
#
CheckOpt()
  {
    if [ -n "$OPTARG" ]
    then
      Checkparam
    fi
    while [ -z "$OPTARG" -a "$?" = "0" ]
    do
      case $option in
        D)	promptinfo="Document Number";;
	R)	promptinfo="R-State";;
	r)	promptinfo="Document Revision";;
	I)	promptinfo="InfoStructure to update in PRIM (DocNo)";;
        *)      promptinfo="Document Responsible Organisation for Infostructure";;
      esac
      if [ -n "$NO_INTERACTIVE" ]
      then
	  Error "Missing ${promptinfo}, aborting!"
      elif clearprompt text -prefer_gui -default "$Default" -prompt "Please enter: <${promptinfo}> " -out ${tmpFile}
      then
        OPTARG=`cat ${tmpFile} ;\rm -f ${tmpFile}`
	Checkparam
      else
	Error 'Faulty Value, aborting!!'
      fi
    done
}

#
# Define procedure that print archive info
#
EchoInfo()
  {
      Echo "Working with file: `basename \"${file}\"`, DocNo=${DocNo2} ${language}, Rev=${Revision}, FormalChange=${FORMCHG}, Status=${stat} (Rstate=${prodrev})"
}

#
# Define procedure that archive file in GASK2
#
Store()
  {
      Echo "Storing file [`basename "${file}"`] to GASK, please wait."
      Lang=`echo ${language} | sed 's/^U//' | tr '[:lower:]' '[:upper:]'`
      # Store file to GASK and monitor progress
      barGraph start
      if ! runPDIcmd pdi putgask -user ${RACF_USER} -DocNo "${DocNo2}" -Lang ${Lang} -Rev ${Revision} -DocStatus ${stat} -Format ${DOCTYPE} -PaperForm ${FORM} -FormalChange ${FORMCHG} -CharacterSet ${CHSET} -file "${file}" -timeout 1800 SaveFileName=Y
      then
	  barGraph stop
	  Error 'GASK storage failed, aborting!'
      else
	  barGraph stop
	  Echo 'GASK storage succeded!'
      fi
}

#
# Define procedure that check if file already is stored in GASK2
#
CheckIfStored()
  {
      # Check if Stored in GASK
      ArchName=`runPDIcmd pdi lsdoc -src cpws -dno ${DocNo2} -rev ${Revision} -csv_hd_flds ArchName -csv_noheader 2> /dev/null | head -1` || true
      if [ "${ArchName}" = "GASK2" ]
      then
	  Echo "Document [${DocNo2} Rev=${Revision}] already stored!"
	  return 1
      else
	  return 0
      fi
}

#
# Define procedure that check and create R-state of product if missing
#
CheckRstate()
  {
      # Check if Stored in GASK
      rStateOk=''
      pnr=`echo "${ProdNr}" | tr -d ' ' | tr '_' '/'`
      regRstate=`runPDIcmd pdi lsprod -prodno "${pnr}" -rstate "${prodrev}" -csv_hd_flds RState -csv_noheader -skp_tag retrieveInformationCode.output.row 2> /dev/null` || true
      if [ "${regRstate}" = "${prodrev}" ]
      then
	  rStateOk=yes
      elif runPDIcmd pdi mkrstate -prodno "${pnr}" -rstate "${prodrev}" > /dev/null
      then
	  rStateOk=yes
      fi
      if [ -z "${rStateOk}" ]
      then
	  Warn "Creation of R-state [${prodrev}] for product [${pnr}] in PRIM failed!"
	  return 1
      else
	  return 0
      fi
}

#
# Define procedure that fetch highest GASK stored Rev
#
FetchHighestRev()
  {
    highStoredRev=`runPDIcmd pdi finddoc -dno ${DocNo2} -rev highest -csv_hd_flds DocumentRevision -csv_noheader | head -1`
    if [ -z "${highStoredRev}" ]
    then
	Warn "PRIM search for Rev of document [${DocNo}] failed!\n"
    fi
}

#
# Define procedure that update information in PRIM
#
UpDate()
  {
    # Check, if disabled
    if [ -z "${NO_STRUCT_UPDATE}" ]
    then
      if [ -n "${infostruct}" ]
      then
	Struct=`echo "${infostruct}" | awk -F- '{print $1}'`"-"
      fi
      if CheckRstate
      then
	  pnr=`echo "${ProdNr}" | tr -d ' ' | tr '_' '/'`
	  tmpIsFile=${tmpFile}.IS
	  if ! pdi lsis -isdc 1095- -prodno "${pnr}" -rst "${prodrev}" -ofmt NUL 2> /dev/null
	  then
	      if pdi lsis -isdc 1095- -prodno "${pnr}" -status FREE -csv_hd_flds AKod:ChildNoE,ChildSelAttre -csv_noheader -ofmt CSV -csv_sep ':' 2> /dev/null | egrep -v '131 61-|131 32-' | sed -e 's/:$/:\//g' > ${tmpIsFile}
	      then
		  if [ -s "${tmpIsFile}" ]
		  then
		      if pdi add2is -isdc 1095- -prodno "${pnr}" -rstate "${prodrev}" -head acode:dno:rev -rows_f ${tmpIsFile} -ofmt NUL 2> /dev/null
		      then
			  Echo "Info Structure [1095-${pnr}, ${prodrev}] successfully copied in PRIM!"
		      else
			  Error "Info Structure copy [1095-${pnr}, ${prodrev}] in PRIM failed!"
		      fi
		  fi
	      else
		  Warn "Reading FREE Info Structure [1095-${pnr}] in PRIM failed!"
	      fi
	  fi
	  if runPDIcmd pdi lsis -isdc "${Struct}" -prodno "${pnr}" -rstate "${prodrev}" -csv_hd_flds ChildNoE:ChildSelAttre -csv_noheader -ofmt CSV -csv_sep ':' 2> /dev/null | tr -d ' "' | grep "${DocNo2}*:${rev}"'$' > /dev/null 2>&1
	  then
	      Echo "Info Structure [${Struct}${pnr}] already up to date!\n"
	  else
	      if runPDIcmd pdi add2is -isdc "${Struct}" -prodno "${pnr}" -rstate "${prodrev}" -object "${DocNo2}" -objver "${Revision}" > /dev/null
	      then
		  Echo "Info Structure [${Struct}${pnr}] update succeded!\n"
	      else
		  Error "Info Structure [${Struct}${pnr}] update failed, aborting!\n" 
	      fi
	  fi
      else
	  Error "Product R-state [${prodrev}] is missing (cant update ${Struct}${pnr}), aborting!\n" 
      fi
    fi
}



#
# Here starts the executing
#


# Get options
while getopts Ndfnsur:I:R:D: option
do
  case $option in
    N)          NO_INTERACTIVE=1;;
    D)
      if CheckOpt
      then
	  DocNo="$OPTARG"
      else
	  exit
      fi
      ;;
    R)
      if CheckOpt
      then
	prodrev="$OPTARG"
      else
	exit
      fi
      ;;
    r)
      if CheckOpt
      then
	Revision="$OPTARG"
      else
	exit
      fi
      ;;
    I)
      if CheckOpt
      then
	infostruct="${OPTARG}"
	Individual=`echo "$infostruct" | awk -F\- '{print $1}' | tr -d '[:space:]' | tr '_' '/'`
	ProdNr=`echo "$infostruct" | awk -F\- '{print $2}'| tr -d '[:space:]' | tr '_' '/'`
	if [ -z "${ProdNr}" ]
	then
	    Error "Missing Product number for info structure update"
	fi
      else
	exit
      fi
      ;;
    d)		DEBUG=Y;;
    s)		stepRev=Y;;
    u)		updateOnly=Y;;
    f)		FORMCHG=Y;;
    n)		NO_STRUCT_UPDATE=1;;
    \?)		Error "${USAGE}";;
  esac
done
shift `expr $OPTIND - 1` 
OptNo="1"

# Check number of options
if [ -n "${NO_INTERACTIVE}" ]
then
    OptNo=`expr ${OptNo} + 1`
fi
if [ -n "${prodrev}" ]
then
    OptNo=`expr ${OptNo} + 2`
fi
if [ -n "${DocNo}" ]
then
    OptNo=`expr ${OptNo} + 2`
fi
if [ -n "${Revision}" ]
then
    OptNo=`expr ${OptNo} + 2`
fi
if [ -n "${infostruct}" ]
then
    OptNo=`expr ${OptNo} + 2`
fi
if [ -n "${NO_STRUCT_UPDATE}" ]
then
    OptNo=`expr ${OptNo} + 1`
fi
if [ -n "${DEBUG}" ]
then
    set -x
    OptNo=`expr ${OptNo} + 1`
fi
if [ -n "${stepRev}" ]
then
    OptNo=`expr ${OptNo} + 1`
fi
if [ -n "${updateOnly}" ]
then
    OptNo=`expr ${OptNo} + 1`
fi
if [ "${FORMCHG}" = "Y" ]
then
    OptNo=`expr ${OptNo} + 1`
fi

if [ "$OPTIND" != "${OptNo}" ]
then
  Error "Wrong number of options!"
  printf "${USAGE}\n" >&2

fi

# Check file
if [ -n "$1" ]
then
  file=$1
else
  Error "Missing file!"
  printf "${USAGE}\n" >&2
fi

# Check that file exist
if [ ! -f "$file" ]
then
  Error "File [$file] does not exist"
fi


# Logon on RACF to make pdi commands to work...
${progdir}/logonRACF.sh

# Check/get DocNo
if [ -z "${DocNo}" ]
then
    ${progdir}/docHandler.sh "${file}" > ${tmpFile2}
    . ${tmpFile2}
fi

# Split DocNo to GASK/IASDB usable parameters
DocNo2=`echo ${DocNo} | sed 's/[ ]*U[a-z,A-Z]*$//' | tr -d '[:space:]' | tr "[:lower:]" "[:upper:]"`
decclass="`echo ${DocNo} | awk -F- '{print $1}' | sed 's/^.*[/,-]//'`"
individual="`echo ${DocNo} |  awk -F- '{print $2}' | sed -e 's/[-].*$//' -e 's/ U[a-z,A-Z]*//' | tr -d '[:space:]' | tr '[:lower:]' '[:upper:]'`"

# Auto step Document Revision
if [ -n "${stepRev}" ]
then
    FetchHighestRev
    if [ -n "${highStoredRev}" ]
    then
	# Calculate "next" Rev to use
	Revision=`echo ${highStoredRev} | ChRev | awk -F: '{print $2}' | tr -d '[:cntrl:]'`
    else
	tryrev=A
	Revision=`runPDIcmd pdi finddoc -dno ${DocNo2} -rev ${tryrev} -csv_hd_flds DocumentRevision -csv_noheader`
	if [ "${tryrev}" = "${Revision}" ]
	then
	    Error "Error with PRIM comunication, failed to calculate Revision!"
	    Revision=''
	else
	    Echo "Info: No previous Revision found in PRIM, using Rev:${tryrev}"
	    Revision=${tryrev}
	fi
    fi
fi

# Set default values
stat="FREE"
Struct="1095-"

# Get Language
language=`echo ${DocNo} | awk '{if ($(NF)~/U[a-z,A-Z]+/)\
				    {print $(NF)}\
				else\
			            {print "Ux"}}'`

# Get File Type and define GASK storage parameters
fileType=`file -iN "${file}" | awk -F: '{print $2}' | tr ';' '\n'| grep -v 'charset=' | awk -F= '{print $2}'`
fileCharset=`file -iN "${file}" | awk -F: '{print $2}' | tr ';' '\n'| grep 'charset=' | awk -F= '{print $2}'`

case ${fileCharset} in 
  'binary')	
    CHSET="BINAR"
    FORM="UNDEF"
    case "${file}" in
      *.pkg.Z)
	DOCTYPE="SOLPKGZ"
	;;
      *.cxp|*.cxr|*.cxs|*.tgz|*.tar.gz)
	DOCTYPE="TAR_GZIPV1"
	;;
      *.dump.Z )
	DOCTYPE="SOLDUMPZ"
	;;
      *.tar.Z)
	DOCTYPE="UNIX_TAR_COMPRESS"
	;;
      *.tar)
	DOCTYPE="TAR"
	;;
      *.gz)
	DOCTYPE="GZIPV1"
	;;
      *.rpm)
	DOCTYPE="GZIPV1"
	;;
      *.z|.alx)
	DOCTYPE="UNIX_COMPRESS"
	;;
      *.doc)
	DOCTYPE="MSWORD_DOC"
	;;
      *.docx)
	DOCTYPE="MSWORD_DOCX"
	;;
      *.dot)
	DOCTYPE="MSWORD_DOT"
	;;
      *.dotm)
	DOCTYPE="MSWORD_DOTM"
	;;
      *.dotx)
	DOCTYPE="MSWORD_DOTX"
	;;
      *.pps)
	DOCTYPE="MSPOWERPOINT_PPS"
	;;
      *.ppsx)
	DOCTYPE="MSPOWERPOINT_PPSX"
	;;
      *.ppt)
	DOCTYPE="MSPOWERPOINT_PPT"
	;;
      *.pptx)
	DOCTYPE="MSPOWERPOINT_PPTX"
	;;
      *.vsd)
	DOCTYPE="VISIOVSDV2003"
	;;
      *.pdf)
	DOCTYPE="PDF_ANY_VERSION"
	;;
      *.sdif)
	DOCTYPE="ISO9069"
	;;
      *.xls)
	DOCTYPE="MSEXCEL_XLS"
	;;
      *.xlsx)
	DOCTYPE="MSEXCEL_XLSX"
	;;
      *.zip)
	DOCTYPE="PKZIPV2R04"
	;;
      *)
	DOCTYPE="TEXT"
	;;
    esac
    ;;
  *)
    case ${fileCharset} in 
      */html*)
        CHSET="ISO646"
	FORM="A4S"
	DOCTYPE="HTMLV4R0"
	language="Uen"
	;;
      */xml*)
        CHSET="ISO646"
	FORM="A4S"
	DOCTYPE="XMLV1R0"
	;;
      text/*)
        CHSET="ISO646"
	FORM="A4S"
	case "${file}" in
	    *.sgml)
	      DOCTYPE="ISO8879"
	      ;;
	    *)
	      DOCTYPE="TEXT"
	      ;;
	esac
	;;
      *)
        Error "Unknown file type [$eltype], dont' know how to arcive in GASK"
	;;
    esac
    ;;
esac

# Get Rev info
if [ -z "${Revision}" ]
then 
    Error "Missing Revision [$file]"
fi

# 
# Execute
# 

EchoInfo
if [ -z "${updateOnly}" ]
then
    if [ "${FORMCHG}" = "N" ]
    then
	if CheckIfStored
        then
	    Store
	fi
    else
	Store
    fi
fi

# Only update 1095- if Product R-state is given
if [ -n "${prodrev}" ]
then
    UpDate
fi
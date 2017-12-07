#!/bin/sh
# ----------------------------------------------------------------------
# Short description: Metadata and Cache management of 3PPs (external CXA's and CXP's)
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
# main/1   2016-12-22 etxbjca     Created
#----------------------------------------------------------------------
#


#
# Set default variables
#
progname="`basename $0`"
debug=''
topDir=''
retry="3" 
retryDelay="10"
USAGE="\n\t${progname} -i '< Id list>' -f <file> [-p <Path to 3PP cache>] [-c <confidence level>] [-d]\n\n"
Exit=0
tmp="`mktemp`"

for dir in  /repo /tmp ${HOME}
do
  if [ -d "${dir}" ]
  then
    tmpDir=`mktemp -d --tmpdir=${dir}`
    break
  fi
done

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    local exit_line=$1
    local exit_code=$2
    test -z "${debug}" && \rm -rf ${tmpDir} ${tmp} ${tmp}* > /dev/null 2>&1
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
while getopts c:i:f:p:d option
do
  case ${option} in
    c)  cLevel="${OPTARG}";;
    i)  idList="${OPTARG}";;
    f)  file="${OPTARG}";;
    p)  topDir="${OPTARG}";;
    d)  debug="-${option}";;
    \?)	Error "${USAGE}\n";;
  esac
done
shift `expr ${OPTIND} - 1` 

#
# Check number of options
#
OptNo="5"
if [ -n "${debug}" ]
then
    OptNo="`expr ${OptNo} + 1`"
fi
if [ -n "${cLevel}" ]
then
    OptNo="`expr ${OptNo} + 2`"
fi

if [ -n "${topDir}" ]
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

if [ -n "${topDir}" -a ! -d "${topDir}" ]
then
    mkdir -p ${topDir}
fi


#
# Handle Debug mode
#
if [ -n "${debug}" ]
then
    set -x
fi


manage3pp()
  {
      PROD=`ci_data.sh -f ${file} -a read -i ${ID} -p 2 | tr '/' '_' | tr -d '[:space:]'`
      NAME=`ci_data.sh -f ${file} -a read -i ${ID} -p 7 | tr -d '[:space:]'`
      FLOW=`ci_data.sh -f ${file} -a read -i ${ID} -p 11 | tr -d '[:space:]'`
      CL_PRIO=`ci_data.sh -f ${file} -a read -i ${ID} -p 21`
      PROJECT=`echo "${FLOW}" | awk -F/ '{print $2}'`
      TRACK=`echo "${FLOW}" | awk -F/ '{print $3}'`
      if [ -n "${cLevel}" ]
      then
	  clList="${cLevel}"
      else
	  clList="0 1 3"
      fi
      for CL in ${clList}
      do
	if [ "${ID}" = "164" ]
	then
	    getrevPROD=`ci_data.sh -f ${file} -a read -i 165 -p 2 | tr '/' '_'`
	else
	    getrevPROD=${PROD}
	fi
	REV=''
	GETREV=''
        GETINFO=`curl -s --retry ${retry} --retry-delay ${retryDelay} http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${getrevPROD}\&confidence_level=${CL}\&verdict=SUCCESS\&latest=1\&project=${PROJECT}\&increment_number=${TRACK}\&rstate_type=R 2> /dev/null`
	if echo ${GETINFO} | grep '"version"' > /dev/null 2>&1
	then
	    GETREV=$(echo $GETINFO | python -mjson.tool | grep '"version"' | tail -1 | cut -f4 -d\")
	fi
	if [ -n "${GETREV}" ]
	then
	    case ${CL} in
		'0') REV0=`ci_data.sh -f ${file} -a write -i ${ID} -c ${GETREV} -p 3`;;
		'1') REV1=`ci_data.sh -f ${file} -a write -i ${ID} -c ${GETREV} -p 4`;;
		'3') REV3=`ci_data.sh -f ${file} -a write -i ${ID} -c ${GETREV} -p 5`;;
	    esac
	fi
        GETURL=''
        GETCACHE=''
        GETTYPE=''
	GETPATH=''
	GETCL=''
	if [ -n "${GETREV}" ]
	then
	    GETINFO2=`curl -s --retry ${retry} --retry-delay ${retryDelay} http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${PROD}\&version=${GETREV}\&latest=1\&project=${PROJECT}\&increment_number=${TRACK} 2> /dev/null`
	else
	    GETINFO2=`curl -s --retry ${retry} --retry-delay ${retryDelay} http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${PROD}\&latest=1\&project=${PROJECT}\&increment_number=${TRACK} 2> /dev/null`
	fi
        if echo $GETINFO2 | egrep '"productUrl"|"productType"' > /dev/null 2>&1
        then
	  GETURL=$(echo $GETINFO2 | python -mjson.tool | grep '"productUrl"' | tail -1 | cut -f4 -d\")
	  GETTYPE=$(echo $GETINFO2 | python -mjson.tool | grep -A1 '"productType"' | grep '"type"' | tail -1 | cut -f4 -d\")
        fi
        if echo $GETINFO2 | egrep '"sdk_installation_path"' > /dev/null 2>&1
        then
	  GETPATH=$(echo $GETINFO2 | python -mjson.tool | grep -A1 '"sdk_installation_path"' | tail -1 | cut -f4 -d\")
	fi
        if echo $GETINFO2 | grep '"build_cache"' > /dev/null 2>&1
        then
	  GETCACHE=$(echo $GETINFO2 | python -mjson.tool | grep -A1 '"build_cache"' | grep -v 'build_cache' | tail -1 | cut -f4 -d\")
        fi
        case ${CL} in
	    '0') GETURL0=${GETURL}
	    	 GETTYPE0=${GETTYPE}
	    	 GETPATH0=${GETPATH}
		 GETCACHE0=${GETCACHE};;
	    '1') GETURL1=${GETURL}
	    	 GETTYPE1=${GETTYPE}
	    	 GETPATH1=${GETPATH}
		 GETCACHE1=${GETCACHE};;
	    '3') GETURL3=${GETURL}
	    	 GETTYPE3=${GETTYPE}
	    	 GETPATH3=${GETPATH}
		 GETCACHE3=${GETCACHE};;
        esac
      done
      if [ -n "${CL_PRIO}" ]
      then
        case ${PROD} in	  
	  CXA11*|CXA201*)	
	     EXPAND=yes	 ;; 		
	  *)
	     EXPAND='' ;;
        esac	
        if [ "${CL_PRIO}" = "3" ]
	then
           if [ -n "${REV3}" -a \( -n "${GETURL3}" -o -n "${GETPATH3}" \) ]
	   then
	     REV=${REV3}
	     GETURL=${GETURL3}
	     GETTYPE=${GETTYPE3}
	     GETPATH=${GETPATH3}
	     GETCACHE=${GETCACHE3}
	   elif [ -n "${REV1}" -a \( -n "${GETURL1}" -o -n "${GETPATH1}" \) ]
	   then
	     REV=${REV1}
	     GETURL=${GETURL1}
	     GETTYPE=${GETTYPE1}
	     GETPATH=${GETPATH1}
	     GETCACHE=${GETCACHE1}
	   elif [ -n "${REV0}" -a \( -n "${GETURL0}" -o -n "${GETPATH0}" \) ]
	   then
	     REV=${REV0}
	     GETURL=${GETURL0}
	     GETTYPE=${GETTYPE0}
	     GETPATH=${GETPATH0}
	     GETCACHE=${GETCACHE0}
	   else
	     REV=${GETREV}
	   fi
        elif [ "${CL_PRIO}" = "1" ]
	then
           if [ -n "${REV1}" -a \( -n "${GETURL1}" -o -n "${GETPATH1}" \) ]
	   then
	     REV=${REV1}
	     GETURL=${GETURL1}
	     GETTYPE=${GETTYPE1}
	     GETPATH=${GETPATH1}
	     GETCACHE=${GETCACHE1}
	   elif [ -n "${REV3}" -a \( -n "${GETURL3}" -o -n "${GETPATH3}" \) ]
	   then
	     REV=${REV3}
	     GETURL=${GETURL3}
	     GETTYPE=${GETTYPE3}
	     GETPATH=${GETPATH3}
	     GETCACHE=${GETCACHE3}
	   elif [ -n "${REV0}" -a \( -n "${GETURL0}" -o -n "${GETPATH0}" \) ]
	   then
	     REV=${REV0}
	     GETURL=${GETURL0}
	     GETTYPE=${GETTYPE0}
	     GETPATH=${GETPATH0}
	     GETCACHE=${GETCACHE0}
	   else
	      REV=${GETREV}
	   fi
	else
           if [ -n "${REV0}" -a \( -n "${GETURL0}" -o -n "${GETPATH0}" \) ]
	   then
	     REV=${REV0}
	     GETURL=${GETURL0}
	     GETTYPE=${GETTYPE0}
	     GETPATH=${GETPATH0}
	     GETCACHE=${GETCACHE0}
	   elif [ -n "${REV1}" -a \( -n "${GETURL1}" -o -n "${GETPATH1}" \) ]
	   then
	     REV=${REV1}
	     GETURL=${GETURL1}
	     GETTYPE=${GETTYPE1}
	     GETPATH=${GETPATH1}
	     GETCACHE=${GETCACHE1}
	   elif [ -n "${REV3}" -a \( -n "${GETURL3}" -o -n "${GETPATH3}" \) ]
	   then
	     REV=${REV3}
	     GETURL=${GETURL3}
	     GETTYPE=${GETTYPE3}
	     GETPATH=${GETPATH3}
	     GETCACHE=${GETCACHE3}
	   else
	      REV=${GETREV}
	   fi 
	fi
	GETCL="${CL_PRIO}"
      else   
        EXPAND=''
        case ${PROD} in
	     CXP102*)	
	  		if [ -n "${REV3}" -a -n "${GETURL3}" ]
	   		then
	   		  REV=${REV3}
			  GETURL=${GETURL3}
			  GETTYPE=${GETTYPE3}
			  GETCACHE=${GETCACHE3}
			  GETCL='3'
			elif [ -n "${REV1}" -a -n "${GETURL1}" ]
			then
			  REV=${REV1}
			  GETURL=${GETURL1}
			  GETTYPE=${GETTYPE1}
			  GETCACHE=${GETCACHE1}
			  GETCL='1'
			elif [ -n "${REV0}" -a -n "${GETURL0}" ]
			then
			  REV=${REV0}
			  GETURL=${GETURL0}
			  GETTYPE=${GETTYPE0}
			  GETCACHE=${GETCACHE0}
			  GETCL='0'
			else
			  REV=${GETREV}
			fi;;
	  CXA11*|CXA201*)	
	  		EXPAND=yes
			if [ -n "${REV1}" -a \( -n "${GETURL1}" -o -n "${GETPATH1}" \) ]
			then
			  REV=${REV1}
			  GETURL=${GETURL1}
			  GETTYPE=${GETTYPE1}
			  GETPATH=${GETPATH1}
			  GETCACHE=${GETCACHE1}
			  GETCL='1'
			elif [ -n "${REV3}" -a \( -n "${GETURL3}" -o -n "${GETPATH3}" \) ]
			then
			  REV=${REV3}
			  GETURL=${GETURL3}
			  GETTYPE=${GETTYPE3}
			  GETPATH=${GETPATH3}
			  GETCACHE=${GETCACHE3}
			  GETCL='3'
			elif [ -n "${REV0}" -a \( -n "${GETURL0}" -o -n "${GETPATH0}" \) ]
			then
			  REV=${REV0}
			  GETURL=${GETURL0}
			  GETTYPE=${GETTYPE0}
			  GETPATH=${GETPATH0}
			  GETCACHE=${GETCACHE0}
			  GETCL='0'
			else
			  REV=${GETREV}
			fi;;
	  *)		
	  		if [ -n "${REV0}" -a \( -n "${GETURL0}" -o -n "${GETPATH0}" \) ]
			then
			  REV=${REV0}
			  GETURL=${GETURL0}
			  GETTYPE=${GETTYPE0}
			  GETPATH=${GETPATH0}
			  GETCACHE=${GETCACHE0}
			  GETCL='0'
			elif [ -n "${REV1}" -a \( -n "${GETURL1}" -o -n "${GETPATH1}" \) ]
			then
			  REV=${REV1}
			  GETURL=${GETURL1}
			  GETTYPE=${GETTYPE1}
			  GETPATH=${GETPATH1}
			  GETCACHE=${GETCACHE1}
			  GETCL='1'
			elif [ -n "${REV3}" -a \( -n "${GETURL3}" -o -n "${GETPATH3}" \) ]
			then
			  REV=${REV3}
			  GETURL=${GETURL3}
			  GETTYPE=${GETTYPE3}
			  GETPATH=${GETPATH3}
			  GETCACHE=${GETCACHE3}
			  GETCL='3'
			else
			  REV=${GETREV}
			fi;;
        esac
      fi

      # Infobank sanity check
      if [ -z "${GETURL}" -a -z "${GETPATH}" ]
      then
	Error "Missing \"Url\" and \"Path\" in infobank for ID: [${ID}]: ${NAME}, ${PROD} CL0=\"${REV0}\", CL1=\"${REV1}\", CL3=\"${REV3}\" - aborting!"
      fi
      if [ -z "${REV}" ]
      then
	Error "Missing Version in infobank for ID: [${ID}]: ${NAME}, ${PROD} - aborting!"
      fi

      # Update Pos 6 with HA (Highest Available) Revision 
      ci_data.sh -f "${file}" -a write -i "${ID}" -c "${REV}" -p 6 > /dev/null
      URL=''
      PRODVERDIR=${topDir}/${NAME}/${REV}
      PRODDIR=${PRODVERDIR}

      if [ -z "${GETURL}" ]
      then
	  GETURL=`ci_data.sh -f ${file} -a read -i ${ID} -p 12`
	  if [ -n "${GETURL}" ]
	  then
	      case ${PROD} in
		  *CXC*) 	GETURL=${GETURL}
		         	PRODDIR=${PRODVERDIR}/${NAME}_${PROD};;
		  *)	 
		    	 	if ( echo ${GETURL} | grep -v "@@/${PROD}-${REV}" > /dev/null 2>&1 )
			 	then
			     	  GETURL=${GETURL}@@/${PROD}-${REV}
			 	fi;;
	      esac
	  fi
      fi
      if [ -n "${GETURL}" ] # Update Baseline file with proper URL
      then
	  URL=`ci_data.sh -f ${file} -a write -i ${ID} -c ${GETURL} -p 12`
      fi
      if [ -z "${GETPATH}" ] # Check if CACHE PATH not exist
      then
        if [ -n "${topDir}" ] # Check/create cache structure for product if 3PP option is used
        then
	  if [ -n "${NAME}" -a -n "${PROD}" ]
	  then
	      if [ -n "${URL}" ]
	      then
		  test -d "${PRODDIR}" || mkdir -p ${PRODDIR}
		  ( 
		      cd ${topDir}
		      prodPackage=`basename ${URL}`
		      downloaded=''
		      if ( find ${PRODVERDIR}/*${PROD}* -maxdepth 2 -type f -print 2> /dev/null | grep '.*' > /dev/null 2>&1 ) 
		      then
			  if [ -s "${PRODDIR}/${prodPackage}" -a -n "${EXPAND}" -a "${GETTYPE}" != "UP" ] # previous attempt to extract package failed, package need to be re-downloaded/extracted
			  then
			      \rm -rf ${PRODVERDIR}/*${PROD}* > /dev/null 2>&1 || true
			      mkdir -p ${PRODDIR}
			  else
			      downloaded=yes
			  fi
		      fi
			    
		      if [ -z "${downloaded}" ]
		      then
			( cd ${tmpDir}
			  CUTDIRS=`echo ${URL} | gawk -F'(http[s]*://[^/]+|?)' '$0=$2' | tr '/' ' ' | wc -w`
			  Echo "Downloading url: [${URL}]"
			  case ${PROD} in
			      *CXC*) 	INCDIR=`echo ${URL} | gawk -F'(http[s]*://[^/]+|?)' '$0=$2'`
					CUTDIRS=`echo ${INCDIR} | tr '/' ' ' | wc -w`
		  		     	wget -m -e robots=off --no-parent --no-check-certificate --no-proxy -q -np -nH --cut-dirs=${CUTDIRS} --exclude-directories="${INCDIR}/*CAX*" --reject="index.html*" --include ${INCDIR} ${URL};;
			      *)	wget --no-check-certificate -nd -q --no-proxy ${URL};;
			  esac
			  if [ -n "${EXPAND}" ]
			  then
			  Echo "Expanding package: [${prodPackage}]"
			      case "`file ${prodPackage}`" in
				  *[zZ][iI][pP]*archive*) ( cd ${PRODDIR} && unzip -q ${tmpDir}/${prodPackage} && chmod -R ugo-w ./* );;
		      		  *gzip*compressed*)
				  			SDK_CONTAINER=$(tar tf ${prodPackage} | egrep '[^/]*/[^/]*.tar.gz|*-sdk.sh' || true)
				  			case "${SDK_CONTAINER}" in
							    *.tar.gz)
							    		tar xpf ${prodPackage}
							    		tar xpf ${SDK_CONTAINER}
							    		./wrlinux-*-sdk.sh -y -d ${PRODVERDIR}/${NAME}_${PROD};;
							    *-sdk.sh)
							    		tar xpf ${prodPackage}
							    		${SDK_CONTAINER} -y -d ${PRODVERDIR}/${NAME}_${PROD};;
							    *)
							    		( cd ${PRODDIR} && tar -xpf ${tmpDir}/${prodPackage} && chmod -R ugo-w ./* );;
							esac;;
		      		  *tar*archive*) ( cd ${PRODDIR} && tar xpf ${tmpDir}/${prodPackage} && chmod -R ugo-w ./* );;
			      esac
			  else
			      if [ -s "${tmpDir}/${prodPackage}" ]
			      then
				  cp -axf ${tmpDir}/${prodPackage} ${PRODDIR} && chmod -R ugo-w ${PRODDIR}
				  \rm -rf ${tmpDir}/${prodPackage} || true
			      else
				  chmodList=`find ${tmpDir} -type f -print | xargs file | egrep 'executable|script' | cut -f 1 -d :`
				  if [ -n "${chmodList}" ]
				  then
				      chmod ugo+x ${chmodList}
				  fi
				  cp -axf ${tmpDir}/* ${PRODDIR} && chmod -R go-w ${PRODDIR}
				  \rm -rf ${tmpDir}/* || true
			      fi
			  fi
			)
		      fi
		  )
		  if [ -d "${GETCACHE}" ]
		  then
		      ci_data.sh -f ${file} -a write -i ${ID} -c "${GETCACHE}" -p 13 > /dev/null
		  else
		      PROD_ENTRY=`find ${PRODDIR} -maxdepth 1 -name '*'"${PROD}"'*' \( -type d -o -type f \) -print`
		      if [ -z "${PROD_ENTRY}" ]
		      then
			  Error "Missing Cache Path for ID: [${ID}]: ${NAME}, ${PROD} ${REV} - cant update baseline file!"
		      fi
		      ci_data.sh -f ${file} -a write -i ${ID} -c "${PROD_ENTRY}" -p 13 > /dev/null
		  fi
	      else
		  Error "Missing \"Url\" for ID: [${ID}]: ${NAME}, ${PROD} ${REV} - cant fetch content"
	      fi
	  else
	      Error "Missing Product Name [${NAME}] or No [${PROD}] for ID: [${ID}] with Rev [${REV}]"
	  fi
        fi
      else
	  if [ -d "${GETPATH}" ]
	  then
	      ci_data.sh -f ${file} -a write -i ${ID} -c "${GETPATH}" -p 13 > /dev/null
	  fi
      fi
      Echo "Done!\t\t\t\t\t\t[${ID}]: ${NAME}, ${PROD} ${REV}, CL=${GETCL}"
}


#
# Execution start
#
Echo "Extracting/refreshing 3PP Cache/info for ID(s):\t[${idList}]..."
idPid=''
for ID in ${idList}
do
  Echo "Cache management of ID: [${ID}] started..."
  ( manage3pp > ${tmp}.${ID} 2>&1 || echo $? > ${tmp}.${ID}.error )&
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
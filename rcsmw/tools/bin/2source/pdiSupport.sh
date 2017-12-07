#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	pdiSupport.sh %
# %CCaseRev:	/main/70 %
# %CCaseDate:	2017-05-05 %
# %CCaseDocNo:	511/190 55-LXA 119 334 Ux %
# Author      : Björn Carlsson (etxbjca)
#
# Short description: Common procedure definitions for PDI usage
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
# main/1   2014-09-17 etxbjca     Created
#----------------------------------------------------------------------
#

#
# Init PDI
#
. /app/modules/0/init/sh
module add pdi/LZN90113983-R17A


#
# Define Echo Procedure
#
Echo()
    {
	printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: $*\n" >&2
}

#
# Define Error Procedure
#
Error()
    {
	printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Error: $*\n" >&2
	exit 1
}

#
# Define Error Procedure
#
Error2()
    {
	printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Error: $*\n" >&2
}

#
# Define Warning Procedure
#
Warn()
    {
	printf "# `date '+%Y-%m-%d %H:%M:%S'` # ${progname}: Warning: $*\n" >&2
}

#
# Define procedure that manage pdi commands with error handling and retry support
#
runPDIcmd() {
    #
    # Get DesignResponsible for ${Individual}
    #
    pdiCMD=$*
    COUNTER=1
    result=''
    TIME2WAIT=0.3
    errFile=${tmpFile}.err
    echo '' > ${errFile}
    until [ -n "${result}" -o ${COUNTER} -gt 3 ]
    do
      OK=OK
      # Delay retries to limit load on PRIM/CPWS
      sleep ${TIME2WAIT}
      if ! result="`${pdiCMD} 2> ${errFile}`"
      then
	  OK=NOK
      fi
      if [ -n "${result}" -a ! -s  "${errFile}" ]
      then
	  case ${result} in
	      *ERR:*|*ERROR:*)	OK=NOK;echo "${result}" > ${errFile};result='';;
	      *WARNING:*)	echo "${result}" > ${errFile};result='';;
	      \#\#\#*)		echo "${result}" > ${errFile};result='';;
	  esac
      fi
      if [ -z "${result}" -a ! -s  "${errFile}" ]
      then
	  return 0
      fi
      if grep '[[:alnum:]]' ${errFile} > /dev/null 2>&1
      then
	case `cat ${errFile}` in
	    *PIFFM1G*,PIFFL1M*) 
	    		Error "PSD update failed, R-state missing in PRIM for product(s):"
			grep 'PIFFL1M' ${errFile} | sed 's/.* E \(.*\),.*/\t\1/g' | tr -d ' ' | awk -F, '{print $1}' >&2
			OK=OK
			result=''
	    		echo '' > ${errFile}
			return 2;;
	    lsis-*:*) 	result=`grep '^lsis-' ${errFile}`
	    		OK=OK;;
	    *PICXH7B*|\
	    *PIFZZC4*)	printf "\n(already released) " >&2
	    		OK=OK
	    		echo '' > ${errFile}
			return 0;;
	    *PIFFM1G*)	printf "\n(product frozen due to DS/PR code) " >&2
	    		OK=OK
	    		echo '' > ${errFile}
			return 0;;
	    *PIFAA1C*|\
	    *PIFAA2Q*)	printf "\n(higher DS/PR code already set) " >&2
	    		OK=OK
	    		echo '' > ${errFile}
			return 0;;
	    *ERR:*'DB2 engine SQL error'*)
	    		Error2 "Command failed: [${pdiCMD}]"
	    		printf "\t" >&2
			cat ${errFile} >&2
			exit 1;; 
	    *pdi_lspsd:*WARNING:*No*such*-csv_hd_flds*PlSel*|\
	    *PIFGFD1*|\
	    *PIFGGB1*|\
	    *PIFFL2J*|\
	    *PIFFL2M*|\
	    *PICYD1A7*|\
	    *PIFZZD4*|\
	    *GAFDD05*|\
            *GAFDD06*|\
	    *GAPP012A*|\
	    *PIP044D*|\
	    *PIPA31I*|\
	    *PIFZZA1*|\
	    *PIFZZB2*|\
	    *PJCWG01*|\
	    *PJCWG1D*|\
	    *PJCWD1B*|\
	    *PJCWD3B*|\
	    *PICWK1C*)	OK=OK
	    		echo '' > ${errFile}
	    		return 0;;
	    *ticket*expired*)	logonRACF.sh 300;;
	    *cpws::ExecCmd*ERROR:*NOT*FULFILL*REQUIREMENTS*|\
	    *PIFAA*|\
	    *PJCWJ01*)	Error2 "Command failed: [${pdiCMD}]"
	    		printf "\t" >&2
			cat ${errFile} >&2
			OK=OK
			return 1;; 
	    *cpws::ExecCmd*ERROR:*|\
	    *ERROR:*Rest/ESB:*PDMSYS*|\
	    *ERROR:*Unexpected*Severity*|\
	    *ERR:*DB2*error*) 
	    		Warn "Command OK with remarks: [${pdiCMD}] (try: ${COUNTER})"
	    		printf "\t" >&2
			cat ${errFile} >&2
	    		echo '' > ${errFile};; 
	    *PID000B*|\
	    *ERR:*|\
	    *ERROR:*)	Error2 "Command failed: [${pdiCMD}]"
	    		printf "\t" >&2
			cat ${errFile} >&2
			OK=OK
			return 1;; 
	    *)		Warn "Command OK with remarks: [${pdiCMD}] (try: ${COUNTER})"
	    		printf "\t" >&2
			cat ${errFile} >&2
	    		echo '' > ${errFile};; 
	esac
      fi
      COUNTER="`expr ${COUNTER} + 1`"
      # IBM/PRIM recomended time out for retry
      TIME2WAIT=90
    done
    if [ -n "${result}" ]
    then
	echo "${result}"
    fi
    if [ "${OK}" = "NOK" ]
    then
	Error "Final try for unsuccessful command [${pdiCMD}]\n"
	return 2
    fi
    return 0
}


#! /bin/bash
## ----------------------------------------------------------------------
## %CCaseFile:	pms_app.sh %
## %CCaseRev:	/main/R2A/9 %
## %CCaseDate:	2014-04-24 %
## %CCaseDocNo: %
## Author:	erarafo
## Author: <name>, <e-mail address>
##
## Short description:
## <Some rows here>
## ----------------------------------------------------------------------
## %CCaseTemplateFile:	template.c %
## %CCaseTemplateId: CCver: /main/1 %
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013-2014 All rights reserved.
## 
## The information in this document is the property of Ericsson.
## 
## Except as specifically authorized in writing by Ericsson, the 
## receiver of this document shall keep the information contained 
## herein confidential and shall protect the same in whole or in 
## part from disclosure and dissemination to third parties.
## 
## Disclosure and disseminations to the receivers employees shall 
## only be made on a strict need to know basis.
## %CCaseCopyrightEnd%
##
## ----------------------------------------------------------------------
##
## Revision history:
##
## Rev        Date       Name        What
## -----      -------    --------    --------------------------
## R2A/2      2013-02-03   erarafo     Created
## ----------------------------------------------------------------------

declare -r Configuration=Debug
declare -r CEC=/vobs/rcs/delivery/RCP_CSX10179/RCS_CRX901266/CEC/CEC_CXA1105463
declare -r CecPortDefault=2345
declare -r ScenarioLengthDefault=30

########################################################################

declare -r Script=`basename $0`
declare -r ScriptDir=`dirname $0`
declare Options=":"

declare Options+="d"
declare Options+="h"
declare Options+="p:"
declare Options+="s"
declare Options+="T:"

function help() {
printf "Usage is: $Script [OPTIONS]\n"
printf "Options are:\n"
printf "  -d       debug\n"
printf "  -h       this help\n"
printf "  -p PORT  specify PORT for CEC (default to $CecPortDefault)\n"
printf "  -s       synchronized initialize\n"
printf "  -T       scenario seconds (defaults to $ScenarioLengthDefault)\n"
}


function fatal() {
    printf "$Script: FATAL: $1\n" >&2
    exit 1
}

function info() {
    printf "$Script: INFO: $1\n"
}

function debug() {
    if [[ "$Debug" == true ]]; then
	printf "$Script: DEBUG: $1\n"
    fi
}


########################################################################
# Options processing

CEC_PORT=$CecPortDefault
ScenarioLength=$ScenarioLengthDefault
while getopts $Options OPT; do
    case "$OPT" in
	d)
	    Debug=true;;
	f)
	    File="$OPTARG";;
	h)
	    help
	    exit;;
	p)
		CEC_PORT="$OPTARG";;
	s)
		info "assuming synchronized initialize"
		export PMI_SYNC=1;;
	T)
	   ScenarioLength="$OPTARG";;
	*)
	    fatal "unknown option: -$OPTARG, try -h for help"  
    esac
done

shift $((OPTIND - 1))


########################################################################
# Script execution

declare -r ROOT=`readlink -f $ScriptDir/..`

export CEC_PORT


if [[ "$Debug" == true ]]; then
	debug "using CEC_PORT: $CEC_PORT"
	debug "scenario length: $ScenarioLength s"
fi


export LD_LIBRARY_PATH+=":$CEC/tgt_i686/lib32"
export SCENARIO_LENGTH=$ScenarioLength

debug "$ScriptDir"

case "$ScriptDir" in
    *eclipse*)
        export LD_LIBRARY_PATH+=":$ROOT/../eclipse/pms_pmi/$Configuration"
        declare Executable=`readlink -f $ROOT/../eclipse/pms_app/$Configuration/pms_app`;;
    *)
        export LD_LIBRARY_PATH+=":$ROOT/../out/tgt_i686/lib32"
        declare Executable=`readlink -f $ROOT/../out/tgt_i686/bin/pms_app`
esac

for p in `echo $LD_LIBRARY_PATH | tr : '\n'`; do
    debug "LD_LIBRARY_PATH contains: $p"
done

debug "executing: $Executable"
exec $Executable
# exec valgrind -v --leak-check=full --show-reachable=yes $Executable


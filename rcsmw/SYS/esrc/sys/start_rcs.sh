#!/bin/bash
## -----------------------------------------------------------------------------
## %CCaseFile:	start_rcs.sh %
start_rcs_rev="  %CCaseRev:	/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/1 %"
## %CCaseDate:	2017-09-19 %
## %CCaseDocNo:  %
##
## Author:      etxlg
## Description: Start RCS middleware, i.e. the erlang/OTP run-time
##	        Now only for simulated environment.
## -----------------------------------------------------------------------------
##
## ----------------------------------------------------------
## %CCaseTemplateFile:   %
## %CCaseTemplateId: %
##
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
##     .... Removed older history .....
## R4A/1      2015-09-02   erarafo     Added TIM_LIB_DIR to LD_LIBRARY_PATH
## R5A/1      2015-10-28   etxpeno     Set time warp mode to Multi Time
## R5A/2      2015-11-06   etxarnu     Removed ppc & arm stuff, this is only used by rcssim.
## R5A/3      2016-01-28   etxarnu     Don't  symlink rootfs files if WR6 is used
## R5A/4      2016-03-06   etxarnu     Updates for use in VRCS
## R5A/6      2016-03-08   etxarnu     Use rcs cookie in VRCS
## R5A/7      2016-03-08   etxarnu     Put cslibs in correct place in LD_LIBRARY_PATH
## R6A/1      2016-05-16   etxarnu     Preparation to run dual in VRCS
## R7A/1      2016-09-29   etxarnu     Removed VRCS stuff (has been moved to start_vrcs.sh)
## R8A/1      2016-12-07   etxarnu     Added handling of $noInstall
## R8A/2      2017-01-19   erarafo     Inserting dev_patches/README.txt
## R11A/1     2017-09-19   etxarnu     Create rcs/sim_uuid
## --------- ---------------------------------------------------- column 80 --->
##
set -e #exit if command returns error
set -u #exit if a variable is unset
#set -x

Arch=`arch`
if  [ ${Arch} == "x86_64" ]; then
    ArchDir="i686"
else
    ArchDir="i686_32" #proot
fi

#Functions

#
# Define Append Procedure
#
AppendLog()
    {
     	cat - >> ${logFile}

    # if [ -n "${verbose}" -o -n "${debug}" ]
    # then
    # 	tee -a ${logFile}
    # else
    # 	cat - >> ${logFile}
    # fi
}

#log a message to stderr and exit
#printf(bash builtin) avoids problem with -- not recognized by echo
#TBA add logging to file, console, and anywhere else
function fatal() {
    DATE=`date "+%Y-%m-%d %H:%M:%S"`
    printf "$DATE start_rcs.sh: %s\n" "$*"  | AppendLog
#	printf "%s\n" "$*" >&2
    exit 1;
}

#log a message to stderr
#printf(bash builtin) avoids problem with -- not recognized by echo
#TBA add logging to file, console, and anywhere else
function start_log() {
    DATE=`date "+%Y-%m-%d %H:%M:%S"`
    printf "$DATE start_rcs.sh: %s\n" "$*" | AppendLog
#	printf "%s\n" "$*" >&2
}

# Return absolute value of argument.
function abs() {
    if (( ${1:?} < 0 )); then
        echo $(( $1 * -1 ))
    else
        echo $1
    fi
}


# Produce a timestamp in format X Y, where X is a four digit year
# and Y is the number of seconds passed since the beginning of the year.
function timestamp() {
    set -- $(date +"%Y %j %H %M %S")

    # Force base 10 because of leading 0 otherwise interpreted as octal.
    echo $1 $(( 10#$2*24*3600 + 10#$3*3600 + 10#$4*60 + 10#$5 ))
}

# Return true if the interval between given timestamps
# (as produced by timestamp()) is less than diff seconds,
# assuming that the interval is more than diff seconds if
# the year has turned.
function time_interval_less_than() {
    local year1=${1:?}
    local sec1=${2:?}
    local year2=${3:?}
    local sec2=${4:?}
    local diff=${5:?}

    if (( year1 != year2 )); then
        return 1 # We have turned the year (in any direction).
    elif (( $(abs $(( sec1 - sec2 ))) > diff )); then
        return 1 # The interval is more than diff seconds.
    else
        return 0
    fi
}

# strategy for limiting the number of restarts
# currently only used in simulated environment
# or if the file ${CYCLIC_RST_FILE} exists
# ================================================

function check_start_log() {
    local max_reboot_short=5
    local max_time_short=900 #15minutes
    local length=50          #number of entries in log
    local start_log=${ERLANG_LOG_DIR}/start.log

    #this makes the latest entry end up first in the log
    timestamp > ${start_log}.tmp
    [ -r ${start_log} ] && head -n ${length} ${start_log} >> ${start_log}.tmp

    mv -f ${start_log}.tmp ${start_log}
}


#
# Find patched version of a file.
# for now there is only one single dir we look in
#
function find_patch() {
    local file=${1##*/} #get the basename
    local dev=${DEV_PATCHES}/${file}
    local patch

    for patch in $dev; do
        if [ -f $patch ]; then
            echo $patch
            return 0
        fi
    done
    return 1
}

# $1 is the filepath to logfile, create/chown if not existing
# move previous to $1.old
# hopefully ensures that it can't become to big/lg
rotate_startlog()
{
    [ -f $1 ] && {
	mv -f $1 $1.old
    }
	> $1
}

#
# Patch support. A call to find_and_exec_patch $* in the beginning
# of a script will check for a patch for the script and execute the
# patched file instead.
# NOTE: Can not use start_log in this function since directories
# are not yet created (and want to patch as early as possible)!
#
function find_and_exec_patch() {
    local script=${0##*/}  #get the basename
    local flag=${script//[\.-]/_}_patch #substitute "." and "-" to "_"

    if [ ${!flag:-false} = false ]; then
        if patch=$(find_patch $script) && [ -x $patch ]; then
            echo "Patched version $patch of $script found, using it!"
            eval export ${flag}=true
            exec $patch $*
        fi
    else
        unset ${flag}
    fi
}



#ensure that the argument $1 which is expected to contain metacharacters for
#file-expansion resolves to a directory and only one directory
function resolve_dir_path_int() {
    set -- $1
    if [ $# -ne 1 -o  ! -d ${1} ]; then
	echo ""
    else
	echo ${1}
    fi
}

# Check if directory exist in dev_patches and then ordinary
function resolve_dir_path() {
    local in_path="$RCS_CODE_ROOT/${1}"
    local dev_path="$DEV_PATCHES/${1}"

    if [ "$(resolve_dir_path_int $dev_path)" != "" ]; then
	echo $dev_path
    elif [ "$(resolve_dir_path_int $in_path)" = "" ]; then
	start_log "Couldn't resolve path: ${in_path}"
	echo ""
    else
	echo $in_path
    fi
}

#try to ensure that erlang is in the path, if not assume OTP_ROOT ok and add it
#then check again. Exit with error if unsuccesful
function check_fix_path(){
	if
	    type -afP run_erl >/dev/null
	then
	    : all is well
	elif
	    PATH=${OTP_ROOT}/bin:$PATH type -afP run_erl >/dev/null
	then
	    PATH=${OTP_ROOT}/bin:$PATH  #we fixed it
	else
	    fatal "Erlang run-time not in PATH"
	fi
}

function create_directories() {
    local dir
    for dir
    do
	[ -d $dir ] || {
	    mkdir -p $dir || fatal "Failed to create directory: $dir"
	    start_log "Created directory: $dir"

	}
    done
}

## make_release.escript <CodeRoot> <OtpRoot> <LogFile>
## the <Version> (RCS_VERSION) and <System> (RCS_SYSTEM) are  set and written
## to a sourceable script-file: ${RCS_ROOT}${HOME}/releases/start_data
## release created in cwd so change to where we want it
function create_release() {
    local script=${1##*/}  #get the basename
    local script_file_path=${1}
    local fixed_code_root=${CODE_ROOT/\/\//\/}
    local fixed_otp_root=${OTP_ROOT/\/\//\/}
    if [ -x ${DEV_PATCHES}/${script} ]; then
	script_file_path=${DEV_PATCHES}/${script}
	start_log "Patched version ${script_file_path} of $script found, using it!"
    fi
    {
	cd ${RCS_ROOT}${HOME} &&
	${script_file_path} $fixed_code_root $fixed_otp_root $logFile
    } || fatal "Couldn't change directory or make_release.script returned an error"

    if [ ! -f ${RCS_ROOT}${HOME}/releases/start_data ]; then
	fatal "No start_data -file created by make_release.escript"
    fi
}


function symlink_files() {
    local file
    for file
    do
	ln -sf $file ${RELEASES_DIR}/cslibs/${ArchDir}/. || fatal "Failed to symlink: $file"
	start_log "Symlinked: $file to  ${RELEASES_DIR}/cslibs/${ArchDir}/"
    done
}

# This function sets up the Execution Environment in simulated RBS CS.
function setupEEinSim {
    if [ -n "${RCSSIM_DEBUG:-}" ]; then
        sh -x rcsEEsim restartUsel
    else
        rcsEEsim restartUsel
    fi
}

function insert_readme() {
  local -r sysEtc=$(dirname $(dirname $1))/etc
  local -r devPatches=$2
  sed -e '1,/===end=of=header===do=not=edit=this=line===/d' $sysEtc/README-dev_patches.txt >$devPatches/README.txt
}


###############################################################################
########################### main Main MAIN ####################################
###############################################################################

export RCS_ROOT=${RCS_SIM_ROOT:-/local/scratch/${USER:?Environment variable USER must be set}/RCS_ROOT}
export CODE_ROOT=${CODE_ROOT:-${RCS_ROOT}${HOME:?Environment variable HOME must be set}/software}

if  [ ${Arch} == "x86_64" ]; then
    RCS_CODE_ROOT="$CODE_ROOT/RCS*_CXP*"
    GCCLIBS=${GCC_LIBS:-/app/gcc/4.5.1/LMWP3/lib/}
fi

UNPACK_ROOTFS=${UNPACK_ROOTFS:-yes}

export OTP_ROOT=${OTP_ROOT:?Environment variable OTP_ROOT must be set}
export RCS_MODE=simulated
export SNAME=${SNAME:-$USER}
if [ ! -z "${PGH_WDFN+x}" ];then
    export PGH_WDFN=${PGH_WDFN}
fi

# The following is needed for TRI but should be moved to a
# file to be sourced by PGH before starting an application
export LTTNG_HOME=${LTTNG_HOME:-${RCS_ROOT}/rcs/lttng}
export PFS_PATH=${PFS_PATH:-${LTTNG_HOME}/tri}

ERLANG_LOG_DIR=${RCS_ROOT}/rcs/erlang
HEART_LOG=${ERLANG_LOG_DIR}/heart.log

export DEV_PATCHES=${RCS_ROOT}${HOME}/dev_patches
logFile=${ERLANG_LOG_DIR}/start_rcs.log


PIPE_DIR=${PIPE_DIR:-${RCS_ROOT}/tmp}
# To mimic HW clean /tmp directory:
[ -d ${RCS_ROOT}/tmp ] && rm -rf ${RCS_ROOT}/tmp/*
[ -d ${RCS_ROOT}/colish ] && rm -rf ${RCS_ROOT}/colish/*


export PIPE_DIR=${PIPE_DIR:-/tmp}

find_and_exec_patch $*

create_directories  ${RCS_ROOT} $CODE_ROOT $ERLANG_LOG_DIR ${PIPE_DIR}/${SNAME}@${HOST:?Environment variable HOST must be set}

rotate_startlog $logFile

# Redirect all stdout/stderr to ${logFile}
#    exec >> ${logFile} 2>&1

start_log "Simulated environment"
echo  "start_rcs.sh: $start_rcs_rev "


TRI_LOG_DIR=${RCS_ROOT}/rcs/lttng/tri/telog
LTTNG_LOG_DIR=${RCS_ROOT}/rcs/lttng/tri/lttng
DEF_LOG_DIR=${RCS_ROOT}/rcs/lttng/default

export TE_LTTNG_SNAPSHOT_PATH=${TRI_LOG_DIR}
export TE_LTTNG_SESSIONS=rcs

CORE_DUMP_DIR=${RCS_ROOT}/rcs/dumps/
export DUMP_DIR=${RCS_ROOT}/rcs/dumps/appdump
export COLI_CMDS_PATH=${RCS_ROOT}/colish


# Don't make this path longer, then itcworld won't start
RUNDIR_PATH=/tmp/${USER}/rcssim.${RCSSIM_PID}/run
export ITC_RUNDIR_PATH=${RUNDIR_PATH}/itc
export ITC_INSTANCE_NAME=itc_${SNAME}
create_directories $ITC_RUNDIR_PATH

create_directories $TRI_LOG_DIR $LTTNG_LOG_DIR $DEF_LOG_DIR $CORE_DUMP_DIR $COLI_CMDS_PATH  $DUMP_DIR

check_fix_path

sys_priv_path="$(resolve_dir_path "SYS*_CXC1733862_*/sys-*/priv/bin/")"



if [ ! -f ${RCS_ROOT}${HOME}/install_complete ]; then
    if [ "${noInstall:-}" == "yes" ]
    then
	# to ensure correct libraries for beam in .escript
	. ${RCS_ROOT}${HOME}/releases/start_data #source to get RCS_VERSION and RCS_SYSTEM
	RELEASES_DIR=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}
	LD_LIBRARY_PATH=${LD_LIBRARY_PATH:-}
	export LD_LIBRARY_PATH=${RELEASES_DIR}/cslibs/${ArchDir}:${LD_LIBRARY_PATH}
    fi
    if patch=$(find_patch ${sys_priv_path}/make_release.escript) && [ -x $patch ]; then
	start_log "Patched version $patch of ${sys_priv_path}/make_release.escript found, using it!"
	create_release $patch
    else
	create_release ${sys_priv_path}/make_release.escript
    fi
fi

. ${RCS_ROOT}${HOME}/releases/start_data #source to get RCS_VERSION and RCS_SYSTEM

RELEASES_DIR=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}

CXPs=`\ls -1 ${RCS_ROOT}${HOME}/software | grep -v '.cxp'  | grep -v '.xml'`
for CXP in $CXPs
do
  create_directories   ${RCS_ROOT}/rcs/applicationlogs/${CXP%_*}
  create_directories   ${RCS_ROOT}/tmp/applicationtmp/${CXP%_*}
done

 [ ! -e ${RCS_ROOT}/rcs/sim_uuid ] &&  $( uuidgen > ${RCS_ROOT}/rcs/sim_uuid )
create_directories ${RCS_ROOT}/rcs/persistent
chmod 777  ${RCS_ROOT}/rcs/persistent

create_directories ${RELEASES_DIR}/libs/${ArchDir}

create_directories ${RELEASES_DIR}/cslibs/${ArchDir}

export RCS_LIB_ROOT=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}/libs

LD_LIBRARY_PATH=${LD_LIBRARY_PATH:-}

if [ "$UNPACK_ROOTFS" == "yes" ]
then
    # When started by rcssim this unpacking has already been done. When
    # starting the To-system during upgrade the unpacking has to be done
    # here.
    rootfs_path="$(resolve_dir_path "ROOTFS*_CXC1733870_*/rootfs*/priv/x86")"
    for march in i686 x86_64; do
	[[ ! -d ${rootfs_path}/$march ]] &&  {
	    printf "Unpacking $march ROOTFS\n"
	    mkdir ${rootfs_path}/$march
	    (cd ${rootfs_path}/$march && tar -xf ${rootfs_path}/rootfs-$march.tgz || fatal "Unpacking ${rootfs_path}/rootfs-$march.tgz failed.")
	}
    done
    ROOTFS=${rootfs_path}/i686
    symlink_files ${ROOTFS}/lib/libcrypto.so.1.0.0 ${ROOTFS}/lib/libtinfo.so.5 ${ROOTFS}/lib/libz.so.1 ${ROOTFS}/lib/libgcc_s.so.1

    LITS="$(resolve_dir_path "LIBLITS2_CXC1734538_2/liblits*/priv/x86")"
    ITC="$(resolve_dir_path "ITC2_CXC1736334_2/itc*/priv/x86")"
    SDS="$(resolve_dir_path "LIBSDS2_CXC1734539_2/libsds*/priv/x86")"
    COLI="$(resolve_dir_path "COLI2_CXC1734536_2/coli*/priv/x86")"
    TRI="$(resolve_dir_path "TRI2_CXC1736488_2/tri*/priv/x86")"
    TRUTIL="$(resolve_dir_path "TRACE-UTILS2_CXC1735093_2/tr*/priv/x86")"
    NS="$(resolve_dir_path "NS2_CXC1736336_2/ns*/priv/x86")"
    ERRMAN="$(resolve_dir_path "ERRMAN-PMD2_CXC1736371_2/errman-pmd*/priv/x86")"

    LD_LIBRARY_PATH=${NS}/lib:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${ITC}/lib:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${LITS}/lib:${SDS}/lib:${COLI}/lib:${TRI}/lib:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${ERRMAN}/lib:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${RELEASES_DIR}/cslibs/${ArchDir}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${GCCLIBS}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${ROOTFS}/usr/lib:${LD_LIBRARY_PATH}
    export PATH=${COLI}/bin:${COLI_CMDS_PATH}:${TRI}/bin:${TRUTIL}/bin:${TRUTIL}/sbin:${NS}/bin:${ITC}/bin:${ERRMAN}/bin:${PATH}:${ROOTFS}/usr/bin
else
    LD_LIBRARY_PATH=${RELEASES_DIR}/cslibs/${ArchDir}:${LD_LIBRARY_PATH}
fi

export RCS_LIB_DIR=${RCS_LIB_ROOT}/${ArchDir}

SAFC="$(resolve_dir_path "SAF2_CXC*/safc-*/priv/tgt_${ArchDir}/lib*")"
APPM="$(resolve_dir_path "APPM2_CXC*/appm-*/priv/tgt_${ArchDir}/lib*")"
EQS="$(resolve_dir_path "EQS2_CXC*/eqs*/priv/tgt_${ArchDir}/lib*")"
CEC="$(resolve_dir_path "CEC2_CXC*/cec*/priv/tgt_${ArchDir}/lib*")"
LIH="$(resolve_dir_path "LIH2_CXC*/lih*/priv/tgt_${ArchDir}/lib*")"
CLH="$(resolve_dir_path "CLH2_CXC*/clh*/priv/tgt_${ArchDir}/lib*")"
GMF="$(resolve_dir_path "GMF2_CXC*/gmf*/priv/tgt_${ArchDir}/lib*")"
ALH="$(resolve_dir_path "ALH2_CXC*/alh*/priv/tgt_${ArchDir}/lib*")"
PMS="$(resolve_dir_path "PMS2_CXC*/pms*/priv/tgt_${ArchDir}/lib*")"
SYS="$(resolve_dir_path "SYS2_CXC*/sys-*/priv/tgt_${ArchDir}/lib*")"
OOT="$(resolve_dir_path "OOT2_CXC*/oot*/priv/tgt_${ArchDir}/lib*")"
CERT="$(resolve_dir_path "CERT2_CXC*/cert*/priv/tgt_${ArchDir}/lib*")"
PES="$(resolve_dir_path "PES2_CXC*/pes*/priv/tgt_${ArchDir}/lib*")"
TIM="$(resolve_dir_path "TIM2_CXC*/tim*/priv/tgt_${ArchDir}/lib*")"

symlink_files ${SAFC}/* ${APPM}/* ${EQS}/*  ${CEC}/*  ${LIH}/*  ${CLH}/* ${GMF}/* ${ALH}/* ${PMS}/* ${SYS}/* ${OOT}/* ${CERT}/* ${PES}/* ${TIM}/*

export LD_LIBRARY_PATH=${DEV_PATCHES}:${OTP_ROOT}/lib:${LD_LIBRARY_PATH}

insert_readme $SYS $DEV_PATCHES

check_start_log

if  [ "${INSTALL_ONLY:-false}" = "false" ]
then
    if  [ ${Arch} == "x86_64" ]; then
        if  [[ -z "${RESTART_HELPER_PID:-}" ]]
        then
            start_log "***************Setting up RestartHelper***************"

            # Spawn a restart helper process that can spawn a new instance of
            # start_rcs.sh. This is intended to happen whenever the 'heart' process
            # is triggered by a crashing or rebooting Erlang VM, or as part of
            # a system upgrade.
            #
            # The RESTART_HELPER_PID variable is trusted to be unset when start_rcs.sh
            # is invoked from the command line, typically via the rcssim script.
            # If, on the other hand, start_rcs.sh has been invoked by the restart helper
            # process then the variable is set, preventing another restart helper from
            # being started.
            RestartHelper="`dirname $(readlink -f $0)`/restart_helper.sh ${sys_priv_path} ${SNAME}  ${logFile} ${RCS_ROOT}"
            printf "%s\n" "spawn: $RestartHelper" | AppendLog
            $RestartHelper &

            # The pid of the restart helper is needed by the Erlang node that
            # is about to be started.
            export RESTART_HELPER_PID=$!

	fi
	HEART_CMD="/bin/kill -s USR1 $RESTART_HELPER_PID"
	export HEART_COMMAND="echo \`date\` $HEART_CMD >> $HEART_LOG; $HEART_CMD "
	start_log "HEART_COMMAND set to: $HEART_COMMAND"

	setupEEinSim
    else
	export SIM_TGT='tgt_'${ArchDir}
	HEART_CMD=""
	export HEART_COMMAND="echo \`date\` $HEART_CMD >> $HEART_LOG; $HEART_CMD "
	start_log "HEART_COMMAND set to: $HEART_COMMAND"
    fi
fi


#start_log "LD_LIBRARY_PATH set to: $LD_LIBRARY_PATH"

if [ "${INSTALL_ONLY:-false}" = true ]; then
    cat <<EOF > ${RCS_ROOT}/sourceMe.csh
setenv PATH ${PATH}:\$PATH
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:\$LD_LIBRARY_PATH
setenv RCS_SIM_ROOT ${RCS_SIM_ROOT}
setenv OTP_ROOT ${OTP_ROOT}
EOF
    cat <<EOF > ${RCS_ROOT}/sourceMe.sh
export PATH=${PATH}:\$PATH
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:\$LD_LIBRARY_PATH
export RCS_SIM_ROOT=${RCS_SIM_ROOT}
export OTP_ROOT=${OTP_ROOT}
EOF


    start_log "Installation done.  Start with start_rcs.sh"

else

    BOOT_FILE=${RCS_ROOT}${HOME}/releases/${RCS_VERSION}/${RCS_SYSTEM}-${RCS_VERSION}

    if [ ${BT} == "VM" ]; then
	SETCOOKIE="-setcookie rcs"
    else
	SETCOOKIE=
    fi

    run_cmd="erl -boot ${BOOT_FILE} -boot_var OTP_ROOT $OTP_ROOT \
       -boot_var CODE_ROOT $CODE_ROOT -sname $SNAME  -heart $SETCOOKIE\
       -async_shell_start -smp enable \
       -env ERL_CRASH_DUMP $ERLANG_LOG_DIR/erl_crash_dump.`date +%FT%T` \
       -env ERL_CRASH_DUMP_SECONDS 10 \
       -config ${RCS_ROOT}${HOME}/releases/${RCS_VERSION}/sys.config \
       -pa ${DEV_PATCHES} \
       +W w +A 64 +Bc +d +C multi_time_warp"

    start_log "run_cmd=  $run_cmd"

    export RUN_ERL_LOG_GENERATIONS=10
    export RUN_ERL_LOG_MAXSIZE=1000000

    #when run from heart this line cause error, workaround - ensure always true
    printf "to_erl ${PIPE_DIR}/${SNAME}@${HOST}/\n" || true

    run_erl  -daemon ${PIPE_DIR}/${SNAME}@${HOST:?Environment variable HOST must be set}/ $ERLANG_LOG_DIR "$run_cmd"
    if [ "${RCS_MODE}" = simulated ]; then
	printf "rcssim started with RCS_ROOT= ${RCS_ROOT}"
    fi
fi

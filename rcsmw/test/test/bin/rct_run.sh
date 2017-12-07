#!/bin/sh
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
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2012-02-01   etxkols     Created
## R1A/2      2012-02-03   etxkols     Replacing several scrips with this one
## R1A/4      2012-02-20   etxkols     Experimenting
## R1A/5      2012-02-20   etxkols     Experimenting
## R1A/6      2012-02-20   etxkols     Experimenting
## R1A/7      2012-03-07   etxkols     Experimenting
## R1A/8      2012-03-09   etxkols     beam path /test/release/labb/ebin
## R1A/9      2012-03-14   etxkols     Changed log dir to /proj/rcs-tmp
## R1A/10     2012-03-14   etxkols     Added -pa /proj/rcs/rct_dev_patches
## R1A/11     2012-03-15   etxbjca     Added Log URL
## R1A/13     2012-03-21   etxpeno     Added path to rct-proxy
## R1A/14     2012-06-01   etxkols     Added -enable_builtin_hooks false
## R1A/15     2012-06-01   etxkols     Moved -enable_builtin_hooks false
## R1A/17     2012-10-29   etxkols     Added -pa ${RCT_TOP}/test/release/imm-rpc/ebin
## R1A/18     2012-11-05   etxkols     Added warning if -sim $USER
## R2A/4      2013-02-15   etxkols     Added global -config rcsci.cfg file
## R2A/5      2013-02-27   etxkols     Proper exit codes added
## R2A/6      2013-04-09   etxkols     Cleanup and added check for upgrade.sh
## R2A/7      2013-04-09   etxkols     Updated usage
## R2A/8      2013-04-25   etxivri     Changed imm-rpc to saf-rpc
## R2A/9      2013-05-30   etxkols     Added rct-snmpmgr to -pa
## R2A/10     2013-06-24   etxjovp     Added -pa /proj/rcs/rct_dev_patches/$OTP_RELEASE
## R2A/11     2013-09-17   etxkols     Added ${RCT_TOP}/test/release/lib-netconf/ebin
## R2A/12     2013-10-10   etxkols     Added ${RCT_TOP}/test/release/lib-netconf/ebin
## R2A/13     2014-06-04   etxkols     Added ${RCT_TOP}/test/release/lib/ebin
## R2A/14     2014-06-25   etxarnu     Added ERL_LIBS for new SAFE path
## R2A/15     2014-06-25   etxarnu     Fixed bug
## R2A/16     2014-06-25   etxarnu     exported ERL_LIBS
## R23/1      2014-11-27   etxkols     -create_priv_dir auto_per_tc flag
## R3A/2      2015-01-31   erarafo     Copyright updated
## R3A/3      2015-02-12   etxkols     Cookie option removed
## R3A/4      2015-02-19   etxarnu     setcookie back for target
## R3A/5      2015-02-26   etxkols     Added config sim_sftp_server.cfg as preparation for two labs
## R3A/6      2015-02-27   etxkols     Use sim_lab_servers.cfg
## R3A/7      2015-03-20   erarafo     Support for new library saf-ift
## R3A/8      2015-03-21   erarafo     Don't clobber CT_INCLUDE_PATH if preset
## R4A/1      2015-09-17   etxkols     Cluster sim stuff
## R4A/2      2015-09-17   etxkols     Messed up -config files
## R4A/3      2016-03-03   etxkols     5G
## R4A/4      2016-03-04   etxkols     5G
## R4A/5      2016-03-14   etxkols     5G
## R5A/1      2016-04-19   erarafo     Make SAF oi.hrl available to test suites
## R5A/2      2016-03-22   etxkols     Change ccs to rcf
## R6A/1      2016-06-21   etxkols     GIT
## R6A/2      2016-08-16   etxkols     GIT no2
## R6A/3      2016-08-22   etxkols     include path to SWM to support GIT
## R8A/1      2016-11-21   etxkols     redhat
## R9A/1      2017-02-03   etxkols     Added rct-jsone
## R9A/2      2017-04-25   etxkols     Fix for new GIT build env
## R9A/3      2017-04-26   etxkols     ERL_LIBS  for new GIT build env
## R10A/1     2017-05-03   etxkols     Fixes for multi node testing in Jenkins
## R10A/2     2017-05-04   etxkols     Use $RCS_TOP instead of /repo/${USER}/rcsmw
## R10A/3     2017-05-09   etxkols     Load all .cfg files in /proj/rcs-tmp/stps/$STPname/config/
## R10A/4     2017-05-10   etxkols     Bug Config for -sim
## R10A/5     2017-05-10   etxkols     Forgot to remove exit 0
## R11A/1     2017-08-30   etxkols     Moved compile_git_test_libs
## R11A/2     2017-10-06   etxkols     Support rcs-sim in cloud env (GIT preparation)
## ----------------------------------------------------------

progname="`basename $0`"
STP_ROOT=/proj/rcs-tmp
STP_DIR=${STP_ROOT}/stps
WEB_SERVER=https://rbs-rde.rnd.ki.sw.ericsson.se
GLOBAL_CFG_FILE=${RCT_TOP}/test/config/common.cfg

usage() {
   echo "usage: rct_run.sh -only_compile | -oc | [ -stp <STPname> | -sim <STPname> ] [ -no_compile (-nc) ] <Args>

<STPname>           For Target environment (-stp).
                    /proj/rcs-tmp/stps/STPname/config/*.cfg is used for reading config data to Common Test
                    (created with ${RCT_TOP}/test/bin/rct_add_stp.sh).
                    ${STP_DIR}/STPname/logs in shared proj area is used for storing Common Test log files.
                    Common Test erlang node name is set to STPname@hostname.
                    Common Test cookie is set to rcs.

                    For simulated environment (-sim).
                    ${STP_DIR}/STPname/logs in shared proj area is used for storing Common Test log files.
                    Common Test erlang node name is set to STPname@hostname

-only_compile | -oc Only valid in GIT env. Will compile RCT libs and ALL testsuites, but will NOT start any tests.
                    This option is intended to be used by Jenkins in multinode testing where we don't want to compile everything several times.
 
-no_compile | -nc   Only valid in GIT env. Will NOT compile RCT libs or ALL testuites, i.e. only the specified testsuite(s) will be compiled if necessary.
                    This option is intended to be used by Jenkins in multinode testing where -only_compile option has previously been run
                    If not given, default behavior is to only compile RCT libs before tests are started and then only compile the specified testsuites if necessary.

<Args>              ct_run or erl arguments, see erlang documentation.

Description: Wrapper for ct_run which set
ct_run flags: -logdir, -pa, -config (not when -sim), -stylesheet
erl flags: -sname, -kernel, -hidden, -setcookie
Usage of flags above in Args overrides settings made by this script.
Exports environment variable SIM_OR_TARGET to be used by Common Test

examples:
rct_run.sh -stp mystp -suite \$RCT_TOP/test/suites/example_single_SUITE -case mytest
rct_run.sh -stp mystp -spec \$RCT_TOP/test/suites/example_spec
rct_run.sh -stp mystp -shell
rct_run.sh -sim mystp -suite \$RCT_TOP/test/suites/example_single_SUITE -case mytest
rct_run.sh -only_compile
rct_run.sh -stp rcf_etxkols -no_compile -spec \$RCT_TOP/test/suites/example_spec"
   exit 1
}

error() {
    echo "${progname}: ERROR $1"
    exit 1
}

cc_or_git() {
    if echo $RCT_TOP | grep "^/vobs" > /dev/null; then
	echo "${progname}: Running in Clearcase environment"
	CC_OR_GIT="CC"
    else
	echo "${progname}: Running in GIT environment"
	CC_OR_GIT="GIT"
    fi	
}

compile_git_test_libs() {   
    echo "${progname}: Compiling RCT libs"	
    set -e
    cd $RCT_TOP/test/
    waf configure
    waf
    waf install --destdir=release
    cd -
    set +e
}

compile_rct_test_suites() {
    echo "${progname}: Compiling RCT test suites with:"	
    echo "            Includes: $INCLUDE_LIST"	
    echo "            ERL_LIBS: $ERL_LIBS"	
    PWD=`pwd`
    RCT_TEST_SUITES=`find $RCT_TOP/test/suites -name '*.erl'`
    for ERLPATH in $RCT_TEST_SUITES ; do
	DIR=`dirname $ERLPATH`
	echo "erlc -o $DIR $ERLPATH"
	erlc $INCLUDE_LIST -o $DIR $ERLPATH
	if [ $? -ne 0 ] ; then
	    cd $PWD
	    error "Could not compile $ERLPATH"
	fi
    done
    cd $PWD
}

compile_block_test_suites() {
    echo "${progname}: Compiling RCS block test suites with:"
    echo "            Includes: $INCLUDE_LIST"	
    echo "            ERL_LIBS: $ERL_LIBS"	
    PWD=`pwd`
    RCT_TEST_SUITES=`find $RCS_TOP/[A-Z][A-Z]*/test/suites/ -name '*.erl'`
    for ERLPATH in $RCT_TEST_SUITES ; do
	DIR=`dirname $ERLPATH`
	echo "erlc -o $DIR $ERLPATH"
	erlc $INCLUDE_LIST -o $DIR $ERLPATH
	if [ $? -ne 0 ] ; then
	    cd $PWD
	    error "Could not compile $ERLPATH"
	fi
    done
    cd $PWD
}

appendPath() {
  case "$1" in
    undefined)
      echo "$2";;
    *)
      echo "$1:$2"
  esac
}

make_configs() {
    Config=""
    ConfigFiles=`ls $STP_DIR/$STPname/config/*.cfg 2>&1`
    if [ $? -eq 0 ] ; then
	for File in $ConfigFiles ; do
	    Config="$Config $File"
	done
    fi
}

start_test() {
    mkdir -p ${STP_DIR}/$STPname/logs
    # Temporary workaround to always use -create_priv_dir auto_per_tc
    # Will be simplified when -create_priv_dir auto_per_tc flag is removed from Jenkins,
    # because apparenlty the flag cannot be added twice
    if echo $Args | grep "create_priv_dir auto_per_tc" > /dev/null; then
	create_priv_dir=""
    else
	create_priv_dir="-create_priv_dir auto_per_tc"
    fi
    Cmd="ct_run \
        -logdir $STP_DIR/$STPname/logs \
        -pa ${RCT_TOP}/test/release/labb/ebin \
        -pa ${RCT_TOP}/test/release/supp/ebin \
        -pa ${RCT_TOP}/test/release/rct-proxy/ebin \
        -pa ${RCT_TOP}/test/release/rct-jsone/ebin \
        -pa ${RCT_TOP}/test/release/rct-snmpmgr/ebin \
        -pa ${RCT_TOP}/test/release/saf-rpc/ebin \
        -pa ${RCT_TOP}/test/release/saf-ift/ebin \
        -pa ${RCT_TOP}/test/release/lib-netconf/ebin \
        -pa ${RCT_TOP}/test/release/lib-cli/ebin \
        -pa ${RCT_TOP}/test/release/lib/ebin \
        -pa /proj/rcs/rct_dev_patches \
        -pa /proj/rcs/rct_dev_patches/$OTP_RELEASE \
        -config $Config \
        -stylesheet ${RCT_TOP}/test/styles/rct_categories.css \

        $Args \
        $create_priv_dir \
        -enable_builtin_hooks false \
        -erl_args \
        -sname $STPname \
        $Cookie \
        -kernel net_ticktime 8 \
        -hidden"
    echo $Cmd
    $Cmd
}

echo "${progname}: $@"

COMPILE="default"
if [ $# -ge 1 ]; then
    case $1 in
	-only_compile) COMPILE="only_compile";;
	-oc)           COMPILE="only_compile";;
	-h) usage;;
	-stp)
	    echo "${progname}: Target environment"
	    STPname=$2; shift; shift
       	    case $1 in
		-no_compile) COMPILE="no_compile";shift;;
		-nc)         COMPILE="no_compile";shift;;
	    esac
	    if echo $STPname | grep "^rcf" > /dev/null; then
		SIM_OR_TARGET=cloudish
	    elif echo $STPname | grep "^sim" > /dev/null; then
		SIM_OR_TARGET=cloudish
	    else
		SIM_OR_TARGET=target
	    fi
	    export SIM_OR_TARGET
	    Cookie=" -setcookie rcs"
	    make_configs
	    Config="$Config $GLOBAL_CFG_FILE";;
	-sim)
	    echo "${progname}: Simulated environment"
	    STPname=$2; shift; shift
	    if [ $STPname == $USER ]; then
		echo -n "${progname}: '-sim $USER' will conflict unless rcssim is started with -s flag (same erlang node names). "
		echo -n "Do you want to continue (yes/no)? "
                read answer
                case $answer in
		    yes) ;;
		    *)   exit 1;;
                esac
	    fi
   	    SIM_OR_TARGET=sim; export SIM_OR_TARGET
	    Cookie=""
       	    case $1 in
		-no_compile) COMPILE="no_compile";shift;;
		-nc)         COMPILE="no_compile";shift;;
	    esac
	    make_configs
	    Config="$Config $RCT_TOP/test/config/sim_lab_servers.cfg $GLOBAL_CFG_FILE";;
	*)
	    usage;;
    esac
else
    usage
fi
Args=$@

cc_or_git
# Additional include directory paths for the test suite compilation.
# Still further directory paths may be specified with the -include
# command-line option. Paths specified with -include will be searched
# before paths defined here.
IncludePath=`appendPath "${CT_INCLUDE_PATH:-undefined}" $RCT_TOP/test/release/saf-ift/inc`
if [ $CC_OR_GIT = "CC" ] ; then
    IncludePath+=":/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/SAF/SAF_CXA1105667/inc"
    IncludePath+=":/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/LMA/LMA_CNX9013077/test/suites"
    IncludePath+=":/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/COMTE/COMTE_CXA1105461/inc"
    IncludePath+=":/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/SWM/SWM_CXA1105449/inc"
    export CT_INCLUDE_PATH=$IncludePath
    echo "${progname}: CT_INCLUDE_PATH $CT_INCLUDE_PATH"	
    OTP_RELEASE=$(cleartool ls $OTP_ROOT/Install | awk '{print $3}')
    echo "${progname}: OTP_RELEASE $OTP_RELEASE"
    export ERL_LIBS=/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/OTP/OTP12_CXC1733859_12/otp/priv/tgt_i686/lib
    echo "${progname}: ERL_LIBS $ERL_LIBS"
else 
    if printenv LOADEDMODULES | grep "rcsmw/default" > /dev/null; then
	echo "${progname}: new GIT"
	OTP_RELEASE=`which erl | sed -n "s/.*OTP\/\(R.*\)\/OTP.*/\1/p"`
	DIR=`which erl | xargs dirname | xargs dirname`
	OTP_VERSION=`cat $DIR/releases/*/OTP_VERSION`
	echo "${progname}: OTP_RELEASE $OTP_RELEASE ($OTP_VERSION)"
	export ERL_LIBS="${OTP_ROOT}/lib:${SAFE_OTP_ROOT}/lib"
	echo "${progname}: ERL_LIBS $ERL_LIBS"
	if [ $COMPILE = "default" ] || [ $COMPILE = "only_compile" ]; then
	    compile_git_test_libs
	fi
	if [ $COMPILE = "only_compile" ]; then
	    INCLUDE_LIST="-I ${RCS_TOP}/test/test/lib/saf-ift/esrc -I ${RCS_TOP}/SAF/inc -I ${RCS_TOP}/LMA/test/suites -I ${RCS_TOP}/COMTE/inc -I ${RCS_TOP}/SWM/inc"
	    compile_block_test_suites
	    compile_rct_test_suites
	    exit 0
	fi
    else	
	echo "${progname}: old GIT"
	OTP_RELEASE=`which erl | sed -n "s/.*\(OTP-.*\)-[a-z].*/\1/p"`
	echo "${progname}: OTP_RELEASE $OTP_RELEASE"
	export ERL_LIBS=$RCT_TOP/OTP/tgt_i686_32/lib/
	echo "${progname}: ERL_LIBS $ERL_LIBS"
	compile_git_test_libs
    fi
    IncludePath+=":$RCS_TOP/SAF/inc"
    IncludePath+=":$RCS_TOP/LMA/test/suites"
    IncludePath+=":$RCS_TOP/COMTE/inc"
    IncludePath+=":$RCS_TOP/SWM/inc"
    export CT_INCLUDE_PATH=$IncludePath
    echo "${progname}: CT_INCLUDE_PATH $CT_INCLUDE_PATH"	
fi

echo "${progname}: SIM_OR_TARGET=$SIM_OR_TARGET"
echo "${progname}: Args=$Args"
echo "${progname}: -sname $STPname"
echo "${progname}: -config $Config"
#exit 0

Result=start_test
echo "${progname}: Test Log is available at: ${WEB_SERVER}/`basename ${STP_ROOT}`/`basename ${STP_DIR}`/${STPname}/logs"
$Result

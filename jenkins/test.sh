#!/bin/bash

# Script to run jenkins commit tests. Could also be run by a developer
# but requires clearcase at this point.
# Do not modify without handshaking with design!

set -eu

print_usage() {
	echo "$0: <branch> <workspace> <target>"
	echo " -c <cxs-version>    Can be one of latest or stable"
	echo " -f                  Fetch CXS only, do not use built RCSEE CXC"
	echo " -s <test-spec>      Test specification"
	echo " -m <mem-config>     File specifying which limit to use for memory check"
	echo " -r <count>          Repeat the test run a number of times"
	echo " -g <gerrit-host>    Gerrit host with which to communicate"
	echo " -p <patchset>       Gerrit patchset on which to comment"
	echo " -u <path-to-up>     Use provided CXS for testing"
	echo " -k                  Kill possible old CT related process on client"
	echo " -n                  Do not download CXS and do not run tftpprep"
	echo " <branch>            The branch to build, currently R2, R3,  R4 or R5"
	echo " <workspace>         Jenkins workspace, a unique name for this executor"
	echo "                     used for setting a clearcase view"
	echo " <target>            The target on which to run the tests"
	echo " -h                  Help, this text."
	echo
	echo "Example basic usage:"
	echo " $ cp -r ../rcs-ee/tcu-delivery ."
	echo " $ ./jenkins/test.sh R4 this tcuXXX"
	echo
	echo "Example usage for testing a new test-case, using an already stable cxs:"
	echo " $ ./jenkins/test.sh -r 10 -f -c stable R4 this tcuXXX"
	echo
}

trap print_usage EXIT

# Options
while getopts ":hc:s:m:fr:g:p:u:kn" opt; do
	case $opt in
		c)
			cxs=$OPTARG
			;;
		s)
			spec=$OPTARG
			;;
		m)
			mem_config=$OPTARG
			;;
		f)
			fetch_cxs_only="-f"
			;;
		r)
			repeat=$OPTARG
			;;
		g)
			gerrit_host=$OPTARG
			;;
		p)
			gerrit_patchset=$OPTARG
			;;
		u)
			up=$OPTARG
			;;
		k)
			kill_old=true
			;;
		n)
			no_prep=true
			;;
		h)
			exit 1
			;;
		\?)
			echo "Invalid option: -$OPTARG" >&2
			;;
	esac
done


for (( cnt=1; cnt<$OPTIND; cnt++ )) ; do
	shift
done


# Arguments
branch=$1
workspace=$2
target=$3

# Variable definitions
config_spec=
cxs=${cxs-stable}
up=${up-}
spec=${spec-rcs-test/specs/commit_basic.erl}
mem_config=${mem_config-rcs-test/rct/properties/default_mem_config.cfg}
repeat="-repeat ${repeat-1}"
fetch_cxs_only=${fetch_cxs_only-}
kill_old=${kill_old-}
no_prep=${no_prep-}
gerrit_host=${gerrit_host-}
gerrit_patchset=${gerrit_patchset-}
gerrit_fifo_cmd=cat
workspace_no_at=$(basename ${workspace} | sed -e 's/@/_/g')
ccview=${USER}_${HOST}_${target}_${workspace_no_at}_rcs
logdir=/proj/rcs-tmp/stps/${target}/logs/${USER}/$(date +"%Y-%m-%d")/
mkrcseecxs=$(git rev-parse --show-toplevel)/rcs-tools/ee-install/mkrcseecxs.sh

# VOB paths
rct_top=/vobs/rcs/test/RCT_CRX901275/
all_top=/vobs/rcs/delivery/RCP_CSX10179_1/RCP-ALL_CXS101553

# Decide on config spec depending on branch
if [[ ${branch} == R2 ]]; then
	config_spec=${all_top}/doc/19066/RCP-ALL_CXS101553_cs@@/main/R1A/R2A/LATESTSTABLE
elif [[ ${branch} == R3 ]]; then
	config_spec=${all_top}/doc/19066/RCP-ALL_CXS101553_cs@@/main/R1A/R2A/R3A/LATESTSTABLE
elif [[ ${branch} == R4 ]]; then
	config_spec=${all_top}/doc/19066/RCP-ALL_CXS101553_cs@@/main/R1A/R2A/R3A/R4A/LATESTSTABLE
elif [[ ${branch} == R5 ]]; then
	config_spec=${all_top}/doc/19066/RCP-ALL_CXS101553_cs@@/main/R1A/R2A/R3A/R4A/R5A/LATESTSTABLE
else
	echo "Unknown branch $branch" >&2
	exit 1
fi

if [ -n "${no_prep}" ] ; then
        echo "Skipping CXS processing"
elif [ ! -z "${up}" ] ; then
        echo "Skip CXS build and use CXS provided by user"
        cxs=${up}
elif [[ ${target} == tcu ]]; then
        echo "Creating tcu CXS"
        ${mkrcseecxs} ${fetch_cxs_only} -p tcu -m ${cxs} -r .
        cxs=EE_BLACK_RCP-T*
elif [[ ${target} == dus* ]]; then
        echo "Creating dus CXS"
        ${mkrcseecxs} ${fetch_cxs_only} -p dus -m ${cxs} -r .
	cxs=EE_BLACK_DUS*
else
	echo "Unkown board type for $target" >&2
	exit 1
fi

# Make sure the cxs exists.
if [[ ! -f ${cxs} && -z "${no_prep}" ]]; then
	echo "No such file $cxs" >&2
	exit 1
fi

# Create a log directory
mkdir --mode=775 -p ${logdir}

# If no_prep is set, skip tftp setup
if [ -z "${no_prep}" ] ; then
    echo "Setting up TFTP environment"
    /env/RCSDE/bin/rcstprep.sh ${target} ${PWD}/${cxs}
fi

echo "Setting up ClearCase View"
viewPrep.sh -v ${ccview} -c ${config_spec}

# !!NOTICE!!
#
# ExecProjName below will execute commands inside clearcase,
# a wrapper essentially.
#

# Set up gerrit commenting
send_gerrit_test_url () {
	local test_url=""

	# Loop until we find the test url
	while read line && [ -z "$test_url" ]; do
		if [[ "$line" == "Test run log:"* ]]; then
			test_url=$(echo $line | sed 's|.*\(http.*\.html\)|\1|')
			echo "Found test URL: $test_url" >&2
			local message=$(printf "%s\n\n%s" \
				"Test run log for ${spec} on ${target}:" \
				"$test_url")
			ssh $gerrit_host \
				-p 29418 \
				gerrit review $gerrit_patchset \
				"--message \"${message}\""
		fi
		echo $line
	done
	# Then cat the rest
	cat
}

if [ -n "$gerrit_host" ]; then
	if [ -n "$gerrit_patchset" ]; then
		echo "Will send test run URL to gerrit"
		gerrit_fifo_cmd="send_gerrit_test_url"
	else
		echo "Gerrit host provided, but no gerrit patchset!?" >&2
		exit 1
	fi
fi

echo "Initiating test environment"
ExecProjName -v ${ccview} RCSDE "1.0" "$rct_top/test/bin/rct_add_stp.sh ${target}"


# if -k option kill old processes that might hang
if [ -n "$kill_old" ]; then
    # Get list of old ct related procs for STP
    echo kill old procs option present
    for OUTPUT in $(ps -efa | grep $USER | grep ct_run | grep ${target} | awk '{print $2}')
    do
        echo killing proc: $(ps -p $OUTPUT -f --no-heading)
        kill -9 $OUTPUT || true
    done
fi


# Run the installation suite
export RCS_TEST_WORKDIR=${PWD}
echo "Running tests"
ExecProjName -v ${ccview} \
	RCSDE "1.0" "$rct_top/test/bin/rct_run.sh \
	-stp ${target} -spec ${spec} -config ${mem_config} -noshell \
	-logdir ${logdir} -ct_hooks cth_surefire '[{path,\"${RCS_TEST_WORKDIR}/report.xml\"}]' \
	-pa ${PWD}/patches/ ${repeat}" | $gerrit_fifo_cmd

trap "" EXIT

#!/usr/bin/env bash

set -euo pipefail

source ${MODULESHOME-/app/modules/0}/init/bash
module use /app/plf_tools/bs_g2/python_modules/modulefiles
module add ciutils/${CIUTILS_VERSION-2.2.7}


cleanup() {
    if [[ ${root-} ]]; then
        rm -fr "$root"
    fi
    if [[ ${rootbrcs-} ]]; then
        rm -fr "$rootbrcs"
    fi
}

trap cleanup EXIT

bail_out() {
    if [[ $@ ]]; then
        echo "Bail out! $@"
    else
        echo "Bail out!"
    fi
    exit 0
}

program=$(readlink -f $(dirname ${BASH_SOURCE[0]})/../bin/radiator)
[[ -e $program ]] || bail_out "$(ls $program 2>&1)"

program=$(readlink -f $program)
[[ -f $program ]] || bail_out "$program is not a file"
[[ -x $program ]] || bail_out "$program has not the executable flag set"

echo 1..18

bash -n $program
tap-ok "$?" 'Compiles'

root=$(readlink -f $(dirname ${BASH_SOURCE[0]}))/tmp-$$
rootbrcs=${root}-brcs
mkdir -p "$root"

export RADIATOR_ROOT=$root
tap-like "$($program dummy-action 2>&1)" 'Error:' 'Unknown action'

tap-is "$($program new-id 2>&1)" '1' "Allocate first row id"

tap-is "$($program id 2>&1)" '' 'Unable to determine id'

export RADIATOR_BUILD_ID=42
export CXS101657_4_VERSION=P1A01
tap-is "$($program id 2>&1)" '42' 'Id from RADIATOR_BUILD_ID'

id=53
echo $id > $RADIATOR_ROOT/CXS101657_4-P1A01

unset RADIATOR_ROOT RADIATOR_BUILD_ID
export CXS101657_4_VERSION=P1A01
tap-is "$($program --radiator-root $root id 2>&1)" $id 'Id from MW UP data and --radiator-root'

id=99
export RADIATOR_ROOT=$root
export RADIATOR_BUILD_ID=$id
mkdir -p $RADIATOR_ROOT/$id

tap-is "$($program id 2>&1)" '99' 'Id from RADIATOR_BUILD_ID has highest priority'

tap-is "$($program set foo 'bar "baz"' 2>&1)" '' 'Setting foo'
tap-ok $? 'Setting foo=bar "baz"'
tap-is "$(ls $RADIATOR_ROOT/$id/foo 2>&1)" "$RADIATOR_ROOT/$id/foo" 'foo created'
tap-is "$(cat $RADIATOR_ROOT/$id/foo 2>&1)" 'bar "baz"' 'foo contains bar "baz"'

tap-is "$($program new-id 2>&1)" '100' "Allocate new row id"
tap-is "$(ls -d $RADIATOR_ROOT/100 2>&1)" "$RADIATOR_ROOT/100" "New id directory created"

# BRCS tests

mkdir -p "$rootbrcs"
export RADIATOR_ROOT=$rootbrcs
unset RADIATOR_BUILD_ID CXS101657_4_VERSION
export CXS101665_3_VERSION=P1A01

echo 1 > $RADIATOR_ROOT/CXS101665_3-P1A01

tap-is "$($program new-id 2>&1)" '1' "Allocate first BRCS row id"
tap-is "$($program set bfoo 'bbar bbaz' 2>&1)" '' 'Setting bbfoo'
tap-ok $? 'Setting bfoo="bbar bbaz"'
tap-is "$(ls $RADIATOR_ROOT/1/bfoo 2>&1)" "$RADIATOR_ROOT/1/bfoo" 'bfoo created'
tap-is "$(cat $RADIATOR_ROOT/1/bfoo 2>&1)" "bbar bbaz" 'bfoo contains bbar bbaz'

#!/usr/bin/env bash
set -euo pipefail

source tools/env-ee-i586.sh

if [[ ! ${EE_CXP_LABEL-} ]]; then
    echo Error: mandatory environment variable EE_CXP_LABEL is not set 1>&2
    exit 1
fi

if [[ ! ${EE_CXA_LABEL-} ]]; then
    echo Error: mandatory environment variable EE_CXA_LABEL is not set 1>&2
    exit 1
fi

cd $TOPDIR
test -a out && rm -fr out
mkdir -p out
cd $TOPDIR/out

vrcs_url=https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/VRCS_CSX10179_2

# download CXP from CC
wget --quiet ${vrcs_url}/VRCS-UP_CXS101657_2/VRCS-EE_CXP9029177_4/doc/19010/VRCS-EE_CXP9029177_4.cxp@@/${EE_CXP_LABEL} -O VRCS-EE_CXP9029177_4.cxp
wget --quiet ${vrcs_url}/VRCSI_CXS101659/SDK_CXA11448_2/doc/19010/SDK_CXA11448_2.cxa@@/${EE_CXA_LABEL} -O SDK_CXA11448_2.cxa

cd $TOPDIR && ls -lrth out/*

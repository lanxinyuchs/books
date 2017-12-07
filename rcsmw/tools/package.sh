#!/usr/bin/env bash
set -euo pipefail
cd $TOPDIR
git clean -xdf out 

# Create the final structure under out
./waf install --destdir out

# Create the packages.
# This step needs to be run with '-j1' as the same temp files are used for each cxp
if [[ ${1-} = --image ]]; then
    ./waf --img -j1
else
    ./waf --pkg -j1
fi

ls -lrth $BLDDIR/out/*
git clean -xdf   out/ 
cp -p $BLDDIR/out/* out/

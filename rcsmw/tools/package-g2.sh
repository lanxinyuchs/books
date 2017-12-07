#!/usr/bin/env bash

set -euo pipefail

source ${MODULESHOME-/app/modules/0}/init/bash
module use /app/plf_tools/bs_g2/python_modules/modulefiles
module add ciutils/2.2.18

cd $TOPDIR

source env-rcssim.sh
BLDDIR_SIM=$TOPDIR/$(basename $BLDDIR)
waf
git clean -xdf out
waf install --destdir out
shopt -s extglob
for tgt in out/*_CXA*/tgt_i686; do
    cxa=$(dirname $tgt)
    rm -f $tgt/lib32
    mv $cxa/lib $tgt/lib32
    rm -rf $cxa/!(tgt_i686)
done
RCSSIM_CXP_DIR=$(python -c"
import os
execfile(os.path.join(os.environ['TOPDIR'], 'tools', 'config', 'rcssim_product.py'))
print LMC['mw'][0] + '_' + LMC['mw'][1]
")
cd out/$RCSSIM_CXP_DIR
tar xf $EE_CXP_PATH
# FIXME: Broken link below makes 'waf --pkg' to crash
rm RHAI2_CXC1734540_2/rhai2-*/priv/x86/scripts/startup
cd $TOPDIR
waf --pkg -j1

source env-msrcs.sh
waf
waf install --destdir out
waf --pkg -j1

cp -p $BLDDIR_SIM/out/*.{cxs,cxp,xml,dbg} out
cp -p $BLDDIR/out/* out/

config.py merge out/baseline.config $BLDDIR_SIM/out/baseline.config $BLDDIR/out/baseline.config


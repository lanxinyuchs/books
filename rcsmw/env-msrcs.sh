VARIANT=${1:-arm32}

export TOPDIR="$(dirname $(readlink -f ${BASH_SOURCE[0]}))"
export TGT=tgt_arm-wr6
source $TOPDIR/tools/trim-path.sh $TOPDIR/tools/env-host_x86_64.sh
source $TOPDIR/tools/env-ee-msrcs.sh
export CFLAGS="-fno-omit-frame-pointer -funwind-tables"
export CPPFLAGS="$CPPFLAGS \
 -I$OS/A4CI/A4CI_CXA11468/include-wr6\
 -I$OS/CAPI/CAPI_CXA11479/include-wr6\
 -I$OS/COLI/COLI_CXA11420/include-wr6\
 -I$OS/ERRMAN-PMD/ERRMAN-PMD_CXA11469/include-wr6\
 -I$OS/HWTEST/HWTEST_CXA11492/include-wr6\
 -I$OS/ITC/ITC_CXA11466/include-wr6\
 -I$OS/ITCLNH/ITCLNH_CXA1106419/include-wr6\
 -I$OS/ITCLNH/ITCLNH_CXA11482/include-wr6\
 -I$OS/LIBLITS/LIBLITS_CXA11428/include-wr6\
 -I$OS/LIBSDS/LIBSDS_CXA11429/include-wr6\
 -I$OS/NS/NS_CXA11467/include-wr6\
 -I$OS/PGH/PGH_CXA11472/include-wr6\
 -I$OS/RCSEE/NVPI_CXA11493/include-wr6\
 -I$OS/RHAI/RHAI_CXA1105840/include-wr6\
 -I$OS/RHAI/RHAI_CXA11430/include-wr6\
 -I$OS/RTE/RTE_CXA11458/include-wr6\
 -I$OS/TRACE-UTILS/TRACE-UTILS_CXA11481/include-wr6\
 -I$OS/TRI/TRI_CXA11471/include-wr6"

export LDFLAGS="\
 -L$OS/A4CI/A4CI_CXA11468/arm-wr6/lib\
 -L$OS/CAPI/CAPI_CXA11479/arm-wr6/lib\
 -L$OS/COLI/COLI_CXA11420/arm-wr6/lib\
 -L$OS/ERRMAN-PMD/ERRMAN-PMD_CXA11469/arm-wr6/lib\
 -L$OS/HWTEST/HWTEST_CXA11492/arm-wr6/lib\
 -L$OS/ITC/ITC_CXA11466/arm-wr6/lib\
 -L$OS/ITCLNH/ITCLNH_CXA1106419/arm-wr6/lib\
 -L$OS/ITCLNH/ITCLNH_CXA11482/arm-wr6/lib\
 -L$OS/LIBLITS/LIBLITS_CXA11428/arm-wr6/lib\
 -L$OS/LIBSDS/LIBSDS_CXA11429/arm-wr6/lib\
 -L$OS/NS/NS_CXA11467/arm-wr6/lib\
 -L$OS/PGH/PGH_CXA11472/arm-wr6/lib\
 -L$OS/RCSEE/BSCAN_CXA11470/arm-wr6/lib\
 -L$OS/RCSEE/NVPI_CXA11493/arm-wr6/lib\
 -L$OS/RHAI/RHAI_CXA1105840/arm-wr6/lib\
 -L$OS/RHAI/RHAI_CXA11430/arm-wr6/lib\
 -L$OS/RTE/RTE_CXA11458/arm-wr6/lib\
 -L$OS/TESTBOX/TESTBOX_CXA1105670/arm-wr6/lib\
 -L$OS/TRI/TRI_CXA11471/arm-wr6/lib"

LDFLAGS_ADD="-L$TOPDIR/SAF/$TGT/lib -L$TOPDIR/OTP/$TGT/usr/lib/"
CPPFLAGS_ADD="-I$TOPDIR/OTP/$TGT/usr/include"
export LDFLAGS="${LDFLAGS_ADD} ${LDFLAGS//$LDFLAGS_ADD/} -ldl"
export CPPFLAGS="${CPPFLAGS_ADD} ${CPPFLAGS//$CPPFLAGS_ADD/}"
export WAFLOCK=.lock-$VARIANT
export BLDDIR=build-$VARIANT
${TOPDIR}/waf configure --prd msrcs --out $BLDDIR --prefix="" --tgt="tgt_arm-wr6" --lib="lib32"

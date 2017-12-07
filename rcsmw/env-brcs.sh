VARIANT=${1:-arm32}

export TOPDIR="$(dirname $(readlink -f ${BASH_SOURCE[0]}))"
export TGT=tgt_arm-wr6
source $TOPDIR/tools/trim-path.sh $TOPDIR/tools/env-ee-brcs.sh
source $TOPDIR/tools/trim-path.sh $TOPDIR/tools/env-host_x86_64.sh
source ${TOPDIR}/tools/env.sh

LDFLAGS_ADD="-L/tmp/proj/5G_rcs/3pp//protobuf/protobuf_c-2.4.1-arm-wrs-linux-gnueabi-8.0.0.4-1/lib/  -L$TOPDIR/OTP/$TGT/usr/lib/"
LDFLAGS_ADD="-L$TOPDIR/SAF/$TGT/lib -L$TOPDIR/OTP/$TGT/usr/lib/"
CPPFLAGS_ADD="-I$TOPDIR/OTP/$TGT/usr/include"
export LDFLAGS="${LDFLAGS_ADD} ${LDFLAGS//$LDFLAGS_ADD/} -ldl"
export CPPFLAGS="${CPPFLAGS_ADD} ${CPPFLAGS//$CPPFLAGS_ADD/}"
export WAFLOCK=.lock-$VARIANT
export BLDDIR=build-$VARIANT
${TOPDIR}/waf configure --prd brcs --out $BLDDIR --prefix="" --tgt=$TGT --lib="lib32"

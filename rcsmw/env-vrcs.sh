VARIANT=${1:-i586}

export TOPDIR="$(dirname $(readlink -f ${BASH_SOURCE[0]}))"
export TGT=tgt_i686_32
source $TOPDIR/tools/trim-path.sh $TOPDIR/tools/env-ee-i586.sh
source $TOPDIR/tools/trim-path.sh $TOPDIR/tools/env-host_x86_64.sh
source ${TOPDIR}/tools/env.sh

LDFLAGS_ADD="-L$TOPDIR/SAF/$TGT/lib -L$TOPDIR/OTP/$TGT/usr/lib/"
CPPFLAGS_ADD="-I$TOPDIR/OTP/$TGT/usr/include"
export LDFLAGS="${LDFLAGS_ADD} ${LDFLAGS//$LDFLAGS_ADD/} -ldl"
export CPPFLAGS="${CPPFLAGS_ADD} ${CPPFLAGS//$CPPFLAGS_ADD/}"
export WAFLOCK=.lock-$VARIANT
export BLDDIR=build-$VARIANT
${TOPDIR}/waf configure --prd vrcs --out $BLDDIR --prefix="" --tgt=$TGT --lib="lib32"

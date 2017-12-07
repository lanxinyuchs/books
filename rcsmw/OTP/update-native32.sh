set -e
source ${MODULESHOME-/app/modules/0}/init/bash
module use /app/rbs/modules
module add wrtools-i686/1.0
export VERSION=OTP-19.1.6
export SAFEVER=safe-1.3.4
export BUILD_REV=0
export URL=ssh://gerrit.ericsson.se:29418/erlang/otp
export SAFEURL=ssh://gerrit.ericsson.se:29418/erlang/ericsson-internal
export RCSMW3PPDIR=/proj/5G_rcs/3pp/
export OTP=erlang-otp
export SAFE=erlang-ericsson-internal
export TOP=`pwd` 
export ERL_TOP_DIR=$TOP/OTP/$OTP
export SAFE_TOP=$TOP/OTP/$SAFE
export OPENSSL=$TOP/OTP/openssl
export WR_SYSROOT=/proj/5G_rcs/3pp/msrcs-ee/CXP9031275_4_R7J30/DISTRO/DISTRO_CXA11448/i686
export OFFICIAL_APPS="asn1 compiler crypto eldap erl_interface erts et inets kernel mnesia observer os_mon public_key runtime_tools sasl snmp ssh ssl stdlib syntax_tools tools xmerl"
export DUMMY_APPS="safe"
export V=1

export CC="i686-wrs-linux-gnu-gcc --sysroot=/ -L/usr/lib -L/lib"
export LD="i686-wrs-linux-gnu-gcc --sysroot=/ -L/usr/lib -L/lib"

if [[ $(whoami) != "rcsci1" ]]; then
    export RCSMW3PPDIR=~/3pp/
    mkdir -p $RCSMW3PPDIR
    export BUILD_REV=$(date "+%s")
    echo "WARNING: This 3pp dir is only for test. You cannot use the result in production" 1>&2
fi


function filter {
  for dir_all in *; do
    flag=0
    for filter_apps in $1; do
      if [[ $dir_all == $filter_apps* ]];then
        flag=1
      fi
    done
    if [[ $flag == 0 ]]; then
      echo "Deleting dir inside `pwd`: $dir_all..."
      rm -rf $dir_all
    fi
  done
}
export -f filter

cd $TOP/OTP
rm -rf $ERL_TOP_DIR $SAFE_TOP $OPENSSL
git clone $URL $OTP
git clone $SAFEURL $SAFE
(cd $SAFE_TOP && git checkout $SAFEVER && git clean -xfd)
(cd $ERL_TOP_DIR && git checkout $VERSION && git clean -xfd)
mkdir -p $OPENSSL

cd $ERL_TOP_DIR
./otp_build autoconf
PREFIX=${RCSMW3PPDIR}/OTP/$VERSION-native32-$BUILD_REV
ln -s $WR_SYSROOT/lib $OPENSSL/lib
ln -s $WR_SYSROOT/usr/include $OPENSSL/include
./configure --prefix=$PREFIX --enable-m32-build --with-ssl=$OPENSSL --without-termcap

make -j8
make install

set -e
export VERSION=OTP-20.1.7
export SAFEVER=safe-1.3.4
export BUILD_REV=0
export URL=ssh://gerrit.ericsson.se:29418/erlang/otp
export SAFEURL=ssh://gerrit.ericsson.se:29418/erlang/ericsson-internal
export RCSMW3PPDIR=/proj/5G_rcs/3pp/
export OTP=erlang-otp
export SAFE=erlang-ericsson-internal
export TOP=`pwd`
export ERL_TOP_DIR=$TOP/$OTP
export SAFE_TOP=$TOP/$SAFE
export OFFICIAL_APPS="asn1 compiler crypto eldap erl_interface erts et inets kernel mnesia observer os_mon public_key runtime_tools sasl snmp ssh ssl stdlib syntax_tools tools xmerl"
export DUMMY_APPS="safe"
export V=1

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


git clone $URL     $OTP
git clone $SAFEURL $SAFE
cd $SAFE_TOP && git checkout $SAFEVER && git clean -xfd
cd $ERL_TOP_DIR && git checkout $VERSION && git clean -xfd

cd $ERL_TOP_DIR
./otp_build autoconf
PREFIX=${RCSMW3PPDIR}/OTP/$VERSION-native-$BUILD_REV
./configure --prefix=$PREFIX
make -j8
make install

# add the native OTP to the path, so it can be used to bootstrap the other
# (cross-compiled) OTPs.
export PATH=$PREFIX/bin:$PATH

cd $ERL_TOP_DIR/lib && ln -s $SAFE_TOP/safe
cd $ERL_TOP_DIR && ./otp_build autoconf

cd $TOP
./update-arm32.sh
./update-i586.sh
./update-x86_64.sh

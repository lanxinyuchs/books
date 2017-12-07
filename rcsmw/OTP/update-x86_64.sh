set -e
export SAF_DIR=$TOP/saf/x86_64

source ../tools/env-ee-vrcs64.sh
PREFIX=${RCSMW3PPDIR}/OTP/$VERSION-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV
PREFIX_TEST=${RCSMW3PPDIR}/OTP/$VERSION-test-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV

cd $ERL_TOP_DIR
./configure erl_xcomp_sysroot=$OECORE_TARGET_SYSROOT $CONFIGURE_FLAGS --prefix=$PREFIX --disable-hipe --disable-amf --disable-clm --disable-smf

make -j8
make install

cp -a $PREFIX $PREFIX_TEST

cd $PREFIX/lib/erlang/lib
filter "$OFFICIAL_APPS"

cd $PREFIX_TEST/lib/erlang/lib
filter "$DUMMY_APPS"

cd $TOP && rm -f tgt_x86_64 && ln -s $PREFIX/lib/erlang tgt_x86_64
cd $TOP/../test/OTP && rm -f tgt_x86_64 && ln -s $PREFIX_TEST/lib/erlang tgt_x86_64
cd $TOP/../test/OTP/tgt_x86_64/lib && ln -s safe* safe
cd $TOP/tgt_x86_64 && ./Install -cross -sasl .
cd $TOP && cp -f src/erl tgt_x86_64/bin/erl
cd $TOP && cp -f src/erl tgt_x86_64/erts-*/bin/erl

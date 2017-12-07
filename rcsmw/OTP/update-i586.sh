set -e
export SAF_DIR=$TOP/saf/i586

source ../tools/env-ee-i586.sh
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

cd $TOP && rm -f tgt_i686_32 && ln -s $PREFIX/lib/erlang tgt_i686_32
cd $TOP/../test/OTP && rm -f tgt_i686_32 && ln -s $PREFIX_TEST/lib/erlang tgt_i686_32
cd $TOP/../test/OTP/tgt_i686_32/lib && ln -s safe* safe
cd $TOP/tgt_i686_32 && ./Install -cross -sasl .
cd $TOP && cp -f src/erl tgt_i686_32/bin/erl
cd $TOP && cp -f src/erl tgt_i686_32/erts-*/bin/erl

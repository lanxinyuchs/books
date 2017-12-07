set -e
export PROTO_VER=2.4.1
export PROTO_C_VER=0.15
export BUILD_REV=1
export RCSMW3PPDIR=/proj/5G_rcs/3pp/
export TOP=`pwd`

rm -rf $TOP/protobuf-${PROTO_VER}
rm -rf $TOP/protobuf-c-${PROTO_C_VER}
curl -L https://github.com/google/protobuf/releases/download/v$PROTO_VER/protobuf-${PROTO_VER}.tar.bz2 > protobuf-${PROTO_VER}.tar.bz2
tar xf protobuf-${PROTO_VER}.tar.bz2
cd $TOP/protobuf-${PROTO_VER}

# Native version
PREFIX=${RCSMW3PPDIR}/protobuf/protobuf-$PROTO_VER-native-$BUILD_REV
./configure --prefix $PREFIX
make
make install

cd $TOP
PREFIX_C=${RCSMW3PPDIR}/protobuf/protobuf-c-$PROTO_C_VER-native-$BUILD_REV
curl -L https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/tools/RDE_LXA119945/tools/protobuf-c/protobuf-c-${PROTO_C_VER}.tar.gz > protobuf-c-${PROTO_C_VER}.tar.gz
tar xf protobuf-c-${PROTO_C_VER}.tar.gz
cd $TOP/protobuf-c-${PROTO_C_VER}

PATH=$PATH:$PREFIX/bin CPPFLAGS=-I$PREFIX/include LDFLAGS=-L$PREFIX/lib \
    ./configure --prefix $PREFIX
make
make install


# ARM-32 Version
(
cd $TOP
source $TOP/../tools/env-ee-brcs.sh

TGT_PREFIX=${RCSMW3PPDIR}/protobuf/protobuf-$PROTO_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV
TGT_PREFIX_C=${RCSMW3PPDIR}/protobuf/protobuf-c-$PROTO_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV

rm -rf $TOP/protobuf-${PROTO_VER}
rm -rf $TOP/protobuf-c-${PROTO_C_VER}
tar xf protobuf-${PROTO_VER}.tar.bz2
tar xf protobuf-c-${PROTO_C_VER}.tar.gz

cd $TOP/protobuf-${PROTO_VER}
./configure $CONFIGURE_FLAGS --prefix=$TGT_PREFIX --with-protoc=$PREFIX/bin/protoc
make
make install

cd $TOP/protobuf-c-${PROTO_C_VER}
PATH=$PATH:$PREFIX/bin CPPFLAGS=-I$TGT_PREFIX/include LDFLAGS=-L$TGT_PREFIX/lib \
    ./configure $CONFIGURE_FLAGS --prefix=$TGT_PREFIX_C --disable-protoc
make
make install

cd $TOP
rm -f tgt_arm-wr6
ln -s $TGT_PREFIX_C tgt_arm-wr6
)

# i686_32 Version
(
cd $TOP
source $TOP/../tools/env-ee-i586.sh

TGT_PREFIX=${RCSMW3PPDIR}/protobuf/protobuf-$PROTO_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV
TGT_PREFIX_C=${RCSMW3PPDIR}/protobuf/protobuf-c-$PROTO_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV

rm -rf $TOP/protobuf-${PROTO_VER}
rm -rf $TOP/protobuf-c-${PROTO_C_VER}
tar xf protobuf-${PROTO_VER}.tar.bz2
tar xf protobuf-c-${PROTO_C_VER}.tar.gz

cd $TOP/protobuf-${PROTO_VER}
./configure $CONFIGURE_FLAGS --prefix=$TGT_PREFIX --with-protoc=$PREFIX/bin/protoc
make
make install

cd $TOP/protobuf-c-${PROTO_C_VER}
PATH=$PATH:$PREFIX/bin CPPFLAGS=-I$TGT_PREFIX/include LDFLAGS=-L$TGT_PREFIX/lib \
    ./configure $CONFIGURE_FLAGS --prefix=$TGT_PREFIX_C --disable-protoc
make
make install

cd $TOP
rm -f tgt_i686_32
ln -s $TGT_PREFIX_C tgt_i686_32
)

# x86_64 Version
(
cd $TOP
source $TOP/../tools/env-ee-vrcs64.sh

TGT_PREFIX=${RCSMW3PPDIR}/protobuf/protobuf-$PROTO_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV
TGT_PREFIX_C=${RCSMW3PPDIR}/protobuf/protobuf-c-$PROTO_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV

rm -rf $TOP/protobuf-${PROTO_VER}
rm -rf $TOP/protobuf-c-${PROTO_C_VER}
tar xf protobuf-${PROTO_VER}.tar.bz2
tar xf protobuf-c-${PROTO_C_VER}.tar.gz

cd $TOP/protobuf-${PROTO_VER}
./configure $CONFIGURE_FLAGS --prefix=$TGT_PREFIX --with-protoc=$PREFIX/bin/protoc
make
make install

cd $TOP/protobuf-c-${PROTO_C_VER}
PATH=$PATH:$PREFIX/bin CPPFLAGS=-I$TGT_PREFIX/include LDFLAGS=-L$TGT_PREFIX/lib \
    ./configure $CONFIGURE_FLAGS --prefix=$TGT_PREFIX_C --disable-protoc
make
make install

cd $TOP
rm -f tgt_x86_64
ln -s $TGT_PREFIX_C tgt_x86_64
)


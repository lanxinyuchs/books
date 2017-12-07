set -e
source ../tools/env-ee-rcssim.sh

cd $COM_MAIN
git clean -xdf
mkdir -p build-rcssim && cd build-rcssim
cmake -DSYSROOT=${SYSROOT}\
   -DBUILDFLAGS="--sysroot=${SYSROOT} ${CFLAGS}"\
   -DCMAKE_C_COMPILER="${CROSS_COMPILE}gcc"\
   -DCMAKE_CXX_COMPILER="${CROSS_COMPILE}g++"\
   -DCMAKE_TOOLCHAIN_FILE=$COM_BLOCK/rcs-sim32.cmake ..
make -j4

PREFIX=${RCSMW3PPDIR}/COM/com-$COM_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV

mkdir -p $PREFIX/mib && cd $PREFIX
cp -p $MIBS mib/
tar xf $COM_MAIN/build-rcssim/dist/com*runtime*.tar.gz
for file in *.tar.gz;
do
   tar xf $file
   rm $file
done
cp -af $COM_BLOCK/patches/* ./

cd $COM_BLOCK
rm -f tgt_i686
ln -s $PREFIX tgt_i686

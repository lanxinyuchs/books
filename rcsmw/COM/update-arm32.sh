set -xe
source ../tools/env-ee-brcs.sh

cd $COM_MAIN
mkdir -p build-arm32 && cd build-arm32
cmake\
 -DCOM_INSTALL_PREFIX=opt/com\
 -DFT_PSO_INSTALL_PREFIX=opt/com\
 -DRPM=OFF\
 -DFT=OFF\
 -DUSEFILEM=OFF\
 -DPM=OFF\
 -DPT=OFF\
 -DUT=OFF\
 -DACCESSMGMT=OFF\
 -DTLSPROXY=OFF\
 -DLDAP_MODULE=OFF\
 -DFT_IP_VERSION=4\
 -DFT_NETSNMP_VERSION=30\
 -DTARGET_ARCHITECTURE="x86_64"\
 -DSYSROOT=${OECORE_TARGET_SYSROOT}\
 -DBUILDFLAGS="--sysroot=${OECORE_TARGET_SYSROOT} ${CFLAGS}"\
 ..
make -j4

PREFIX=${RCSMW3PPDIR}/COM/com-$COM_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV
mkdir -p $PREFIX/mib && cd $PREFIX
cp -p $MIBS mib/
tar xf $COM_MAIN/build-arm32/dist/com*runtime*.tar.gz
for file in *.tar.gz;
do
   tar xf $file
   rm $file
done
cp -af $COM_BLOCK/patches/* ./

cd $COM_BLOCK
rm -f tgt_arm-wr6
ln -s $PREFIX tgt_arm-wr6

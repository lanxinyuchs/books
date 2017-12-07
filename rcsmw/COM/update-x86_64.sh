set -ex
source ../tools/env-ee-vrcs64.sh

cd $COM_MAIN
git clean -xdf
mkdir -p build-x86_64 && cd build-x86_64

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

make VERBOSE=1

PREFIX=${RCSMW3PPDIR}/COM/com-$COM_VER-$CROSS_COMPILE$OECORE_DISTRO_VERSION-$BUILD_REV

mkdir -p $PREFIX/mib && cd $PREFIX
cp -p $MIBS mib/
tar xf $COM_MAIN/build-x86_64/dist/com*runtime*.tar.gz
for file in *.tar.gz;
do
   tar xf $file
   rm $file
done
cp -af $COM_BLOCK/patches/* ./

cd $COM_BLOCK
rm -f tgt_x86_64
ln -s $PREFIX tgt_x86_64

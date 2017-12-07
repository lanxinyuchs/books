set -e
export COM_VER=7.3.1-1
export GIT_TAG=7.3.1-1
export BUILD_REV=1
export RCSMW3PPDIR=/proj/5G_rcs/3pp

git clone ssh://gerritmirror-ha.rnd.ki.sw.ericsson.se:29418/CBA/com-main
export COM_BLOCK=$(pwd)
export COM_MAIN=$COM_BLOCK/com-main

export MIBS="\
 $COM_MAIN/model/instances/ComFmFmSecurityRules_mp.xml\
 $COM_MAIN/model/instances/ComLocalAuthorizationInstances_mp.xml\
 $COM_MAIN/model/instances/ComSysMSysMSecurityRules_mp.xml\
 $COM_MAIN/model/instances/ComTopTopSecurityRules_mp.xml"

cd $COM_MAIN
git checkout $GIT_TAG
git submodule update --init
cd $COM_BLOCK

./update-arm32.sh
./update-x86_64.sh
./update-i586.sh
#./update-rcssim.sh

PREFIX=${RCSMW3PPDIR}/COM/com-$COM_VER-$BUILD_REV
EXTERNAL_COM=COM_CXA11462_4
INTERNAL_COM=COM_CXA1105460_4

COM_INCS="\
 ComCliApi*.h\
 ComMgmtSpi*.h\
 ComOamSpiTransaction*.h\
 ComMwSpiCrypto_1.h\
 ComMwSpiServiceIdentities_1.h\
 ComOamSpiModelRepository_1.h\
 ComOamSpiSecurityManagement_1.h\
 ComOamSpiServiceIdentities_1.h"

COM_MAF_INCS="\
 MafMgmtSpi*.h\
 MafMwSpiLog_1.h\
 MafMwSpiServiceIdentities_1.h\
 MafOamSpiTransaction*.h"

cd $COM_BLOCK
mkdir -p $PREFIX/$EXTERNAL_COM
cp -r emx $PREFIX/$EXTERNAL_COM

mkdir -p $PREFIX/$EXTERNAL_COM/inc
cp $COM_BLOCK/inc/*.h $PREFIX/$EXTERNAL_COM/inc
for file in $COM_INCS; do
   cp $COM_MAIN/inc/$file $PREFIX/$EXTERNAL_COM/inc
done

for file in $COM_MAF_INCS; do
   cp $COM_MAIN/3pp/src/maf/inc/$file $PREFIX/$EXTERNAL_COM/inc
done

# model
mkdir -p $PREFIX/$EXTERNAL_COM/model
cp $COM_MAIN/model/*.xml                                    $PREFIX/$EXTERNAL_COM/model
cp $COM_MAIN/model/mp.dtd                                   $PREFIX/$EXTERNAL_COM/model
cp $COM_MAIN/build-i586/tmp/installation/com/model_file_list.cfg $PREFIX/$EXTERNAL_COM/model

#INTERNAL CXA
mkdir -p $PREFIX/$INTERNAL_COM/erlinc
mkdir -p $PREFIX/$INTERNAL_COM/inc

cp $COM_MAIN/inc/* $PREFIX/$INTERNAL_COM/inc
cp $COM_MAIN/3pp/src/maf/inc/* $PREFIX/$INTERNAL_COM/inc

cd $COM_BLOCK
rm -f int_include ext_include
ln -s $PREFIX/$INTERNAL_COM int_include
ln -s $PREFIX/$EXTERNAL_COM ext_include

echo "Done!"

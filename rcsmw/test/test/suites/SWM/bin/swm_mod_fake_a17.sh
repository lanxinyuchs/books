#!/bin/bash 
## Dependencies:
## 
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014-2016 All rights reserved.
## 
## The information in this document is the property of Ericsson.
## 
## Except as specifically authorized in writing by Ericsson, the 
## receiver of this document shall keep the information contained 
## herein confidential and shall protect the same in whole or in 
## part from disclosure and dissemination to third parties.
## 
## Disclosure and disseminations to the receivers employees shall 
## only be made on a strict need to know basis.
## %CCaseCopyrightEnd%
## 
## ----------------------------------------------------------------------
## 
## Revision history:
## 
## Rev        Date       Name        What
## -----      -------    --------    ------------------------------------
## R2A/1      2014-02-19 etxivri     Created.
##                                   In ugt_s1v2_cfg.xml A15 is set to idicate which fault tha shall be 
##                                   used in the application ugt_s1v2
##                                   Use also a11_mod_ugt_1_imm.xml to increase ver of s1v2.
##                                   Otherwise the new sw will not be used.
##                                   Added use of mod S1V2_imm_classes.xml, needed when new sw is used.
##                                   Otherwise tc wil fail due to alphaT attribute is needed in to UP.
## R3A/1      2015-06-23 etxjovp     Fix bug in rev update

TMPDIR=$1

if [ "$TMPDIR" = "CXP" ]; then
  echo "CXP9021691"
   exit 0
fi
CXP_XML=$(ls $TMPDIR/tmpdir/cxp*.xml )
echo "##########################################################################"              
ROW=$(cat  $CXP_XML | grep CXP9021691 | grep DUMMY)
REV=$(echo "$ROW" | cut -f6- -d\" | cut -f1 -d\" )
echo $REV 
    N1=$( expr $(echo $REV| sed 's/[A-Z]/ /g' | awk '{print $2}' ) + 1 )
    N2=$(echo $REV| sed 's/[A-Z]/ /g' | awk '{print $1}' )
    R1=$(echo $REV| sed 's/[0-9]/ /g' | awk '{print $1}')
    R2=$(echo $REV| sed 's/[0-9]/ /g' | awk '{print $2}')
    R=$R1$N2$R2
    if [ "$(echo $N1 | wc -c | awk '{print $1}')" = "2" ]; then
	N="0$N1"
    else
	N="$N1"
    fi
    echo " R=$R N=$N "

NEW_REV="$R$N"
echo "$NEW_REV"
N=$( expr $(grep -n "$ROW" $CXP_XML | cut -f1 -d:)  )
M=$( expr $(wc -l $CXP_XML | awk '{print $1 }') - $N )
NEW_ROW=$( echo "$ROW" | sed  's/'${REV}'/'${NEW_REV}'/g')
head -$N $CXP_XML | grep -v "$ROW" > ${CXP_XML}_tmp 
echo "$NEW_ROW" >> ${CXP_XML}_tmp
tail -$M $CXP_XML >> ${CXP_XML}_tmp
chmod 666 $TMPDIR/tmpdir/cxp*.xml
mv ${CXP_XML}_tmp ${CXP_XML}
echo "##########################################################################"
FAKE_CXC=$(ls $TMPDIR/tmpdir/squashfs-root/ | grep CXC1734197 )
NAME_REV=$(ls $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/)
REV=$(echo $NAME_REV | cut -f2- -d- )
NAME=$(echo $NAME_REV | cut -f1 -d- )

echo $REV 
    N1=$( expr $(echo $REV| sed 's/[A-Z]/ /g' | awk '{print $2}' ) + 1 )
    N2=$(echo $REV| sed 's/[A-Z]/ /g' | awk '{print $1}' )
    R1=$(echo $REV| sed 's/[0-9]/ /g' | awk '{print $1}')
    R2=$(echo $REV| sed 's/[0-9]/ /g' | awk '{print $2}')
    R=$R1$N2$R2
    if [ "$(echo $N1 | wc -c | awk '{print $1}')" = "2" ]; then
	N="0$N1"
    else
	N="$N1"
    fi
    echo " R=$R N=$N "

NEW_REV="$R$N"
echo "$NEW_REV"
echo $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_${NEW_REV}
cp -r $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_REV $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}

rm  -rf $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_REV
ls -l $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/

rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/ugt_s1v2_cfg.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_imm.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/S1V2_imm_classes.xml
#rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/tgt_arm/bin/ugt_s1v2
#rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/tgt_powerpc/bin/ugt_s1v2

chmod 666 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml

cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/fault_test/a17_ugt_s1v2_cfg.xml $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/ugt_s1v2_cfg.xml

cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/fault_test/a11_mod_ugt_1_imm.xml $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_imm.xml

cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/imm_classes/S1V2_imm_classes.xml $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/S1V2_imm_classes.xml

#cp /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/out/tgt_arm/bin/ugt_s1v2 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/tgt_arm/bin/ugt_s1v2
#cp /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/out/tgt_powerpc/bin/ugt_s1v2 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/tgt_powerpc/bin/ugt_s1v2


chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml
chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/*.xml


#echo "${REV} ${NEW_REV}"
#sed  's/'${REV}'/'${NEW_REV}'/g' ${CXP_XML} > ${CXP_XML}_tmp
echo "${REV} ${NEW_REV}"
CCC=$(cat ${CXP_XML} | grep CXC1734197 | grep FAKE | grep "product name" )
NEW_CCC=$(cat ${CXP_XML} | grep CXC1734197 | grep FAKE | grep "product name" | sed  's/'${REV}'/'${NEW_REV}'/g' )
echo "${CCC} ${NEW_CCC}"

sed  's/'fake-${REV}'/'fake-${NEW_REV}'/g' ${CXP_XML} > ${CXP_XML}_tmp1

N=$( expr $(grep -n "$CCC" ${CXP_XML}_tmp1 | cut -f1 -d:)  )
M=$( expr $(wc -l ${CXP_XML}_tmp1 | awk '{print $1 }') - $N )


echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "$NEW_CCC"
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
head -$N ${CXP_XML}_tmp1 | grep -v "$CCC" > ${CXP_XML}_tmp 
echo "$NEW_CCC" >> ${CXP_XML}_tmp
tail -$M ${CXP_XML}_tmp1 >> ${CXP_XML}_tmp


echo "===================================================================="
echo " diff ${CXP_XML} ${CXP_XML}_tmp "
diff ${CXP_XML} ${CXP_XML}_tmp
echo "===================================================================="
mv ${CXP_XML}_tmp ${CXP_XML}


echo "==== Update fake app with new rew =============="
sed  's/'${REV}'/'${NEW_REV}'/g' $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app > fake.app_tmp1
diff $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app  fake.app_tmp1
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app
cp fake.app_tmp1 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app
cat $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app


chmod 444 ${CXP_XML}
ls -l $TMPDIR/tmpdir
ls -l $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/




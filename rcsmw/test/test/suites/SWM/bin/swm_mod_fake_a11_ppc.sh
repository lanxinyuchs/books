#!/bin/bash 
## Dependencies:
## 
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
## R2A/1      2014-02-12 etxjovp     Created, Dataconversion fail.
## R2A/3      2014-02-13 etxivri     Updated for all targets types.
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

rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_appm_powerpc.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_imm.xml

chmod 666 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml

cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/fault_test/a11_mod_ugt_1_appm_powerpc.xml $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_appm_powerpc.xml

cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/fault_test/a11_mod_ugt_1_imm.xml $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_imm.xml

chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml
chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/*.xml


echo "${REV} ${NEW_REV}"
sed  's/'${REV}'/'${NEW_REV}'/g' ${CXP_XML} > ${CXP_XML}_tmp


echo "===================================================================="
echo " diff ${CXP_XML} ${CXP_XML}_tmp "
diff ${CXP_XML} ${CXP_XML}_tmp
echo "===================================================================="
mv ${CXP_XML}_tmp ${CXP_XML}
chmod 444 ${CXP_XML}
ls -l $TMPDIR/tmpdir
ls -l $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/




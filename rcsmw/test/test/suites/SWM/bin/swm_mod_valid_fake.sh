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
## R2A/1      2014-05-13 etxivri     Created.
##                                   No changes will be done. This is just to 
##                                   used to modify DUMMY cxp rev to be increased.
## R3A/1      2015-06-23 etxjovp     Fix bug in rev update
## R8A/1      2016-11-21 etxkivri    Update for new prod nr on MSRCS git build.
## R8A/2      2016-11-24 etxkivri    Update for TCU in MSRCS git build

TMPDIR=$1
DUMMY=$2
if [ "$TMPDIR" = "CXP" ] && [ "$DUMMY" = "CXS101665_3" ]; then
  echo "CXP9029440_3"
   exit 0
elif [ "$TMPDIR" = "CXP" ] && [ "$DUMMY" = "CXS2010013_2" ]; then  # MSRCS DUS git build
    echo "CXP2020234"
  exit 0
elif [ "$TMPDIR" = "CXP" ] && [ "$DUMMY" = "CXS2010014_1" ]; then  # MSRCS TCU git build
    echo "CXP2020234"
  exit 0
elif [ "$TMPDIR" = "CXP" ]; then
    echo "CXP9021691"
    exit 0
fi


if [ "$DUMMY" = "CXS101665_3" ]; then
    FAKE="CXC1734197"
    DUMMY="CXP9029440_3"
elif [ "$DUMMY" = "CXS2010013_2" ]; then # MSRCS DUS git build
    ###FAKE="CXC1734197"
    # FAKE="CXC2010722_1"
    # DUMMY="CXP2020234_1"
    FAKE="CXC2010722"
    DUMMY="CXP2020234"
    MSRCS_BUILD="GIT"
elif [ "$DUMMY" = "CXS2010014_1" ]; then # MSRCS TCU git build
    FAKE="CXC2010722"
    DUMMY="CXP2020234"
    MSRCS_BUILD="GIT"
else
    DUMMY="CXP9021691"
    FAKE="CXC1734197"
fi

echo "C #######"

CXP_XML=$(ls $TMPDIR/tmpdir/cxp*.xml )
ls $TMPDIR/tmpdir/cxp*.xml
echo "1 ##########################################################################"              
ROW=$(cat  $CXP_XML | grep "$DUMMY" | grep DUMMY)

echo $DUMMY $ROW
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

echo "2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "$NEW_ROW"
echo "2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
head -$N $CXP_XML | grep -v "$ROW" > ${CXP_XML}_tmp 
echo "$NEW_ROW" >> ${CXP_XML}_tmp
tail -$M $CXP_XML >> ${CXP_XML}_tmp
chmod 666 $TMPDIR/tmpdir/cxp*.xml
mv ${CXP_XML}_tmp ${CXP_XML}


echo "Start 2.1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
# Special due to if git build then update of FAKE3 cxc version is needed due to fake path is differ. 
echo $DUMMY
#if [ "$DUMMY" = "CXP2020234_1" ]; then # MSRCS git
if [ "$MSRCS_BUILD" = "GIT" ]; then # MSRCS git
echo "Dummy is from git. speciall build is needed."
ROW=$(cat  $CXP_XML | grep "$FAKE" | grep FAKE3)
echo $FAKE $ROW
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

ls $TMPDIR/tmpdir/cxp*.xml
echo "Row: $ROW"
NEW_REV="$R$N"
echo "old rev: $REV"
echo "new rev: $NEW_REV"
N=$( expr $(grep -n "$ROW" $CXP_XML | cut -f1 -d:)  )
M=$( expr $(wc -l $CXP_XML | awk '{print $1 }') - $N )
NEW_ROW=$( echo "$ROW" | sed 's/'${REV}'/'${NEW_REV}'/g')
MOD_FAKE=$(cat  $CXP_XML | grep "$FAKE" )


head -$N $CXP_XML | grep -v "$ROW" > ${CXP_XML}_tmp 
echo "$NEW_ROW" >> ${CXP_XML}_tmp
tail -$M $CXP_XML >> ${CXP_XML}_tmp
chmod 666 $TMPDIR/tmpdir/cxp*.xml
mv ${CXP_XML}_tmp ${CXP_XML}

echo $NEW_ROW

else
    echo "Dummy is CC build"
fi
echo "End 2.1>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"



echo "3 ##########################################################################"
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

chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml
chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/*.xml
echo "#¤#¤#¤#¤#¤#¤#¤#¤"

echo "${REV} ${NEW_REV}"
CCC=$(cat ${CXP_XML} | grep $FAKE | grep FAKE | grep "product name" )
NEW_CCC=$(cat ${CXP_XML} | grep $FAKE | grep FAKE | grep "product name" | sed  's/'${REV}'/'${NEW_REV}'/g' )
echo "${CCC} ${NEW_CCC}"

sed  's/'fake-${REV}'/'fake-${NEW_REV}'/g' ${CXP_XML} > ${CXP_XML}_tmp1

N=$( expr $(grep -n "$CCC" ${CXP_XML}_tmp1 | cut -f1 -d:)  )
M=$( expr $(wc -l ${CXP_XML}_tmp1 | awk '{print $1 }') - $N )


echo "4 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "$NEW_CCC"
echo "5 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
head -$N ${CXP_XML}_tmp1 | grep -v "$CCC" > ${CXP_XML}_tmp 
echo "$NEW_CCC" >> ${CXP_XML}_tmp
tail -$M ${CXP_XML}_tmp1 >> ${CXP_XML}_tmp



echo "6 ===================================================================="
echo " diff ${CXP_XML} ${CXP_XML}_tmp "
diff ${CXP_XML} ${CXP_XML}_tmp
echo "7 ===================================================================="
mv ${CXP_XML}_tmp ${CXP_XML}

echo "8 ==== Update fake app with new rew =============="
sed  's/'${REV}'/'${NEW_REV}'/g' $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app > fake.app_tmp1
diff $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app  fake.app_tmp1
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app
cp fake.app_tmp1 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app
cat $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/fake-${NEW_REV}/ebin/fake.app


chmod 444 ${CXP_XML}
ls -l $TMPDIR/tmpdir
ls -l $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/




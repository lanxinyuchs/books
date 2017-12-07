#!/bin/bash
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
## R2A/1      2013-10-22 etxjovp     create
## R2A/3      2014-02-12 etxjovp     update for all target types
## R2A/4      2014-04-08 erarafo     Deprecation warning
## R2A/5      2014-09-30 etxivri     update to increase rev on only FAKE
##                                   Same update is made in swm_mod_valid_fake.sh
## R3A/1      2015-06-23 etxjovp     Fix bug in rev update
## R8A/1      2016-11-21 etxkivri    Update for new prod nr on MSRCS git build.
## R8A/1      2016-11-24 etxkivri    Update for new prod nr on MSRCS tcu git build.

########################################################################
#                                                                      #
#  Do not update this file. Future versions should be maintained       #
#  in $RCS_TOP/SWM/*CNX*/test/suites/bin                               #
#                                                                      #
########################################################################


TMPDIR=$1
DUMMY=$2
if [ "$TMPDIR" = "CXP" ] && [ "$DUMMY" = "CXS2010013_2" ]; then  # MSRCS DUS git build
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

CXP_XML=$(ls $TMPDIR/tmpdir/cxp*.xml )
echo "1 ##########################################################################"
#ROW=$(cat  $CXP_XML | grep CXP9021691 | grep DUMMY)
ROW=$(cat  $CXP_XML | grep "$DUMMY" | grep DUMMY)
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

echo "2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "$NEW_ROW"
echo "2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
head -$N $CXP_XML | grep -v "$ROW" > ${CXP_XML}_tmp
echo "$NEW_ROW" >> ${CXP_XML}_tmp
tail -$M $CXP_XML >> ${CXP_XML}_tmp
chmod 666 $TMPDIR/tmpdir/cxp*.xml
mv ${CXP_XML}_tmp ${CXP_XML}
echo "##########################################################################"

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
#echo "# FAKE_CXC: $FAKE_CXC"
#echo "# NAME: $NAME"
#echo "# NAME_REV: $NAME_REV"

#echo $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_${NEW_REV}
echo $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_REV
echo $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}
cp -r $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_REV $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}
#ls -l $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv

echo "# start cleanup preparation #"
rm -rf $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/$NAME_REV
ls -l $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/S0_imm_classes.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/S0_mp.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/S0instances_imm_objects.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_imm.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_2_imm.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_1_mim.xml
#rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_2_imm.xml
rm -f $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/ugt_2_mmm.xml
chmod 666 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml
echo "# end cleanup preparation #"
#cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/metadata/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/
#cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/imm_classes/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/
#cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/imm_objects/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/
#cp  /vobs/rcs/test/RCT_CRX901275/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade/mim_classes/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/

echo "# check if env is for CC or GIT "
echo "# RCT_TOP: $RCT_TOP"
Env=$(echo $RCT_TOP | grep vobs )
echo "#Env: $Env"

if [ "$Env" = "" ]; then # MSRCS git
    echo "## env is for git. cp from gith path is needed. "
    NISSE=$RCT_TOP/FAKE/test/upgrade
else
    echo "## env is clearcase. "
    NISSE=$RCT_TOP/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/test/upgrade
fi

echo "# path: $NISSE"
cp  $NISSE/metadata/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/
cp  $NISSE/imm_classes/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/
cp  $NISSE/imm_objects/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/
cp  $NISSE/mim_classes/* $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/

chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/*.xml
chmod 444 $TMPDIR/tmpdir/squashfs-root/${FAKE_CXC}/${NAME}-${NEW_REV}/priv/appdata/*.xml

echo "${REV} ${NEW_REV}"
## sed  's/'${REV}'/'${NEW_REV}'/g' ${CXP_XML} > ${CXP_XML}_tmp
#CCC=$(cat ${CXP_XML} | grep CXC1734197 | grep FAKE | grep "product name" )
CCC=$(cat ${CXP_XML} | grep FAKE | grep "product name" )
#NEW_CCC=$(cat ${CXP_XML} | grep CXC1734197 | grep FAKE | grep "product name" | sed  's/'${REV}'/'${NEW_REV}'/g' )
NEW_CCC=$(cat ${CXP_XML} | grep FAKE | grep "product name" | sed  's/'${REV}'/'${NEW_REV}'/g' )
echo "${CCC} ${NEW_CCC}"
sed  's/'fake-${REV}'/'fake-${NEW_REV}'/g' ${CXP_XML} > ${CXP_XML}_tmp1
N=$( expr $(grep -n "$CCC" ${CXP_XML}_tmp1 | cut -f1 -d:)  )
M=$( expr $(wc -l ${CXP_XML}_tmp1 | awk '{print $1 }') - $N )

echo "4 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo " $NEW_CCC"
echo "5 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
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




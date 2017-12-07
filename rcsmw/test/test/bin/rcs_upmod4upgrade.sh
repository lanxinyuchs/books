#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2012 - 2017 All rights reserved.
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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2013-10-15   etxjovp     Created
## R2A/6      2013-10-25   etxjovp     updated
## R2A/7      2013-11-20   etxjovp     add -u flag
## R2A/8      2014-01-08   etxjovp     add -S and -P flags
## R2A/12     2014-01-09   etxjovp     add simplifications
## R2A/13     2014-01-10   etxjovp     add --no-check-certificate
## R2A/14     2014-01-10   etxjovp     bug fix
## R2A/15     2014-01-13   etxjovp     bug fix
## R2A/16     2014-06-05   etxjovp     add TCU03 
## R2A/19     2014-08-28   etxjovp     Update to handle rev start with 0, ex R2A08
## R2A/20     2014-09-28   etxjovp     Update to handle DUS2
## R4A/01     2015-06-09   etxjovp     Update to handle TCU type T
## R4A/02     2015-06-23   etxjovp     fix bug in rev update
## R4A/03     2015-06-25   etxjovp     fix bug in rev update
## R6A/01     2016-05-03   etxivri     Adapt script to use zip with global-up xml in R6 (17A)
## R6A/04     2016-09-08   etxivri     Update for HAL up.
## R7A/01     2016-10-26   etxkols     Update GIT built DUS2 DUMMY CXS
## R8A/02     2016-11-21   etxkivri    Update for new prod nr on MSRCS git build.
## R8A/03     2016-11-24   etxkivri    Update for new prod nr on MSRCS tcu git build.
usage()

{
echo "
 rcs_upmod4upgrade.sh -t du|tcu -m modscript  -r CXSREV
 rcs_upmod4upgrade.sh -t du|tcu -m modscript  -r CXSREV -S yes|no
 rcs_upmod4upgrade.sh -t du|tcu -m modscript  -r CXSREV -P yes|no
 rcs_upmod4upgrade.sh -t du|tcu -c modcxp  -r CXSREV
 rcs_upmod4upgrade.sh -t du|tcu -c modcxp  -u UPURL
 rcs_upmod4upgrade.sh -t du|tcu -n newcxp  -u UPURL


-S step CXS rev (default no)
-P step CXP rev (default yes)
Ex:
rcs_upmod4upgrade.sh -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A2602
rcs_upmod4upgrade.sh -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A2602 -S yes
rcs_upmod4upgrade.sh -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A2602 -S yes -P no

rcs_upmod4upgrade.sh -t du -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A2602
rcs_upmod4upgrade.sh -t du -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A2602 -S yes
rcs_upmod4upgrade.sh -t du -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A2602 -S yes -P no
rcs_upmod4upgrade.sh -t tcu -m  /home/nisse/mod_fake.sh  -r CXS101549_3-R2A1134
rcs_upmod4upgrade.sh -t tcu -c   DUMMY-ARM_CXP9021691_3.cxp  -r CXS101549_3-R2A1134
rcs_upmod4upgrade.sh -t du -c   RCS_CXP9021221_1.cxp  -u https://rbs-g2-ci.rnd.ki.sw.ericsson.se/job/ModifyUP/8255//artifact/CXP9023001_1-wrat.tgz
rcs_upmod4upgrade.sh -t du -n   RCS_CXP9021221_1.cxp  -u https://rbs-g2-ci.rnd.ki.sw.ericsson.se/job/ModifyUP/8255//artifact/CXP9023001_1-wrat.tgz

Ex modscript

----------------------------------------------------------------------
#!/bin/bash
TMPPATH=\$1
if [ \"\$TMPPATH\" = \"CXP\" ]; then
echo CXP9021691

exit 0
fi
#####################################################################
# Implementing functions for manipulating  the upgrade package here.

#####################################################################
exit 0

---------------------------------------------------------------------
"
}


cleanup() {
	rm -rf sw.ar 
	rm -rf sqfs_new.img
	rm -rf squashfs-root 
	cd ${TMPDIR}/
	rm -rf $TMPDIR/tmpdir
	rm -f $UP_cxs
}
progname="`basename $0`"
PPC_cxp='RCS_CXP9021221_1.cxp'
ARM_cxp='RCS-ARM_CXP9021221_3.cxp'

CIRESULT="/proj/webdocs/rbs-rde-ci/root/ciresults"

RCS_T_CONTAINER_URL="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T_CXS101549_6/doc/19010/RCP-T_CXS101549_6.zip@@/"
HAL_RCS_T_CONTAINER_URL="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCS-T_CXS101549_9/doc/19010/RCS-T_CXS101549_9.zip@@/"
RCS_DUS2_CONTAINER_URL="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.zip@@/"
HAL_RCS_DUS2_CONTAINER_URL="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCS-DUS2_CXS101549_8/doc/19010/RCS-DUS2_CXS101549_8.zip@@/"
RCS_BRCS_CONTAINER_URL="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/BRCS_CSX10179_3/BRCS-UP_CXS101665_3/doc/19010/BRCS-UP_CXS101665_3.zip@@/"
MOD_SCRIPT=""
MODIFY_CXP=""
REF_PWD=$PWD
UP=""
######################################################################################
#   MAIN
######################################################################################
while getopts ht:c:r:u:t:m:n:f:s:S:P:N: option
do
	case "$option"
	in
                t) TYPE=$OPTARG #  
                   ;;
                N) NODE=$OPTARG #  
                   ;;
                c) MODIFY_CXP=$OPTARG #  
                   ;;
                n) NEW_CXP=$OPTARG #  
                   ;;
                r) RCS_REV=$OPTARG #  
                   ;;  
                u) UP=$OPTARG #  
                   ;; 
                f) UP_PATH=$OPTARG #  
                   ;;   
                m) MOD_SCRIPT=$OPTARG #  
                   ;; 
                s) STOP=$OPTARG #  
                   ;;   
                S) STEP_CXS_REV=$OPTARG #  
                   ;;  
                P) STEP_CXP_REV=$OPTARG #  
                   ;;                                 
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done

if [ "$#" = "0" ]; then
	usage
	exit
fi

if [ "$MOD_SCRIPT" != "" ] &&  [ "$MODIFY_CXP" = "" ]; then
     if [ "$MOD_SCRIPT" != "undefined" ]; then
	MODIFY_CXP=$($MOD_SCRIPT CXP $(echo $RCS_REV | cut -f1 -d-))
	echo $MODIFY_CXP 
     else
        MODIFY_CXP="CXP9021221"
     fi
fi

if [ "$USER" = "rcsci1" ]; then
	TMPDIR="$PWD/rcs_mod"
else
      mkdir -p /tmp/$USER/
      if [ "$NODE" = "" ]; then
	TMPDIR="/tmp/$USER/rcs_mod"
      else
        mkdir -p /tmp/$USER/$NODE
        TMPDIR="/tmp/$USER/$NODE/rcs_mod"
      fi
fi

mkdir -p $TMPDIR
rm -rf $TMPDIR/*
mkdir -p $TMPDIR/tmpdir

cd $TMPDIR
TFTPDir=$TMPDIR
#GIT_PRODNO_REV=""
if [ "$UP" = "" ]; then
if [ "$TYPE" = "tcu" ]; then
	UP=${RCS_ARM_CONTAINER_URL}${RCS_REV}
elif [ "$TYPE" = "du" ]; then
	UP=${RCS_CONTAINER_URL}${RCS_REV}    
elif [ "$TYPE" = "sim" ]; then
	UP=${RCS_SIM_CONTAINER_URL}${RCS_REV}   
elif [ "$TYPE" = "TCU03" ]; then
	UP=${RCS_TCU03_CONTAINER_URL}${RCS_REV}
elif [ "$TYPE" = "T" ]; then
	UP=${RCS_T_CONTAINER_URL}${RCS_REV}
elif [ "$TYPE" = "DUS2" ]; then
	UP=${RCS_TCU03_CONTAINER_URL}${RCS_REV}

elif [ "$(echo $RCS_REV | cut -f1 -d-)" = "CXS101665_3" ]; then
   UP=${RCS_BRCS_CONTAINER_URL}${RCS_REV} 

#elif [ "$(echo $RCS_REV | cut -f1 -d-)" = "CXS101698_6" ]; then
elif [ "$(echo $RCS_REV | cut -f1 -d-)" = "CXS2010013_2" ]; then # MSRCS DUS git
    ProdNo=`echo $RCS_REV | cut -f1 -d-`
    Revision=`echo $RCS_REV | cut -f2 -d-`
    GIT_PRODNO_REV="${ProdNo}-${Revision}"
    UP="https://arm001-eiffel002.rnd.ki.sw.ericsson.se:8443/nexus/content/repositories/releases/com/ericsson/RCP-DUS2_${ProdNo}/${Revision}/RCP-DUS2_${ProdNo}-${Revision}.cxs"
elif [ "$(echo $RCS_REV | cut -f1 -d-)" = "CXS2010014_1" ]; then # MSRCS TCU git
    ProdNo=`echo $RCS_REV | cut -f1 -d-`
    Revision=`echo $RCS_REV | cut -f2 -d-`
    GIT_PRODNO_REV="${ProdNo}-${Revision}"
    UP="https://arm001-eiffel002.rnd.ki.sw.ericsson.se:8443/nexus/content/repositories/releases/com/ericsson/RCS-T_${ProdNo}/${Revision}/RCS-T_${ProdNo}-${Revision}.cxs"
else
        T=$(echo $RCS_REV | cut -f2- -d_ | cut -f1 -d- )
        if [ "$T" = "1" ]; then
           TYPE="du"
        elif [ "$T" = "2" ]; then
           TYPE="sim"
        elif [ "$T" = "3" ]; then  
           TYPE="tcu"
        elif [ "$T" = "4" ]; then  
           TYPE="TCU03"
        elif [ "$T" = "6" ]; then  
           TYPE="T"
        elif [ "$T" = "5" ]; then  
           TYPE="DUS2"
        elif [ "$T" = "8" ]; then  
           TYPE="HAL_DUS"
        elif [ "$T" = "9" ]; then  
           TYPE="HAL_TCU"
        else
	echo "$TYPE not exist "
	exit
        fi
    if [ "$TYPE" = "tcu" ]; then
	UP=${RCS_ARM_CONTAINER_URL}${RCS_REV}
    elif [ "$TYPE" = "du" ]; then
	UP=${RCS_CONTAINER_URL}${RCS_REV} 
    elif [ "$TYPE" = "sim" ]; then
	UP=${RCS_SIM_CONTAINER_URL}${RCS_REV}   
    elif [ "$TYPE" = "TCU03" ]; then
	UP=${RCS_TCU03_CONTAINER_URL}${RCS_REV} 
    elif [ "$TYPE" = "T" ]; then
	UP=${RCS_T_CONTAINER_URL}${RCS_REV} 
    elif [ "$TYPE" = "DUS2" ]; then
	UP=${RCS_DUS2_CONTAINER_URL}${RCS_REV} 
    elif [ "$TYPE" = "HAL_DUS" ]; then
	UP=${HAL_RCS_DUS2_CONTAINER_URL}${RCS_REV} 
    elif [ "$TYPE" = "HAL_TCU" ]; then
	UP=${HAL_RCS_T_CONTAINER_URL}${RCS_REV} 
    fi 
fi 



fi


if [ "$UP_PATH" = "" ]; then
    if [ "$GIT_PRODNO_REV" = "" ]; then
	UP_cxs=$(basename $UP)
    else
	UP_cxs=$GIT_PRODNO_REV
    fi
    cd ${TFTPDir}
    echo "${progname}: Fetching UP from $UP to $TFTPDir  "
    echo "wget -nd -q --no-proxy   --no-check-certificate --output-document  $UP_cxs $UP -t 5 -T 120 "
    wget -nd -q --no-proxy   --no-check-certificate --output-document  $UP_cxs $UP -t 5 -T 120 
else
echo "${progname}: Fetching UP from $UP_PATH to $TFTPDir   "
  cp $UP_PATH .
  UP_cxs=$(basename $UP_PATH)
fi 
echo "##########################################################################" 
ls -lrt
echo "##########################################################################" 
if file $UP_cxs | grep "Zip archive data" > /dev/null; then
    echo "${progname}: unzip $UP_cxs "
#tar -xzf  $UP_cxs
    unzip $UP_cxs
else
    tar -xzf  $UP_cxs
fi
rm -f $UP_cxs
ls -lrt 

##################################################################################
if [ "$NEW_CXP" != ""  ]; then 
  if  [ -f $NEW_CXP ]; then

   STRING=$( basename $NEW_CXP )
   XML_CXS=$(ls *.xml )
   echo $XML_CXS
    if [ "$( echo $STRING | grep R )" != "" ]; then
    CXP_REV_IN_CXS=$(cat *.xml | grep $(echo $STRING | tail -1 | sed 's/CXP/# CXP/' | cut -f1 -d. | awk '{print $2}' | cut -f1 -d- ) )
    elif [ "$( echo $STRING | grep CXP )" != "" ]; then
    CXP_REV_IN_CXS=$(cat *.xml | grep $(echo $STRING | tail -1 | sed 's/CXP/# CXP/' | cut -f1 -d. | awk '{print $2}') )
    else
    CXP_REV_IN_CXS=$(cat *.xml | grep $(echo $STRING  | tail -1 | cut -f1 -d.))
    fi 
echo "##########################################################################" 
    cd ${TMPDIR}/tmpdir
    ls -l 
    #tar -xzf  $TMPDIR/$( basename $NEW_CXP )
    tar -xzf  $NEW_CXP
echo "##########################################################################" 
    if [ "$( echo $STRING | grep R )" != "" ]; then
       NEW_CXP_REV=$(cat *.xml | grep $(echo $STRING | tail -1 | sed 's/CXP/# CXP/' | cut -f1 -d. | awk '{print $2}' | cut -f1 -d- )  )
    elif [ "$( echo $STRING | grep CXP )" != "" ]; then
       NEW_CXP_REV=$(cat *.xml | grep $(echo $STRING | tail -1 | sed 's/CXP/# CXP/' | cut -f1 -d. | awk '{print $2}') )
    
    else
       NEW_CXP_REV=$(cat *.xml | grep $(echo $STRING | tail -1  | cut -f1 -d. ))
    fi
    echo "##########################################################################" 
    echo ">$CXP_REV_IN_CXS"
    echo ">$NEW_CXP_REV"
    echo "##########################################################################" 
    N=$(grep -n "${CXP_REV_IN_CXS}" ${TMPDIR}/${XML_CXS}  | tail -1 | cut -f1 -d: )
    T=$(wc -l ${TMPDIR}/${XML_CXS} | awk '{print $1}' ) 
    echo $N $T
    head -$( expr $N - 1 ) ${TMPDIR}/${XML_CXS} > ${TMPDIR}/${XML_CXS}_tmp

    echo ${NEW_CXP_REV} >> ${TMPDIR}/${XML_CXS}_tmp


    tail -$( expr $T - $N ) ${TMPDIR}/${XML_CXS} >> ${TMPDIR}/${XML_CXS}_tmp
    echo "##########################################################################" 
    diff ${TMPDIR}/${XML_CXS}_tmp ${TMPDIR}/${XML_CXS}
    mv ${TMPDIR}/${XML_CXS}_tmp ${TMPDIR}/${XML_CXS}



    echo "##########################################################################" 
    cat ${TMPDIR}/${XML_CXS}
   echo "##########################################################################" 

    
#sleep 10

if [ "$STOP" != "" ]; then
	echo -n "${progname}: 'Make your changes in the file structure ${TMPDIR} "
	echo -n "When you are finished with the updates press return "
                read answer
                case $answer in
		    yes) ;;
		    *)  ;;
                esac
fi


cd ${TMPDIR}/

rm -rf ${TMPDIR}/tmpdir
rm -rf ${TMPDIR}/$STRING
ls -l 
cp $NEW_CXP ${TMPDIR}/
#echo "${progname}: tar -czf $TMPDIR/$UP_cxs $(ls)"
	#tar -czf $TMPDIR/$UP_cxs $(ls)
echo "${progname}: zip $TMPDIR/$UP_cxs $(ls)"
	zip $TMPDIR/$UP_cxs $(ls)
	echo "${progname}: Remove temporary files"
	rm -rf *.cxp
	rm -rf *.xml
else
  echo " $NEW_CXP not exist "
  exit
fi

else
if [ "$STEP_CXS_REV" = "yes" ]; then
    echo "##########################################################################" 
    CXS_NAME=$(echo $UP_cxs | cut -f1 -d-)
    CXS_REV=$(echo $UP_cxs | cut -f2- -d-)
echo " CXS_NAME=$CXS_NAME CXS_REV=$CXS_REV "

    XML_CXS=$(ls *.xml )
    echo $XML_CXS
    



#############################################################################
    N1=$( expr $(echo $CXS_REV| sed 's/[A-Z]/ /g' | awk '{print $2}' ) + 1 )
    N2=$(echo $CXS_REV| sed 's/[A-Z]/ /g' | awk '{print $1}' )
    R1=$(echo $CXS_REV| sed 's/[0-9]/ /g' | awk '{print $1}')
    R2=$(echo $CXS_REV| sed 's/[0-9]/ /g' | awk '{print $2}')
    R=$R1$N2$R2
    if [ "$(echo $N1 | wc -c | awk '{print $1}')" = "2" ]; then
	N="0$N1"
    else
	N="$N1"
    fi
#############################################################################




    echo " X=$X R=$R N=$N "
    NEW_CXS_REV="$R$N"
    echo " NEW_CXS_REV=$NEW_CXS_REV "
    NEW_CXS_REV_STRING=$(grep "${CXS_NAME}" ${TMPDIR}/${XML_CXS}  | tail -1 | sed 's/'${CXS_REV}'/'${NEW_CXS_REV}'/')
    N=$(grep -n "${CXS_NAME}" ${TMPDIR}/${XML_CXS}  | tail -1 | cut -f1 -d: )
    T=$(wc -l ${TMPDIR}/${XML_CXS} | awk '{print $1}' ) 
    echo $N $T
    head -$( expr $N - 1 ) ${TMPDIR}/${XML_CXS} > ${TMPDIR}/${XML_CXS}_tmp

    echo ${NEW_CXS_REV_STRING} >> ${TMPDIR}/${XML_CXS}_tmp


    tail -$( expr $T - $N ) ${TMPDIR}/${XML_CXS} >> ${TMPDIR}/${XML_CXS}_tmp
    echo "##########################################################################" 
    diff ${TMPDIR}/${XML_CXS}_tmp ${TMPDIR}/${XML_CXS}
    mv ${TMPDIR}/${XML_CXS}_tmp ${TMPDIR}/${XML_CXS}

    STEP_UP_cxs=${CXS_NAME}_${NEW_CXS_REV}
    echo "STEP_UP_cxs=$STEP_UP_cxs"
    echo "##########################################################################" 
    cat ${TMPDIR}/${XML_CXS}
   echo "##########################################################################" 
fi
##################################################################################
MODIFY_CXP=$(ls $TMPDIR/ | grep $MODIFY_CXP | grep cxp )

if [ $(tar -tf $TMPDIR/$MODIFY_CXP | grep sw.ar ) ]; then
	echo "${progname}: SQUASHFS $MODIFY_CXP detetcted"
        cd ${TMPDIR}/tmpdir
        ls -l 
        tar -xzf  $TMPDIR/$MODIFY_CXP
        ls -l   
        ar -x sw.ar sqfs.img 
	echo "${progname}: Testing for command unsquashfs"
	if [ $(command -v unsquashfs ) ]; then
		echo "${progname}: Found `command -v unsquashfs`"
		cd $TMPDIR/tmpdir
         	unsquashfs -n sqfs.img > /dev/null
	else
	    	echo "${progname}: Not found, trying hardcoded afs path: /app/rbs/wrtools/tools-sdk-20130220/usr/sbin/unsquashfs"
	    	cd $TMPDIR/tmpdir
            	/app/rbs/wrtools/tools-sdk-20130220/usr/sbin/unsquashfs -n sqfs.img > /dev/null
	fi
	echo "${progname}: Extracting files necessary for installation from sqfs.img"
	echo "##########################################################################"
	echo "mofify files "
	ls -l 
	rm -f sqfs.img 
	rm -f sw.ar
	rm -f ${TMPDIR}/tmpdir/squashfs-root/*xml
	ls -l ${TMPDIR}/tmpdir/squashfs-root
	XML=$(ls $TMPDIR/*xml)
	echo "modify $XML"

	if [ "$MOD_SCRIPT" = "" ]; then
	echo -n "${progname}: 'Make your changes in the file structure ${TMPDIR} "
	echo -n "When you are finished with the updates press return "
                read answer
                case $answer in
		    yes) ;;
		    *)  ;;
                esac
	rm -f $XML~
	ls -l $XML
	else
		echo "##########################################################################"
		echo $MODIFY_CXP
               if [ "$STEP_CXP_REV" != "no" ]; then
		MODIFY_CXP_CXP=$(echo $MODIFY_CXP | cut -f2- -d_ | cut -f1 -d. )
		MODIFY_CXP_NAME=$(echo $MODIFY_CXP | cut -f1 -d_ )
		ROW="<$(cat  $XML | grep $MODIFY_CXP_CXP | grep $MODIFY_CXP_NAME | tail -1 | cut -f2- -d\<)" # ta sista raden för att få endast en label. samt ta bort tab blanksteg
		REV=$(echo "$ROW" | cut -f6- -d\" | cut -f1 -d\" )
cat  $XML | grep $MODIFY_CXP_CXP | grep $MODIFY_CXP_NAME | cut -f2- -d\<

#############################################################################
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
#############################################################################

cp $XML org
		NEW_REV="$R$N"
NLIST=$(grep -n "$ROW" $XML | cut -f1 -d:)
echo $XML
echo $ROW
echo $NLIST
echo $UP_cxs
for N in $NLIST 
do
		#N=$( expr $(grep -n "$ROW" $XML | cut -f1 -d:)  )

		M=$( expr $(wc -l $XML | awk '{print $1 }') - $N )
		NEW_ROW=$( echo "$ROW" | sed  's/'${REV}'/'${NEW_REV}'/g')
		head -$N $XML | grep -v "$ROW" > ${XML}_tmp 
		echo "$NEW_ROW" >> ${XML}_tmp
	        tail -$M $XML >> ${XML}_tmp
echo "$ROW"

		
		echo "##########################################################################"
		echo " diff ${XML}_tmp $XML "
		diff ${XML}_tmp $XML
		echo "##########################################################################"
		chmod 666 $XML
		mv ${XML}_tmp $XML
		chmod 444 $XML
done

		diff ${XML} org
                fi
		echo "${progname}: $MOD_SCRIPT $TMPDIR"
                if [ "$MOD_SCRIPT" != "undefined" ]; then
	           $MOD_SCRIPT $TMPDIR $(echo $RCS_REV | cut -f1 -d-)
                
                fi
	fi 
	echo "##########################################################################"
	echo "${progname}: create_sig.sh -f $(basename $(ls cxp*.xml)) -o $MODIFY_CXP -v squashfs-root"
	create_sig.sh -f $(basename $(ls cxp*.xml)) -o $MODIFY_CXP -v squashfs-root
	cd $TMPDIR
	rm -f ${TMPDIR}/$MODIFY_CXP
	cp ${TMPDIR}/tmpdir/$MODIFY_CXP ${TMPDIR}/$MODIFY_CXP
	cd ${TMPDIR}/
	rm -rf $TMPDIR/tmpdir
	echo "##########################################################################"
	ls -l
	echo "##########################################################################"
        if [ "$STEP_CXS_REV" = "yes" ]; then
           UP_cxs=$STEP_UP_cxs
        fi 
	#echo "${progname}: tar -czf $TMPDIR/$UP_cxs $(ls)"
	#tar -czf $TMPDIR/$UP_cxs $(ls)
        echo "${progname}: zip $TMPDIR/$UP_cxs $(ls)"
	zip $TMPDIR/$UP_cxs $(ls)
	echo "${progname}: Remove temporary files"
	rm -rf *.cxp
	rm -rf *.xml
fi
##################################################################################
fi
pwd
ls -l $TMPDIR/
if [ "$USER" = "rcsci1" ]; then   
   	cp $TMPDIR/${UP_cxs}.zip $REF_PWD/$UP_cxs
        echo $REF_PWD/$UP_cxs
else
        mv $TMPDIR/${UP_cxs}.zip $TMPDIR/$UP_cxs
        echo $TMPDIR/$UP_cxs
fi




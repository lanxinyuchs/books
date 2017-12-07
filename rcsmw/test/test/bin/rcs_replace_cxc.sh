#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2012 - 2014 All rights reserved.
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
## -          2013-12-17   etxjovp     Created
## R2A/2      2013-12-20   etxjovp     Add suport for xml update
## R2A/5      2014-01-10   etxjovp     add --no-check-certificate
## R2A/5      2014-02-12   etxjovp     added support for updating multiple blocks
## R2A/6      2014-02-25   etxjovp     change UP_cxs to RCP-<xxx>_CXS101549_<y>
## R2A/9      2014-08-27   etxjovp     added support for RCT
usage()

{
echo "
 rcs_replace_cxc.sh -s CXS -b BLOCK_PATH -x CNX -t TYPE
 TYPE = (1 2 3 > ppc sim arm)
Ex:
rcs_replace_cxc.sh -s CXS101549_2-R2A2684  -b /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/LOG -x CNX9012616-R2A74 -t 1

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





RCS_CONTAINER_URL="https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs@@"
RCS_SIM_CONTAINER_URL="https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-SIM_CXS101549_2/doc/19010/RCP-SIM_CXS101549_2.cxs@@"
RCS_ARM_CONTAINER_URL="https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-ARM_CXS101549_3/doc/19010/RCP-ARM_CXS101549_3.cxs@@"

RCS_CXS="CXS101549_1"
RCS_SIM_CXS="CXS101549_2"
RCS_ARM_CXS="CXS101549_3"
REF_PWD=$PWD

######################################################################################
#   MAIN
######################################################################################
while getopts ht:s:b:x:t:p: option
do
	case "$option"
	in
                s) CXS=$OPTARG #  
                   ;;
                b) BLOCK_PATH_LIST=$OPTARG #  
                   ;;  
                x) CNX_LIST=$OPTARG #  
                   ;;  
                t) TYPE=$OPTARG #  
                   ;;  
                p) GET_RCS_CXP=$OPTARG #  
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




if [ "$USER" = "rcsci1" ]; then
	TMPDIR="$PWD/rcs_mod"
else
	mkdir -p /tmp/$USER/
	TMPDIR="/tmp/$USER/rcs_mod"
fi

case "$(echo $CXS | cut -f1 -d-)"
  in
      $RCS_CXS ) 
         CXS_URL=$RCS_CONTAINER_URL/$CXS                    
      ;;
      $RCS_ARM_CXS ) 
         CXS_URL=$RCS_ARM_CONTAINER_URL/$CXS                     
      ;;
      $RCS_SIM_CXS ) 
         CXS_URL=$RCS_SIM_CONTAINER_URL/$CXS                     
      ;;                
esac


mkdir -p $TMPDIR
rm -rf $TMPDIR/*


cd $TMPDIR
TFTPDir=$TMPDIR
#UP_cxs=$(basename $CXS_URL)
UP_cxs=$(basename $(dirname $CXS_URL) | cut -f1 -d@ )
cd ${TFTPDir}
echo "${progname}: Fetching UP from $CXS_URL to $TFTPDir"
wget -nd -q --no-proxy --no-check-certificate --output-document $UP_cxs $CXS_URL -t 5 -T 120
if [ "$BLOCK_PATH_LIST" != "/vobs/rcs/test" ]; then
echo "${progname}: $(ls -lrt $UP_cxs)"
echo "${progname}: unzip $UP_cxs "
tar -xzf  $UP_cxs
rm -f $UP_cxs
#######################################################################################################
M="1"
for BLOCK_PATH in $(echo $BLOCK_PATH_LIST | sed 's/:/ /g')
do
  CNX=$(echo $CNX_LIST | cut -f$M -d: )
  M=$( expr $M + 1 )
  #######################################################################################################
  B=$(basename $BLOCK_PATH)
  CXP_LIST=$(ls -d /vobs/rcs/delivery/RCP_CSX10179_1/*CXS*_${TYPE}/*CXP* | grep 'RCS\|DUMMY' | grep -v FACTORY )
  TO_DO=""
  for i in $CXP_LIST
  do
    #echo $i
    cd $i
    CXC_LIST=$(clearmake getcxclist)
    for j in $CXC_LIST
    do
      if [ "$(echo $(dirname $j) | grep $B$ )" != "" ]; then
         CXP=$(basename $i.cxp)
         TO_DO="$j;$CXP $TO_DO"
      fi
    done 
  done
  for k in $TO_DO
  do
    mkdir -p $TMPDIR/tmpdir
    CXC_PATH=$(echo $k | cut -f1 -d\;)
    CXP=$(echo $k | cut -f2- -d\;)
    CXC=$(basename $CXC_PATH)
    if [ $(tar -tf $TMPDIR/$CXP | grep sw.ar ) ]; then
	echo "${progname}: SQUASHFS $CXP detetcted"
        cd ${TMPDIR}/tmpdir
        echo "${progname}: tar -xzf  $TMPDIR/$CXP "
        tar -xzf  $TMPDIR/$CXP
        echo "${progname}: ar -x sw.ar sqfs.img "
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
	rm -f sqfs.img 
	rm -f sw.ar
	rm -f ${TMPDIR}/tmpdir/squashfs-root/*xml
	XML=$(ls $TMPDIR/*xml)
        CXC_DIR=$(basename $CXC_PATH)        
        echo "${progname}: Fetching DIR from $CXC_PATH to $TFTPDir/tmpdir"       
        CXN_REV=$(echo $CNX | cut -f2- -d-)        
        for i in $(ls ${TMPDIR}/tmpdir/squashfs-root/$CXC/ | cut -f1 -d- )
          do
             ii=$(ls ${TMPDIR}/tmpdir/squashfs-root/$CXC/ | grep "${i}-")                       
             if [ -d ${TMPDIR}/tmpdir/squashfs-root/$CXC/${i}-${CXN_REV} ]; then
                echo "${progname}: Exist ${TMPDIR}/tmpdir/squashfs-root/$CXC/${i}-${CXN_REV} "
             else
             echo "${progname}: Make dir ${TMPDIR}/tmpdir/squashfs-root/$CXC/${i}-${CXN_REV} "
             mkdir  ${TMPDIR}/tmpdir/squashfs-root/$CXC/${i}-${CXN_REV}        
             for j in $(ls ${TMPDIR}/tmpdir/squashfs-root/$CXC/$ii/)
             do
                echo "${progname}: Copy  $CXC_PATH/$i/$j "
                echo "${progname}: To    ${TMPDIR}/tmpdir/squashfs-root/$CXC/${i}-${CXN_REV}/"
                cp -r $CXC_PATH/$i/$j ${TMPDIR}/tmpdir/squashfs-root/$CXC/${i}-${CXN_REV}/
            done
             echo "${progname}: Remove   ${TMPDIR}/tmpdir/squashfs-root/$CXC/$ii "
             rm -rf ${TMPDIR}/tmpdir/squashfs-root/$CXC/$ii
            fi
          done

    fi 
    BLOCK=$B
    NEW_REV=$CXN_REV
    CXP_XML=$(ls ${TMPDIR}/tmpdir/*xml)
    echo $BLOCK
    CXC_STRING=$(cat $CXP_XML | grep -n "\"${BLOCK}\"\|\"${BLOCK}${TYPE}\"" )
    ROW_CXC=$( echo $CXC_STRING | cut -f1 -d:)
    CXC=$( echo $CXC_STRING | awk '{print $4}' | cut -f2- -d\" | cut -f1 -d\")
    OLD_REV=$(echo $CXC_STRING | awk '{print $5}' | cut -f2- -d\" | cut -f1 -d\")
    ROW_LIST=$(cat $CXP_XML | grep -n $CXC | cut -f1 -d: )
    MAX_ROW=$(wc -l $CXP_XML | awk '{print $1}')
    rm -f tmp.xml
    touch tmp.xml
    OLD_i="0"

    echo "${progname}: Update $CXP_XML"
    for i in $ROW_LIST
    do 
      head -$( expr $i - 1 ) $CXP_XML | tail -$( expr $i - $( expr $OLD_i + 1 ) ) >> tmp.xml
      OLD_i=$i
      echo "${progname}: Change ${OLD_REV} to ${NEW_REV} in $(head -$i $CXP_XML | tail -1)"
      head -$i $CXP_XML | tail -1 | sed  's/'${OLD_REV}'/'${NEW_REV}'/g' >> tmp.xml
    done
    head -$MAX_ROW $CXP_XML | tail -$( expr $MAX_ROW - $OLD_i   ) >> tmp.xml
    echo "${progname}: Diff $CXP_XML tmp.xml "
    diff $CXP_XML tmp.xml
    echo "${progname}: Copy tmp.xml to $CXP_XML  "
    cp tmp.xml $CXP_XML
    echo "${progname}: create_sig.sh -f $(basename $(ls cxp*.xml)) -o $CXP -v squashfs-root"
    create_sig.sh -f $(basename $(ls cxp*.xml)) -o $CXP -v squashfs-root
    cd $TMPDIR
    rm -f ${TMPDIR}/$CXP
    cp ${TMPDIR}/tmpdir/$CXP ${TMPDIR}/$CXP
    cd ${TMPDIR}/
    rm -rf $TMPDIR/tmpdir
  done
done
######################################################################################################
   if [ "$GET_RCS_CXP" != "" ]; then
        REV=$(cat *.xml | grep $GET_RCS_CXP  | cut -f6 -d\")
        NAME=$(cat *.xml | grep $GET_RCS_CXP  | cut -f4 -d\")
        CXP=$(ls | grep $GET_RCS_CXP)
        cp $CXP ${NAME}-${REV}
        #CXP9021221_1-R2A3107
    fi
	echo "${progname}: tar -czf $TMPDIR/$UP_cxs $(ls)"
	tar -czf $TMPDIR/$UP_cxs $(ls)
	echo "${progname}: Remove temporary files"
	rm -rf *.cxp
	rm -rf *.xml
        ls -l
        ls  $TMPDIR/*
fi



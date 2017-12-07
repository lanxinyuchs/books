#!/bin/bash


set -x



###################################################################################
spec_list()
{
if [ "${SPEC_LIST}" != "" ]; then
    if [ -f ${SPEC_LIST}  ]; then
      SPEC_LIST_NO=$( ci_data.sh -f $SPEC_LIST -a read -i 0 -p 2 | sed 's/,/ /g')
      SPEC_LIST_1=$SPEC_LIST
    else
      SPEC_LIST_NO=$( echo $SPEC_LIST | sed 's/,/ /g')
      SPEC_LIST_1=$CI_DATA
    fi
    echo "SPEC_LIST_NO= $SPEC_LIST_NO "
    for NO in $SPEC_LIST_NO ; do
      VAR_TMP=$(ci_data.sh -f $SPEC_LIST_1 -a read -i $NO -p 3  )
      echo "VAR_TMP $VAR_TMP "
      VAR=""
       for V in $VAR_TMP ; do
         if [ "$(echo $V | grep ">" | grep "<" | grep ":" )" = "" ]; then
              VAR="$VAR $V" 
            else
              V3=$(echo $V | cut -f2- -d\< | cut -f1 -d: )
              V4=$(echo $V | cut -f2- -d: | cut -f1 -d\> )
              V5=$(ci_data.sh -f $CI_DATA -a read -i $V3 -p $V4 )
              VAR="$VAR $V5" 
            fi  
      done
echo "VAR $VAR "
      TMP_PROP=$(ci_data.sh -f $SPEC_LIST_1 -a read -i $NO -p 4 )
      for CONFIG in $(echo $TMP_PROP | sed 's/,/ /g' ); do
            if [ "$(echo $CONFIG | grep ">" | grep "<" | grep ":" )" = "" ]; then
              echo "$CONFIG"  >> ${WORKSPACE}/tmp_prop  
            else
                V1=$(echo $CONFIG | cut -f1 -d= )
                V2=$(echo $CONFIG | cut -f2- -d= )
                V3=$(echo $V2 | cut -f2- -d\< | cut -f1 -d: )
                V4=$(echo $V2 | cut -f2- -d: | cut -f1 -d\> )
                V5=$(ci_data.sh -f $CI_DATA -a read -i $V3 -p $V4 )
                echo "${V1}=${V5}"  >> ${WORKSPACE}/tmp_prop
            fi  
      done  
      gitWrapper.sh -P tools/bin/dci_create_properties.sh -D ${WORKSPACE} -d $VAR
      rm -f ${WORKSPACE}/tmp_prop
    done
fi
if [ "${FULL}" != "" ]; then
for CONFIG  in $CONFIG_LIST ; do
      T=$(echo "$CONFIG" | cut -f2- -d\; )
      if [ "$T" = "" ] || [ "$T" = "stp_config" ]; then
        echo "$CONFIG" | cut -f1 -d\; >> ${WORKSPACE}/info.txt
      fi
 done
fi



}
###################################################################################
dumy()
{
if [ "${SUITES_LIST}" != "" ]; then
 
    LABEL_HEAD=$(echo ${LABEL} | cut -f1 -d: )
    LABEL_TAIL=$(echo ${LABEL} | sed 's/:/ /' | awk '{print $2}' )
    if [ "$LABEL_TAIL" = "" ]; then
      LABEL_TAIL=no
    fi
    UG=false
    if [ "$FSW_ID" != "" ]; then
      UG=true
      FSW=$(ci_data.sh -f $CI_DATA -a read -i $FSW_ID -p 12  )
      PREP_UP=$FSW
      prep
      echo "SWLabel_UpgradeFrom:$FSW" >> ${WORKSPACE}/prep4TestInfo.txt
    fi
    VAR="${LABEL_HEAD} ${MAXTIME} ${TEST_ACTIVITY_TYPE} $UG ${LABEL_TAIL} SW ${SUITES_LIST} $PT no"
    gitWrapper.sh -P tools/bin/dci_create_send_testspecs.sh -D ${WORKSPACE} -d $VAR 
    rm -f ${WORKSPACE}/tmp_prop
    cp -f *spec_* ${WORKSPACE}/
fi
}
###################################################################################
node_from()
{
if [ "$FROM_LIST" != "" ]; then
  UG=true
  UP_ID_LIST=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 13 | sed 's/>,/> /g')
  echo "SWLabel_UpgradeFrom:noo" >> ${WORKSPACE}/prep4TestInfo.txt
  for i in $FROM_LIST ; do    
    UP_ID=$(echo $i | cut -f2 -d\< | cut -f1 -d\; )
    FROM_UP=$( ci_data.sh -f $CI_DATA -a read -i $UP_ID -p 12  | sed 's/,/ /g' )
    IB_Flow=$( ci_data.sh -f $CI_DATA -a read -i $UP_ID -p 11  )
    TN=$( ci_data.sh -f $CI_DATA -a read -i $UP_ID -p 7 )
    TRACK_N=$(basename $IB_Flow)
    for CONFIG in $(echo $CONFIG_LIST | sed 's/,/ /g' ); do
      T=$(echo "$CONFIG" | cut -f2- -d\; )
      if [ "$T" = "" ] || [ "$T" = "${LABEL_HEAD}" ]; then
        echo "$CONFIG" | cut -f1 -d\; >> ${WORKSPACE}/tmp_prop
      fi    
    done  
    N=1
    for sw in $i  ; do
      UP_ID_1=$(echo $sw | cut -f2 -d\< | cut -f1 -d\; )
      SW_TYPE_1=$(echo $sw | cut -f2 -d\; | cut -f1 -d\>)
      UP_1=$( ci_data.sh -f $CI_DATA -a read -i $UP_ID_1 -p 12  )
      echo "CONTAINER_UPGRADE_${SW_TYPE_1}_${N}=$UP_1" >> ${WORKSPACE}/tmp_prop
      N=$( expr $N + 1 )
    done
    VAR="${LABEL_HEAD} ${MAXTIME} ${TEST_ACTIVITY_TYPE}_from_${TRACK_N}_${TN}_pre_dc $UG ${LABEL_TAIL} SW  ${SUITES_LIST} $PT $UP $FROM_UP"
    gitWrapper.sh -P tools/bin/dci_create_send_testspecs.sh -D ${WORKSPACE} -d $VAR
    rm -f ${WORKSPACE}/tmp_prop 
  done
else
  UG=false
  #echo "OAMAP_IPV4=[]" > ${WORKSPACE}/tmp_prop
   for CONFIG in $(echo $CONFIG_LIST | sed 's/,/ /g' ) ; do
      T=$(echo "$CONFIG" | cut -f2- -d\; )


      if [ "$T" = "" ] || [ "$T" = "${LABEL_HEAD}" ]; then
echo "################################################################################
CONFIG	$CONFIG 
T=	$T
LABEL_HEAD	$LABEL_HEAD
################################################################################"
        echo "$CONFIG" | cut -f1 -d\; >> ${WORKSPACE}/tmp_prop
      fi    
    done  
  VAR="${LABEL_HEAD} ${MAXTIME} ${TEST_ACTIVITY_TYPE}_${FULL} $UG ${LABEL_TAIL} SW  "${SUITES_LIST}" $PT $UP"
  gitWrapper.sh -P tools/bin/dci_create_send_testspecs.sh -D ${WORKSPACE} -d $VAR
    rm -f ${WORKSPACE}/tmp_prop 
fi
}
###################################################################################
prep()
{
if [ "${PREP}" != "" ]; then
 PREP_SCRIPT=$(echo $PREP | sed 's/PREP_SCRIPT/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
 PREP_PROJECT=$(echo $PREP | sed 's/PREP_PROJECT/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
 $PREP_SCRIPT -p $PREP_PROJECT -c $PREP_UP

fi
}

###################################################################################
TEST_ACTIVITY_TYPE=$1
touch info.txt
touch ${WORKSPACE}/info.txt
CI_DATA=${WORKSPACE}/baseline.txt

###################################################################################
ACTIVITY_ID=$(cat ${RADIATOR_ROOT}/${TEST_TYPE}_ciresults_branch/${BRANCH}/${LSV_BUILD_NUMBER}/${TEST_ACTIVITY_TYPE}_${TEST_TYPE} | grep  "${JOB_NAME},"  | cut -f2 -d, )
SUITES_LIST=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 9 )
SPEC_LIST=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 8 )
MAXTIME=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 15  )
PREP=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 6  )
TEST_ACTIVITY_NAME=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 2  )
if [ "$(echo $TEST_ACTIVITY_NAME | grep "NODE" )" != "" ] && [ "$FULL" = "" ]; then
   FULL="pre_bdc"
fi
###################################################################################
echo "
TEST_ACTIVITY_NAME=$TEST_ACTIVITY_NAME
TEST_ACTIVITY_TYPE=$TEST_ACTIVITY_TYPE
FULL=$FULL
"
###################################################################################
if [ "${FULL}" = "" ]; then
    LABEL=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 11  )
    SW_ID=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 12  )
    FSW_ID=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 13  )
    PT=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 7  )
    SW=$( ci_data.sh -f $CI_DATA -a read -i $SW_ID -p 13  )
    PREP_UP=$SW
    prep
###################################################################################
    echo "BRANCH:$BRANCH" > ${WORKSPACE}/prep4TestInfo.txt 
    echo "TEST_TYPE:$TEST_TYPE">> ${WORKSPACE}/prep4TestInfo.txt 
    echo "COMMIT:$COMMIT" >> ${WORKSPACE}/prep4TestInfo.txt
    echo "SWLabel:$SW" >> ${WORKSPACE}/prep4TestInfo.txt 
###################################################################################
  dumy
  spec_list

###################################################################################
else
###################################################################################
  echo "BRANCH:$BRANCH" > ${WORKSPACE}/prep4TestInfo.txt 
  echo "TEST_TYPE:$TEST_TYPE">> ${WORKSPACE}/prep4TestInfo.txt 
  echo "COMMIT:$COMMIT" >> ${WORKSPACE}/prep4TestInfo.txt
  echo "SWLabel:no" >> ${WORKSPACE}/prep4TestInfo.txt 
###################################################################################
    FROM_LIST=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 13  | sed 's/,/ /g' )
    PT=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 7 )
    LABEL_LIST=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 11 | sed 's/,/ /g')
    UP_ID_LIST=$( ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 12 | sed 's/>,/> /g'  )
    CONFIG_LIST=$(ci_data.sh -f $CI_DATA -a read -i $ACTIVITY_ID -p 14  | sed 's/:/ /g' )
    LABEL_TYPE_LIST=$(echo $LABEL_LIST  | sed 's/ /\n/g' | cut -f2 -d\; | cut -f1 -d: | sort -u )
    echo "#########################################################"
    echo "LABEL_LIST	$LABEL_LIST "
    echo "LABEL_TYPE_LIST	$LABEL_TYPE_LIST "
    echo "CONFIG_LIST	$CONFIG_LIST "
    echo "#########################################################" 
    N=1
    echo "UP_ID_LIST $UP_ID_LIST "
    for sw in $UP_ID_LIST ; do
      UP_ID=$(echo $sw | cut -f2 -d\< | cut -f1 -d\; )
      SW_TYPE=$(echo $sw | cut -f2 -d\; | cut -f1 -d\>)
      UP=$(ci_data.sh -f $CI_DATA -a read -i $UP_ID -p 12 )
      if [ "$UP" = "" ]; then
        UP=$(ci_data.sh -f $CI_DATA -a read -i $UP_ID -p 13  )
      fi
      echo "CONTAINER_${SW_TYPE}_${N}=$UP" >> ${WORKSPACE}/info.txt
      if [ "$N" = "1" ]; then
        CONTAINER=$UP
      fi
      N=$( expr $N + 1 )
    done

 if [ "${SUITES_LIST}" != "" ]; then
    
    
    for SUB_LABEL_LIST in $(echo ${LABEL_LIST} ) ; do
      LABEL=$( echo $SUB_LABEL_LIST | awk '{print $1}' | cut -f1 -d\; )
      LABEL_HEAD=$(echo ${LABEL} | cut -f1 -d: )
      LABEL_TAIL=$(echo ${LABEL} | sed 's/:/ /' | awk '{print $2}' )
      if [ "$LABEL_TAIL" = "" ]; then
        LABEL_TAIL=no
      fi
     # LABEL_TYPE_LIST=$(echo $LABEL_LIST  | sed 's/,/ /g' | sed 's/ /\n/g' | cut -f2 -d\; | cut -f1 -d: | sort -u )
      echo "LABEL_TYPE_LIST	$LABEL_TYPE_LIST "
      for lt in $LABEL_TYPE_LIST ; do
        N=1
      echo "LABEL_LIST	$LABEL_LIST "
        for l in $LABEL_LIST ; do
          L=$(echo $l | cut -f1 -d\; | sed 's/:/\&\&/'  )
          L_TYPE=$(echo $l | cut -f2 -d\; | cut -f1 -d: )
echo "###################>${L_TYPE}< = >${lt}<"
          if [ "$L_TYPE" = "$lt" ] && [ "$(cat ${WORKSPACE}/info.txt | grep "LABEL_${L_TYPE}_${N}=$L")" = "" ]; then
echo ">>>>>>> LABEL_${L_TYPE}_${N}=$L"
            echo "LABEL_${L_TYPE}_${N}=$L" >> ${WORKSPACE}/info.txt
            N=$( expr $N + 1 )
          fi
         done
      done
      node_from
    done
    echo "FROM_LIST $FROM_LIST "
    
###################################################################################
 cp -f *spec_* ${WORKSPACE}/
 fi
 
  spec_list
  
###################################################################################
###################################################################################

fi
if [ "${PREP}" != "" ]; then
 PREP_SCRIPT=$(echo $PREP | sed 's/PREP_SCRIPT/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
 PREP_PROJECT=$(echo $PREP | sed 's/PREP_PROJECT/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
 PREP_LAB=$(echo $PREP | sed 's/PREP_LAB/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
 PREP_FLAVOR=$(echo $PREP | sed 's/PREP_FLAVOR/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
 PREP_NETWORK=$(echo $PREP | sed 's/PREP_NETWORK/#/' | cut -f2- -d\# | awk '{print $1}' | cut -f2- -d=)
echo "PREP_SCRIPT=$PREP_SCRIPT
PREP_PROJECT=$PREP_PROJECT
PREP_LAB=$PREP_LAB
PREP_FLAVOR=$PREP_FLAVOR
PREP_NETWORK=$PREP_NETWORK" > ${WORKSPACE}/info.txt
fi

cat ${WORKSPACE}/info.txt




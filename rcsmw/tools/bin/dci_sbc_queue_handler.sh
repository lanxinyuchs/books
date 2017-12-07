#!/bin/bash

comment()
{
if [ "$JOB_NAME" != "multi_gen_queue_lsv" ]; then
  cd ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/$BUILD_NUMBER
  umask 002
  echo "${COMMENT}" >> comments.txt
  chmod g+w comments.txt
fi
}

JENKINS_URL_B="https://rbs-rde-ci.rnd.ki.sw.ericsson.se/"
JENKINS_URL_C="https://fem007-eiffel002.rnd.ki.sw.ericsson.se:8443/jenkins/"
 if [ "${REPONAME}" != "" ]; then
   REPO_TYPE=$(echo $REPONAME | sed 's/\//_/g' )
   echo "$BUILD_NUMBER:$COMMIT" > ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/.${REPO_TYPE}_${JOB_NAME}
   CHECK_FILE=${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/.${REPO_TYPE}_${JOB_NAME}
   CHECK="$BUILD_NUMBER:$COMMIT"
 else
   PROD=$(echo $MESSAGE | cut -f1 -d-)
   echo "$BUILD_NUMBER:$MESSAGE" > ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/.${PROD}_${JOB_NAME}
   CHECK_FILE=${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/.${PROD}_${JOB_NAME}
   CHECK="$BUILD_NUMBER:$MESSAGE" 
 fi
J=1
K=0
while [ $J -gt 0 ]; do 
   echo "----------------------------------------------------------------------------------------------"
   echo "$CHECK_FILE	$(cat $CHECK_FILE)	$CHECK"
   if [ " $(cat $CHECK_FILE | cut -f1 -d:)"  -gt " $(echo  $CHECK | cut -f1 -d:)" ]; then
    J=0
    COMMENT="Lack of capacity! LSV $(cat $CHECK_FILE | cut -f1 -d:) take over and run the tests"
    echo "Letting $JOB_NAME job number $(cat $CHECK_FILE | cut -f1 -d:) take over and run the tests"
    echo "System Build Inhibited"
    comment
    rm -f ${WORKSPACE}/prop.txt
    exit
  elif [ " $(cat $CHECK_FILE | cut -f1 -d:)"  -lt " $(echo  $CHECK | cut -f1 -d:)" ]; then
    echo "$CHECK" > $CHECK_FILE
    sleep 60
  else
    J_A=$(curl -s ${JENKINS_URL}queue/api/json?pretty=true | grep "name" | grep "run_testspec_[0-9][0-9]_CI_5.0" | wc -l | awk '{print $1}' )
    J_B=$(curl -s ${JENKINS_URL_B}queue/api/json?pretty=true | grep "name" | grep "stealing_STP_resource" | wc -l | awk '{print $1}' )
    J_C=$(curl -s ${JENKINS_URL_C}queue/api/json?pretty=true | grep "name" | grep "stealing_STP_resource" | wc -l | awk '{print $1}' )

    J=$( expr $J_A + $J_B + $J_C )

       LAST_BUILD_NUMBER=$(curl -s ${JOB_URL}/api/json?pretty=true | grep "number" | head -1 | awk '{print $3}' | cut -f1 -d, )
       OLDEST_BUILD_NUMBER=$( expr $(curl -s ${JOB_URL}/api/json?pretty=true | grep -A1 "last[A-Z].*Build" | grep number | awk '{print $3}' | cut -f1 -d, | sort -n | tail -1 ) + 1)
       ME_IN_QUEUE=$(curl -s ${JENKINS_URL}queue/api/json?pretty=true | grep "name" | grep "${JOB_NAME}" | wc -l | awk '{print $1}' )
       echo "## Total=$J	LAST_BUILD_NUMBER=$LAST_BUILD_NUMBER	OLDEST_BUILD_NUMBER=$OLDEST_BUILD_NUMBER 	BUILD_NUMBER=${BUILD_NUMBER}	ME_IN_QUEUE=$ME_IN_QUEUE"
    if [ "$J" != "0" ]; then
      echo "Total $J	jobs in the queue! Wait 1 minutes and check the status again."
     echo "$J_A run_testspec_*	${JENKINS_URL} "
     echo "$J_B stealing_STP_resource	${JENKINS_URL_B} "
     echo "$J_C stealing_STP_resource	${JENKINS_URL_C} "
     sleep 60
    else
       if [ "$K" = "120" ]; then
           K=0
           sleep $K
       fi

       LAST_BUILD_NUMBER=$(curl -s ${JOB_URL}/api/json?pretty=true | grep "number" | head -1 | awk '{print $3}' | cut -f1 -d, )
       OLDEST_BUILD_NUMBER=$( expr $(curl -s ${JOB_URL}/api/json?pretty=true | grep -A1 "last[A-Z].*Build" | grep number | awk '{print $3}' | cut -f1 -d, | sort -n | tail -1 ) + 1)
       ME_IN_QUEUE=$(curl -s ${JENKINS_URL}queue/api/json?pretty=true | grep "name" | grep "${JOB_NAME}" | wc -l | awk '{print $1}' )
       echo "OLDEST_BUILD_NUMBER=$OLDEST_BUILD_NUMBER 	BUILD_NUMBER=${BUILD_NUMBER}	ME_IN_QUEUE=$ME_IN_QUEUE"
       if [ "$OLDEST_BUILD_NUMBER" -lt "${BUILD_NUMBER}" ] || [ "$ME_IN_QUEUE" != "0" ]; then
           sleep 60
           J=1
           K=120
       fi
    fi
  fi
done


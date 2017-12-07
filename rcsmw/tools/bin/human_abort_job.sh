#!/usr/bin/env bash
#set -x

## EXEMPEL
##  /home/etxjovp/CI/human_abort_job.sh
##    -R /proj/webdocs/rbs-rde-ci/root
##    -S 1
##    -G master
##    -B https://fem025-eiffel002.rnd.ki.sw.ericsson.se:8443/jenkins/view/DCI/job/run_testspec_73_CI_5.0/3012/


# Default values
SLEEP=10
RADIATOR_ROOT="/proj/webdocs/rbs-rde-ci/root"

while getopts J:B:G:R:S:T:d: option
do
        case "$option"
        in
                J) JENKINS_URL=$OPTARG
                   ;;
                B) BUILD_URL=$OPTARG
                   ;;
                G) BRANCH=$OPTARG
                   ;;
                R) RADIATOR_ROOT=$OPTARG
                   ;;
                S) SLEEP=$OPTARG
                   ;;
                d)  debug="-${option}"
                  ;;
                \?) usage
                   exit 1;;
        esac
done

# Dig out the instance numer of the first job in chain, currently rcsmw_gen_lsv
INSTANCE=$(curl ${BUILD_URL}/ 2>&1  | egrep "originally caused by" | tail -1 | tr '"' '\012' | sed 's%/$%%' | grep -v '<' | tail -1 | awk -F/ 'NF>1{print $NF}')

if [[ $INSTANCE == "" ]]; then
    # We are not called by a downstream job, it has to be the top-job!!!
    # In this case we take the local BUILD_ID, there is no upstream job to look for !!!
    INSTANCE=$BUILD_ID
fi


killFile=${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${INSTANCE}/.abortMe
while [[ ! -f $killFile ]]; do
    #echo $killFile
    # if job result is not null, job has ended and we shall die, nothing more to kill
    X=$(curl -s ${BUILD_URL}/api/json?pretty=true | grep "\"result\" : null")
    if [[ $X == "" ]]; then
        exit 0
    fi
    sleep $SLEEP
done

WHOAMI=$(whoami)

FILE=/home/${WHOAMI}/.ssh/.pw_apiToken_${WHOAMI}
if [[ ${JENKINS_URL-} ]]; then
    JENKINS=$(echo $JENKINS_URL | tr '-' '/' | cut -f3 -d/)
    FILE=/home/${WHOAMI}/.ssh/.pw_apiToken_${JENKINS}_${WHOAMI}
fi

if [[ ! -f $FILE ]]; then
    # echo "Missing file"
    exit 1
fi

curl --request POST -k -s -u  ${WHOAMI}:"`cat $FILE`" ${BUILD_URL}stop


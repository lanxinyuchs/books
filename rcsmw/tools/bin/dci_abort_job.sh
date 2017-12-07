#!/bin/bash 

while getopts J:T:N:B:S:d: option
do
	case "$option"
	in
                J) JENKINS_URL=$OPTARG
                   ;;
                T) TRIGG_JOB_URL=$OPTARG
                   ;;  
                N) TRIGG_BUILD_NUMBER=$OPTARG
                   ;;              
                B) BUILD_URL=$OPTARG
                   ;; 
                S) SLEEP=$OPTARG
                   ;; 
                d)  debug="-${option}"
                  ;;                 
		\?) usage
		   exit 1;; 
	esac
done 

X=$(curl -s ${TRIGG_JOB_URL}${TRIGG_BUILD_NUMBER}/api/json?pretty=true | grep "\"result\" : null")
while [  "$X" != "" ] 
  do
    sleep $SLEEP
    X=$(curl -s ${TRIGG_JOB_URL}${TRIGG_BUILD_NUMBER}/api/json?pretty=true | grep "\"result\" : null")
  done
WHOAMI=$(whoami)

FILE=/home/${WHOAMI}/.ssh/.pw_apiToken_${WHOAMI}
if [[ ${JENKINS_URL-} ]]; then
    JENKINS=$(echo $JENKINS_URL | tr '-' '/' | cut -f3 -d/)
    FILE=/home/${WHOAMI}/.ssh/.pw_apiToken_${JENKINS}_${WHOAMI}
fi

if [[ ! -f $FILE ]]; then
    echo "Missing file"
    exit 1
fi

curl --request POST -k -s -u  ${WHOAMI}:"`cat $FILE`" ${BUILD_URL}stop


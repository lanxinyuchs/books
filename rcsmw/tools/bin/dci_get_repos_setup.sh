#!/bin/bash 

###################################################################################################
mia_sync()
{
if [ "${ARTIFACTID}" != "" ]; then
CL=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""confidenceLevels"" | sed 's/\"confidenceLevels\"/#/g' | grep "#" | cut -f2- -d# | cut -f2 -d\" )
VE=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""confidenceLevels"" | sed 's/\"confidenceLevels\"/#/g' | grep "#" | cut -f2- -d# | cut -f4 -d\" )

S=" 5 10 20 60 "
for i in $S
do
if [ "$(curl -s http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${ARTIFACTID}\&version=${VERSION}\&confidence_level=${CL}\&verdict=${VE} | grep ${ARTIFACTID} )" = "" ] ; then
  echo " $ARTIFACTID $VERSION $CL $VE not exist in MIA !! wait $i s"
  sleep $i
else
 break
fi
done
fi
}
###################################################################################################







while getopts F:B:C:N:U:m:d:b: option
do
	case "$option"
	in
                F) FILE=$OPTARG
                   ;;  
                B) BUILD_CAUSE_INFORMATION=$OPTARG
                   ;;  
                R) RADIATOR=$OPTARG
                   ;;  
                M) MASTER_GIT_URL=$OPTARG
                   ;;    
                C) COMMIT=$OPTARG
                   ;; 
                N) REPONAME=$OPTARG
                   ;;  
                U) REPOURI=$OPTARG
                   ;;
                m) COMMITMESSAGE=$OPTARG
                   ;;    
                b) BRANCH=$OPTARG
                   ;;             
                d)  debug="-${option}"
                  ;;                 
		\?) usage
		   exit 1;; 
	esac
done 
echo ">${BUILD_CAUSE_INFORMATION}<"
if [ "${BUILD_CAUSE_INFORMATION}" != "" ]; then

ARTIFACTID=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""artifactId"" | sed 's/\"artifactId\"/#/g' | grep "#" | cut -f2- -d# | cut -f2 -d\" )
VERSION=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""version"" | sed 's/\"version\"/#/g' | grep "#" | cut -f2- -d# | cut -f2 -d\" )
COMMIT=$(cat ${BUILD_CAUSE_INFORMATION}  | grep ""sha1"" | sed 's/\"sha1\"/#/g' | grep "#" | cut -f2- -d# | cut -f2 -d\" )
REPONAME=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""repoName"" | sed 's/\"repoName\"/#/g' | grep "#" | cut -f2- -d# | cut -f2 -d\")
REPOURI=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""repoUri"" | sed 's/\"repoUri\"/#/g' | grep "#"| cut -f2- -d# | cut -f2 -d\")
COMMITMESSAGE=$(cat ${BUILD_CAUSE_INFORMATION} | grep ""commitMessage""| sed 's/\"commitMessage\"/#/g' | cut -f2- -d# | cut -f2 -d\" | cut -f1 -d\\ )
mia_sync
fi

GIT_URL=""
GIT_URL_LIST=$(ci_data.sh -f ${FILE} -a read -i 00 -p 2 | sed 's/,/ /g')
RADIATOR_ROOT=$(ci_data.sh -f ${FILE} -a read -i 00 -p 8 )
TOP_COMMIT_TYPE=$(ci_data.sh -f ${FILE} -a read -i 00 -p 6)
TOP_REPONAME=$(ci_data.sh -f ${FILE} -a read -i 00 -p 5 )
TOP_REPO_TYPE=$(echo $TOP_REPONAME | sed 's/\//_/g' )
TOP_COMMIT=$(cat ${RADIATOR_ROOT}/$BRANCH/${TOP_COMMIT_TYPE}_${TOP_REPO_TYPE}_commit )
echo "
##################################################################################
ARTIFACTID=$ARTIFACTID
VERSION=$VERSION
COMMIT=$COMMIT
BRANCH=$BRANCH
REPONAME=$REPONAME
REPOURI=$REPOURI
##################################################################################
"
  for i in $GIT_URL_LIST
  do
    echo "$i"

    COMMIT_TYPE=$(ci_data.sh -f ${FILE} -a read -i $i -p 6)
    REPO_URL=$(ci_data.sh -f ${FILE} -a read -i $i -p 4)
    REPONAME_1=$(ci_data.sh -f ${FILE} -a read -i $i -p 5)
    REPO_TYPE=$(echo $REPONAME_1 | sed 's/\//_/g' )
    echo ${RADIATOR_ROOT}/$BRANCH/${COMMIT_TYPE}_${REPO_TYPE}_commit
    if [ "$REPONAME" = "$REPONAME_1" ] && [ "$TOP_REPONAME" = "$REPONAME" ] ; then
      GIT_URL="${GIT_URL},${REPO_URL}/${REPONAME_1};${COMMIT}"
      TOP_COMMIT=$COMMIT
    elif [ "$REPONAME" = "$REPONAME_1" ] ; then
      GIT_URL="${GIT_URL},${REPOURI};${COMMIT}"
    elif [ -f ${RADIATOR_ROOT}/$BRANCH/${COMMIT_TYPE}_${REPO_TYPE}_commit ]; then
      COMMIT_1=$(cat ${RADIATOR_ROOT}/$BRANCH/${COMMIT_TYPE}_${REPO_TYPE}_commit)
      GIT_URL="${GIT_URL},${REPO_URL}/${REPONAME_1};${COMMIT_1}"
    else
      BRANCH_1=$(ci_data.sh -f ${FILE} -a read -i $i -p 7)
      echo "COMMIT for $REPONAME_1 not exist used ${BRANCH_1}"
      GIT_URL="${GIT_URL},${REPO_URL}/${REPONAME_1};${BRANCH_1}"     
   fi
  done
if [ "${ARTIFACTID}" = "" ]; then
  MESSAGE=${COMMITMESSAGE}
  SBC_TRIGG=${REPONAME}
else
  MESSAGE=${ARTIFACTID}-${VERSION}
  TOP_REPO_TYPE=$(echo $TOP_REPONAME | sed 's/\//_/g' )
  COMMIT=$TOP_COMMIT
  SBC_TRIGG=${ARTIFACTID}
fi

echo "
TOP_COMMIT=$TOP_COMMIT
COMMIT=$COMMIT
REPONAME=$REPONAME
BRANCH=$BRANCH
LSV_BUILD_NUMBER=${BUILD_NUMBER}
GIT_URL=${GIT_URL}
MESSAGE=${MESSAGE}
RADIATOR_ROOT=$(dirname ${RADIATOR_ROOT})
SBC_TRIGG=${SBC_TRIGG}" > ${WORKSPACE}/prop.txt
echo "${WORKSPACE}/prop.txt"
echo "
##################################################################################
# ${WORKSPACE}/prop.txt
##################################################################################"
cat ${WORKSPACE}/prop.txt
echo "
##################################################################################
"


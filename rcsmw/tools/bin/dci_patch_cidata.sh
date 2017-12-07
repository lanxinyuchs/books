#!/bin/bash 

while getopts T:C:B:F:A:R:d: option
do
	case "$option"
	in
                A) ACTION=$OPTARG
                   ;;  
                T) TEST=$OPTARG
                   ;;  
                C) CLEAN=$OPTARG
                   ;;              
                B) BRANCH=$OPTARG
                   ;; 
                F) FILE=$OPTARG
                   ;; 
                R) RADIATOR_ROOT=$OPTARG
                   ;; 
                d)  debug="-${option}"
                  ;;                 
		\?) usage
		   exit 1;; 
	esac
done 
if [ -n "${debug}" ]
then
    set -x
fi
if [ "$ACTION" = "update" ]; then
  if [ "$CLEAN" = "true" ]; then
    rm -f ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE 
  fi
  if [ "$( cat $FILE )" != "" ]; then  
    cat ${FILE} | grep -v  "^#" > CI_DATA_PATCH_FILE_tmp
    if [ "$CLEAN" = "true" ]; then
      cat CI_DATA_PATCH_FILE_tmp >  ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE 
    else
      touch ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE
      cat  ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE > tmp_old
      while read p; do 
       I=$(echo $p | cut -f1 -d\|)
       OLD_N=$(cat tmp_old | grep -n "^${I}|" | cut -f1 -d:)
       OLD_ROW=$(cat tmp_old | grep -n "^${I}|" | cut -f2- -d: )
       if [ "$OLD_N" = "" ] && [ "$p" != "" ]; then
        echo "$p" >> ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE
       elif [ "${p}" != "" ]; then
        sed -i "s%${OLD_ROW}%${p}%" ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE
       fi
     done < CI_DATA_PATCH_FILE_tmp
     #diff ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE tmp_old
   fi
  fi  
  rm -f *CI_DATA_PATCH_FILE*
  if [ "$(ls  ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/ | grep "_CI_DATA_PATCH_FILE" )" != "" ]; then
   cp   ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/*_CI_DATA_PATCH_FILE .
  else
   touch ${TEST}_CI_DATA_PATCH_FILE
  fi
elif [ "$ACTION" = "insert" ]; then

  echo " ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE "
  if [ -f ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE ]; then
     #cat  ${FILE} > tmp_old
    while read p; do 
       I=$(echo $p | cut -f1 -d\|)
       OLD_N=$(cat ${FILE} | grep -n "^${I}|" | cut -f1 -d:)
       OLD_ROW=$(cat ${FILE} | grep -n "^${I}|" | cut -f2- -d: )
       if [ "$OLD_N" = "" ]; then
        LAST_N=$(cat ${FILE} | grep -v "#" | cut -f1 -d\| | sort -n  | awk -v p=${I} '$1 < p' | tail -1 )       
        OLD_LAST_N=$(expr $(cat ${FILE} | grep -n "^${LAST_N}|" | cut -f1 -d:) + 1 )
        awk -v n=${OLD_LAST_N} -v s="$p" 'NR == n {print s} {print}' ${FILE} > ${FILE}_tmp
        #diff ${FILE}  ${FILE}_tmp
        cat ${FILE}_tmp > ${FILE}
        rm -f ${FILE}_tmp
       elif [ "${p}" != "" ]; then
        sed -i "s%${OLD_ROW}%${p}%" ${FILE}
       fi
     done < ${RADIATOR_ROOT}/aggregated_rcsmw/${BRANCH}/${TEST}_CI_DATA_PATCH_FILE
     #diff ${FILE} tmp_old
  fi
fi

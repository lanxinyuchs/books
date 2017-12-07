#!/bin/bash

SPECS=$1
######################################################################################################
mod1()
{
  cat $s | grep "{[^()]*define[^()]*,[^()]*'Top[^()]*'[^()]*," | grep -v "/repo/" > mod1
  while read p; do 
    if [ "$( echo $p | grep RCS )" = "" ]; then
       K="$(echo $p | cut -f1-2 -d,), \"${RCT_TOP}/test\"}."
    else
       M="$(echo $p | cut -f2- -d\" | cut -f1 -d\")"
       N=$(echo $M | cut -f7- -d\/ | sed 's|/| |g' | awk '{print "/"$1"/"$3}')   
       K="$(echo $p | cut -f1-2 -d,), \"${RCS_TOP}${N}\"}."
    fi
    sed  -i "s|${p}|${K}|" s
  done < mod1
rm -f mod1
}
######################################################################################################
mod2()
{
  cat $s | grep "{[^()]*groups[^()]*,[^()]*\"" | grep -v "/repo/" > mod2
  cat $s | grep "{[^()]*cases[^()]*,[^()]*\"" | grep -v "/repo/" >> mod2
  while read p; do 
    K="$(echo $p | cut -f1 -d\")"
    L="$(echo $p | cut -f2 -d\")"
    M="$(echo $p | cut -f3- -d\")"
    N="$K\"$L\""
    if [ "$( echo $L | grep "test/RCT" )" != "" ]; then
       LL="$(echo $L | cut -f6- -d\/  )"
       LLL="${RCT_TOP}/${LL}"
    elif [ "$( echo $L | grep "RCT" )" != "" ]; then
       LL="$(echo $L | cut -f7- -d\/  )"
       LLL="${RCT_TOP}/${LL}"
    elif [ "$( echo $L | grep RCS )" != "" ]; then
       LL=$(echo $L | cut -f7- -d\/ | sed 's|/| |g' | awk '{print "/"$1"/"$3}')   
       LLL="${RCS_TOP}${LL}"
    elif [ "$( echo $L | grep "test/test" )" != "" ]; then
       A=$(basename $L)
       B=$(basename $(dirname $L))
       LLL="${RCT_TOP}/test/${B}/${A}"
    else
       A=$(basename $L)
       B=$(basename $(dirname $L))
       LL=$(echo $L | cut -f1 -d\/ )
       LLL="${RCS_TOP}/${LL}/${B}/${A}"
    fi
    KK="${K}\"${LLL}\" ${M}"
    KK="${K}\"${LLL}\""
    sed  -i "s|${N}|${KK}|" s
  done < mod2
rm -f mod2
}
######################################################################################################
if [ -n ${RCT_TOP} ] && [ -n ${RCT_TOP} ]; then
  for s in $SPECS
  do
    echo "mod	$s "
    cp -f $s s
    mod1
    mod2
    cp -f s ${s}
    rm s
  done
else
  echo "RCT_TOP RCS_TOP not defined "
  exit 1
fi

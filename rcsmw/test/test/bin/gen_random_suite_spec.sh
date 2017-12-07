#!/bin/bash
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2013 All rights reserved.
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
## R2A/1      2013-03-18   etxjovp     Created
## ----------------------------------------------------------
TESTSPEC=$1
NO=$2
SPECNAME=$3
if [ "$TESTSPEC" = "result" ]; then

 CONFIG=$SPECNAME
 LOG="/proj/webdocs/rbs-rde-ci/jenkinsdata/jobs/${CONFIG}/builds/${NO}/log"

 cat $LOG | grep "TEST COMPLETE," | grep -v install_dus_SUITE  | grep -v  check_after_install_SUITE > l.txt
 N=$(wc -l l.txt |awk '{print $1}')

 M=1
 echo $N Suites

 echo "No		Fail	Skip	Total	Product	Suite"
 echo "--------------------------------------------------------------------------------"
 while [ $N -gt 0 ]; do
   FAILED=""
   SKIPPED=""
    R=$(head -$M l.txt | tail -1)
    S=$(echo $R  | awk '{print$2}' | sed 's/\:/ /g' )
    RR=$(echo $R  | awk '{print $7"	0	"$10"	"}' )
    if [ "$(echo $R  | grep -v " 0 failed" )" != "" ]; then
       FAILED=" F"
    fi
    if [ "$(echo $R | grep  "skipped" )" != "" ]; then
       SKIPPED=" S"
       RR=$(echo $R   | awk '{print $7"	"$9"	"$12"	"}' )
    fi

    echo "$M	$FAILED$SKIPPED	$RR$S"
    M=$( expr $M + 1  )
    N=$( expr $N - 1  )
  done

\rm l.txt
  echo $LOG
  INDEX=$(cat $LOG | grep "Test run log:" | grep -v "TESTCASE" | tail -1 | cut -f4- -d\/)
  if [ "$INDEX" = "" ]; then
     INDEX=$(cat $LOG | grep "Test run log:"  | tail -1  | cut -f4- -d: | cut -f4- -d\/)
  fi
  START=$(cat -n $INDEX | grep "<tbody>" | awk '{print $1}')
  STOP=$(cat -n $INDEX | grep "</tbody>" | awk '{print $1}')
  D=$(cat $INDEX | grep "<td><a href" | cut -f2- -d\/ | cut -f1 -d\/ | sort -n)
  echo "START $START STOP $STOP"
  head -$START $INDEX > tmp.html
  for i in $D 
  do
    N=$(cat -n $INDEX | grep $i | awk '{print $1}')
    for n in $N
    do
     echo "<tr>" >> tmp.html
     head -$( expr $n + 5 ) $INDEX  | tail -6 >> tmp.html
    done
  done
  MAX=$( wc -l $INDEX | awk '{print $1}')

  tail -$( expr $MAX - $( expr $STOP - 1 ) ) $INDEX >> tmp.html
  ls -l tmp.html

  mv $INDEX $INDEX.old
  ls -l $INDEX.old
  mv tmp.html $INDEX
  exit 
fi
if [ "$SPECNAME" = "" ]; then
 SPECNAME=random_${TESTSPEC}
fi
if [ "$NO" = "" ]; then
 NO=1
fi
touch .testspec.ts
\rm .testspec.ts
touch .suites1.txt
\rm .suites1.txt
touch .spec1
\rm .spec1
touch .spec2
\rm .spec2
touch .spec3
\rm .spec3
echo "      {merge_tests, false}." > .testspec.ts
TESTSPECLIST=$(ls $RCT_TOP/test/testspecs/$TESTSPEC*)
echo "Test specs "
echo "####################################################################"
echo "$TESTSPECLIST"
I=0
for j in $TESTSPECLIST
do
I=$( expr $I + 1 )
cat $j | grep define | grep "'Top'," | sort -u | sed 's/'Top'/'Top${I}'/g' >> .spec1
cat $j  | grep define | grep -v "'Top',"  | sort -u | sed 's/'Top'/'Top${I}'/g' | sed 's/'\','/'.${I}\','/g' >> .spec2
cat $j | grep 'groups,\|cases,' | grep -v "%"  | grep -v "measure_restart_SUITE"  | sed 's/'\','/'.${I}\','/g' >> .spec3

done

cat .spec1 >> .testspec.ts
cat .spec2 >> .testspec.ts
while [ $NO -gt 0 ]; do
  cat .spec3 >> .suites1.txt
  NO=$( expr $NO - 1 )
done
N=$(cat .suites1.txt | wc -l)
\rm .spec1
\rm .spec2
\rm .spec3
echo "$N test suites"


while [ $N -gt 1 ]; do
 R=$(( 1+(`od -An -N2 -i /dev/random` )%($N) )) 
  SUITE=$(head -${R} .suites1.txt | tail -1)
  S=$(echo $SUITE | awk '{print $3}')
  #cat .suites1.txt | grep -v "$S" > .suites.txt
  #X=$(cat .suites1.txt | grep -n "$S" | tail -1 | cut -f1 -d:)
#echo $R $N $RANDOM
  head -$( expr $R - 1 ) .suites1.txt > .suites.txt
  tail -$( expr $N - $R ) .suites1.txt >> .suites.txt
  cp .suites.txt  .suites1.txt
  N=$( expr $N - 1 )
  echo "$SUITE" >> .testspec.ts
done

cat .suites1.txt  >> .testspec.ts
echo "####################################################################"
echo "Random test spec $SPECNAME"
echo "####################################################################"
cat .testspec.ts
cat  .testspec.ts > $SPECNAME
\rm .testspec.ts

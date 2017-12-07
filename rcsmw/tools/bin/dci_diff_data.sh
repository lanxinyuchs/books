#!/bin/bash

usage()
{
echo "




Usage:

$0 -F lsvno_A -T lsvno_B | -f commit_A -t commit_B -s | -p | -a | -r [ -g ]

-s diff skeleton 
-p diff after prepare
-a diff after msrcs_gen_lsv
-r diff after sbc 
-g graphical mode


"


}
#######################################################################################
diff_main()
{
  echo "########################### DIFF $FROM_LSV $TO_LSV"

    FCID=$(ls -d $RADIATOR_ROOT/*/$FROM_LSV)
    TCID=$(ls -d $RADIATOR_ROOT/*/$TO_LSV )
    if [ "$SKELETON" != "" ]; then
      TYPE=$SKELETON
      diff_ci
    fi
    if [ "$PREPARE" != "" ]; then
     TYPE=$PREPARE
     diff_ci
    fi
    if [ "$ALL" != "" ]; then
     TYPE=$ALL
     diff_ci
    fi
    if [ "$RESULT" != "" ]; then
     TYPE=""
     diff_ci
    fi
  
}
#######################################################################################
diff_ci()
{
if [ -f $FCID/CIdata.txt${TYPE} ] && [ -f $FCID/CIdata.txt${TYPE}  ]; then

  FCI=$(ls $FCID/CIdata.txt${TYPE} )
  TCI=$(ls $TCID/CIdata.txt${TYPE} )
  echo "########################### DIFF $FCI $TCI"
  echo ""
  if [ "$GRAPHICAL" = "yes" ]; then
    cleartool diff -graphical $FCI $TCI &
  else
    diff $FCI $TCI > diff_CIdata.txt${TYPE}
    cat diff_CIdata.txt${TYPE}
  fi
fi
}

RADIATOR_ROOT=/proj/webdocs/rbs-rde-ci/root/aggregated_rcsmw
######################################################################################
#   MAIN
######################################################################################
while getopts T:F:t:f:R:d:st:pt:at:gt:rt:-ht: option
do
	case "$option"
	in
                F) FROM_LSV=$OPTARG
                   ;;  
                T) TO_LSV=$OPTARG
                   ;;              
                f) FROM_COMMIT=$OPTARG
                   ;;  
                t) TO_COMMIT=$OPTARG
                   ;;  
                R) RADIATOR_ROOT=$OPTARG
                   ;; 
                s) SKELETON=".skeleton"
                   ;;  
                p) PREPARE=".prepare"
                   ;;  
                a) ALL=".all"
                   ;;
                r) RESULT=".result"
                   ;;
                g) GRAPHICAL=yes
                   ;;
                h) usage
                   ;;
                d)  debug="-${option}"
                  ;;                 
		\?) usage
		   exit 1;; 
	esac
done 

if [ -n "${debug}" ]
then
echo "
		   FROM_LSV=$FROM_LSV
                   TO_LSV=$TO_LSV
                   FROM_COMMIT=$FROM_COMMIT
                   TO_COMMIT=$TO_COMMIT
                   RADIATOR_ROOT=$RADIATOR_ROOT
                   SKELETON=$SKELETON
                   PREPARE=$PREPARE
                   ALL=$ALL
                   RESULT=$RESULT
                   debug=$debug
"
    set -x
fi
if [ "$FROM_LSV" != "" ] && [ "$TO_LSV" != "" ]; then 
  if [ -d  $RADIATOR_ROOT/*/$FROM_LSV ] && [ -d  $RADIATOR_ROOT/*/$TO_LSV ]; then
    diff_main
  fi
elif [ "$FROM_COMMIT" != "" ] && [ "$TO_COMMIT" != "" ]; then 
  echo "diff_commit"
  FROM_LSV=$(basename $(dirname $(egrep "q/$FROM_COMMIT" $RADIATOR_ROOT/*/*/commit_id | head -1 | cut -f1 -d: )))
egrep "q/$FROM_COMMIT" $RADIATOR_ROOT/*/*/commit_id | head -1 | cut -f1 -d:
  TO_LSV=$(basename $(dirname $(egrep "q/$TO_COMMIT" $RADIATOR_ROOT/*/*/commit_id | head -1 | cut -f1 -d: )))
egrep "q/$TO_COMMIT" $RADIATOR_ROOT/*/*/commit_id | head -1 | cut -f1 -d:


  if [ -d  $RADIATOR_ROOT/*/$FROM_LSV ] && [ -d  $RADIATOR_ROOT/*/$TO_LSV ]; then
    diff_main
  fi
else
 exit
fi



#!/bin/bash


usage()
{


echo "
Usage:
$0 -f file -a read|info|name|content [-i id] [-p pos] 

"



}

######################################################################################
info()
{
  INFO=$(cat $CI_DATA_FILE | grep ^"${ID}|" | awk -F\| '{print $NF}')
  echo $INFO
}
######################################################################################
p_list()
{
        if [ "$ACTION_1" = "" ] || [ "$ACTION_1" = "$NNN" ]; then 
          P_N="$NNN,"
         for p in $(echo $POS | sed 's/,/ /g' )
         do
         PP_N=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $iii -p $p  )
         P_N="${P_N}${PP_N}," 
         done
       else 
         P_N=""
       fi
}
######################################################################################
update_cl()
{
clList="4 3 2 1 0"
CL="-"  
#echo "http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${P}\&version=${R}\&latest=1\&project=${PROJECT}\&increment_number=${TRACK} "
X=$(curl -s --retry ${retry} --retry-delay ${retryDelay} http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${P}\&version=${R}\&latest=1\&project=${PROJECT}\&increment_number=${TRACK} 2> /dev/null )
if [ "$?" = "0" ] && [ "$(echo $X | grep "does not exist" )" = "" ]; then
  for cl in ${clList}
  do
    if [ "$CL" = "-" ]; then
      if [ "$(echo $X | python -mjson.tool | grep -A16 "\"confidenceLevelName\": \"${cl}\"" | grep  "\"name\": \"SUCCESS\"" )" != "" ]; then
        CL=$cl
        COLOR=green
      else 
        if [ "$(echo $X | python -mjson.tool | grep -A16 "\"confidenceLevelName\": \"${cl}\"" | grep  "\"name\": \"UNSTABLE\"" )" != "" ]; then
          CL=${cl}
          COLOR=red
        fi
     fi
    fi
  done
fi
echo " $CL_F		$P-$R	CL=$CL "
    if [ "$CL" != "-" ]; then
     INFOBANK1="https://rbs-g2-mia.rnd.ki.sw.ericsson.se/products/${P}/revision/${R}/"
     echo "<a href=\"${INFOBANK1}\"> <font color=${COLOR}>${CL}</font></a> " > $CL_F
    fi

}
######################################################################################
#   MAIN
######################################################################################


CI_DATA=ci_data.sh
##MANAGE3PP=manage3PP.sh
MANAGE3PP=cacheManager.sh
retry=3
retryDelay=5


while getopts a:A:i:p:f:c:C:t:r:dt:ht: option
do
	case "$option"
	in
                a) ACTION=$OPTARG
                  ;;
                A) ACTION_1=$OPTARG
                  ;;
                i) ID=$OPTARG
                  ;;
                t) TYPE=$OPTARG
                  ;;
                p) POS=$OPTARG
                  ;;
                c) COLUMN=$OPTARG
                  ;;
                C) CACHE=$OPTARG
                  ;;
                f) CI_DATA_FILE=$OPTARG
                  ;;
                r) RADIATOR_ROOT=$OPTARG
                  ;;
                d)  debug="-${option}"
                  ;;
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done

######################################################################################
if [ -n "${debug}" ]
then
    set -x
fi

if [ "$ACTION" = "get_test_type" ]; then
     L=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ID -p 6 | sed 's/,/ /g' | grep -v "=" )
     for i in $L
      do

         if [ "$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $i -p 5  )" = "$TYPE" ]; then
          T=$i
         fi

      done
echo $T


elif [ "$ACTION" = "get_test_activity_list" ]; then

     LIST=""
     P_LIST=""
     L=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ID -p 6 | sed 's/,/ /g' )
     for i in $L
      do
         M=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $i -p 6 | sed 's/,/ /g' | grep -v "=" )
         N=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $i -p 10  )
         if [ "$M" = "" ] && [ "$N" != "" ]; then
            LIST="${LIST}${N},"
            if [ "$POS" != "" ]; then 
               NNN=$N
               iii=$i
               p_list
               if [ "$P_N" != "" ]; then 
               P_LIST="${P_LIST}${P_N}|" 
               fi
            fi
         elif [ "$M" != "" ]; then

           for ii in $M
             do
              MM=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ii -p 6 | sed 's/,/ /g' | grep -v "=" )
              NN=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ii -p 10  )
              if [ "$MM" = "" ] && [ "$NN" != "" ]; then
               LIST="${LIST}${NN}," 

               if [ "$POS" != "" ]; then 
                  NNN=$NN
                  iii=$ii
                  p_list
                  if [ "$P_N" != "" ]; then 
                  P_LIST="${P_LIST}${P_N}|"
                  fi 
               fi
              elif [ "$MM" != "" ]; then

             for iil in $MM
             do
              MML=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $iil -p 6 | sed 's/,/ /g' | grep -v "=")
              NNL=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $iil -p 10  )
              if [ "$MML" = "" ] && [ "$NNL" != "" ]; then
               LIST="${LIST}${NN}," 

               if [ "$POS" != "" ]; then 
                  NNN=$NNL
                  iii=$iil
                  p_list
                  if [ "$P_N" != "" ]; then 
                  P_LIST="${P_LIST}${P_N}|"
                  fi 
               fi

              fi
           done




              fi
           done
         fi
       done
  if [ "$POS" != "" ] && [ "$ACTION_1" = "" ]; then 
   echo $P_LIST 
  elif [ "$POS" != "" ]; then
   echo $P_LIST  | sed 's/|//g'
  else
   echo $LIST
  fi

elif [ "$ACTION" = "get_test_suites_list" ]; then

     LIST=""
     L=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ID -p 6 | sed 's/,/ /g' | grep -v "=" )
     #echo $L
     for i in $L
      do
         M=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $i -p 6 | sed 's/,/ /g' | grep -v "=" )
         N=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $i -p 11  )
         if [ "$M" = "" ] && [ "$N" != "" ]; then
            LIST="${LIST}${N}," 
         elif [ "$M" != "" ]; then
           for ii in $M
             do
              MM=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ii -p 6 | sed 's/,/ /g' | grep -v "=" )
              NN=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ii -p 11  )
              if [ "$MM" = "" ] && [ "$NN" != "" ]; then
               LIST="${LIST}${NN}," 
              fi
           done
         fi
       done
echo $LIST
elif [ "$ACTION" = "url" ]; then
     U=$($CI_DATA -f $CI_DATA_FILE -a 'read' -i $ID -p 10  )
     echo ${U}
    
elif [ "$ACTION" = "refreshing" ]; then
       echo "# $(date '+%Y-%m-%d %H:%M:%S') #	$0:	refreshing "
L=$(ci_data.sh -f $CI_DATA_FILE -a read -i $ID -p 2 | tr , " " )
for i in $L
do
CL=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 18 )
    echo "# $(date '+%Y-%m-%d %H:%M:%S') #	$0:	CL = $CL "
if [ "$CL" = "0" ] &&  [ "$CACHE" != "" ]; then
   PRODNUMBER=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 2 | tr / _ | awk '{print $1}' )
   EXTENSION=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 15 | awk '{print $1 }')
       echo "# $(date '+%Y-%m-%d %H:%M:%S') #	$0:	PRODNUMBER = $PRODNUMBER EXTENSION = $EXTENSION"
   UPP=$(ls ${CACHE}/ | grep ${PRODNUMBER})
   if [ "$UPP" != "" ]; then
       UP=$(ls ${CACHE}/*${PRODNUMBER}*.${EXTENSION})
       echo "# $(date '+%Y-%m-%d %H:%M:%S') #	$0:	UP = $UP "
      # XML=$(echo ${PRODNUMBER}   | tr '[:upper:]' '[:lower:]')
      # REV=$(cat ${CACHE}/${XML}*.xml |grep ${PRODNUMBER} | awk '{print $4}' | cut -f2 -d\")
      #
       echo "# $(date '+%Y-%m-%d %H:%M:%S') #	$0:	ci_data.sh -f  $CI_DATA_FILE -a write -i $i -c $UP -p 13 "
       ci_data.sh -f  $CI_DATA_FILE -a write -i $i -c $UP -p 13
   else

     $MANAGE3PP  -i $i -f $CI_DATA_FILE  -c $CL
   fi

elif [ "$CL" != "" ]; then
  echo "$MANAGE3PP  -i $i -f $CI_DATA_FILE  -c $CL "
  $MANAGE3PP  -i $i -f $CI_DATA_FILE  -c $CL
else
  $MANAGE3PP  -i $i -f $CI_DATA_FILE  
fi

done
elif [ "$ACTION" = "update_radiator" ]; then
  mkdir -p ${RADIATOR_ROOT}/.product_dir
  LIST=$(echo $ID | sed 's/,/ /g')
  for I in $LIST
  do
    L=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 8 | sed 's/,/ /g' )
    N=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 7 | sed 's/ /-/g' )
    U=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 12)
    PA=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 13)
    P=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 2 | sed 's/\//_/g')
    R=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 3)
    IB_Flow=$(ci_data.sh -f $CI_DATA_FILE -a read -i $I -p 11)
    TRACK=$(basename $IB_Flow )
    PROJECT=$(dirname $IB_Flow )
    #echo "${I}_${N}	${P}-${R} "
    if [ "$P" != "" ] && [ "$R" != "" ]; then
    INFOBANK="http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${P}&version=${R}"
    touch ${RADIATOR_ROOT}/.product_dir/${P}-${R}


    if [ "$(curl -s -k ${INFOBANK} | grep ${P} | grep ${R})" = "" ]; then 
      if [ "$U" != "" ]; then
        echo "<a href=\"${U}\"> <font color=blue>${R}</font></a> " > ${RADIATOR_ROOT}/${I}_${N}
      else
        echo "<a href=\"file://${PA}\"> <font color=blue>${R}</font></a> " > ${RADIATOR_ROOT}/${I}_${N}
      fi

    else

      INFOBANK1="https://rbs-g2-mia.rnd.ki.sw.ericsson.se/products/${P}/revision/${R}/"
      #echo "${RADIATOR_ROOT}/${I}_${N}	 $INFOBANK1 "
      echo "<a href=\"${INFOBANK1}\"> <font color=blue>${R}</font></a> " > ${RADIATOR_ROOT}/${I}_${N}
      CL_F=${RADIATOR_ROOT}/CL_${I}_${N}
      update_cl
    fi
    fi
    for i in $L
    do
      N=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 7 | sed 's/ /-/g')
      U=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 12)
      PA=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 13)
      P=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 2 | sed 's/\//_/g')
      R=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 3)
      IB_Flow=$(ci_data.sh -f $CI_DATA_FILE -a read -i $i -p 11)
      TRACK=$(basename $IB_Flow )
      PROJECT=$(dirname $IB_Flow )
      #echo "	${i}_${N}	${P}-${R} "
      if [ "$P" != "" ] && [ "$R" != "" ]; then
      INFOBANK="http://rbs-g2-infobank.rnd.ki.sw.ericsson.se/infobank/rest/v2/product/revision?product_number=${P}&version=${R}"
      touch ${RADIATOR_ROOT}/.product_dir/${P}-${R}
      if [ "$(curl -s -k ${INFOBANK} | grep ${P} | grep ${R})" = "" ]; then 
        if [ "$U" != "" ]; then
          echo "<a href=\"${U}\"> <font color=blue>${R}</font></a> " > ${RADIATOR_ROOT}/${i}_${N}
        else
          echo "<a href=\"file://${PA}\"> <font color=blue>${R}</font></a> " > ${RADIATOR_ROOT}/${i}_${N}
        fi 

      else

        INFOBANK1="https://rbs-g2-mia.rnd.ki.sw.ericsson.se/products/${P}/revision/${R}/"
        #echo "${RADIATOR_ROOT}/${i}_${N}	 $INFOBANK1 "
        echo "<a href=\"${INFOBANK1}\"> <font color=blue>${R}</font></a> " > ${RADIATOR_ROOT}/${i}_${N}
        CL_F=${RADIATOR_ROOT}/CL_${i}_${N}
        update_cl
      fi
     fi
    done
  done
          
else
    echo " $ACTION ? "
    exit 1
fi

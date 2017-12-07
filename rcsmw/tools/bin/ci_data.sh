#!/bin/bash


LOCKTIMEOUT=60
Exit=0

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    rm -f ${CI_DATA_FILE}.lock
    local exit_line=$1
    local exit_code=$2   
    if [ "${exit_code}" = "0" ]
    then
	exit 0
    else
	echo "Exit [${exit_code}] triggered by line [${exit_line}]"
	exit ${exit_code}
    fi
}

# Catch exit's
trap 'trap_handler ${LINENO} $?' 0 1 2 3 15

set -e







usage()
{


echo "
Usage:
$0 -f file -a read|info|name|content [-i id] [-p pos] 
$0 -f file -a write -i id -p pos -c items

example:
cat file
1|CSX10179|||RBSCS|2,3|13161-|1095-|DURA/G2/main|RCS Source System (G2)
2|CNX9013327|||RCSEE|5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27|13161-|1095-|DURA/G2/main|MW SW Block (G2)
3|CNX9013431|||RCSMW|4|13161-|1095-|DURA/G2/main|MW SW Block (G2)

$0 -f file -a read -i 2 -p  2
CNX9013327
$0 -f file -a write -i 2 -p  3 -c R1A01
2|CNX9013327|R1A01||RCSEE|5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27|13161-|1095-|DURA/G2/main|MW SW Block (G2)
"



}
######################################################################################
write()
{
 sed -i 's/\[/%bracket%/g' $CI_DATA_FILE
 ROW=$(cat $CI_DATA_FILE | grep ^"${ID}|")
 N=$(echo "$ROW" | awk -F\| '{print NF - 1 }'  )

 if [ "$POS" = "new" ]; then
   POS=$( expr $N + 1 )
   info
 fi


 P=$(echo "$ROW" | cut -f${POS} -d\| )
#echo $P
#echo $N
 B=$(echo "$ROW" | rev | cut -f$( expr 3 + $( expr $N - $POS ) )- -d\| | rev )
 A=$(echo "$ROW" | cut -f$( expr $POS + 1 )- -d\| )
 if [ "$B" = "" ]; then
   NEW_ROW="${COLUMN}|${A}"
 elif [ "$INFO" != "" ]; then
   NEW_ROW="${B}|${COLUMN}|${INFO}"
 elif [ "$A" = "" ]; then
   NEW_ROW="${B}|${COLUMN}"
 else
   NEW_ROW="${B}|${COLUMN}|${A}"
 fi
 sed -i "s¤${ROW}¤${NEW_ROW}¤" $CI_DATA_FILE
 sed -i 's/%bracket%/\[/g' $CI_DATA_FILE
 read
}
######################################################################################
read()
{
if [ "$ID" = "" ] && [ "$POS" = "" ]; then
  cat $CI_DATA_FILE | grep -v "#" | awk 'NF'
elif [ "$POS" = "" ]; then
  cat $CI_DATA_FILE | grep ^"${ID}|"
elif [ "$ID" = "" ] && [ "$POS" != "" ]; then
  cat $CI_DATA_FILE | grep -v "#" | cut -f${POS} -d\| | awk 'NF'
else
#cat $CI_DATA_FILE | grep ^"${ID}|" | cut -f${POS} -d\|
  data=''
  DATA="`cat $CI_DATA_FILE | grep ^"${ID}|" | cut -f${POS} -d\|`"
  read_data
fi
}



######################################################################################
read_data()
{
case "$DATA" in
      *'<'*':'*'>'*) 
 		  for WORD in ""$DATA
  		  do
    		    case "$WORD" in
 			'<'*':'*'>') 
 				   id=`echo $WORD | tr -d '<>' | cut -f1 -d:`
 				   pos=`echo $WORD | tr -d '<>' | cut -f2 -d:`
                                   DATA=`cat $CI_DATA_FILE | grep '^'${id}'|' | cut -f${pos} -d\|`
				   if [ "${data}" != "" ]; then
				     data="${data} $(read_data)"
				   else
				     data=$(read_data)
				   fi;;
 				   #data="${data} `cat $CI_DATA_FILE | grep '^'${id}'|' | cut -f${pos} -d\|`";;
  			*)	   data="${data} ${WORD}";;
  		    esac
 		  done
 		  echo "${data}";;
  	*)	  echo "${DATA}";;
  esac
}

######################################################################################
info()
{
  INFO=$(grep ^"${ID}|" $CI_DATA_FILE | awk -F\| '{print $NF}')
  echo $INFO
}
######################################################################################
#   MAIN
######################################################################################
while getopts a:i:p:f:c:ht: option
do
	case "$option"
	in
                a) ACTION=$OPTARG
                  ;;
                i) ID=$OPTARG
                  ;;
                p) POS=$OPTARG
                  ;;
                c) COLUMN=$OPTARG
                  ;;
                f) CI_DATA_FILE=$OPTARG
                  ;;
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done
######################################################################################

if [ -f $CI_DATA_FILE ]; then
 if  ! lockfile -r $LOCKTIMEOUT ${CI_DATA_FILE}.lock ; then
   exit 1
 fi
fi
######################################################################################

if [ "$ACTION" = "read" ]; then
     read
elif [ "$ACTION" = "write" ]; then
     write
elif [ "$ACTION" = "info" ]; then
     info
elif [ "$ACTION" = "content" ]; then
     POS=6
     read
elif [ "$ACTION" = "name" ]; then
     POS=5
     read 
else
    echo " $ACTION ? "
    exit 1
fi

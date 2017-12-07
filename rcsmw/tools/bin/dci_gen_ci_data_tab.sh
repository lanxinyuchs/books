#!/bin/bash

CIDATA=$1
CIDATA_HTML=$( echo $CIDATA | cut -f1 -d. ).html
if ! which ci_data.sh > /dev/null 2>&1; then
  ExecProjName RCSDE "RCSMW/1.0" "git archive --remote=ssh://gerrit.ericsson.se:29418/rcsmw/rcsmw.git HEAD tools/bin/ci_data.sh | tar -x " 
  CIDATA_SH=$(pwd)/tools/bin/ci_data.sh
  ls -l $CIDATA_SH
else
  CIDATA_SH=ci_data.sh
fi
 echo "
" > $CIDATA_HTML
NT=$(cat $CIDATA | wc -l | awk '{print $1}' )
ID_LIST=$(cat $CIDATA | grep -n "#" | grep "Id|" | cut -f1 -d: )
echo "ID_LIST=$(echo $ID_LIST | sed 's/\n/ /g')"
for I in $ID_LIST
do
NO=$( cat $CIDATA | head -$( expr $I - 1 ) | tail -1  | sed 's/|/ /g' | sed 's/#//' )
NA=$( cat $CIDATA | head -$I | tail -1 | sed 's/|/ /g' | sed 's/#//' )

echo $NO
echo $NA
N=1
echo "
<!DOCTYPE html>
<html>
<head>
<title>HTML Tables</title>
</head>
<body>
<table width=\"%95\" border=\"1\">
 " >> $CIDATA_HTML
echo "<tr> " >> $CIDATA_HTML
################################################################################

for i in $NA
do
echo "<th> ${N} ${i}</th> " >> $CIDATA_HTML
N=$( expr $N + 1 )
done

 echo "</tr> " >> $CIDATA_HTML
T=$( expr $NT - $I  )
echo "##################################################"
NEXT_ID=$(cat $CIDATA | tail -${T} | grep -n "#" | grep "Id|" | cut -f1 -d:  | head -1 )
if [ "$NEXT_ID" != "" ]; then
  ID_LIST_1=$(cat $CIDATA | tail -${T} | head -${NEXT_ID} | grep -v "#" | grep "|" | cut -f1 -d\|)
else
  ID_LIST_1=$(cat $CIDATA | tail -${T} | grep -v "#" | grep "|" | cut -f1 -d\|)
fi
NN=1
echo "NEXT_ID=$NEXT_ID	T=$T "
echo "ID_LIST_1=$(echo $ID_LIST_1 | sed 's/\n/ /g')"
for J in $ID_LIST_1
do
#echo "2	J	$J"
echo "<tr> " >> $CIDATA_HTML
for j in $NO 
do
#echo "3	j	$j"
  C=$($CIDATA_SH -a read -f $CIDATA -i $J -p $j )
 
  if [ "$C" = "" ]  || [ "$C" = " " ]; then
     C="-"
  fi

  echo "<td>${C}</td> " >> $CIDATA_HTML
done 
echo "</tr> " >> $CIDATA_HTML
done

echo "  
</table>
</body>
</html>
 " >> $CIDATA_HTML


################################################################################

done


exit

echo "
"


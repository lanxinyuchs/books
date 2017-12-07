#!/bin/bash


CIDATA=$1
TOP=$2
LSV=$3
RADIATOR_ROOT=$4
SUB=$(ci_data.sh -f $CIDATA -a read -i $TOP -p 4 | sed 's/,/ /g')
HOVER=$(ci_data.sh -f $CIDATA -a read -i $TOP -p 3  | sed 's/,/ /g')
NAME=$(ci_data.sh -f $CIDATA -a read -i $TOP -p 2  | sed 's/,/ /g')
FILE=$(ci_data.sh -f $CIDATA -a read -i $TOP -p 5  )
HEAD=""
PHEAD=""
HHEAD=""
SUBHEAD=""
HSUBHEAD=""
HDATA=""
DATA=""
for i in $SUB ; do
 S=$(ci_data.sh -f $CIDATA -a read -i $i -p 4 | sed 's/,/ /g')
 H=$(ci_data.sh -f $CIDATA -a read -i $i -p 3 | sed 's/= /=/g' | sed 's/ \&/\&/g' )
 N=$(ci_data.sh -f $CIDATA -a read -i $i -p 2 | sed 's/,/ /g' | sed 's/= /=/g' | sed 's/ \&/\&/g')
P=$(echo $S | wc -w | awk '{print $1}')
PHEAD="$PHEAD | $P "
HHEAD="$HHEAD | $H "
HEAD="$HEAD | $N "

for j in $S ; do

SUBHEAD="$SUBHEAD | $(ci_data.sh -f $CIDATA -a read -i $j -p 2) "
HSUBHEAD="$HSUBHEAD | $(ci_data.sh -f $CIDATA -a read -i $j -p 3) "
DT=$(ci_data.sh -f $CIDATA -a read -i $j -p 4)
DH=$(ci_data.sh -f $CIDATA -a read -i $j -p 5)
if [ "$DT" != "" ]; then
T=$(echo $DT | sed 's/ //g' )
else
T=""
fi 
DATA="$DATA | ${T} "
HDATA="$HDATA | ${DH} "
done

done 

echo $HEAD > radiator_row.txt
echo $HHEAD >> radiator_row.txt
echo $PHEAD >> radiator_row.txt
echo $SUBHEAD >> radiator_row.txt
echo $HSUBHEAD >> radiator_row.txt
echo $DATA >> radiator_row.txt
echo $HDATA >> radiator_row.txt
if [ "$USER" = "rcsci1" ] && [ "$FILE" != "" ]; then
 echo "<a href=https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/edit_comments.pl?${RADIATOR_ROOT}/${LSV}/comments.txt>edit</a>" > ${RADIATOR_ROOT}/${LSV}/edit_comments
 cat radiator_row.txt > ${RADIATOR_ROOT}/${LSV}/$FILE
else
echo "---------------------------------------------------------------------------------------------------"
echo $PHEAD
echo "---------------------------------------------------------------------------------------------------"
echo $HEAD
echo $HHEAD
echo "---------------------------------------------------------------------------------------------------"
echo $SUBHEAD
echo $HSUBHEAD
echo "---------------------------------------------------------------------------------------------------"
echo $DATA
echo $HDATA
echo "---------------------------------------------------------------------------------------------------"
fi 



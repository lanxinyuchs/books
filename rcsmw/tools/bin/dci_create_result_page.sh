#!/bin/bash 

usage()
{
echo " Create files <filename>.html and <filename> in dir <resultdir>
$0  -N <node> -R <Rev> -S <testspec> -L <resulttdir> -F <filename> [-T <FlowJobUrl> ]
https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?1+NISSE+master+sbc_result
ex:
$0  -N rcf1013 -R P1A78 -S sbc_rcf_spec_9 -L /proj/webdocs/rbs-rde-ci/root/NISSE_ciresults_branch/master/1 -F sbc_result_9
$0  -N rcf* -R P1A78 -S sbc_rcf_spec_* -L /proj/webdocs/rbs-rde-ci/root/NISSE_ciresults_branch/master/1 -F sbc_result
$0  -N rcf* -R P1A78 -S sbc_rcf_spec_* -L /proj/webdocs/rbs-rde-ci/root/NISSE_ciresults_branch/master/1 -F sbc_result -T https://fem024-eiffel002.rnd.ki.sw.ericsson.se:8443/jenkins/job/vrcsmw-5G-main-sbc-buildFlow/87/console

cat /proj/webdocs/rbs-rde-ci/root/NISSE_ciresults_branch/master/1/sbc_result
<a href=https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?1+NISSE+master+sbc_result> <font color=blue> 1</font></a> 


"
}
H="https://rbs-rde.rnd.ki.sw.ericsson.se"
PROJ="/proj/rcs-tmp/stps/"

while getopts ht:N:R:S:L:F:T:dt: option
do
        case "$option"
        in
                 F) FILE=$OPTARG #  
                   ;;              
                L) LSV_DIR=$OPTARG #  
                   ;;  
                N) NODE=$OPTARG #  
                   ;;  
                R) REV=$OPTARG #  
                   ;;  
                S) SPEC=$OPTARG #  
                   ;;   
                T) TOP_FLOW_JOB=$OPTARG #  
                   ;; 
                d) debug=yes #  
                   ;;                     
                h) usage
                  exit;; 
                \?) usage
                exit;; 
        esac
done


if [ -n "${debug}" ]
then
    set -x
fi
Jenkins_head=""
Jenkins_tail=""
rm -f .testExec.txt
LSV_NO=$(basename $LSV_DIR )
BRANCH=$(basename $(dirname $LSV_DIR ) )
TYPE=$(echo $(basename $(dirname $(dirname $LSV_DIR ) ) ) | cut -f1 -d_ )
rm -f $LSV_DIR/${FILE}.html
if [ "$TOP_FLOW_JOB" != "" ]; then
 J_URL="https://$(echo $TOP_FLOW_JOB | cut -f3 -d\/)"
 Tduration=$(curl -s $( dirname $TOP_FLOW_JOB)/api/json?pretty=true | grep "duration" | awk '{print $3}' | cut -f1 -d\,)
 if [ "$Tduration" = "0" ]; then

    Ttimestamp=$(curl -s $( dirname $TOP_FLOW_JOB)/api/json?pretty=true | grep "timestamp" | awk '{print $3}' | cut -f1 -d\, |  head -1)

    Tduration=$( expr $( expr $( date +"%s" | awk '{print $1}' ) \* 1000 ) - $Ttimestamp )
 fi

 TESTEXEC=$(curl -s $( dirname $TOP_FLOW_JOB)/logText/progressiveText | grep Build | grep started | grep testExec | sed 's/#//g' | awk '{print "/jenkins/job/"$3"/"$4"/"}' )
if [ "$TESTEXEC" = "" ]; then

 TESTEXEC=$(curl -s $( dirname $TOP_FLOW_JOB)/logText/progressiveText | grep "completed. Result" | grep testExec | sed 's/#//g' | awk '{print "/jenkins/job/"$1"/"$2"/"}' )
fi
 TTduration=$( expr $Tduration / 60000  )
echo $TESTEXEC
 echo "<th ><b>  <a href="$TOP_FLOW_JOB"> FLOW JOB $TTduration min </a> </b></th> 
<br /><br />
<tr ><b>" >> $LSV_DIR/${FILE}.html
for i in $TESTEXEC
do
LNAME=$(basename $i)
result=$(curl -s ${J_URL}${i}api/json?pretty=true | grep '"result"' | awk '{print $3}' | cut -f1 -d\, | cut -f2 -d\")
duration=$(curl -s ${J_URL}${i}api/json?pretty=true | grep "duration" | awk '{print $3}' | cut -f1 -d\,)
TSPEC=$(curl -s ${J_URL}${i}api/json?pretty=true | grep -A1 "TEST_SPEC" | grep value | cut -f4 -d\")
echo $i $TSPEC $duration
if [ "$TSPEC" = "" ]; then

TSPEC=$(curl -s ${J_URL}${i}/logText/progressiveText | grep "TEST_SPEC\=" | cut -f2 -d\=   | head -1 | awk '{print $1}' | tr -d '\015')
echo "-------------------------------------------"
echo "#${TSPEC}#  $result ${J_URL}${i}api/json?pretty=true"
echo "-------------------------------------------"
fi
echo $i $TSPEC $duration
case "$result"
        in
                 FAILURE) COLOR=red #  
                   ;;              
                 SUCCESS) COLOR=green #  
                   ;;  
                 UNSTABLE) COLOR=orange # 
                   ;;  
       esac
echo "$TSPEC ${J_URL}${i}console  $LNAME  $( expr $duration / 60000  ) $COLOR" >> .testExec.txt

done

 Jenkins_head="<th>Jenkins console </th><th>duration (min)</th>"
 Jenkins_tail="<th> </th><th> </th>"
fi

echo "<!DOCTYPE html>
<html>
<head>
<title>HTML Table Header</title>
</head>
<body>
<table border=\"1\">
<tr >
<th>Testspec</th>
<th>Ok</th>
<th>Failed</th>
<th>Skipped(User/Auto)</th>
$Jenkins_head
</tr>" >> $LSV_DIR/${FILE}.html
rm -f $LSV_DIR/.${FILE}
echo "${PROJ}/${NODE}/${REV}/$SPEC/config_${SPEC}/index.html"

#SPECLISTS=$(ls ${PROJ}/${NODE}/${REV}/$SPEC/config_${SPEC}/index.html)

SPECLISTS=$(ls -d ${PROJ}/${NODE}/${REV}/$SPEC/config_${SPEC}| sed 's/\/config_/ config_/g' | awk '{print $2}' | sort -u | sort -n )
TTOk=0
TTFailed=0
TTSkipped=0
TTSkippedU=0
TTSkippedA=0
for i in $SPECLISTS
do
spec=$(echo $i | cut -f2- -d_)
echo "###################################################"
echo $spec
echo "###################################################"
if [ "$(ls ${PROJ}/${NODE}/${REV}/$SPEC/$i/ | grep index.html)" != "" ]; then
PP=$(ls -tr ${PROJ}/${NODE}/${REV}/$SPEC/$i/index.html)
HS=""
HN=$(echo $PP | wc -w | awk '{print $1}')
if [ "$HN" = "1" ]; then
  OLD_P=$(echo $PP | sed 's/index.html/index_1.html/' )
  if [ -f $OLD_P ]; then
     N=$(basename $(dirname $OLD_P) )
     L="<tr><td><a href=\"${H}${OLD_P}\" <font color=black>${N}</font> </a></td>"
     F=$(cat $OLD_P | grep -A9 "<td><b>Total</b></td>" | grep -v  "<td><b>Total</b></td>" )
     Ok=$(echo "$F" | head -3 | tail -1 | sed 's/td/th/g')
     Failed=$(echo "$F" | head -4 | tail -1 | sed 's/td/th/g')
     Skipped=$(echo "$F" | head -5 | tail -1 | sed 's/td/th/g')
     Jenkins=""
     if [ "$TOP_FLOW_JOB" != "" ]; then
     JURL=$(cat .testExec.txt | grep "$spec " | awk '{print $2}' | head -1)
     JNUMBER=$(cat .testExec.txt | grep "$spec " | awk '{print $3}' | head -1)
     COLOR=$(cat .testExec.txt | grep "$spec " | awk '{print $5}' | head -1)
     DURATION=$(cat .testExec.txt | grep "$spec " | awk '{print $4}' | head -1)
     Jenkins="<th><a href=\"$JURL \"> <font color=$COLOR> #$JNUMBER</a>  </b></th><th>$DURATION</th></th>"
     fi
     HS="${HS}${L}${Ok}${Failed}${Skipped}${Jenkins}</tr>" 
  fi
fi
for P in $PP
do
Jenkins=""
N=$(basename $(dirname $P) )
L="<tr><td><a href=\"${H}${P}\" <font color=black>${N}</font> </a></td>"
F=$(cat $P | grep -A9 "<td><b>Total</b></td>" | grep -v  "<td><b>Total</b></td>" )
Ok=$(echo "$F" | head -3 | tail -1 | sed 's/td/th/g')
Failed=$(echo "$F" | head -4 | tail -1 | sed 's/td/th/g')
Skipped=$(echo "$F" | head -5 | tail -1 | sed 's/td/th/g')

#echo " ${TTOk} ${TTFailed} $TTSkipped ($TTSkippedU/$TTSkippedA)"
if [ "$HN" != "1" ]; then
  if [ "$TOP_FLOW_JOB" != "" ]; then
     JURL=$(cat .testExec.txt | grep "$spec " | awk '{print $2}' | head -1)
     JNUMBER=$(cat .testExec.txt | grep "$spec " | awk '{print $3}' | head -1)
     COLOR=$(cat .testExec.txt | grep "$spec " | awk '{print $5}' | head -1)
     DURATION=$(cat .testExec.txt | grep "$spec " | awk '{print $4}' | head -1)
     Jenkins="<th><a href=\"$JURL \"> <font color=$COLOR> #$JNUMBER</a>  </b></th><th>$DURATION</th></th>"
  fi
  HS="${HS}${L}${Ok}${Failed}${Skipped}${Jenkins}</tr>" 
fi
HN=$( expr $HN - 1 )
done
############################################################################################
if [ "$HS" != "" ]; then
echo "$HS" >> $LSV_DIR/.${FILE}
fi
############################################################################################
Jenkins=""
 if [ "$TOP_FLOW_JOB" != "" ]; then
    JURL=$(cat .testExec.txt | grep "$spec " | awk '{print $2}' | tail -1)
     JNUMBER=$(cat .testExec.txt | grep "$spec " | awk '{print $3}' | tail -1)
     COLOR=$(cat .testExec.txt | grep "$spec " | awk '{print $5}' | tail -1)
     DURATION=$(cat .testExec.txt | grep "$spec " | awk '{print $4}' | tail -1)
     Jenkins="<th><a href=\"$JURL \"> <font color=$COLOR> #$JNUMBER</a>  </b></th><th>$DURATION</th></th>"

  fi

echo "${L}${Ok}${Failed}${Skipped}${Jenkins}</tr>" >> $LSV_DIR/${FILE}.html
TOk=$(echo $Ok  | cut -f3- -d\> | cut -f1 -d\< )
TFailed=$(echo $Failed  | cut -f3- -d\> | cut -f1 -d\< )
TSkipped=$(echo $Skipped | cut -f2- -d\> | awk '{print $1}')
TSkippedU=$(echo $Skipped  | cut -f2- -d\(  | cut -f1 -d\/ )
TSkippedA=$(echo $Skipped  | cut -f2- -d\/ | cut -f1 -d\) )
TTOk=$( expr $TTOk + $TOk )

TTFailed=$( expr $TTFailed + $TFailed )
TTSkipped=$( expr $TTSkipped + $TSkipped )
TTSkippedU=$( expr $TTSkippedU + $TSkippedU )
TTSkippedA=$( expr $TTSkippedA + $TSkippedA )
fi
done

echo "<tr >
<th>Total</th>
<th align=right>$TTOk</th>
<th align=right>$TTFailed</th>
<th align=right>$TTSkipped ($TTSkippedU/$TTSkippedA)</th>
$Jenkins_tail
</tr>" >> $LSV_DIR/${FILE}.html
echo "</table>
</body>
</html>" >> $LSV_DIR/${FILE}.html
if [ "$(cat $LSV_DIR/${FILE}.html | grep "color=red" )" != "" ]; then

      COLOR_MASTER=red
fi
############################################################################################
if [ -f $LSV_DIR/.${FILE} ]; then
echo "
<br /><br />
<tr ><b>
NON PASSED AND LATER RERUN TESTS 
</b></tr>


<head>
<title>HTML Table Header</title>
</head>
<body>
<table border=\"1\">

<tr >
<th>Testspec</th>
<th>Ok</th>
<th>Failed</th>
<th>Skipped(User/Auto)</th>
$Jenkins_head
</tr>" >> $LSV_DIR/${FILE}.html
cat $LSV_DIR/.${FILE} >> $LSV_DIR/${FILE}.html

echo "</table>
</body> " >> $LSV_DIR/${FILE}.html
fi
############################################################################################
if [ "$TTOk" = "0" ] && [ "$TTFailed" = "0" ] && [ "$TTSkipped" = "0" ]; then
echo "<a href="https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${LSV_NO}+${TYPE}+${BRANCH}+${FILE}"> <font color=red> 0</font></a> " > $LSV_DIR/${FILE}
elif [ "$TTOk" != "0" ] && [ "$TTFailed" = "0" ] && [ "$TTSkipped" = "0" ] && [ -f $LSV_DIR/.${FILE} ] && [ "$COLOR_MASTER" = "red" ]; then
echo "<a href="https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${LSV_NO}+${TYPE}+${BRANCH}+${FILE}"> <font color=red> 100</font></a> " > $LSV_DIR/${FILE}
elif [ "$TTOk" != "0" ] && [ "$TTFailed" = "0" ] && [ "$TTSkipped" = "0" ] && [ -f $LSV_DIR/.${FILE} ]; then
echo "<a href="https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${LSV_NO}+${TYPE}+${BRANCH}+${FILE}"> <font color=orange> 100</font></a> " > $LSV_DIR/${FILE}
elif [ "$TTOk" != "0" ] && [ "$TTFailed" = "0" ] && [ "$TTSkipped" = "0" ]; then
echo "<a href="https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${LSV_NO}+${TYPE}+${BRANCH}+${FILE}"> <font color=green> 100</font></a> " > $LSV_DIR/${FILE}
elif [ "$TTFailed" != "0" ] || [ "$TTSkipped" != "0" ]; then
TOT=$( expr $TTOk +  $TTFailed + $TTSkipped )
GGG=$(echo "scale = 4;$TTOk/$TOT" | bc -l)
PROC1=$(echo "scale = 4;$GGG*100" | bc -l)
PROC=$(echo $PROC1 | cut -f1 -d.)
echo "<a href="https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?${LSV_NO}+${TYPE}+${BRANCH}+${FILE}"> <font color=red> ${PROC}</font></a> " > $LSV_DIR/${FILE}


fi

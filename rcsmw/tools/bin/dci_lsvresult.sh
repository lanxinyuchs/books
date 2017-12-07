#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2014 All rights reserved.
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
##
#############################################################
##
##                NOTE!!!!!!!
##  The original file is stored in /vobs/rcs/test/RCT_CRX901275/test/bin 
##  Only do changes there and copy to /proj/rcs/bin/ directory.
##
#############################################################
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R2A/1      2012-        etxjovp     Created
## R2A/5      12-06-12     etxjovp     add support for statistical graph
## R2A/6      12-10-02     etxjovp     add Troubleshooting
## R2A/7      12-10-02     etxjovp     bugfix
## R2A/9      12-10-30     etxjovp     add view for modified UP tester
## R2A/12     12-12-20     etxjovp     add Arm links
## R2A/13     14-01-13     etxjovp     mod ls in main loop
## R2A/13     14-02-19     etxjovp     Add Delivered in

#############################################################
print_table()
{
    TFAILED="0"
    TSKIPPED="0"
    TTOTAL="0"
    TQtime="0"
    TEtime="0"
    TTtime="0"
    COLOR="#E5E5E5"
    COLOR1="#E5E5E5"
    SST="-"
    SPECDIR=""

    if [ "$(ls $j )" != "" ]; then
       SPECDIR=$(ls  $j/*/*)
       if [ -f $RESULTDIR/$LSV_NO/$(basename $j).new_state ]; then
          if [ -f $RESULTDIR/$LSV_NO/$(basename $j).proc ]; then
            N_P=$(cat $RESULTDIR/$LSV_NO/$(basename $j).new_state | sed 's/color=/#/' | cut -f2 -d# | cut -f2 -d\> | cut -f1 -d\<)
            O_P=$(cat $RESULTDIR/$LSV_NO/$(basename $j).proc)
            if [ "$N_P" != "$O_P" ]; then
               #NEW_STATE=$(cat $RESULTDIR/$LSV_NO/$(basename $j).new_state |  sed 's/>'${N_P}'</>'${O_P}'</') 
               if [ "$USER" = "rcsci1" ]; then
                  echo $NEW_STATE
                # echo $NEW_STATE > $RESULTDIR/$LSV_NO/$(basename $j).new_state
               else
                 echo $NEW_STATE >> new_state.txt
                 cat $RESULTDIR/$LSV_NO/$(basename $j).new_state >> new_state.txt
               fi
           fi 
        fi
        CO=$(cat $RESULTDIR/$LSV_NO/$(basename $j).new_state | cut -f3 -d= | cut -f1 -d\> )
        if [ "$CO" = "green" ]; then
          COLOR="#E5FFE5"
        elif [ "$CO" = "red" ]; then
          COLOR="#FFE5E5"
        elif [ "$CO" = "blue" ]; then
          COLOR="#E5E5FF"
        elif [ "$CO" = "orange" ]; then
          COLOR="#FFF5E5"
        fi
        SST=$(cat $RESULTDIR/$LSV_NO/$(basename $j).new_state)
     fi
#---------------------------------------------------------------------------------------
     if [ -f $RESULTDIR/$LSV_NO/SBC_${TEST_TYPE} ]; then
        CI_ID=$(cat $RESULTDIR/$LSV_NO/SBC_${TEST_TYPE} | grep "$(basename $j)," | cut -f2 -d, )
        if [ "$CI_ID" != "" ]; then
           TE_ST=$(grep ^"${CI_ID}|" $RESULTDIR/$LSV_NO/CIdata.txt | awk -F\| '{print $NF}' )       
        elif [ -f $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE} ]; then
          CI_ID=$(cat $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE} | grep "$(basename $j)," | cut -f2 -d, )
          if [ "$CI_ID" != "" ]; then
           TE_ST=$(grep ^"${CI_ID}|" $RESULTDIR/$LSV_NO/CIdata.txt | awk -F\| '{print $NF}' )
          else
           TE_ST=$(basename $j)
          fi
       else
         TE_ST=$(basename $j)
       fi
     else
      TE_ST=$(basename $j)
    fi
  echo "<th bgcolor=\"${COLOR1}\" colspan=\"8\"><b> $TE_ST $SST </b></th>"
  for i in $SPECDIR
  do
    b=$(basename $i)
    spec=$(echo "$i" | sed 's/'$b'//g')
    s=$(basename $spec)
    ss="\/${s}"
    activity=$(echo "$spec" | sed 's/'${ss}'//g')
    a=$(basename $activity)   
    echo "<tr>"

##############################################################################################  
   Qtime="-"
   Etime="-"
   Ttime="-"  
   if [ -f $lsvnodir/$a.start ] && [ -f $lsvnodir/$b.stop ] ; then
       Ttime="$( expr $( expr $(cat $lsvnodir/$b.stop ) - $(cat $lsvnodir/$a.start ) ) / 60 )"
      # echo $Ttime
       if [ 0 -gt "$Ttime" ]; then
          Ttime="-"
       fi
    else
        Ttime="-"
    fi
    if [ -f $lsvnodir/$b.start ] && [ -f $lsvnodir/$b.stop ] && [ "$Ttime" != "-" ]; then
       Etime="$( expr $( expr $(cat $lsvnodir/$b.stop ) - $(cat $lsvnodir/$b.start ) ) / 60 )"
       #echo $Etime
       if [ 0 -gt "$Etime" ]; then
          Etime="-"
       fi
    else
        Etime="-"
    fi
    if [ -f $lsvnodir/$a.start ] && [ -f $lsvnodir/$b.start ] && [ "$Ttime" != "-" ]; then
       Qtime="$( expr $( expr $(cat $lsvnodir/$b.start ) - $(cat $lsvnodir/$a.start ) ) / 60 )"
      # echo $Qtime
       if [ 0 -gt "$Qtime" ]; then
          Qtime=$( expr $( expr $(date +"%s") - $(cat $lsvnodir/$a.start ) ) / 60 )
       fi
    else
        Qtime="-"
    fi
    STP=$(cat $i | cut -f7 -d\/)
     
    echo "<td bgcolor=\"${COLOR}\">  <a href=\"https://rbs-rde-ci.rnd.ki.sw.ericsson.se:443/computer/${STP}\"<font color=black>${STP}</font></a></td>"

############################################################################################## 
   
   TEST_CONFIG_PLOT="$(cat $i | cut -f4- -d: | cut -f4 -d\/)_plot"
   if [ -f $lsvnodir/comments_$(basename $i).txt ]; then
     EDITCOLOR=red
     COMMENTS=$(cat -vE $lsvnodir/comments_$(basename $i).txt | sed 's/\"/\&quot\;/g' | sed 's/\^M\$/,\&#013\;/g' | tr '\n' ' ' )
     #COMMENTS=$(tr '\n' ' ' <$lsvnodir/comments_$(basename $i).txt)
   else
    EDITCOLOR=black
     COMMENTS="No comments"
   fi
   if [ -f ${JENKINSJOBS}/${TEST_CONFIG_PLOT}/config.xml ]; then
      if [ "$(cat ${JENKINSJOBS}/${TEST_CONFIG_PLOT}/config.xml |  grep "<disabled>false</disabled>")" != "" ]; then
         PLOT="<td> $(cat $i) <a href=\"https://rbs-rde-ci.rnd.ki.sw.ericsson.se/view/plot/job/${TEST_CONFIG_PLOT}/plot/getPlot?index=0&width=2000&height=800\"><font color=blue> plot</blink></font></a></td>"
         echo $PLOT
      else
       echo "<td TITLE=\"$COMMENTS\" bgcolor=\"${COLOR}\">$(cat $i) <a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/edit_comments.pl?$lsvnodir/comments_$(basename $i).txt\"><font color=$EDITCOLOR> edit </font> </a></td>"
      fi

   else
       echo "<td TITLE=\"$COMMENTS\" bgcolor=\"${COLOR}\">$(cat $i)   <a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/edit_comments.pl?$lsvnodir/comments_$(basename $i).txt\" ><font color=$EDITCOLOR> edit </font> </a></td>"
   fi
##############################################################################################
    FAILED="-"
    SKIPPED="-"
    TOTAL="-"
    if [ -f $lsvnodir/$b.result ]; then
       TOTAL=$(cat $lsvnodir/$b.result | awk '{print $1}')
       FAILED=$(cat $lsvnodir/$b.result | awk '{print $2}')
       SKIPPED=$(cat $lsvnodir/$b.result | awk '{print $3}')
       TTOTAL=$( expr $TTOTAL + $TOTAL )
       TFAILED=$( expr $TFAILED + $FAILED )
       if [ "$SKIPPED" = "" ]; then
          SKIPPED="0"
       fi
       TSKIPPED=$( expr $TSKIPPED + $SKIPPED )
    fi 
    if [ "$Ttime" != "-" ]; then
          TTtime=$( expr $TTtime + $Ttime )
    fi
    if [ "$Qtime" != "-" ]; then
          TQtime=$( expr $TQtime + $Qtime )
    fi
    if [ "$Etime" != "-" ]; then
          TEtime=$( expr $TEtime + $Etime )
    fi
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $FAILED </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $SKIPPED </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\" > $TOTAL </td>" 
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $Qtime </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $Etime </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $Ttime </td>"
    echo "</tr>"

done
TTTOTAL=$( expr $TTTOTAL + $TTOTAL )
TTFAILED=$( expr $TTFAILED + $TFAILED )
TTSKIPPED=$( expr $TTSKIPPED + $TSKIPPED )
TTQtime=$( expr $TTQtime + $TQtime )
TTEtime=$( expr $TTEtime + $TEtime )
TTTtime=$( expr $TTTtime + $TTtime )
echo "<td bgcolor=\"${COLOR}\" colspan=\"2\"><b> Activity results</b></td>"
PROC="-"
FFF=$( expr $TTOTAL - $TSKIPPED - $TFAILED )
if [ "$FFF" != "0" ] && [ "$TTOTAL" != "0" ]; then
GGG=$(echo "scale = 4;$FFF/$TTOTAL" | bc -l)
PROC1=$(echo "scale = 4;$GGG*100" | bc -l)
PROC=$(echo $PROC1 | cut -f1 -d.)
if [ "$PROC" = "" ]; then
PROC="0"
fi
else
PROC="0"
fi
if [ "$USER" = "rcsci1" ]; then

echo "$PROC" > $RESULTDIR/$LSV_NO/$(basename $j).proc
fi
echo "<td bgcolor=\"${COLOR}\" align=\"right\"><b> $TFAILED </b></td>"
echo "<td bgcolor=\"${COLOR}\" align=\"right\"><b> $TSKIPPED </b></td>"
echo "<td bgcolor=\"${COLOR}\" align=\"right\"><b> $TTOTAL  </b></td>"
echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $TQtime </td>"
echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $TEtime </td>"
echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $TTtime </td>"
echo "</tr>"
fi
#############################################################
}

print_table_jcat()
{
RCS_WEB="https://rbs-rde.rnd.ki.sw.ericsson.se/"
if [ "$TEST_TYPE" = "" ] || [ "$TEST_TYPE" = "no" ]; then
  TEST_REPORT_DIR="/proj/rcs-tmp/rbs_cs_ci_statistics/test_report/${LSV_NO}"
else
  TEST_REPORT_DIR="/proj/rcs-tmp/rbs_cs_ci_statistics/test_report/${TEST_TYPE}/${LSV_NO}"
fi
   
    TFAILED="0"
    TSKIPPED="0"
    TTOTAL="0"
    TQtime="0"
    TEtime="0"
    TTtime="0"
    Qtime="0"
    Etime="-"
    Ttime="-"
    STP="-"
    SST="-"
   COLOR="#E5E5E5"
   COLOR1="#E5E5E5"

   SPECDIR=$(ls  ${j}/ | grep -v ".state")
   #SPECDIR=$(cat $RESULTDIR/$LSV_NO/$(basename $j) )



#---------------------------------------------------------------------------------------
   if [ -f $RESULTDIR/$LSV_NO/$(basename $j).state ]; then
       CO=$(cat $RESULTDIR/$LSV_NO/$(basename $j).state | cut -f3 -d= | cut -f1 -d\> )
       if [ "$CO" = "green" ]; then
          COLOR="#E5FFE5"
       elif [ "$CO" = "red" ]; then
          COLOR="#FFE5E5"
       elif [ "$CO" = "blue" ]; then
          COLOR="#E5E5FF"
       elif [ "$CO" = "orange" ]; then
          COLOR="#FFF5E5"
      fi
      SST=$(cat $RESULTDIR/$LSV_NO/$(basename $j).state)
   fi
#---------------------------------------------------------------------------------------
 if [ -f $RESULTDIR/$LSV_NO/SBC_${TEST_TYPE} ]; then
        CI_ID=$(cat $RESULTDIR/$LSV_NO/SBC_${TEST_TYPE} | grep "$(basename $j)," | cut -f2 -d, )
        if [ "$CI_ID" != "" ]; then
           TE_ST=$(grep ^"${CI_ID}|" $RESULTDIR/$LSV_NO/CIdata.txt | awk -F\| '{print $NF}' )       
        elif [ -f $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE} ]; then
          CI_ID=$(cat $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE} | grep "$(basename $j)," | cut -f2 -d, )
          if [ "$CI_ID" != "" ]; then
           TE_ST=$(grep ^"${CI_ID}|" $RESULTDIR/$LSV_NO/CIdata.txt | awk -F\| '{print $NF}' )
          else
           TE_ST=$(basename $j)
          fi
       else
         TE_ST=$(basename $j)
       fi
     else
      TE_ST=$(basename $j)
    fi



#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
    echo "<th bgcolor=\"${COLOR1}\" colspan=\"8\"><b> $TE_ST $SST </b></th>"
    for m in $SPECDIR
    do
           CO=$(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}.state | cut -f3 -d= | cut -f1 -d\> )
           if [ -f $RESULTDIR/$LSV_NO/$(basename $j)/${m}/${m}.result ]; then
           FAILED=$(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}/${m}.result  | cut -f2 -d\| )
           SKIPPED=$(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}/${m}.result | cut -f3 -d\| )
           TOTAL=$(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}/${m}.result  | cut -f4 -d\| )
           Etime=$(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}/${m}.result  | cut -f5 -d\| )
           STP=$(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}/${m}.result  | cut -f6 -d\| )
           Ttime=$Etime
            else

           FAILED="0"
           SKIPPED="0"
           TOTAL="0"
           Qtime="0"
           Ttime="0"
           Etime="0"
           fi
   if [ -f $RESULTDIR/$LSV_NO/comments_$(basename $m).txt ]; then
     EDITCOLOR=red
     COMMENTS=$(cat -vE $RESULTDIR/$LSV_NO/comments_$(basename $m).txt | sed 's/\^M\$/,\&#013\;/g' | tr '\n' ' ')
     #COMMENTS=$(tr '\n' ' ' <$RESULTDIR/$LSV_NO/comments_$(basename $m).txt)
   else
    EDITCOLOR=black
     COMMENTS="No comments"
   fi
    echo "<tr>"
    echo "<td bgcolor=\"${COLOR}\"><font color=black>${STP}</font></td>"
    echo "<td TITLE=\"$COMMENTS\" bgcolor=\"${COLOR}\"><a href=\"${RCS_WEB}${TEST_REPORT_DIR}/${m}.html\" > <font color=$CO>${m}</font></a> $(cat $RESULTDIR/$LSV_NO/$(basename $j)/${m}.state ) <a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/edit_comments.pl?$lsvnodir/comments_$(basename $m).txt\"</a> <font color=$EDITCOLOR> edit </font> </a></td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $FAILED </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $SKIPPED </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\" > $TOTAL </td>" 
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $Qtime </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $Etime </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $Ttime </td>"
    echo "</tr>"
       TTOTAL=$( expr $TTOTAL + $TOTAL )
       TSKIPPED=$( expr $TSKIPPED + $SKIPPED )
       TFAILED=$( expr $TFAILED + $FAILED )
       TEtime=$( expr $TEtime + $Etime )
       TTtime=$TEtime
    done

    if [ "$SPECDIR" != "" ]; then
    echo "<td bgcolor=\"${COLOR}\" colspan=\"2\"><b> Activity results</b></td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"><b> $TFAILED </b></td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"><b> $TSKIPPED </b></td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"><b> $TTOTAL  </b></td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $TQtime </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $TEtime </td>"
    echo "<td bgcolor=\"${COLOR}\" align=\"right\"> $TTtime </td>"
    echo "</tr>"
    else
      echo "</tr>"
    fi
TTTOTAL=$( expr $TTTOTAL + $TTOTAL )
TTFAILED=$( expr $TTFAILED + $TFAILED )
TTSKIPPED=$( expr $TTSKIPPED + $TSKIPPED )
TTQtime=$( expr $TTQtime + $TQtime )
TTEtime=$( expr $TTEtime + $TEtime )
TTTtime=$( expr $TTTtime + $TTtime )

PROC="-"
FFF=$( expr $TTTOTAL - $TTSKIPPED - $TTFAILED )
 if [ "$FFF" != "0" ] && [ "$TTTOTAL" != "0" ]; then
 GGG=$(echo "scale = 4;$FFF/$TTTOTAL" | bc -l)
 PROC1=$(echo "scale = 4;$GGG*100" | bc -l)
 PROC=$(echo $PROC1 | cut -f1 -d.)
  if [ "$PROC" = "" ]; then
    PROC="0"
  fi
 else
  PROC="0"
 fi
 if [ "$USER" = "rcsci1" ]; then
  echo "$PROC" > $RESULTDIR/$LSV_NO/$(basename $j).proc
 fi
}
#############################################################


result_head()
{
if [ "$TEXT_HEAD" = "" ]; then
 TEXT_HEAD="Test Log Test Build Number"
fi
echo "<th bgcolor=\"#C0C0C0\"><b> STP</b></th>"
echo "<th bgcolor=\"#C0C0C0\"><b> ${TEXT_HEAD}</b></th>"
echo "<th bgcolor=\"#C0C0C0\"><b> Fail</b></th>"
echo "<th bgcolor=\"#C0C0C0\"><b> Skip</b></th>"
echo "<th bgcolor=\"#C0C0C0\"><b>Total</b></th>"
echo "<th bgcolor=\"#C0C0C0\" ><b>Queue(min)</b></th>"
echo "<th bgcolor=\"#C0C0C0\"><b>Execution(min)</b></th>"
echo "<th bgcolor=\"#C0C0C0\" ><b>Total(min)</b></th>"
echo "</tr>" 
 TEXT_HEAD=""
}

######################################################################################
# main 
######################################################################################
LSV_NO=$1  
TEST_TYPE=$2
BRANCH=$3
RFILE=$4
if [ "$TEST_TYPE" = "no" ] || [ "$TEST_TYPE" = "" ] || [ "$TEST_TYPE" = "A" ]; then
CIRESULTS="ciresults"      
oldciresult="/proj/webdocs/rbs-rde-ci/root/ciresults"
ciresults="/proj/webdocs/rbs-rde-ci/root/ciresults/$LSV_NO/*/*/*"
lsvnodir="/proj/webdocs/rbs-rde-ci/root/ciresults/$LSV_NO"
RESULTDIR="/proj/webdocs/rbs-rde-ci/root/ciresults"
JENKINSJOBS="/proj/webdocs/rbs-rde-ci/jenkinsdata/jobs/"
BRANCH_INFO="branch_info.txt"
elif [ "$BRANCH" = "" ]; then
CIRESULTS="${TEST_TYPE}_ciresults"       
oldciresult="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults"
ciresults="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults/$LSV_NO/*/*/*"
lsvnodir="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults/$LSV_NO"
RESULTDIR="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults"
BRANCH_INFO="${TEST_TYPE}_branch_info.txt"
if [ "$JENKINS_HOME" != "" ]; then 
JENKINSJOBS="${JENKINS_HOME}/jobs/"
fi
elif [ "$BRANCH" != "" ]; then
CIRESULTS="${TEST_TYPE}_ciresults"       
oldciresult="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults_branch/$BRANCH"
ciresults="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults_branch/$BRANCH/$LSV_NO/*/*/*"
lsvnodir="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults_branch/$BRANCH/$LSV_NO"
RESULTDIR="/proj/webdocs/rbs-rde-ci/root/${TEST_TYPE}_ciresults_branch/$BRANCH"
BRANCH_INFO="${TEST_TYPE}_branch_info.txt"
if [ "$JENKINS_HOME" != "" ]; then 
JENKINSJOBS="${JENKINS_HOME}/jobs/"
fi
fi
TTFAILED="0"
TTSKIPPED="0"
TTTOTAL="0"
TTQtime="0"
TTEtime="0"
TTTtime="0"
if [ "$RFILE" != "" ] && [ -f $lsvnodir/${RFILE}.html ]; then
  cat $lsvnodir/${RFILE}.html
  exit 0

elif [ -f $lsvnodir/ciresult.html ]; then
  cat $lsvnodir/ciresult.html
  exit 0
fi 
if [ $( echo $LSV_NO | grep "x" ) != "" ]; then
  LSV_NO="$( echo $LSV_NO  | cut -f1 -dx )"
  ciresults="/proj/webdocs/rbs-rde-ci/root/${CIRESULTS}/$LSV_NO/*/*/*"
  lsvnodir="/proj/webdocs/rbs-rde-ci/root/${CIRESULTS}/$LSV_NO"
elif [ $( echo $LSV_NO | grep "t" ) != "" ]; then
  LSV_NO="$( echo $LSV_NO  | cut -f1 -dt )"
  lsvnodir="/proj/webdocs/rbs-rde-ci/root/${CIRESULTS}/$LSV_NO"
  cat $lsvnodir/test_report.html
  exit 0
fi 
######################################################################################

echo "<table width=\"%95\" border=\"1\">"

echo "<tr>"
TEXT="<th colspan=\"10\"><b><center> $(cat $lsvnodir/date_new) LSV Build Number $(echo ${LSV_NO} | cut -f2- -d. | cut -f1 -d. ) "
TESTREPORT=""
XLTESTREPORT=""
PRE_BDC_LMC=""
if  [ "$(ls  /proj/rcs-tmp/pre_bdc_cxp/*/ | grep -w ${LSV_NO} )" != "" ]; then
    L=$(ls  /proj/rcs-tmp/pre_bdc_cxp/*/* | grep -w ${LSV_NO} | cut -f1 -d:)
if  [ "$L" != "" ]; then
    PRE_BDC_LMC="<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se${L}\"> <font color=blue>pre bdc lmc </font></a>"
fi
fi
if  [ -f  /proj/rcs/rbs_cs_ci_statistics/test_report/${LSV_NO}/test_report.html ]; then
    TESTREPORT="<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/rbs_cs_ci_statistics/test_report/${LSV_NO}/test_report.html\"> <font color=blue>test report </font></a>"
fi
if  [ -f  /proj/rcs/rbs_cs_ci_statistics/test_report/${LSV_NO}/xl_test_report.html ]; then
    XLTESTREPORT="<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/rbs_cs_ci_statistics/test_report/${LSV_NO}/xl_test_report.html\"> <font color=blue> xl test report</font></a>"
fi
if  [ -f  /proj/rcs/rbs_cs_ci_statistics/test_report/${LSV_NO}/sqr_report.html ]; then
    SXQRTESTREPORT="<a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/proj/rcs-tmp/rbs_cs_ci_statistics/test_report/${LSV_NO}/sqr_report.html\"> <font color=blue> sqr report</font></a>"
fi
if [ "$(echo $LSV_NO | grep "\." )" = "" ]; then
  OLD_LSV_NO=$(ls -a $RESULTDIR/ | grep .$LSV_NO. | grep  -v .$LSV_NO.1 | sort -n )
  if [ "$OLD_LSV_NO" != "" ]; then
    TEXT="$TEXT Old results "
    for i in $OLD_LSV_NO
    do
      TEXT="$TEXT <a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/lsvresult.pl?$i\"> <font color=blue> $(cat $RESULTDIR/$i/date_new)</font></a></td>"
    done
  fi
fi
TEXT="$TEXT $PRE_BDC_LMC $TESTREPORT $XLTESTREPORT $SXQRTESTREPORT<center></b></th>"
echo $TEXT 
#echo "<th bgcolor=\"#C0C0C0\" colspan=\"10\"><b><center> $(cat $lsvnodir/date) LSV Build Number ${LSV_NO} <center></b></th>"
echo "</tr>"
echo "<tr>"
if [ -f $lsvnodir/comments.txt ]; then
  COMMENTS=$(cat $lsvnodir/comments.txt)
else
  COMMENTS=""
fi
UPD="<a href=\"${JOB_URL}/buildWithParameters?token=update_ciresults&LSV_BUILD_NUMBER=${LSV_NO}&TEST_TYPE=${TEST_TYPE}&BRANCH=${BRANCH}\" <font color=blue> update </font></a>"


COMMENTS_UPDATE="<td colspan=\"1\"><center><b> <a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/edit_comments.pl?/proj/webdocs/rbs-rde-ci/root/${CIRESULTS}/${LSV_NO}/comments.txt\" <font color=blue> edit </font> </a> $UPD </b><center></td>"

echo "<th bgcolor=\"#C0C0C0\" colspan=\"8\"><b><center> Comments <center></b></th>"
echo "</tr>"
echo "<tr>"
#echo "<td colspan=\"1\"><center><b> Comments </b><center></td>"
echo "<td colspan=\"7\"> $COMMENTS </td>"
echo $COMMENTS_UPDATE
#echo "<td colspan=\"1\"><center><b> <a href=\"https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/edit_comments.pl?/proj/webdocs/rbs-rde-ci/root/${CIRESULTS}/${LSV_NO}/comments.txt\" <font color=blue> edit </font> </b><center></td>"
echo "</tr>"

echo "<tr>"

COLOR10="#CC99FF"
echo "<th bgcolor=\"${COLOR10}\" colspan=\"8\"><b> SBC required tests to achieve a confidence level 1 </b></th>"
echo "</tr>"
result_head
######################################################################################
ACIVITYDIR=$(ls -dr $RESULTDIR/$LSV_NO/*/)
ls -dr $RESULTDIR/$LSV_NO/*/ > activity

if [ -f $RESULTDIR/$LSV_NO/SBC_${TEST_TYPE} ]; then
 REQUIRED=$(cat $RESULTDIR/$LSV_NO/SBC_${TEST_TYPE}  | sed 's/,/.new_state,/' | grep ",yes" | cut -f1 -d,)
fi
COLOR10="#CC99FF"
#if [ "$(cat /proj/webdocs/rbs-rde-ci/root/${CIRESULTS}/$LSV_NO/TestCs | cut -f6- -d- | cut -c2)" = "3" ]; then
#  REQUIRED=""
#fi 


for i in $REQUIRED
do


  if [ "$(echo $ACIVITYDIR | grep $(echo $i | cut -f1 -d.))" != "" ]; then
    j=$(cat   activity| grep "\b$(echo $i | cut -f1 -d.)\b")

    if [ "$(echo $(basename $j) | grep "jcat")" != "" ] ; then
       print_table_jcat
    else
       print_table
    fi
  fi
done
echo "<td bgcolor=\"#C0C0C0\" colspan=\"2\"><b>SBC required tests to achieve a confidence level 1 Results</b></td>"
echo "<td bgcolor=\"#C0C0C0\" align=\"right\"><b> $TTFAILED </b></td>"
echo "<td bgcolor=\"#C0C0C0\" align=\"right\"><b> $TTSKIPPED </b></td>"
echo "<td bgcolor=\"#C0C0C0\" align=\"right\"><b> $TTTOTAL </b></td>"
echo "<td bgcolor=\"#C0C0C0\" align=\"right\"> $TTQtime </td>"
echo "<td bgcolor=\"#C0C0C0\" align=\"right\" > $TTEtime </td>"
echo "<td bgcolor=\"#C0C0C0\" align=\"right\" > $TTTtime </td>"
echo "</tr>"

echo "<th bgcolor=\"${COLOR10}\" colspan=\"8\"><b> SBC other tests </b></th>"
echo "</tr>"
result_head


for j in $ACIVITYDIR
do
  if [ "$(echo $REQUIRED | grep $(basename $j).new_state)" = "" ] && [ "$(echo $(basename $j) | grep "test_activity_sbt[0-9]")" = "" ] && [ "$(echo $(basename $j) | grep "test_activity_sbt_")" = "" ]&& [ "$(echo $(basename $j ) | grep "sbt_jcat")" = "" ] && [ "$(echo $( basename $j ) | grep "release_candidate")" = "" ]  && [ "$(echo $(basename $j ) | grep "qualification")" = "" ]; then
    if [ "$(echo $(basename $j ) | grep "jcat")" != "" ] ; then
       print_table_jcat
    else

    print_table
    fi
  fi
done
###################################################################################

###################################################################################
if [ "$(ls  $RESULTDIR/$LSV_NO/* | grep jjjcat)" != "" ] ; then
echo "<th bgcolor=\"${COLOR10}\" colspan=\"8\"><b> SBC jcat tests </b></th>"
echo "</tr>" 
TEXT_HEAD="Jcat Test Log "
result_head

for j in $ACIVITYDIR 
do

  if [ "$(echo $(basename $j ) | grep "jcat")" != "" ] ; then
    print_table_jcat
  fi
done
fi

###################################################################################
if [ "$(echo $ACIVITYDIR | grep "qualification" )" != "" ] ; then
echo "<th bgcolor=\"${COLOR10}\" colspan=\"8\"><b> SBC qualification tests </b></th>"
echo "</tr>" 
result_head
for j in $ACIVITYDIR
do
  if [ "$(echo $(basename $j ) | grep "qualification")" != "" ] ; then
    print_table
  fi
done
fi

if [ "$(echo $ACIVITYDIR | grep "test_activity_sbt_jcat" )" != "" ] || [ "$(echo $ACIVITYDIR | grep "test_activity_jcat_oss_sbt_jcat" )" != "" ] || [ -f $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE} ] ; then
  echo "<th bgcolor=\"${COLOR10}\" colspan=\"8\"><b> SBT tests </b></th>"

  echo "</tr>" 
  result_head
  if [ -f $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE} ] ; then
    for j in $ACIVITYDIR
    do
      if [ "$(cat $RESULTDIR/$LSV_NO/SBT_${TEST_TYPE}  | grep $(basename $j ) )" != "" ]; then
        print_table
      fi
    done
  fi
for j in $ACIVITYDIR
do
   if [ "$(echo $(basename $j ) | grep "sbt_jcat" )" != "" ] ||  [ "$(echo $(basename $j ) | grep "_oss_" )" != "" ] ; then
       print_table_jcat
  fi
done
fi



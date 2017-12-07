#!/bin/bash

## ----------------------------------------------------------
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
## %CCaseCopyrightEnd%
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
## R2A/1      2013-09-04   etxjovp     Created
## R2A/4      2013-10-01   etxjovp     add /home/$USER/.test_my_stuff
## R2A/8      2014-01-13   etxjovp     curl handle calls without certificate
## R2A/9      2014-01-20   etxjovp     curl handle calls without certificate
## R2A/10     2014-01-21   etxjovp     curl handle calls without certificate
## R2A/10     2014-04-21   erarafo     Copyright complaint fixed
#
######################################################################################

function usage {

        echo -e ""
        echo -e ""
        echo -e "$bold##################################################################################################################$nobold"
        echo -e "${bold}#$nobold"
        echo -e "${bold}# $(basename $0) is a script that helps you to reserve node from  the CI machine  $nobold"    
        echo -e "${bold}#$nobold"
        echo -e "$bold##################################################################################################################$nobold"
        echo -e "${bold}Usage:$nobold"
        echo -e "       $(basename $0) ${bold}-r${nobold} <node>  		Reserving a node in the day to be used until at 20:00"
        echo -e "       $(basename $0) ${bold}-R${nobold} <node_type>  		Reserving a node type (dus53 dus52 dus32 tcu03 tcu04 ) in the day to be used until at 20:00"
        echo -e "       $(basename $0) ${bold}-R${nobold} <node_type>  	${bold}-S${nobold} yes	Reserving a sec node type (dus53 dus52 dus32 tcu03 tcu04 ) in the day to be used until at 20:00"
        echo -e "       $(basename $0) ${bold}-r${nobold} <node>  ${bold}-f${nobold} yes	Reserving a node until further for use on office time (06:00 to 20:00)" 
  #      echo -e "       $(basename $0) ${bold}-E${nobold} <node>  		${bold}Should be used only in emergencies ${nobold}(You can book any node at any time and even stop a job that runs on the node)  "
        echo -e "       $(basename $0) ${bold}-c${nobold} <node> 		Cancel node" 
        echo -e "       $(basename $0) ${bold}-c${nobold} <node>  ${bold}-u${nobold} <user>	Cancel node booked by user" 
        echo -e "       $(basename $0) ${bold}-l${nobold} <node> 		Lock a node over a night or a weekend." 
        echo -e "       $(basename $0) ${bold}-a${nobold} <node> | <node_type> 	Print state  node type (tcu dus dus53 dus52 dus32 tcu03 tcu04 sec_dus sec_dus53 sec_dus52 .........) "  
        echo -e "       $(basename $0) ${bold}-a${nobold}  <node_type>:<label>	Print state  label (ru <REV> <KDU> ......) "  
        echo -e "       $(basename $0) ${bold}-a${nobold}  all | idle		Print state  all  or idle  nodes       
"  

        echo -e "${bold}Example:$nobold"
        echo -e "       $(basename $0)  ${bold}-r${nobold} dus034"
        echo -e "       $(basename $0)  ${bold}-R${nobold} dus52"
       # echo -e "       $(basename $0)  ${bold}-E${nobold} dus5104"
        echo -e "       $(basename $0)  ${bold}-R${nobold} dus52 -S${nobold} yes"
        echo -e "       $(basename $0)  ${bold}-c${nobold} dus034 "
        echo -e "       $(basename $0)  ${bold}-l${nobold} dus034  "
        echo -e "       $(basename $0)  ${bold}-r${nobold} dus034 ${bold}-u${nobold} etxabcd"
        echo -e "       $(basename $0)  ${bold}-a${nobold} all  "
        echo -e "       $(basename $0)  ${bold}-a${nobold} KDU137925_4:R6A  "
        echo -e "       $(basename $0)  ${bold}-a${nobold} sec_dus:ru  "
        echo -e "       $(basename $0)  ${bold}-a${nobold} sec_dus52:ru  "
        echo -e "       $(basename $0)  ${bold}-a${nobold} sec_dus53:ru  "
       echo -e ""

}

######################################################################################
function get_jenkins_url {

if [ -d   /proj/eiffel002_config_fem0*/eiffel_home/nodes/$NODE ]; then
 
  X=$(ls -d /proj/eiffel002_config_fem0*/eiffel_home/nodes/$NODE | cut -f3 -d\/ )

  A=$(echo $X | cut -f3- -d_ )
  B=$(echo $X | cut -f1 -d_ )
  JENKINS_URL="https://${A}-${B}.rnd.ki.sw.ericsson.se:8443/jenkins"
  API_TOKEN="/home/rcsci1/.ssh/.pw_apiToken_${A}_rcsci1"
  CURL="curl -k  -X POST"
 
elif [ "$(cat /proj/webdocs/rbs-rde-ci/jenkinsdata/config.xml | grep $NODE | grep "<remoteFS>/" )" != "" ]; then
  JENKINS_URL="https://rbs-rde-ci.rnd.ki.sw.ericsson.se" 
  API_TOKEN="/home/rcsci1/.ssh/.pw_rcsci1_old"
  CURL="curl -k  "
  CURL="curl -k  -X POST"
else

exit 1

fi
#echo "$JENKINS_URL	$API_TOKEN"
}
######################################################################################
function update {
  N_DATE=""
get_jenkins_url
  if [ "$CIUSER" = "rcsci1" ] && [ "$LABEL" = "yes" ]; then
    if [ "$TESTUSER" = "$BUSER" ] || [ "$BUSER" = "" ]; then
      if [ "$STATE" != "lock" ]; then
          if [ "$STATE" = "cancel" ]; then
             if  [ "$BSTATE" = "reserve" ] || [ "$BSTATE" = "lock" ]; then
                  #TESTUSER=""
                  STATE=""
                  rm -f $CI_BOOK_DIR/$NODE
                  update_node_in_jekins_cancel
             else
                    echo ""
                    echo "-------------------------------------------------------------------------------------------------------------------"	
                    echo "You can not cancel the node $NODE"
                    echo "-------------------------------------------------------------------------------------------------------------------"
                    echo ""
                    NODE_LIST=$NODE
                    print_state
                    exit
             fi
          elif [ "$STATE" = "reserve" ] && [ "$BSTATE" = "" ]; then
             if  [ "$OFFLINE" = "ONLINE" ]; then
                if  [ "$UNTIL_FURTHER" != "no" ]; then
                  echo "$UNTIL_FURTHER" > $CI_BOOK_DIR/$NODE
                  chmod 666 $CI_BOOK_DIR/$NODE            
		else
                  rm -f $CI_BOOK_DIR/$NODE  

                fi
                update_node_in_jekins_reserve
             else
                #update_node_in_jekins_lock
                print_state
                exit
             fi
          elif [ "$STATE" = "reserve" ] && [ "$BSTATE" = "reserve" ]; then
            
                if  [ "$UNTIL_FURTHER" != "no" ]; then
                  echo "$UNTIL_FURTHER" > $CI_BOOK_DIR/$NODE
                  chmod 666 $CI_BOOK_DIR/$NODE
                  echo ""
                  echo "-------------------------------------------------------------------------------------------------------------------"	
                  echo "Your node $NODE is reserved until further  for use on office time (06:00 to 20:00)"
                  echo "-------------------------------------------------------------------------------------------------------------------"
                  echo ""
		else
                  rm -f $CI_BOOK_DIR/$NODE  
                fi
               
              exit
           elif [ "$STATE" = "reserve" ] &&  [ "$BSTATE" = "lock" ]; then
             if  [ "$OFFLINE" = "OFFLINE" ]; then
                if  [ "$UNTIL_FURTHER" != "no" ]; then
                  echo "$UNTIL_FURTHER" > $CI_BOOK_DIR/$NODE
                  chmod 666 $CI_BOOK_DIR/$NODE
                  echo ""
                  echo "-------------------------------------------------------------------------------------------------------------------"	
                  echo "Your node $NODE is reserved until further  for use on office time (06:00 to 20:00)"
                  echo "-------------------------------------------------------------------------------------------------------------------"
                  echo ""
		else
                  rm -f $CI_BOOK_DIR/$NODE  
                fi
                #update_node_in_jekins_reserve
             echo ""
             echo "-------------------------------------------------------------------------------------------------------------------"	
             echo "You can not reserve the node $NODE"
             echo "-------------------------------------------------------------------------------------------------------------------"
             echo ""
             else
                #update_node_in_jekins_lock
                print_state
                exit
             fi
             exit
          else 
             echo ""
             echo "-------------------------------------------------------------------------------------------------------------------"	
             echo "You can not reserve the node $NODE"
             echo "-------------------------------------------------------------------------------------------------------------------"
             echo ""
             NODE_LIST=$NODE
             print_state
             exit
          fi
      elif [ "$STATE" = "lock" ] && [ "$BSTATE" = "reserve" ] && [ -f $CI_BOOK_DIR/$NODE ]; then
          STATE="reserve"
          N_DATE=$(date +"%Y%m%d")
          update_node_in_jekins_lock
      elif [ "$STATE" = "lock" ] && [ "$BSTATE" = "reserve" ]; then
         echo ""
        echo "-------------------------------------------------------------------------------------------------------------------"	
        echo " You can not lock $NODE!!   You need reserving $NODE until further before you lock the node"
        echo "-------------------------------------------------------------------------------------------------------------------"
        echo ""
        NODE_LIST=$NODE
        print_state
        exit
      elif [ "$STATE" = "lock" ] && [ "$BSTATE" = "lock" ]; then
        echo ""
        echo "-------------------------------------------------------------------------------------------------------------------"	
        echo " $NODE is  already locked by $BUSER"
        echo "-------------------------------------------------------------------------------------------------------------------"
        echo ""
        NODE_LIST=$NODE
        print_state
        exit
      else
        echo ""
        echo "-------------------------------------------------------------------------------------------------------------------"	
        echo "You can not lock a node that you have not booked"
        echo "-------------------------------------------------------------------------------------------------------------------"
        echo ""
        NODE_LIST=$NODE
        print_state
        exit
      fi
    else
       echo ""
       echo "-------------------------------------------------------------------------------------------------------------------"	
       echo "$NODE is already booked by $BUSER"
       echo "-------------------------------------------------------------------------------------------------------------------"
       echo ""
       NODE_LIST=$NODE
       print_state
       exit
    fi
  else
    echo ""
    echo "-------------------------------------------------------------------------------------------------------------------"	
    echo "You can not book the node $NODE"
    echo "-------------------------------------------------------------------------------------------------------------------"
    echo ""
    NODE_LIST=$NODE
    print_state
    exit
  fi
  



}
######################################################################################
function   print_state {
  PRINT="true"
  curl -k --silent --insecure https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/hw-book.pl > .tmp_hw_book.txt
  echo ""
  echo "-------------------------------------------------------------------------------------------------------------------"	
  echo "NODE	REV			OWNER	STATE	IDLE	FOR BOOKING	BOOKED BY	BOOKING STATE	INFO	"
  echo "-------------------------------------------------------------------------------------------------------------------"	
for NODE in $NODE_LIST
do
  get_state
done
  echo "-------------------------------------------------------------------------------------------------------------------"	
rm -f .tmp_hw_book.txt
}
######################################################################################
function   get_state {
 if [ -f $BOOK/$NODE ]; then
  NN_DATE=$(date +"%Y%m%d")

   get_jenkins_url

   curl -k --silent "$JENKINS_URL/computer/$NODE/api/json" > .tmp_reserve_node.txt

      if $(cat .tmp_reserve_node.txt | grep --silent '"temporarilyOffline":false') && $(cat .tmp_reserve_node.txt | grep --silent 'availablePhysicalMemory'); then

        OFFLINE="ONLINE"
      else
        OFFLINE="OFFLINE"
      fi
      if $(cat .tmp_reserve_node.txt | grep --silent '"idle":false') ; then
        IDLE="no"
      else
        IDLE="yes"
      fi
   INFO=$(cat $NODELIST | grep $NODE | grep -v ":::" )
   BUSER=$(echo $INFO | cut -f2 -d:)
   BSTATE=$(echo $INFO | cut -f3 -d:)
   DATE_RESERVED=$(echo $INFO | cut -f4 -d:)
   CIUSER=$(cat $BOOK/$NODE  )

   LABEL=$(curl -k --silent "$JENKINS_URL/label/available_to_user/" | grep $NODE)
   if [ "$LABEL" != ""  ] && [ "$CIUSER" = "rcsci1"  ]; then
         LABEL="yes"
      else
        LABEL="no"
      fi
     if [ "$OFFLINE" = "OFFLINE"  ] ; then
         IDLE="-"
     fi
    if [ "$DATE_RESERVED" = "$NN_DATE"  ] ; then
         BSTATE="lock"
      
      fi
if [ "$TESTUSER" != "rcsci1"  ]; then
if [ "$PRINT" != "true"  ]; then
 if [ "$USER" = "etxjovp"  ] || [ "$USER" = "eransbn"  ] || [ "$USER" = "etxkols"  ] || [ "$USER" = "etxbolb"  ] || [ "$USER" = "etxprhm"  ] || [ "$USER" = "etxivri"  ] || [ "$USER" = "erarube"  ]; then
    LABEL="yes"
 fi
fi
fi

   if [ "$PRINT" = "true"  ]; then
     if [ "$BSTATE" = "reserve"  ] && [ -f $CI_BOOK_DIR/$NODE ] ; then
       BSTATE="further"
     elif [ "$BSTATE" = "reserve"  ]; then
       BSTATE="today"
     fi
     if [ "$AVAILABLE" != "idle"  ]; then
     REV=$(cat .tmp_hw_book.txt | grep $NODE |  grep ";" | cut -f17 -d\> | cut -f1 -d\< | head -1)
     echo "$NODE	$REV  	$CIUSER	$OFFLINE	$IDLE	$LABEL		$BUSER		$BSTATE		$(cat $BOOK/${NODE}.wp ) "
     NODE_STATE="${NODE}:${REV}:${CIUSER}:${OFFLINE}:${IDLE}:${LABEL}:${BUSER}:${BSTATE}:$(cat $BOOK/${NODE}.wp )" 
     elif [ "$IDLE" = "yes"  ] && [ "$OFFLINE" = "ONLINE"  ] && [ "$LABEL" = "yes"  ]; then
     REV=$(cat .tmp_hw_book.txt | grep $NODE |  grep ";" | cut -f17 -d\> | cut -f1 -d\< | head -1)
     echo "$NODE	$REV  	$CIUSER	$OFFLINE	$IDLE	$LABEL		$BUSER		$BSTATE		$(cat $BOOK/${NODE}.wp ) "
     NODE_STATE="${NODE}:${REV}:${CIUSER}:${OFFLINE}:${IDLE}:${LABEL}:${BUSER}:${BSTATE}:$(cat $BOOK/${NODE}.wp )" 
     fi 
   
   elif [ "$DO" = "type_reserve"  ]; then
     REV=$(cat .tmp_hw_book.txt | grep $NODE |  grep ";" | cut -f17 -d\> | cut -f1 -d\< | head -1)
     NODE_STATE="${NODE}:${REV}:${CIUSER}:${OFFLINE}:${IDLE}:${LABEL}:${BUSER}:${BSTATE}:$(cat $BOOK/${NODE}.wp )" 
   fi
rm -f .tmp_reserve_node.txt
fi
}
######################################################################################
function   reserve {
  STATE="reserve"
  STATE1="OFFLINE"
  get_state
  update
  NODE_LIST=$NODE
  #print_state
  exit
}
######################################################################################
function   reserve_emergency {
  echo " $(date)	$USER	$NODE " >> $CI_BOOK_DIR/.reserve_emergency
  STATE="reserve"
  STATE1="OFFLINE"
  get_state
  LABEL="yes"
  update
  if [ "$IDLE" = "no" ]; then
     
get_jenkins_url
     curl -k --silent "${JENKINS_URL}/computer/$NODE/builds" > .tmp_reserve_emergency_node.txt
     JOB=$(cat .tmp_reserve_emergency_node.txt | grep "model-link" | grep "#" | head -1 | cut -f3 -d\/ )
     BUILD_NUMBER=$( cat .tmp_reserve_emergency_node.txt | grep "model-link" | grep "#" | head -1 | cut -f3- -d\/ | cut -f2 -d# | cut -f1 -d\< )
     rm -f .tmp_reserve_emergency_node.txt
  
     echo $JOB $BUILD_NUMBER
     echo ""
     echo "-------------------------------------------------------------------------------------------------------------------"	
     echo -e "${bold}NOTE! It runs a job ($JOB $BUILD_NUMBER) on the node $NODE, so it may take some time before it becomes available to you. ${nobold} "
     echo "-------------------------------------------------------------------------------------------------------------------"
     echo ""
      echo -e "${bold}Do you want to cancel this job? (yes / no) ${nobold} "
          read answer
                case $answer in
		    yes) stop ;;
		    *) 
                      echo ""
     echo "-------------------------------------------------------------------------------------------------------------------"	
     echo -e "${bold} Good you keep waiting! ${nobold} "
     echo "-------------------------------------------------------------------------------------------------------------------"
     echo "" 
                     ;;
                esac
  fi
  NODE_LIST=$NODE
  echo " Wait ..........."
    sleep 20
  print_state
  exit
}
function stop {
echo ""
     echo "-------------------------------------------------------------------------------------------------------------------"	
     echo -e "${bold} Good luck!  The job $JOB with number $BUILD_NUMBER is killed.   ${nobold} "
     echo "-------------------------------------------------------------------------------------------------------------------"
get_jenkins_url
   curl -k --silent ${JENKINS_URL}/job/stop_job/buildWithParameters\?token=nisse\&NUMBER=${BUILD_NUMBER}\&JOB=${JOB}\&TESTUSER=${USER}

}
######################################################################################
function   cancel {
  STATE="cancel"
  STATE1="ONLINE"
  get_state
  LABEL="yes"
  update
  NODE_LIST=$NODE
  #print_state
  exit
}
######################################################################################
function   lock {
  STATE="lock"
  STATE1="OFFLINE"
  get_state
  LABEL="yes"
  update
  NODE_LIST=$NODE
  #print_state
  exit
}

######################################################################################
function    lock_file {
  if [ -f $CI_BOOK_DIR/lock ]; then
     echo ""
     echo "-------------------------------------------------------------------------------------------------------------------"	
     echo "$NODE temporarily locked, please try again in 20 sec!! "
     echo "-------------------------------------------------------------------------------------------------------------------"
     echo ""

     	if [ "$(find $CI_BOOK_DIR/lock -maxdepth 0 -cmin +2)" != "" ]; then
            echo "REMOVE $CI_BOOK_DIR/lock"
            rm -f $CI_BOOK_DIR/lock
        fi
     exit
  else
     V=$(touch $CI_BOOK_DIR/lock;sleep $SLEEP_TIME; rm -f $CI_BOOK_DIR/lock) &
     #V=$(touch $CI_BOOK_DIR/lock;sleep 5; rm -f $CI_BOOK_DIR/lock) &
  fi


}
######################################################################################
function   update_node_in_jekins_cancel {
get_jenkins_url
#echo "curl -k ${JENKINS_URL}/job/set_node_online/buildWithParameters\?token=nisse\&NODE=${NODE}\&TEST_USER=${TESTUSER}\&N_DATE=${N_DATE}\&B_STATE=${STATE}"
curl -k ${JENKINS_URL}/job/set_node_online/buildWithParameters\?token=nisse\&NODE=${NODE}\&TEST_USER=${TESTUSER}\&N_DATE=${N_DATE}\&B_STATE=${STATE}
#sleep 10
  echo ""
  echo "-------------------------------------------------------------------------------------------------------------------"	
  echo "Your reservation on $NODE will be terminated!"
  echo "-------------------------------------------------------------------------------------------------------------------"
  echo ""
}

######################################################################################
function   update_node_in_jekins_reserve {
get_jenkins_url
#echo "curl -k ${JENKINS_URL}/job/set_node_offline/buildWithParameters\?token=nisse\&NODE=${NODE}\&TEST_USER=${TESTUSER}\&N_DATE=${N_DATE}\&B_STATE=${STATE}"
  curl -k ${JENKINS_URL}/job/set_node_offline/buildWithParameters\?token=nisse\&NODE=${NODE}\&TEST_USER=${TESTUSER}\&N_DATE=${N_DATE}\&B_STATE=${STATE}
  echo ""
  echo "-------------------------------------------------------------------------------------------------------------------"	
  echo "You will receive an email when the node $NODE is ready for use!!!!!"
  if [ "$IDLE" = "no" ]; then
    echo -e "${bold}NOTE! It runs a job on the node, so it may take some time before it becomes available to you. ${nobold} "
  fi
  echo "-------------------------------------------------------------------------------------------------------------------"
  echo ""	


#sleep 10
}

######################################################################################
function   update_node_in_jekins_lock {
get_jenkins_url
 # echo "curl  -k ${JENKINS_URL}/job/set_node_lock/buildWithParameters\?token=nisse\&NODE=${NODE}\&TEST_USER=${TESTUSER}\&N_DATE=${N_DATE}\&B_STATE=${STATE}"
  curl  -k ${JENKINS_URL}/job/set_node_lock/buildWithParameters\?token=nisse\&NODE=${NODE}\&TEST_USER=${TESTUSER}\&N_DATE=${N_DATE}\&B_STATE=${STATE}
  echo ""
  echo "-------------------------------------------------------------------------------------------------------------------"	
  echo "Your reservation on $NODE will be  locked!"
  echo "-------------------------------------------------------------------------------------------------------------------"
  echo ""


#sleep 10
}
######################################################################################
function   get_user_log {
touch $CI_BOOK_DIR/.user_log.txt
chmod 666 $CI_BOOK_DIR/.user_log.txt
cat $CI_BOOK_DIR/.user_log.txt
}
######################################################################################

######################################################################################
#NODELIST="/home/etxjovp/node_list.txt"
CI_BOOK_DIR="/proj/rcs/ci-book"
NODELIST="/proj/rcs/ci-book/*/node_list.txt"
BOOK="/proj/webdocs/rbs-rde/etc/host-book/"
JENKINS_CONFIG="/proj/webdocs/rbs-rde-ci/jenkinsdata/config.xml"
JENKINS_URL="https://rbs-rde-ci.rnd.ki.sw.ericsson.se"
AVAILABLE_TO_USER="false"
NODELISTTMP="/tmp/$USER_$$"
bold='\033[1m'
nobold='\033[0m'
normal='\E[0m'
TESTUSER=$USER
PRINT="false"
UNTIL_FURTHER="no"
SLEEP_TIME=20
######################################################################################
#   MAIN
######################################################################################

while getopts R:S:E:r:c:l:a:u:f:qt:ht:gt: option
do
	case "$option"
	in
                R) TYPE=$OPTARG
                   DO=type_reserve
                   ;;
#                E) NODE=$OPTARG
#                  DO=emergency
#                   ;;
                S) SEC=$OPTARG
                   ;;
                r) NODE=$OPTARG
                   DO=reserve
                   ;;
                c) NODE=$OPTARG
                   DO=cancel
                   ;;
                l) 
                   NODE=$OPTARG 
                   DO=lock                
                   ;;
                q) 
                   DO=users_who_have_booked               
                   ;;
                a) AVAILABLE=$OPTARG
                   ;;
                u) TESTUSER=$OPTARG
                   ;;
                f) UNTIL_FURTHER=$OPTARG
                   ;;
                g) get_user_log
                  exit;; 
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done
if [ "$#" = "0" ]; then
  usage
  exit
fi 
if [ "$USER" != "rcsci1" ]; then
echo "$(date) $USER $0  $@" >> $CI_BOOK_DIR/.user_log.txt
else
echo "$(date) $USER $0  $@" >> $CI_BOOK_DIR/.rcsci1_log.txt
fi
#touch $NODELIST
echo ""

if [ "$(getent group | grep $USER | grep seki-flipper-ts )" = "" ]; then 
  echo "-------------------------------------------------------------------------------------------------------------------"	
  echo "

NOTE ! This service is only available to users who belong to the group seki-flipper-ts

"
  echo "-------------------------------------------------------------------------------------------------------------------"	
  exit
fi
ls -d /proj/eiffel002_config_fem007  > /dev/null
ls -d /proj/eiffel002_config_fem025 > /dev/null
touch /home/$USER/.test_my_stuff
chmod 666 /home/$USER/.test_my_stuff
if [ "$UNTIL_FURTHER" != "no" ] && [ "$UNTIL_FURTHER" != "yes" ] && [ "$UNTIL_FURTHER" != "stable" ] && [ "$UNTIL_FURTHER" != "hc" ] ; then
	echo " $UNTIL_FURTHER"
	exit 
fi
if [ "$DO" = "cancel" ]; then
 lock_file
 cancel
elif [ "$DO" = "reserve" ]; then
 lock_file
 reserve
elif [ "$DO" = "emergency" ]; then
 SLEEP_TIME=40
 lock_file
 reserve_emergency
elif [ "$DO" = "lock" ]; then
 lock_file
 lock
elif [ "$DO" = "users_who_have_booked" ] && [ "$USER" = "rcsci1" ]; then
echo "###################"
###########################################################################################################################################
QUEUE_LIST=$(curl -k --silent "${JENKINS_URL}/queue/api/json?pretty=true" | grep -A5 "${JENKINS_URL}:443/job/run_testspec_60_CI_3.0" | grep buildableStartMilliseconds | awk '{print $3}' | sort -n )
NQ=$(echo $QUEUE_LIST | wc -w | awk '{print $1 }' )
NOW=$(date +"%s")
TQT="0"
TRIG_QT="0"
MEDIAN="0"
MEDIAN_MAX="40"
NQ_MAX="10"
TRIG_QT_MAX="60"
if [ "$NQ" != "0" ]; then
MEDIAN_N=$( expr $NQ / 2 )
   if [ "$MEDIAN_N" = "0" ]; then
    MEDIAN_N=1
   fi
fi
MN=1
for t in $QUEUE_LIST
do
 QT=$(expr $( expr $NOW - $( expr $t / 1000 ) ) / 60 )
   if [ "$MN" = "$MEDIAN_N" ]; then
      MEDIAN=$QT
   fi

echo $QT min
TQT=$( expr $TQT + $QT )
MN=$( expr $MN + 1 )
done
echo "###################"

if [ "$NQ" != "0" ] && [ "$TQT" != "0" ]; then
  TRIG_QT=$( expr $TQT / $NQ )
  echo "$NQ $TRIG_QT min  "
fi
echo "YVALUE=$TRIG_QT" > average_queue.plot
echo "YVALUE=$NQ" > number_queue.plot
echo "YVALUE=$MEDIAN" > median_queue.plot
MAIL_LOCK=/home/$USER/.reserv_mail_lock
K=$(date +"%k" | awk '{print $1}')
if [ $MEDIAN -gt $MEDIAN_MAX ] && [ $TRIG_QT -gt $TRIG_QT_MAX ] && [ $NQ -gt $NQ_MAX ] && [ ! -f $MAIL_LOCK ] && [ $K -gt 7 ]; then
touch $MAIL_LOCK

  PRINT="true"
NODE_LIST=$(cat $JENKINS_CONFIG | grep remoteFS | grep remoteFS | grep 'du\|tcu' | cut -f5- -d\/ | cut -f1 -d\< | sort | grep -v "_" )
 B_L=""
U_L=""

rm -f node_list.txt
      curl -k --silent --insecure https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/hw-book.pl > .tmp_hw_book.txt
for NODE in $NODE_LIST
do
  get_state
MR=$(echo $NODE_STATE | cut -f3 -d:)
R=$(echo $NODE_STATE | cut -f7 -d:)
N=$(echo $NODE_STATE | cut -f1 -d:)
ONLINE=$(echo $NODE_STATE | cut -f4 -d:)

if [ "$R" != "" ] && [ "$ONLINE" = "OFFLINE" ]; then
  B_L="$B_L $N:$R"
echo $NODE_STATE | sed 's/:/\t /g' >> node_list.txt
if [ "$(echo $U_L | grep ${R})" = "" ]; then
  U_L="$U_L ${R}"
  fi
fi
done

echo $U_L > user_list.txt

echo "$(cat node_list.txt)" | mail -s "Queues in Jenkins!  " etxjovp

  echo "MAIL !!! "
  for k in $U_L
   do
   NAME=$(finger $k | head -1 | awk '{print $4}')
  echo " Hi $NAME

Right now, we have long waiting times in CS Jenkins.
You can help to reduce queues by returning the nodes that you absolutely do not need right now. See the list below.

$(cat node_list.txt | grep $k)

Thanks for your help

greetings CSCI" | mail -s "Queues in Jenkins!  " $k
   done


fi
if [ $TRIG_QT -gt $TRIG_QT_MAX ] && [ $NQ -gt $NQ_MAX ] ; then
echo "Not remove $MAIL_LOCK"
else
rm -f $MAIL_LOCK
fi
exit
###########################################################################################################################################
fi
if [ "$AVAILABLE" != "" ]; then
  if [ "$AVAILABLE" = "all" ] || [ "$AVAILABLE" = "idle" ]; then
      NODE_LIST=$(cat $JENKINS_CONFIG | grep remoteFS | grep remoteFS | grep 'du\|tcu' | cut -f5- -d\/ | cut -f1 -d\< | sort | grep -v "_" )
      NODE_LIST="$NODE_LIST $(ls -d /proj/eiffel002_config_fem0*/eiffel_home/nodes/* | grep 'tcu\|dus' | cut -f6- -d\/)"
  else
      #NODE_LIST=$(cat $JENKINS_CONFIG | grep remoteFS | grep remoteFS | grep $AVAILABLE | cut -f5- -d\/ | cut -f1 -d\< | sort | grep -v "_" ) 
  
  i=${JENKINS_URL}
  NNN=0
  MMM=yes
  O_LIST=""
  LABEL_LIST=$(echo $AVAILABLE | sed 's/:/ /g' )

 for k in $LABEL_LIST
    do  


A=https://rbs-rde-ci.rnd.ki.sw.ericsson.se
B=https://fem025-eiffel002.rnd.ki.sw.ericsson.se:8443/jenkins
C=https://fem007-eiffel002.rnd.ki.sw.ericsson.se:8443/jenkins

       L_LIST="$(curl -s $A/label/$k/api/json?pretty=true | grep nodeName | cut -f4 -d\") $(curl -s $B/label/$k/api/json?pretty=true | grep nodeName | cut -f4 -d\") $(curl -s $C/label/$k/api/json?pretty=true | grep nodeName | cut -f4 -d\")"
#echo "$L_LIST"
        T_L=""
       for p in $L_LIST
        do
         if [ "$NNN" = "0" ]; then
          T_L="$T_L $p"
         elif [ "$(echo $O_LIST | grep $p )" != "" ]; then
          T_L="$T_L $p"
         fi
        # fi
      done

       if [ "$T_L" = "" ]; then
          MMM=no
          O_LIST=""
       elif [ "$T_L" != "" ]; then
         O_LIST=$T_L
         T_L=""
       fi
      #echo "LABEL	$k	NODE LIST $O_LIST"
       NNN=1
 done  
  NODE_LIST=$O_LIST
fi
 #fi
  print_state
  exit
elif [ "$DO" = "type_reserve" ]; then
if [ "$(echo $SEC | grep yes )" != "" ]; then
 TYPE="sec_${TYPE}"
fi

TARGET_TYPE=$TYPE
     case "$TYPE_1"
	in
                dus51) TARGET_TYPE=dus5
                   ;;
                dus) TARGET_TYPE=dus
                   ;;
                dus31) TARGET_TYPE=dus3
                   ;;
                dus52) TARGET_TYPE=dus5
                   ;;
                dus32) TARGET_TYPE=dus3
                   ;;
                tcu03) TARGET_TYPE=tcu0
                   ;;
                tcu04) TARGET_TYPE=tcu4                
                   ;;             
		\?) TARGET_TYPE=$TYPE
		exit
                ;; 
	esac
      if [ "$SEC" = "" ]; then
          SEC=no
      elif [ "$SEC" != "no" ] && [ "$SEC" != "yes" ]; then
          SEC=no
      fi
##############################################################################################
  i=${JENKINS_URL}
  LABEL_LIST=$(echo $TARGET_TYPE | sed 's/:/ /g' )
  NNN=0
  MMM=yes
  O_LIST=""

 for k in $LABEL_LIST
  do
       L_LIST=$(curl -s $i/label/$k/api/json?pretty=true | grep nodeName | cut -f4 -d\")
        T_L=""
       for p in $L_LIST
        do
         if [ "$(curl -s $i/computer/$p/api/json?pretty=true | grep temporarilyOffline | awk '{print $3}' )" = "false" ]; then
         if [ "$NNN" = "0" ]; then
          T_L="$T_L $p"
         elif [ "$(echo $O_LIST | grep $p )" != "" ]; then
          T_L="$T_L $p"
         fi
         fi
      done

       if [ "$T_L" = "" ]; then
          MMM=no
          O_LIST=""
       elif [ "$T_L" != "" ]; then
         O_LIST=$T_L
         T_L=""
       fi
     # echo "LABEL	$k	NODE LIST $O_LIST"
       NNN=1
 done  
  NODE_LIST=$O_LIST



##############################################################################################
     # NODE_LIST=$(cat $JENKINS_CONFIG | grep remoteFS | grep remoteFS | grep $TARGET_TYPE | grep -v "_" | cut -f5- -d\/ | cut -f1 -d\< | sort | grep -v "_")
      curl -k --silent --insecure https://rbs-rde.rnd.ki.sw.ericsson.se/cgi-bin/hw-book.pl > .tmp_hw_book.txt
      for NODE in $NODE_LIST
      do
         get_state 
         if [ "$SEC" = "no" ]; then
             N=$(echo $NODE_STATE | grep ":ONLINE:yes:yes:" | grep -v SEC )
         else
             N=$(echo $NODE_STATE | grep ":ONLINE:yes:yes:" | grep SEC )
         fi
         if [ "$N" != "" ]; then
             NODE=$(echo $N | cut -f1 -d: )
             lock_file
             reserve
             exit


         fi
      done
    echo ""
    echo "-------------------------------------------------------------------------------------------------------------------"	
    echo "There is no idle node  type \"$TYPE\" for the moment!"
    echo "Try again later."
    echo "-------------------------------------------------------------------------------------------------------------------"
    echo ""
      exit
else
  usage
  exit
fi
<node_type>

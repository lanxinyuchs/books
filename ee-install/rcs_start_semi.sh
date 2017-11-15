#!/bin/sh

usage() {
 echo "usage: $0 boardname"
}

if [ -z $1 ];
   then usage
   exit 1
fi
   

# GET LMT from lab webserver
WEBSERVURL=https://rbs-rde-wiki.rnd.ki.sw.ericsson.se/Main/LabConfig?raw=on
BOARDNAME=${1:?Missing boardname argument}
SUMFILE="RbsSummaryFile.xml"
if [ "test$2" = "testnl" ];then
   SUMFILE="RbsSummaryFile.xml.nl"
fi
LMT_IP=$(wget --no-check-certificate -nd -q $WEBSERVURL -O - | grep $BOARDNAME | awk -F '|' '{print $11}' | tr -d " ")


## DATA entries passed to netloader
SFTP_IP="host=10.68.101.150"
SFTP_USER="username=dustest"
SFTP_PASS="password=dustest"
TIMESTAMP="utcMs=$(($(date -u +%s)*1000))"
SUMPATH="filename=boards/$BOARDNAME/$SUMFILE"
ACTION="DoIntegrate=Integrate"

# Put it together to a curl command
DATA="--data $SFTP_IP&$SFTP_USER&$SFTP_PASS&$SUMPATH&$TIMESTAMP&$ACTION" 
URL=https://$LMT_IP/cgi-bin/nl_gui:post
CURL_PARS="-v -k --noproxy $LMT_IP $DATA $URL"

# do it!
echo " running: curl $CURL_PARS"
REPLY=$(curl $CURL_PARS)
echo "curl REPLY:"
echo $REPLY




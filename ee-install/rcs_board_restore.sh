#!/bin/sh


usage() {
 echo "usage: $0 boardname [hard]"
}

if [ -z $1 ];
   then usage
   exit 1
fi
   

# GET LMT from lab webserver
WEBSERVURL=https://rbs-rde-wiki.rnd.ki.sw.ericsson.se/Main/LabConfig?raw=on
BOARDNAME=${1:?Missing boardname argument}
ACTION="DoFactoryReset=FactoryReset"
if [ "test$2" = "testhard" ];then
    ACTION="DoHardFactoryReset=HardFactoryReset"
fi
LMT_IP=$(wget --no-check-certificate -nd -q $WEBSERVURL -O - | grep $BOARDNAME | awk -F '|' '{print $11}' | tr -d " ")


# Put it together to a curl command
DATA="--data $ACTION" 
URL=https://$LMT_IP/cgi-bin/aicGui:post
CURL_PARS="-v -k --noproxy $LMT_IP $DATA $URL"

# do it!
echo " running: curl $CURL_PARS"
REPLY=$(curl $CURL_PARS)
echo "curl REPLY:"
echo $REPLY


# curl for hard factory reset
# curl --insecure -v --noproxy $LMT_IP --data "DoHardFactoryReset=HardFactoryReset" https://$LMT_IP/cgi-bin/aicGui:post

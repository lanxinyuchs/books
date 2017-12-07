#!/bin/bash 
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2012 - 2014 All rights reserved.
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
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## -          2014-01-09   etxjovp     Created
usage()

{
echo "Ex:
rcs_upmod_and_upgradeprep.sh -n dus046  -r CXS101549_1-R2A3151
rcs_upmod_and_upgradeprep.sh -n dus046 -m undefined  -r CXS101549_1-R2A3151
rcs_upmod_and_upgradeprep.sh -n dus046 -m /home/nisse/mod_fake.sh  -r CXS101549_1-R2A3151
 "
}
MOD_SCRIPT=""
RCT_UPGRADE_PREP="upgradeprep.sh"
RCS_UPMOD4UGRADE="$RCT_TOP/test/bin/rcs_upmod4upgrade.sh"
######################################################################################
#   MAIN
######################################################################################
while getopts ht:r:m:n: option
do
	case "$option"
	in
                n) NODE=$OPTARG #  
                   ;;
                m) MOD_SCRIPT=$OPTARG #  
                   ;; 
                r) CXSREV=$OPTARG #  
                   ;;                              
		h) usage
                  exit;; 
		\?) usage
		exit;; 
	esac
done
if [ -f $CXSREV ]; then
  if [ "$MOD_SCRIPT" = "undefined" ] || [ "$MOD_SCRIPT" = "" ]; then
   echo "$RCS_UPMOD4UGRADE -N $NODE -m $MOD_SCRIPT -r $CXSREV -S yes -P no "
   $RCS_UPMOD4UGRADE -N $NODE -m undefined -f $CXSREV -S yes -P no > .$$rcs_upmod4upgrade.txt
 else
   echo "$RCS_UPMOD4UGRADE -N $NODE -m $MOD_SCRIPT -r $CXSREV -S yes -P yes"
   $RCS_UPMOD4UGRADE -N $NODE -m $MOD_SCRIPT -f $CXSREV -S yes -P yes > .$$rcs_upmod4upgrade.txt
 fi

else
 if [ "$MOD_SCRIPT" = "undefined" ] || [ "$MOD_SCRIPT" = "" ]; then
   echo "$RCS_UPMOD4UGRADE -N $NODE -m $MOD_SCRIPT -r $CXSREV -S yes -P no "
   $RCS_UPMOD4UGRADE -N $NODE -m undefined -r $CXSREV -S yes -P no > .$$rcs_upmod4upgrade.txt
 else
   echo "$RCS_UPMOD4UGRADE -N $NODE -m $MOD_SCRIPT -r $CXSREV -S yes -P yes"
   $RCS_UPMOD4UGRADE -N $NODE -m $MOD_SCRIPT -r $CXSREV -S yes -P yes > .$$rcs_upmod4upgrade.txt
 fi
fi
cat .$$rcs_upmod4upgrade.txt
UP_PATH=$(tail -1 .$$rcs_upmod4upgrade.txt )
rm -f .$$rcs_upmod4upgrade.txt
if [ -f $UP_PATH ]; then
 $RCT_UPGRADE_PREP -stp $NODE $UP_PATH
else
  exit 1
fi

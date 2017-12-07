#!/bin/sh
## ----------------------------------------------------------
## Copyright (c) Ericsson AB 2017 All rights reserved.
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
## R1A/1      2012-02-23   etxkols     Created
## R1A/2      2012-02-25   etxkols     Error checking when running rct_stp_config.pl
## R1A/3      2012-02-27   etxkols     exit 0 when -sim
## R1A/4      2012-03-09   etxkols     Possible to override hw booking
## R1A/5      2012-03-09   etxkols     Updated usage()
## R1A/5      2012-03-14   etxkols     Changed log dir to 
## R1A/6      2012-05-25   etxkols     Print which user has booked the hw 
## R1A/6      2012-05-25   etxkols     Print which user has booked the hw 
## R2A/1      2013-04-08   etxkols     Aligning syntax with other scrips
## R2A/2      2013-04-09   etxkols     Removed -sim option because it is only confusing
## R2A/3      2013-04-09   etxkols     Added single node option
## R4A/1      2015-09-15   etxkols     Added simulated cluster
## R4A/2      2015-11-06   etxkols     chmod dirs
## R4A/3      2015-11-06   etxkols     chmod dirs
## R4A/5      2016-03-11   etxkols     5G
## R4A/6      2016-03-22   etxkols     Change ccs to rcf
## R4A/7      2016-05-11   etxkols     Bug for cluster
## R4A/8      2016-05-25   etxkols     VRCS + BRCS
## R5A/1      2016-07-05   etxkols     Wrong path to rct_stp_config.pl
## R8A/1      2016-11-21   etxkols     redhat
## R8A/2      2016-12-07   etxkols     print args
## R9A/1      2016-02-09   etxkols     redesign
## R9A/2      2016-02-14   etxkols     Handle exit code
## R9A/3      2016-02-14   etxkols     New try to fix problem with & in argument
## R9A/4      2017-09-11   etxkols     Support several Jenkins Servers
## R9A/5      2017-10-04   etxkols     Run simulated RCS in cloud
## ----------------------------------------------------------

progname="`basename $0`"
STP_DIR=/proj/rcs-tmp/stps
BIN_DIR=${RCT_TOP}/test/bin
BOOKING_DIR="/proj/webdocs/rbs-rde/etc/host-book"
#NODE_BOOKING_LIST="/proj/rcs/ci-book/node_list.txt"
NODE_BOOKING_LIST="/proj/rcs/ci-book/*/node_list.txt" # Support several Jenkins Servers
usage() {
   echo "usage single:    rct_add_stp.sh <Node> 
usage clustered: rct_add_stp.sh -stp <STPname> -nodes <Nodes>
                 rct_add_stp.sh -sim <STPname> -nodes <N>

<Node>      If Node is dus or tcu, Node is identity
            If Node is in cloud, rcf, Node is both identity and IP adddress
<STPname>   Name of STP, same as in upgradeprep.sh and rct_run.sh 
<Nodes>     List of <Node> belonging to STPname              
<N>         Integer, number of nodes when running clustered simulator

Description: Creates ${STP_DIR}/STPname/logs directory and 
             ${STP_DIR}/STPname/config/stp.cfg file.

examples: rct_add_stp.sh dus001 -> rct_add_stp.sh -stp dus001 -nodes dus001
          rct_add_stp.sh rcf001 ssh_lmt_ipv4
          rct_add_stp.sh -stp kalle -nodes dus001 dus002
          rct_add_stp.sh -stp kalle -nodes rcf001 dus002"
   exit 1
}

error() {
    echo "${progname}: ERROR $1"
    exit 1
}
    
SingleRcf="no"
echo "${progname} $@"
if [ $# -eq 1 ]; then
    case $1 in
	rcf*) echo "${progname}: VNF node $Node in cloud needs more parameters"
	      usage;;
	sim*) echo "${progname}: Simulated node $Node in cloud needs more parameters"
	      usage;;
	*)    STPname=$1; Nodes=$1;;
    esac
elif [ $# -eq 2 ]; then
############## BEGIN remove when all new stuff in #####################
    STPname=$1; SingleRcf="yes" # temporary WO to handle vrcstprep.sh
    SSH_LMT_IPV4=`grep KI10_rcs_.*oam /proj/rcs-tmp/stps/$STPname/config/vrcs.txt | cut -d " " -f 2`
    PRAN_FROUNTHAUL_IPV4=`grep KI10_rcs_.*traffic /proj/rcs-tmp/stps/$STPname/config/vrcs.txt | cut -d " " -f 2`
    RcfArgs="SSH_LMT_IPV4 $SSH_LMT_IPV4 0 25 10.68.102.1 PRAN_FROUNTHAUL_IPV4 $PRAN_FROUNTHAUL_IPV4 0 25 10.68.102.129"
############## END remove when all new stuff in #####################
#    usage
elif [ $# -eq 3 ]; then
    usage
elif [ $# -ge 4 ]; then
    case $1 in
	rcf*) STPname=$1; SingleRcf="yes"; Nodes=$1; shift; RcfArgs=$@;;
	sim*) STPname=$1; SingleRcf="yes"; Nodes=$1; shift; RcfArgs=$@;;
	-h) usage;;
	-stp) STPname=$2; shift; shift; 
	    case $1 in 
		-nodes) shift; Nodes=$@;;
	 	*) usage;;
	    esac;;
 	*) usage;;
    esac
else
    usage
fi

mkdir -p "${STP_DIR}/$STPname/logs" || error "Creation of logs Dir [${STP_DIR}/$STPname/logs] Failed!"
chmod 775 ${STP_DIR}/$STPname/logs 2> /dev/null
mkdir -p "${STP_DIR}/$STPname/config" || error "Creation of config Dir [${STP_DIR}/$STPname/config] Failed!"
chmod 775 ${STP_DIR}/$STPname/config 2> /dev/null

###################### Check that boards are booked by $USER ######################
if [ "$SingleRcf" = "yes" ] ; then
    Nodes="single_rcf $RcfArgs"
else
    for Node in $Nodes; do
	case $Node in
	    rcf*) 
		echo "${progname}: $Node in cloud, not checking if resource is booked";;
	    sim*) 
		echo "${progname}: $Node in cloud, not checking if resource is booked";;
	    *)
		echo "${progname}: Verifying that HW is booked by $USER"
		Booker=`cat $BOOKING_DIR/$Node`
		case $Booker in
		    $USER) echo "${progname}: $Node booked by $USER";;
		    *) 
			if [ "$(cat $NODE_BOOKING_LIST | grep $USER | grep $Node)" = "" ]; then
			    echo -n "${progname}: $Node booked by $Booker, do you want to continue (yes/no)? "
			    read answer
			    case $answer in
				yes) ;;
				*)   exit 1;;
			    esac
			else
			    echo "${progname}: $Node booked by $USER"
			fi
		esac
	esac
    done
fi
#ARGS="${STP_DIR} $STPname $Nodes"
#echo "${progname}: /home/etxkols/tmp/ct2/jsone/rct_stp_config.pl ${STP_DIR} $STPname $Nodes"
#if eval ${BIN_DIR}/rct_stp_config.pl $ARGS ; then
#    echo "${progname}: ${STP_DIR}/$STPname/config/stp.cfg updated OK"
#else
#    error "when creating configuration data"
#fi
ARGS="${STP_DIR} $STPname $Nodes"
echo "${progname}: ${BIN_DIR}/rct_stp_config.pl $ARGS"
if eval ${BIN_DIR}/rct_stp_config.pl $ARGS ; then
    echo "${progname}: ${STP_DIR}/$STPname/config/stp.cfg updated OK"
else
    error "when creating configuration data"
fi



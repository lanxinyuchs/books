#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	vrcstprep.sh %
# %CCaseRev:	/main/24 %
# %CCaseDate:	2016-06-13 %
# Author:       etxkols
#
# Short description: Get files needed for installation on target
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2016 All rights reserved.
# 
# The information in this document is the property of Ericsson.
# 
# Except as specifically authorized in writing by Ericsson, the 
# receiver of this document shall keep the information contained 
# herein confidential and shall protect the same in whole or in 
# part from disclosure and dissemination to third parties.
# 
# Disclosure and disseminations to the receivers employees shall 
# only be made on a strict need to know basis.
# %CCaseCopyrightEnd%
#
#----------------------------------------------------------------------
# #1.    REVISION LOG
#----------------------------------------------------------------------
# Rev      Date       Name        What
# -----    -------    --------    -------------------------------------
# main/1   2016-03-22 etxkols     Created
# main/2   2016-03-22 etxkols     only_load_image option
# main/3   2016-03-23 etxkols     dirty fixes
# main/5   2016-03-29 etxkols     fixes
# main/6   2016-03-30 etxkols     fixes
# main/7   2016-03-31 etxkols     fixes
# main/8   2016-03-31 etxkols     fixes after talking to node CI guys
# main/10  2016-04-08 etxkols     several networks
# main/11  2016-04-08 etxkols     typo
# main/12  2016-04-11 etxkols     typo
# main/13  2016-04-11 etxkols     temporary fix
# main/14  2016-04-11 etxkols     temporary fix reverted
# main/15  2016-04-12 etxkols     cloud reinstalled
# main/16  2016-05-12 etxkols     New rcs-ci cloud
# main/17  2016-05-19 etxkols     Removed defaults + added ip option
# main/18  2016-05-19 etxkols     Reverted to /16
# main/19  2016-05-19 etxkols     RCS lab adaptions
# main/20  2016-05-19 etxkols     Reverted to /16
# main/21  2016-05-19 etxkols     Allow -d and -t in same call
# main/22  2016-05-24 etxkols     mkdir -p /proj/rcs-tmp/stps/$INSTANCE/config
# main/23  2016-05-25 etxkols     typo
# main/24  2016-06-13 etxkols     Added rcs project
#----------------------------------------------------------------------
progname="`basename $0`"

error() {
    echo "${progname}: ERROR $1"
    exit 1
}
    
print() {
    echo "${progname}: $1"
}

usage() {
   echo "Usage: ${progname} Options

Options:
-p  <Project>   mandatory rcs-ci | cpp | rbs | youlab | rcs 
-c  <Image>     create Image
-d  <ImageName> delete Image
-l  <Instance>  launch Instance
-t  <Instance>  terminate Instance
-f  <Flavor>    mandatory with -l
-n  <Network>   mandatory with -l
-w  <Lab>       optional, temporary fix to distinguish rcs_lab from other labs
-fl             returns a list of all flavors
-ip <Instance>  returns IP addresses for Instance

${progname} -p rcs-ci -c /proj/rcs-tmp/etxkols/vrcs-R5A24            copies Image to cloud
${progname} -p rcs-ci -d /proj/rcs-tmp/etxkols/vrcs-R5A24            deletes Image from cloud
${progname} -p rcs-ci -t rcf100                                      deletes Instance from cloud
${progname} -p rcs-ci -t rcf100 -d /proj/rcs-tmp/etxkols/vrcs-R5A24  deletes Image and Instance from cloud
${progname} -p rcs-ci -l rcf100 -f VRCS -c /proj/rcs-tmp/etxkols/vrcs-R5A24 -n KI10_rcs_ci_oam -n KI10_rcs_ci_traffic -w rcs_lab
                                                                      loads image vrcs-R5A24 to cloud it is not already loaded
                                                                      terminates rcf001 if it exists
                                                                      launches rcf001 with:
                                                                      Flavor VRCS
                                                                      Image vrcs-R5A24
                                                                      Networks public_net and net04_ext
${progname} -p rcs-ci -fl                                            lists all flavors
${progname} -p rcs-ci -ip rcf100                                     returns all IP addresses"
   exit 1
}

source_env() {
    if [ "$PROJECT" = "cpp" ] ; then
#	print "source /proj/rbs-g2-ci/5G/openstack/cpp-openrc.sh"
	source /proj/rbs-g2-ci/5G/openstack/cpp-openrc.sh
    elif [ "$PROJECT" = "rcs-ci" ] ; then
#	print "source /proj/rbs-g2-ci/5G/openstack/rcs-ci-openrc.sh"
	source /proj/rbs-g2-ci/5G/openstack/rcs-ci-openrc.sh  
    elif [ "$PROJECT" = "rbs" ] ; then
#	print "csh -c 'source /proj/rbs-g2-ci/5G/openstack/rbs-openrc.csh'"
#	csh -c 'source /proj/rbs-g2-ci/5G/openstack/rbs-openrc.csh'
#	print "source /proj/rbs-g2-ci/5G/openstack/rbs-openrc.sh"
	source /proj/rbs-g2-ci/5G/openstack/rbs-openrc.sh  
    elif [ "$PROJECT" = "youlab" ] ; then
#	print "csh -c 'source /proj/rbs-g2-ci/5G/openstack/youlab-openrc.csh'"
#	csh -c 'source /proj/rbs-g2-ci/5G/openstack/youlab-openrc.csh'
#	print "source /proj/rbs-g2-ci/5G/openstack/youlab-openrc.sh"
	source /proj/rbs-g2-ci/5G/openstack/youlab-openrc.sh
    elif [ "$PROJECT" = "rcs" ] ; then
	source /proj/rbs-g2-ci/5G/openstack/rcs-openrc.sh
    else
	error "no -p flag (Project) set"
    fi
    unset http_proxy
    unset https_proxy
    moduleInit="/app/modules/0/init/sh"
    # Init ARC environment
    if [ -s "${moduleInit}" ] ; then
	. ${moduleInit}
    else
	echo "${progname}: ERROR: ${moduleInit} is not available, aborting!!!" >&2
	exit 1
    fi
    # Init pyton environment
    module load /app/rbs/modules/python/2.7.11
}

wait_until_instance_deleted() {
    Result=`nova show $INSTANCE 2>&1`
    if [ $? -eq 0 ] ; then
	if [ $expire -gt `date +%s` ]; then
	    echo "${progname}: $INSTANCE not deleted retrying in 1 sec"
	    sleep 1
	    wait_until_instance_deleted
	else
	    error "$INSTANCE not deleted"
	fi
    else
	echo "${progname}: $INSTANCE deleted"
    fi
}

delete_instance() {
    echo "${progname}: nova show $INSTANCE"
    Result=`nova show $INSTANCE 2>&1`
    if [ $? -eq 0 ] ; then
	echo "${progname}: $INSTANCE created, deleting it"
	echo "${progname}: nova delete $INSTANCE"
	Result=`nova delete $INSTANCE 2>&1`
	echo "${progname}: $Result"
	now=`date +%s`
	expire=`expr $now + 30`
	wait_until_instance_deleted
    else
	echo "${progname}: $INSTANCE not created"
    fi
}

wait_until_instance_created() {
    Result=`nova show $INSTANCE 2>&1`
    if  [ $? -eq 0 ]; then
	echo "${progname}: $INSTANCE created"
    else
	if [ $expire -gt `date +%s` ]; then
	    echo "${progname}: $INSTANCE not created retrying in 1 sec"
	    sleep 1
	    wait_until_instance_created
	else
	    error "$INSTANCE not created"
	fi
    fi
}

create_instance() {
    echo "${progname}: nova boot --flavor $FLAVOR $NETIDS --image $ID $INSTANCE"
    Result=`nova boot --flavor $FLAVOR $NETIDS --image $ID $INSTANCE 2>&1`
    if [ $? -eq 0 ] ; then
	echo "${progname}:"
	echo "$Result"
	now=`date +%s`
	expire=`expr $now + 60`
	wait_until_instance_created
    else
	error "creating instance: $Result"
    fi
}

wait_until_instance_ACTIVE() {
    if nova show $INSTANCE | grep status | grep ACTIVE > /dev/null ; then
	echo "${progname}: $INSTANCE ACTIVE"
    else
	if [ $expire -gt `date +%s` ]; then
	    echo "${progname}: $INSTANCE not ACTIVE retrying in 1 sec"
	    sleep 1
	    wait_until_instance_ACTIVE
	else
	    error "$INSTANCE not ACTIVE"
	fi
    fi
}

glance_image_list() {
    echo "${progname}: glance image-list"
    REPLY=`glance image-list`
    if [ $? -eq 0 ] ; then
	echo "${progname}:"    
	echo "$REPLY"    
	ID=`echo "$REPLY" | sed -n "s/^|\s\s*\(\S\S*\)\s\s*|\s\s*$IMAGE_NAME\s\s*.*/\1/p"`
    else
	error "glance image-list failed"
    fi
}

create_image() {
    glance_image_list
    if [ "$ID" = "" ] ; then
	print "Name $IMAGE_NAME not loaded"
	print "Loading Image $IMAGE_PATH"
	CMD="glance image-create --disk-format qcow2 --container-format qcow2 --container-format bare --name $IMAGE_NAME --file $IMAGE_PATH"
	REPLY=`$CMD`
	if [ $? -eq 0 ] ; then
	    echo "${progname}:"    
	    echo "$REPLY"    
	    ID=`echo "$REPLY" | sed -n "s/^|\s\s*id\s\s*|\s\s*\(\S\S*\).*/\1/p"`
	    print "Image loaded with ID $ID"
	else
	    error "glance image-create failed"
	fi
    else
	print "Name $IMAGE_NAME already loaded with ID $ID"
    fi
}

delete_image() {
    glance_image_list
    if [ "$ID" = "" ] ; then
	print "Name $IMAGE_NAME not loaded"	    
    else
	CMD="glance image-delete $ID"
	REPLY=`$CMD`
	if [ $? -eq 0 ] ; then
	    print "Image $ID ($IMAGE_NAME) deleted"
	else
	    error "glance image-delete $ID ($IMAGE_NAME) failed"
	fi
    fi
}

get_net_id() {
    echo "${progname}: openstack network list"
    REPLY=`openstack network list 2>&1`
    if [ $? -eq 0 ] ; then
	echo "${progname}:"    
	echo "$REPLY"   
	NETID=""
	for NET in $NETWORK ; do
	    NETID=`echo "$REPLY" | grep $NET | cut -d "|" -f 2 | tr -d ' '`
	    if [ "$NETID" = "" ] ; then
		error "Could not find $NET network"
	    else
		NETIDS="$NETIDS --nic net-id=$NETID"
	    fi
	done
	print "Found networks $NETIDS"
    else
	error "$REPLY"
    fi
}

get_ips() {
    echo "${progname}: nova show $INSTANCE"
    REPLY=`nova show $INSTANCE 2>&1`
    if [ $? -eq 0 ] ; then
	echo "${progname}:"    
	echo "$REPLY"   
	NETIP=""
	for NET in $NETWORK ; do
	    NETIP=`echo "$REPLY" | grep $NET | cut -d "|" -f 3 | tr -d ' '`
	    if [ "$NETIP" = "" ] ; then
		error "Could not find $NET IP"
	    else
		NETIPS="$NETIPS $NETIP"
	    fi
	done
	print "Found networks IPs $NETIPS"
    else
	error "$REPLY"
    fi
}

get_network_ips() {
    REPLY=`nova show $INSTANCE 2>&1`
    if [ $? -eq 0 ] ; then
	NETW_IPS=""
	NETW_IPS=`echo "$REPLY" | grep network | sed -n "s/^|\s\s*\(\S\S*\)\s*network\s\s*|\s\s*\(.*\)\s\s*.*/\1 \2/p"`
	if [ "$NETW_IPS" = "" ] ; then
	    error "Could not find network IPs"
	fi
#	NETIP=`echo "$REPLY" | grep network | cut -d "|" -f 3 | tr -d ' '`
#	echo $NETIP
    else
	error "$REPLY"
    fi
}

print "$*"

PROJECT=""
INSTANCE=""
INSTANCE_ACTION=""
IMAGE_ACTION=""
FLAVOR=""
NETWORK=""
WHICH_LAB=""
FLAVOR_LIST=""
IP_LIST=""
if [ "$#" -eq 0 ]; then usage ; fi
while [ "$#" -gt 0 ]; do
    case "$1" in
	-p)  PROJECT=$2;shift;;
	-c)  IMAGE_ACTION="create";IMAGE_PATH=$2;IMAGE_NAME=`basename $IMAGE_PATH`;shift;;
	-d)  IMAGE_ACTION="delete";IMAGE_PATH=$2;IMAGE_NAME=`basename $IMAGE_PATH`;shift;; 
	-l)  INSTANCE_ACTION="launch";INSTANCE=$2;shift;;
	-t)  INSTANCE_ACTION="terminate";INSTANCE=$2;shift;;
	-f)  FLAVOR=$2;shift;;
	-n)  NETWORK="$NETWORK $2";shift;;
	-w)  WHICH_LAB=$2;shift;;
	-fl) FLAVOR_LIST="true";;
	-ip) IP_LIST="true";INSTANCE=$2;shift;;
	-h)  usage;;
	*)   usage
    esac
    shift
done

source_env

if [ "$FLAVOR_LIST" = "true" ] ; then
    REPLY=`nova flavor-list | grep "^|" | grep -v Name | cut -d "|" -f 3`
    if [ $? -eq 0 ] ; then
	echo "$REPLY"
	exit 0
    else
	error "$REPLY"
    fi
fi

if [ "$IP_LIST" = "true" ] ; then
    get_network_ips
    echo "$NETW_IPS"
    exit 0
fi

if [ "$IMAGE_ACTION" = "create" ]; then
    create_image
elif [ "$IMAGE_ACTION" = "delete" ] ; then
    delete_image
fi

if [ "$INSTANCE_ACTION" = "" ] ; then
    exit 0
elif [ "$INSTANCE_ACTION" = "terminate" ] ; then
    delete_instance
    exit 0
elif [ "$INSTANCE_ACTION" = "launch" ] && [ "$IMAGE_ACTION" = "create" ]; then
    if [ "$NETWORK" = "" ]; then
	error "Mandatory option -n not given"
    fi
    if [ "$FLAVOR" = "" ]; then
	error "Mandatory option -f not given"
    fi
    create_image
    get_net_id 
    delete_instance
    create_instance
    now=`date +%s`
    expire=`expr $now + 180`
    wait_until_instance_ACTIVE
else
    usage
fi

get_ips
echo "$INSTANCE IP addresses: $NETIPS"
if [ "$WHICH_LAB" = "rcs_lab" ] ; then
    print "RCS lab detected"    
    get_network_ips 
    mkdir -p /proj/rcs-tmp/stps/$INSTANCE/config
    chmod 775 /proj/rcs-tmp/stps/$INSTANCE/config
    echo "$NETW_IPS" > /proj/rcs-tmp/stps/$INSTANCE/config/vrcs.txt
    if [ $? -eq 0 ] ; then
	print "/proj/rcs-tmp/stps/$INSTANCE/config/vrcs.txt updated OK"
	chmod 664 /proj/rcs-tmp/stps/$INSTANCE/config/vrcs.txt
	IP=`echo $NETIPS | cut -d " " -f1`
	rct_add_stp.sh $INSTANCE $IP
    else
	error "Could not write /proj/rcs-tmp/stps/$INSTANCE/config/vrcs.txt"
    fi
fi

#!/bin/ash
## -----------------------------------------------------------------------------
## %CCaseFile:	dhcpc_conf.sh %
## @version /main/R2A/R3A/R4A/R5A/2
## %CCaseDate:	2016-01-13 %
## %CCaseDocNo:  %
##
## Author:      etxlg
## Description: 
## using "ash" as interpreter to minimize bash:shellshock impact
## callback script for udhcpc
## the file needs to be chmod to executable or it won't run
##
## tested using this version of dhcp-client
## BusyBox v1.21.1 (2014-04-03 22:54:53 CEST) multi-call binary.
## Usage: udhcpc [-fbnqoCRB] [-i IFACE] [-r IP] [-s PROG] [-p PIDFILE]
##        [-V VENDOR] [-x OPT:VAL]... [-O OPT]...
##
## -----------------------------------------------------------------------------
##
## ----------------------------------------------------------
## %CCaseTemplateFile:   %
## %CCaseTemplateId: %
##
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
##
## ----------------------------------------------------------
## #1.    REVISION LOG
## ----------------------------------------------------------
## Rev        Date         Name        What
## --------   --------     --------    ------------------------
## R3A/2      2015-03-31   etxlg       Add default route if namespaced
## R3A/3      2015-04-07   etxlg       link-local IP mask -> 24
## R4A/1      2015-08-12   etxtory     link-local IP mask -> 16 (NL updated)
## R4A/2      2015-12-09   etxlg       Inappropriate lab w/a for secure board
## R4A/3      2015-12-09   etxlg       Somewhat less than perfect - try again
## R5A/1      2015-12-15   etxlg       Branched, R4 will now be reverted
## R5A/2      2016-01-13   etxlg       adddr -> addr

# options to udhcpc, most are not used by us
# -f		- run in foreground
# -b		- background if lease is NOT obtained
# -n		- exit if lease is NOT obtained
# -q		- exit after obtaining lease
# -o		- do not request any option
# -C		- don't send MAC as client identifier
# -R		- release IP on exit
# -B		- something, something broadcast
# -i iface	- use this interface default eth0
# -r IP		- IP address to request
# -s prog	- run this at event
# -p pidfile	- create PID file
# -V vendor	- vendor identifier
# -x opt:val	- include option in sent packets
# -O opt	- request options

#these may not be implemented in the version we run...
# -t retries	- send this many discovery packets
# -T pause	- pause between packets (3s)
# -A tryagain	- wait seconds after failure (20s)
# -S		- log to syslog too
# -a		- Use arping to validate offered address
# -F name	- ask server to update DNS mapping
# -H, -h name	- send name as client hostname

# exampel when testing:
# udhcpc -f -i eth0 -s /rcs/dhcpc_conf.sh
# -f to have it stay in foreground, good when connecting from portprogram in
# erlang - if it exits this is immediately detected and we can have use this
# to implement simple supervision.
# -i eth0 should be always the same 
# -s <filename> replace with the actual filepath in the delivered SW

#$1: deconfig, bound, renew, leasefail
#some variables of use set by the client, values are examples from testing:
#serverid=10.68.200.11
#mask=24
#siaddr=10.68.200.11		- nextserver
#interface=eth0
#dns=10.68.200.11 10.68.200.12
#opt53=05 			- dhcp messagetype
#lease=86400
#domain=ki.sw.ericsson.se
#subnet=255.255.255.0
#ntpsrv=10.86.148.3		- ntpserver
#ip=10.86.148.202		- my ip assigned by server
#router=10.86.148.1		- default router


##exec >>/tmp/dhcp.log 2>&1

case ${1:-unset} in
    deconfig)
	echo "#DECONFIG#"
	# following not normally needed, already done by oneshot-dhcp called by
	# udev, but, the oneshot-dhcp is to be removed from EE, any day now...
	ip link set dev $interface up
	# this adds a FIXED linklocal address to eth0
	ip addr add dev $interface label ${interface}:1 scope link \
		local 169.254.2.2/16 broadcast +
    ;;
    bound|renew)
	ip addr add dev ${interface} local $ip/$mask broadcast +
	#these are read and parsed in ootDhcp
	echo "#BOUND_IP#<space>${ip}"
	echo "#MASK#<space>${mask}"
	echo "#SUBNET#<space>${subnet}"
	echo "#ROUTER#<space>${router}"
	echo "#DNS#<space>${dns}"
	echo "#NEXTSERVER#<space>${siaddr}"
	if
	    cup -c >/dev/null 2>&1 && [ -n "${router}" ]
	    #true && [ -n "${router}" ] #use this when testing
	then
	    #cup -c returned 0 we are secure, add workaround for LAB
	    ip route add to 128.0.0.0/1 via ${router} >/dev/null 2>&1
	    ip route add to 0.0.0.0/1 via ${router} >/dev/null 2>&1
	    echo "#LAB_WORKAROUND#" #trigger info print in ootDhcp
	fi
	echo "#END#" #this will trigger an info print in ootDhcp
## Namespaced LMT not implemented (not to be implemented either?)
##	if [ -f /var/run/netns/LMT_net_ns ]; then
##	    #we assume netspaced if this file is found; add default route
##	    if [ -n "$router" ]; then
##		ip route replace to default via $router >/dev/null 2>&1
##	    fi
##	fi
    ;;
    leasefail)
	echo "#LEASEFAIL#"
	# remove any existing dhcp assigned address (global) and lab routes
	# this construct should ensure link-local address is kept
	if
	    cup -c >/dev/null 2>&1 && [ -n "${interface}" ]
	then
	    ip addr flush scope global dev $interface
	    ip route del to 128.0.0.0/1
	    ip route del to 0.0.0.0/1
	fi
    ;;
    *)
	:
    ;;
esac

#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	rcstprep.sh %
# %CCaseRev:	/main/125 %
# %CCaseDate:	2016-07-05 %
# Author:	etxkols
#
# Short description: Get files needed for installation on target
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
# main/1   2012-05-29 etxkols     Created
# main/100 2016-01-22 etxkols     Reverting to /98 due to primarycoreref
# main/101 2016-01-28 etxkols     ipv6 again
# main/101 2016-02-01 eransbn     Change RbsSummaryFile.xml.nl to include up instead of RCS cxp
# main/103 2016-02-02 etxkols     ipv6 SNMP fix
# main/105 2016-02-02 etxkols     Updated usage 
# main/106 2016-03-07 etxkols     Handle new ipv6 configuration on VDIs 
# main/107 2016-03_21 etxkols     New IP net in lab and cloud
# main/108 2016-03_22 etxkols     Cloud instance ccsxxx calls vrcstprep.sh
# main/109 2016-03-29 etxkols     Cloud fixes
# main/110 2016-03-31 etxkols     vrcstprep.sh rewritten
# main/111 2016-04-07 etxkols     New vlans for new network
# main/112 2016-04-07 etxkols     New params for vrcstprep.sh
# main/113 2016-04-07 etxkols     Gaahhhh
# main/114 2016-04-12 etxkols     Cloud reinstalled
# main/115 2016-04-13 etxkols     KI10_cpp_pran
# main/115 2016-04-22 etxkols     Changing ccs to rcf
# main/116 2016-04-27 etxkols     bpu
# main/117 2016-04-29 etxkols     HALI for R6 (17A)
# main/119 2016-05-12 etxkols     New networks for cloud
# main/120 2016-05-13 etxkols     Typo
# main/121 2016-05-18 etxkols     Default for vrcs
# main/122 2016-05-18 etxkols     VRCS fix
# main/123 2016-07-05 etxkols     HAL
# main/124 2016-07-05 etxkols     Added path to HAL DUMMY UP
# main/125 2016-07-05 etxkols     Moved comments
#----------------------------------------------------------------------
set -euo pipefail

progname="`basename $0`"
BOOKING_DIR="/proj/webdocs/rbs-rde/etc/host-book"
NODE_BOOKING_LIST="/proj/rcs/ci-book/node_list.txt"
tftpDir="/proj/rcs-tmp/tftpboot/"
stpsDir="/proj/rcs-tmp/stps/"
webServer='https://rbs-rde-dev.rnd.ki.sw.ericsson.se'
tcu04package='/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T_CXS101549_6/doc/19010/RCP-T_CXS101549_6.cxs'
dus52package='/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.cxs'
brcspackage='/vobs/rcs/delivery/BRCS_CSX10179_3/BRCS-UP_CXS101665_3/doc/19010/BRCS-UP_CXS101665_3.cxs'
vrcsimage='/vobs/rcs/delivery/VRCS_CSX10179_2/VRCS-UP_CXS101657_2/doc/19010/VRCS-UP_CXS101657_2.qcow2'
tcu04packageHALI='/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T_CXS101549_6/doc/19010/RCP-T_CXS101549_6.zip'
dus52packageHALI='/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.zip'
dus52packageHALISplit='/vobs/rcs/delivery/RCP_CSX10179_1/RCS-DUS2_CXS101549_8/doc/19010/RCS-DUS2_CXS101549_8.zip'
TCU03_cxp='RCS-TCU03_CXP9031274_3.cxp'
TCU04_cxp='RCS-T_CXP9031274_4.cxp'
DUS2_cxp='RCS-DUS2_CXP9031275_3.cxp'
BRCS_cxp='BRCS_CXP9032853_3.cxp'
DUS2HAL_cxp='RCS-DUS2_CXP9031275_4.cxp'

usage() {
   echo "Usage: ${progname} <Node> <UP> [ -no_strip_signature | -no_oamap | -ipv4_oamap | -ipv6_oamap ]

<Node>:      Board
<UP>:        URL, Label or Filepath where to fetch <UP> from
             If Label is given, UPpath@@/Label is downloaded.
             If URL is given, UP is downloaded from URL, ex node CI.
	     If FilePath is given, UP is copied from FilePath
-oamap_ipv4: Optional, board will be installed with ipv4 OamAccessPoint configured (not supported in RCS lab for secure boards in 16A)
-oamap_ipv6: Optional, board will be installed with ipv6 OamAccessPoint configured (not supported in 16A)

Description: Downloads UP to /proj/rcs-tmp/tftpboot/Hw and extracts/creates files necessary for installation and installs the Hw.

example: ${progname} tcu020 CXS101549_3-R2A2239
         ${progname} tcu020 CXS101549_3-R2A2239
         ${progname} tcu020 https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs@@/CXS101549_1-R2A835
         ${progname} tcu020 https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-ARM_CXS101549_3/doc/19010/RCP-ARM_CXS101549_3.cxs
         ${progname} tcu020 /vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs@@/CXS101549_1-R2A835
         ${progname} tcu020 /vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs
         ${progname} tcu020 /vobs/rcs/delivery/RCP_CSX10179_1/RCP-TCU03_CXS101549_4/doc/19010/RCP-TCU03_CXS101549_4.cxs
         ${progname} tcu020 https://rbs-g2-ci.rnd.ki.sw.ericsson.se/view/CI/view/CL1%20Delivery%20Check/view/FW%20-%20CXP9021691/job/DC-CXP9021691/lastSuccessfulBuild/artifact/DC-CXP9021691.tgz"
   exit 1
}

error() {
    echo "${progname}: ERROR $1"
    exit 1
}

LMT_IP() {
    LMT_IP=`wget --no-check-certificate -nd -q https://rbs-rde-wiki.rnd.ki.sw.ericsson.se/Main/LabConfig?raw=on -O - | grep $Board | awk -F '|' '{print $11}'`
    LMT_IP=`echo $LMT_IP | sed -e "s/ //g"` # Remove all blanks
    if [ "$LMT_IP" == "" ]; then
	error "Failed to download LabConfig or IP address not configured for $Board!"
    fi
}

RCS_CXP() {
    if echo $UP | grep @@ > /dev/null; then
	UP_cxs=`expr $UP : '.*/\(.*\)@@'` # Clearcase
    else
	UP_cxs=`basename $UP`  # Not clearcase
    fi
    if file ${installDir}/$UP_cxs | grep "Zip archive data" > /dev/null; then
	echo "${progname}: zipped UP detected"
	if unzip -l ${installDir}/$UP_cxs | grep $TCU03_cxp > /dev/null; then
	    echo "${progname}: Old TCU03 CXP detected"
	    ( \cd ${installDir} && unzip -q $UP_cxs  $TCU03_cxp) ||error "Failed to extract [$TCU03_cxp] from [$UP_cxs]!"
	    RCS_CXP=$TCU03_cxp
	elif unzip -l ${installDir}/$UP_cxs | grep $TCU04_cxp > /dev/null; then
	    echo "${progname}: TCU CXP detected"
	    ( \cd ${installDir} && unzip -q $UP_cxs  $TCU04_cxp) ||error "Failed to extract [$TCU04_cxp] from [$UP_cxs]!"
	    RCS_CXP=$TCU04_cxp
	elif unzip -l ${installDir}/$UP_cxs | grep $DUS2_cxp > /dev/null; then
	    echo "${progname}: DUS CXP detected"
	    ( \cd ${installDir} && unzip -q $UP_cxs  $DUS2_cxp) ||error "Failed to extract [$DUS2_cxp] from [$UP_cxs]!"
	    RCS_CXP=$DUS2_cxp 
	elif unzip -l ${installDir}/$UP_cxs | grep $DUS2HAL_cxp > /dev/null; then
	    echo "${progname}: DUS2HAL CXP detected"
	    ( \cd ${installDir} && unzip -q $UP_cxs  $DUS2HAL_cxp) ||error "Failed to extract [$DUS2HAL_cxp] from [$UP_cxs]!"
	    RCS_CXP=$DUS2HAL_cxp 
	elif unzip -l ${installDir}/$UP_cxs | grep $BRCS_cxp > /dev/null; then
	    echo "${progname}: BRCS CXP detected"
	    ( \cd ${installDir} && unzip -q $UP_cxs  $BRCS_cxp) ||error "Failed to extract [$BRCS_cxp] from [$UP_cxs]!"
	    RCS_CXP=$BRCS_cxp 
	else
	    error "Unknown UP type (not DUS or TCU)!"
	fi
    else
	echo "${progname}: assuming .tgz UP"
	if tar tzf ${installDir}/$UP_cxs | grep $TCU03_cxp > /dev/null; then
	    echo "${progname}: Old TCU03 CXP detected"
	    (cd ${installDir}; tar xf ${installDir}/$UP_cxs $TCU03_cxp) > /dev/null
	    RCS_CXP=$TCU03_cxp
	elif tar tzf ${installDir}/$UP_cxs | grep $TCU04_cxp > /dev/null; then
	    echo "${progname}: TCU CXP detected"
	    (cd ${installDir}; tar xf ${installDir}/$UP_cxs $TCU04_cxp) > /dev/null
	    RCS_CXP=$TCU04_cxp
	elif tar tzf ${installDir}/$UP_cxs | grep $DUS2_cxp > /dev/null; then
	    echo "${progname}: DUS CXP detected"
	    (cd ${installDir}; tar xf ${installDir}/$UP_cxs $DUS2_cxp) > /dev/null
	    RCS_CXP=$DUS2_cxp 
	elif tar tzf ${installDir}/$UP_cxs | grep $DUS2HAL_cxp > /dev/null; then
	    echo "${progname}: DUS2HAL CXP detected"
	    (cd ${installDir}; tar xf ${installDir}/$UP_cxs $DUS2HAL_cxp) > /dev/null
	    RCS_CXP=$DUS2HAL_cxp 
	elif tar tzf ${installDir}/$UP_cxs | grep $BRCS_cxp > /dev/null; then
	    echo "${progname}: BRCS CXP detected"
	    (cd ${installDir}; tar xf ${installDir}/$UP_cxs $BRCS_cxp) > /dev/null
	    RCS_CXP=$BRCS_cxp 
	else
	    error "Unknown UP type (not DUS or TCU)!"
	fi
    fi
}

SECURE_BOARD() {
    SECURE_BOARD="no"
    if grep secure_board $stp_cfg | grep yes > /dev/null; then
	echo "${progname}: SECURE BOARD detected"
	SECURE_BOARD="yes"
    else 
	echo "${progname}: UNSECURE BOARD detected"
    fi
}

UPTYPE() {
    if file ${installDir}/$UP_cxs | grep "Zip archive data" > /dev/null; then
	if unzip -l ${installDir}/$UP_cxs | grep "^DUMMY" > /dev/null; then
	    echo "${progname}: DUMMY UP detected (RCS test UP)"
	    UPTYPE="DUMMY"
	else
	    echo "${progname}: FULL UP detected (Product UP)"
	    UPTYPE="FULL"
	fi
    else
	if tar tzf ${installDir}/$UP_cxs | grep "^DUMMY" > /dev/null; then
	    echo "${progname}: DUMMY UP detected (RCS test UP)"
	    UPTYPE="DUMMY"
	else
	    echo "${progname}: FULL UP detected (Product UP)"
	    UPTYPE="FULL"
	fi
    fi
}

THIRD_NET_OCTET() {
   THIRD_NET_OCTET=`expr $LMT_IP : '[0-9]*\.[0-9]*\.\([0-9]*\)\.[0-9]*'`
}

MW_OR_EE() {
    if [ $THIRD_NET_OCTET -eq 225 ]; then
	echo "${progname}: MW lab detected"
	MW_OR_EE="MW"
    elif [ $THIRD_NET_OCTET -eq 234 ]; then # new lab network
	echo "${progname}: MW lab detected"
	MW_OR_EE="MW"
    elif [ $THIRD_NET_OCTET -eq 239 ]; then # Cloud
	echo "${progname}: Cloud detected"
	MW_OR_EE="MW"
    elif [ $THIRD_NET_OCTET -eq 224 ]; then
	echo "${progname}: EE lab detected"
	MW_OR_EE="EE"
    fi
}

RELEASE() {
    XML=`tar tf ${installDir}/$RCS_CXP | grep \.xml`
#    echo "XML $XML"
    PRODNO=`expr ${installDir}/$RCS_CXP : '.*_\(CXP.*\)\.cxp*'`
#    echo "PRODNO $PRODNO"
    LINE=`tar xf ${installDir}/$RCS_CXP $XML -O | grep $PRODNO`
#    echo "LINE $LINE"
    RELEASE=`expr "$LINE" : '.*version="\(R[0-9]*\)'`
#    echo "RELEASE $RELEASE"
    echo "${progname}: RELEASE $RELEASE"   
}

OAMAP() {
    if [ $OAMAP == "no" ]; then
	echo "${progname}: OaM accesspoint will NOT be configured"
    else 
	if [ $UPTYPE == "DUMMY" ]; then
	    echo -n "${progname}: This is a DUMMY UP which will fail creating OaM accesspoint, do you want to continue (yes/no)? "
	    read answer
	    case $answer in
                yes) ;;
                *)   exit 1;;
	    esac
	fi
	if [ $SECURE_BOARD == "yes" -a $RELEASE == "R4" ]; then
	    echo -n "${progname}: RCS lab does not support OaM accesspoint on secure boards in 16A, do you want to continue (yes/no)? "
	    read answer
	    case $answer in
                yes) ;;
                *)   exit 1;;
	    esac
	fi
	if [ $OAMAP == "ipv6" -a $RELEASE == "R4" ]; then
	    echo -n "${progname}: ipv6 is not supported for OaM accesspoint in 16A, do you want to continue (yes/no)? "
	    read answer
	    case $answer in
                yes) ;;
                *)   exit 1;;
	    esac
	fi
	echo "${progname}: OaM accesspoint will be configured with $OAMAP"
    fi
}

########### Write /config_initial.netconf used for setting NTP and LDAP server addresses ###########
config_initial_netconf() {
    hostname=`hostname`
    LAST=`expr $LMT_IP : '[0-9]*\.[0-9]*\.[0-9]*\.\([0-9]*\)'`
    SNMP_MGR=`host $hostname | awk '/^[[:alnum:].-]+ has address/ { print $4 }'`
    if [ $OAMAP == "ipv6" ]; then
	LAST_MGR=`expr $SNMP_MGR : '[0-9]*\.[0-9]*\.[0-9]*\.\([0-9]*\)'`
	SNMP_MGR=`ip -6 addr show eth0 | grep ":${LAST_MGR}/64.*global"| sed -e 's/^.*inet6 \([^ ]\+\)\/64.*/\1/'`
	if [ -z "$SNMP_MGR" ]; then
	    error "OAM Accesspoint is configured with ipv6 but this VDI/TS DOES NOT HAVE A MATCHING IPV6 ADDRESS"
	fi
	NTP_SERVER="2001:1b70:6282:b280::170"
	LDAP_SERVER="2001:1b70:6282:b280::150"
	if [ $THIRD_NET_OCTET -eq 225 ]; then
	    SNMP_PORT=`expr 27000 + $LAST`
	    TN_A_OAM_IP="2001:1b70:6281:f100::$LAST/64"
	    TN_A_OAM_IP_nextHop="2001:1b70:6281:f100::2"
	    VLANID_OAM_IP="2402"
	    TN_A_OAM_IP_ALT="2001:1b70:6281:f480::$LAST/64"
	    TN_A_OAM_IP_ALT_nextHop="2001:1b70:6281:f480::2"
	    VLANID_OAM_IP_ALT="2415"
	elif [ $THIRD_NET_OCTET -eq 234 ]; then # new lab network
	    SNMP_PORT=`expr 27256 + $LAST`
	    TN_A_OAM_IP="2001:1b70:6281:f580::$LAST/64"
	    TN_A_OAM_IP_nextHop="2001:1b70:6281:f580::2"
	    VLANID_OAM_IP="2417"
	    TN_A_OAM_IP_ALT="2001:1b70:8292:600::$LAST/64"
	    TN_A_OAM_IP_ALT_nextHop="2001:1b70:8292:600::2"
	    VLANID_OAM_IP_ALT="2419"
	elif [ $THIRD_NET_OCTET -eq 294 ]; then # Cloud
	    SNMP_PORT=`expr 27512 + $LAST`
	elif [ $THIRD_NET_OCTET -eq 224 ]; then
	    SNMP_PORT=`expr 27256 + $LAST`
	fi
    else
#	SNMP_MGR=`host $hostname | awk '/^[[:alnum:].-]+ has address/ { print $4 }'`
	NTP_SERVER="10.67.31.10"
	LDAP_SERVER="10.68.101.150"
	if [ $THIRD_NET_OCTET -eq 225 ]; then
	    SNMP_PORT=`expr 27000 + $LAST`
	    TN_A_OAM_IP="10.67.226.$LAST/24"
	    TN_A_OAM_IP_nextHop="10.67.226.1"
	    VLANID_OAM_IP="2402"
	    TN_A_OAM_IP_ALT="10.67.233.$LAST/24"
	    TN_A_OAM_IP_ALT_nextHop="10.67.233.1"
	    VLANID_OAM_IP_ALT="2415"
	elif [ $THIRD_NET_OCTET -eq 234 ]; then # new lab network
	    SNMP_PORT=`expr 27256 + $LAST`
	    TN_A_OAM_IP="10.67.235.$LAST/24"
	    TN_A_OAM_IP_nextHop="10.67.235.1"
	    VLANID_OAM_IP="2417"
	    TN_A_OAM_IP_ALT="10.67.236.$LAST/24"
	    TN_A_OAM_IP_ALT_nextHop="10.67.236.1"
	    VLANID_OAM_IP_ALT="2418"
	elif [ $THIRD_NET_OCTET -eq 239 ]; then # Cloud
	    SNMP_PORT=`expr 27512 + $LAST`
	elif [ $THIRD_NET_OCTET -eq 224 ]; then
	    SNMP_PORT=`expr 27256 + $LAST`
	fi
    fi
    if [ $OAMAP != "no" ]; then
	SNMP_AGENT_IP="0.0.0.0"
	echo "${progname}: OAM Accesspoint IP $TN_A_OAM_IP"
	echo "${progname}: OAM Accesspoint ALT IP $TN_A_OAM_IP_ALT"
    else
	SNMP_AGENT_IP=$LMT_IP	    
    fi
    echo "${progname}: SNMP manager address $SNMP_MGR"
    
    echo "${progname}: Creating ${installDir}/config_initial.netconf"

#            <UserIdentity xmlns=\"urn:com:ericsson:ecim:RcsUser\">
#              <userIdentityId>1</userIdentityId>
#              <MaintenanceUser>
#                 <maintenanceUserId>1</maintenanceUserId>
#                 <userName>muexpert</userName>
#                 <password struct=\"EcimPassword\"><password>muexpert</password><cleartext></cleartext></password>
#              </MaintenanceUser>
#            </UserIdentity>

    echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities>
    <capability>urn:ietf:params:netconf:base:1.0</capability>
  </capabilities>
</hello>]]>]]>
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config>
    <target>
      <running/>
    </target>
    <config>
    <ManagedElement xmlns=\"urn:com:ericsson:ecim:ComTop\">
      <managedElementId>1</managedElementId>
      <SystemFunctions>
        <systemFunctionsId>1</systemFunctionsId>
        <Lm xmlns=\"urn:com:ericsson:ecim:RcsLM\">
          <fingerprint>RCS_MSR</fingerprint>
          <lmId>1</lmId>
        </Lm>                 
        <SecM xmlns=\"urn:com:ericsson:ecim:ComSecM\">
          <secMId>1</secMId>
          <UserManagement>
            <userManagementId>1</userManagementId>
            <LdapAuthenticationMethod xmlns=\"urn:com:ericsson:ecim:ComLdapAuthentication\">
              <ldapAuthenticationMethodId>1</ldapAuthenticationMethodId>
              <administrativeState>UNLOCKED</administrativeState>
              <Ldap>
                <ldapId>1</ldapId>
                <ldapIpAddress>$LDAP_SERVER</ldapIpAddress>
                <bindDn>cn=king,dc=mordor,dc=invalid</bindDn>
                <bindPassword struct=\"EcimPassword\"><password>1:7TcvZCTcqkKUI6RNL3IKSlMB/kas</password></bindPassword>
                <baseDn>ou=people,dc=mordor,dc=invalid</baseDn>
                <profileFilter>POSIX_GROUPS</profileFilter>
                <userLabel>kalles nya config</userLabel>
              </Ldap>
            </LdapAuthenticationMethod>
            <UserIdentity xmlns=\"urn:com:ericsson:ecim:RcsUser\">
              <userIdentityId>1</userIdentityId>
              <MaintenanceUser>
                <maintenanceUserId>1</maintenanceUserId>
                <userName>muexpert</userName>
                <password struct=\"EcimPassword\"><password>muexpert</password><cleartext></cleartext></password>
              </MaintenanceUser>
            </UserIdentity>
          </UserManagement>
        </SecM>
        <SysM xmlns=\"urn:com:ericsson:ecim:ComSysM\">
           <sysMId>1</sysMId>
           <NtpServer>
              <ntpServerId>1</ntpServerId>
              <serverAddress>$NTP_SERVER</serverAddress>
              <administrativeState>UNLOCKED</administrativeState>
           </NtpServer>
           <OamAccessPoint>
             <oamAccessPointId>1</oamAccessPointId>
             <netconfPort>2022</netconfPort>
           </OamAccessPoint>
	   <Snmp xmlns=\"urn:com:ericsson:ecim:ComSnmp\">
	      <snmpId>1</snmpId>
	      <administrativeState>UNLOCKED</administrativeState>
	      <agentAddress struct=\"HostAndPort\">
		<host>$SNMP_AGENT_IP</host>
		<port>6161</port>
	      </agentAddress>
	      <SnmpTargetV2C xmlns=\"urn:com:ericsson:ecim:ComSnmp\">
		<snmpTargetV2CId>1</snmpTargetV2CId>
		<address>$SNMP_MGR</address>
		<informRetryCount>10</informRetryCount>
		<transportMethod>INFORM</transportMethod>
		<community>public</community>
		<port>$SNMP_PORT</port>
	      </SnmpTargetV2C>
	   </Snmp>
       </SysM>
      </SystemFunctions>
    </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
<capabilities>
   <capability>urn:ietf:params:netconf:base:1.0</capability>
</capabilities>
</hello>
]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!-- System -->
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
<edit-config>
   <target>
     <running/>
   </target>
   <config>
    <ManagedElement xmlns=\"urn:com:ericsson:ecim:ComTop\">
     <managedElementId>1</managedElementId>
     <SystemFunctions>
       <systemFunctionsId>1</systemFunctionsId>
       <Lm xmlns=\"urn:com:ericsson:ecim:RcsLM\">
         <lmId>1</lmId>
         <FeatureState>
           <featureState>ACTIVATED</featureState>
           <featureStateId>CXC4040010</featureStateId>
         </FeatureState>
       </Lm>
      </SystemFunctions>
    </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>" > ${installDir}/config_initial.netconf
    echo "no" > ${installDir}/is_oam_accesspoint_configured
    if [ $OAMAP == "ipv4" ]; then
	echo "yes" > ${installDir}/is_oam_accesspoint_configured
	echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities>
    <capability>urn:ietf:params:netconf:base:1.0</capability>
  </capabilities>
</hello>]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config>
    <target>
      <running/>
    </target>
    <config>
      <ManagedElement>
        <managedElementId>1</managedElementId>
        <Equipment>
          <equipmentId>1</equipmentId>
          <FieldReplaceableUnit>
            <fieldReplaceableUnitId>1</fieldReplaceableUnitId>
            <administrativeState>UNLOCKED</administrativeState>
            <TnPort>
              <tnPortId>TN_A</tnPortId>
            </TnPort>
          </FieldReplaceableUnit>
        </Equipment>
        <Transport>
          <transportId>1</transportId>
          <EthernetPort>
            <ethernetPortId>TN_A</ethernetPortId>
            <encapsulation>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,TnPort=TN_A</encapsulation>
            <administrativeState>UNLOCKED</administrativeState>
          </EthernetPort>
          <VlanPort>
            <vlanPortId>TN_A_OAM_IP</vlanPortId>
            <vlanId>$VLANID_OAM_IP</vlanId>
            <encapsulation>ManagedElement=1,Transport=1,EthernetPort=TN_A</encapsulation>
            <isTagged>true</isTagged>
          </VlanPort>
          <Router>
            <routerId>OAM</routerId>
            <InterfaceIPv4>
              <interfaceIPv4Id>TN_A_OAM_IP</interfaceIPv4Id>
              <encapsulation>ManagedElement=1,Transport=1,VlanPort=TN_A_OAM_IP</encapsulation>
              <mtu>9000</mtu>
              <AddressIPv4>
                <addressIPv4Id>TN_A_OAM_IP</addressIPv4Id>
                <address>$TN_A_OAM_IP</address>
              </AddressIPv4>
            </InterfaceIPv4>
            <RouteTableIPv4Static>
              <routeTableIPv4StaticId>1</routeTableIPv4StaticId>
              <Dst>
                <dst>0.0.0.0/0</dst>
                <dstId>default</dstId>
                <NextHop>
                  <address>$TN_A_OAM_IP_nextHop</address>
                  <nextHopId>$TN_A_OAM_IP_nextHop</nextHopId>
                </NextHop>
              </Dst>
            </RouteTableIPv4Static>
          </Router>
          <VlanPort>
            <vlanPortId>TN_A_OAM_IP_ALT</vlanPortId>
            <vlanId>$VLANID_OAM_IP_ALT</vlanId>
            <encapsulation>ManagedElement=1,Transport=1,EthernetPort=TN_A</encapsulation>
            <isTagged>true</isTagged>
          </VlanPort>
          <Router>
            <routerId>OAM_ALT</routerId>
            <InterfaceIPv4>
              <interfaceIPv4Id>TN_A_OAM_IP_ALT</interfaceIPv4Id>
              <encapsulation>ManagedElement=1,Transport=1,VlanPort=TN_A_OAM_IP_ALT</encapsulation>
              <mtu>9000</mtu>
              <AddressIPv4>
                <addressIPv4Id>TN_A_OAM_IP_ALT</addressIPv4Id>
                <address>$TN_A_OAM_IP_ALT</address>
              </AddressIPv4>
            </InterfaceIPv4>
            <RouteTableIPv4Static>
              <routeTableIPv4StaticId>1</routeTableIPv4StaticId>
              <Dst>
                <dst>0.0.0.0/0</dst>
                <dstId>default</dstId>
                <NextHop>
                  <address>$TN_A_OAM_IP_ALT_nextHop</address>
                  <nextHopId>$TN_A_OAM_IP_ALT_nextHop</nextHopId>
                </NextHop>
              </Dst>
            </RouteTableIPv4Static>
          </Router>
        </Transport>
        <SystemFunctions>
          <systemFunctionsId>1</systemFunctionsId>
          <Lm>
            <lmId>1</lmId>
            <FeatureState>
              <featureStateId>CXC4011823</featureStateId>
              <featureState>ACTIVATED</featureState>
            </FeatureState>
          </Lm>
          <SysM>
            <sysMId>1</sysMId>
            <OamAccessPoint>
              <oamAccessPointId>1</oamAccessPointId>
              <accessPoint>ManagedElement=1,Transport=1,Router=OAM,InterfaceIPv4=TN_A_OAM_IP,AddressIPv4=TN_A_OAM_IP</accessPoint>
            </OamAccessPoint>
            <OamAccessPoint>
              <oamAccessPointId>Alternative</oamAccessPointId>
              <accessPoint>ManagedElement=1,Transport=1,Router=OAM_ALT,InterfaceIPv4=TN_A_OAM_IP_ALT,AddressIPv4=TN_A_OAM_IP_ALT</accessPoint>
            </OamAccessPoint>
          </SysM>
        </SystemFunctions>
        <NodeSupport>
          <nodeSupportId>1</nodeSupportId>
          <MpClusterHandling>
            <mpClusterHandlingId>1</mpClusterHandlingId>
              <primaryCoreRef>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1</primaryCoreRef>
          </MpClusterHandling>
        </NodeSupport>
      </ManagedElement>
    </config>
  </edit-config>
</rpc>]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>" >> ${installDir}/config_initial.netconf
    elif [ $OAMAP == "ipv6" ]; then
	echo "yes" > ${installDir}/is_oam_accesspoint_configured
	echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities>
    <capability>urn:ietf:params:netconf:base:1.0</capability>
  </capabilities>
</hello>]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config>
    <target>
      <running/>
    </target>
    <config>
      <ManagedElement>
        <managedElementId>1</managedElementId>
        <Equipment>
          <equipmentId>1</equipmentId>
          <FieldReplaceableUnit>
            <fieldReplaceableUnitId>1</fieldReplaceableUnitId>
            <administrativeState>UNLOCKED</administrativeState>
            <TnPort>
              <tnPortId>TN_A</tnPortId>
            </TnPort>
          </FieldReplaceableUnit>
        </Equipment>
        <Transport>
          <transportId>1</transportId>
          <EthernetPort>
            <ethernetPortId>TN_A</ethernetPortId>
            <encapsulation>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,TnPort=TN_A</encapsulation>
            <administrativeState>UNLOCKED</administrativeState>
          </EthernetPort>
          <VlanPort>
            <vlanPortId>TN_A_OAM_IP</vlanPortId>
            <vlanId>$VLANID_OAM_IP</vlanId>
            <encapsulation>ManagedElement=1,Transport=1,EthernetPort=TN_A</encapsulation>
            <isTagged>true</isTagged>
          </VlanPort>
          <Router>
            <routerId>OAM</routerId>
            <InterfaceIPv6>
              <interfaceIPv6Id>TN_A_OAM_IP</interfaceIPv6Id>
              <encapsulation>ManagedElement=1,Transport=1,VlanPort=TN_A_OAM_IP</encapsulation>
              <mtu>9000</mtu>
              <AddressIPv6>
                <addressIPv6Id>TN_A_OAM_IP</addressIPv6Id>
                <address>$TN_A_OAM_IP</address>
              </AddressIPv6>
            </InterfaceIPv6>
            <RouteTableIPv6Static>
              <routeTableIPv6StaticId>1</routeTableIPv6StaticId>
              <Dst>
                <dst>::/0</dst>
                <dstId>default</dstId>
                <NextHop>
                  <address>$TN_A_OAM_IP_nextHop</address>
                  <nextHopId>$TN_A_OAM_IP_nextHop</nextHopId>
                </NextHop>
              </Dst>
            </RouteTableIPv6Static>
          </Router>
          <VlanPort>
            <vlanPortId>TN_A_OAM_IP_ALT</vlanPortId>
            <vlanId>$VLANID_OAM_IP_ALT</vlanId>
            <encapsulation>ManagedElement=1,Transport=1,EthernetPort=TN_A</encapsulation>
            <isTagged>true</isTagged>
          </VlanPort>
          <Router>
            <routerId>OAM_ALT</routerId>
            <InterfaceIPv6>
              <interfaceIPv6Id>TN_A_OAM_IP_ALT</interfaceIPv6Id>
              <encapsulation>ManagedElement=1,Transport=1,VlanPort=TN_A_OAM_IP_ALT</encapsulation>
              <mtu>9000</mtu>
              <AddressIPv6>
                <addressIPv6Id>TN_A_OAM_IP_ALT</addressIPv6Id>
                <address>$TN_A_OAM_IP_ALT</address>
              </AddressIPv6>
            </InterfaceIPv6>
            <RouteTableIPv6Static>
              <routeTableIPv6StaticId>1</routeTableIPv6StaticId>
              <Dst>
                <dst>::/0</dst>
                <dstId>default</dstId>
                <NextHop>
                  <address>$TN_A_OAM_IP_ALT_nextHop</address>
                  <nextHopId>$TN_A_OAM_IP_ALT_nextHop</nextHopId>
                </NextHop>
              </Dst>
            </RouteTableIPv6Static>
          </Router>
        </Transport>
        <SystemFunctions>
          <systemFunctionsId>1</systemFunctionsId>
          <Lm>
            <lmId>1</lmId>
            <FeatureState>
              <featureStateId>CXC4011823</featureStateId>
              <featureState>ACTIVATED</featureState>
            </FeatureState>
            <FeatureState>
              <featureStateId>CXC4040006</featureStateId>
              <featureState>ACTIVATED</featureState>
            </FeatureState>
          </Lm>
          <SysM>
            <sysMId>1</sysMId>
            <OamAccessPoint>
              <oamAccessPointId>1</oamAccessPointId>
              <accessPoint>ManagedElement=1,Transport=1,Router=OAM,InterfaceIPv6=TN_A_OAM_IP,AddressIPv6=TN_A_OAM_IP</accessPoint>
            </OamAccessPoint>
            <OamAccessPoint>
              <oamAccessPointId>Alternative</oamAccessPointId>
              <accessPoint>ManagedElement=1,Transport=1,Router=OAM_ALT,InterfaceIPv6=TN_A_OAM_IP_ALT,AddressIPv6=TN_A_OAM_IP_ALT</accessPoint>
            </OamAccessPoint>
          </SysM>
        </SystemFunctions>
        <NodeSupport>
          <nodeSupportId>1</nodeSupportId>
          <MpClusterHandling>
            <mpClusterHandlingId>1</mpClusterHandlingId>
              <primaryCoreRef>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1</primaryCoreRef>
          </MpClusterHandling>
        </NodeSupport>
      </ManagedElement>
    </config>
  </edit-config>
</rpc>]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>" >> ${installDir}/config_initial.netconf
    fi
}

########### Write config_lab.sh used for setting LMT eth0 IP address after reboot ###########
config_lab_sh() {
    echo "${progname}: LMT IP address $LMT_IP"
    echo "${progname}: Creating ${installDir}/config_lab.sh"
    echo "#!/bin/bash
ifconfig eth0 $LMT_IP netmask 255.255.255.0
route add -net 0.0.0.0 netmask 128.0.0.0 gw 10.67.$THIRD_NET_OCTET.1
route add -net 128.0.0.0 netmask 128.0.0.0 gw 10.67.$THIRD_NET_OCTET.1
exit 0" > ${installDir}/config_lab.sh
    if [ $UPTYPE == "DUMMY" ]; then
	echo "#no_lmt_namespace" >> ${installDir}/config_lab.sh
    fi
}

########### Write RbsSummaryFile.xml used by ARM autointegration semi ###########
RbsSummaryFile_xml() {
    echo "${progname}: Creating ${installDir}/RbsSummaryFile.xml"
    echo "<summary:AutoIntegrationRbsSummaryFile
   xmlns:summary=\"http://www.ericsson.se/RbsSummaryFileSchema\"
   xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
   xsi:schemaLocation=\"http://www.ericsson.se/RbsSummaryFileSchemaSummaryFile.xsd\">
<Format revision=\"F\"/>
<ConfigurationFiles
   siteBasicFilePath=\"boards/$Board/config_initial.netconf\"
   siteEquipmentFilePath=\"boards/$Board/config_eqm_dummy.netconf\"
   licensingKeyFilePath=\"boards/$Board/LKF.xml\"
   upgradePackageFilePath=\"boards/$Board/$UP_cxs\"
   labConfigFilePath=\"boards/$Board/config_lab.sh\"
   initialSecurityConfigurationFilePath=\"%ForIPsec_path, not supported yet%\"/>
</summary:AutoIntegrationRbsSummaryFile>" > ${installDir}/RbsSummaryFile.xml
}
RbsSummaryFile_xml_nl() {
    echo "${progname}: Creating ${installDir}/RbsSummaryFile.xml.nl"
    echo "<summary:AutoIntegrationRbsSummaryFile
   xmlns:summary=\"http://www.ericsson.se/RbsSummaryFileSchema\"
   xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
   xsi:schemaLocation=\"http://www.ericsson.se/RbsSummaryFileSchemaSummaryFile.xsd\">
<Format revision=\"F\"/>
<ConfigurationFiles
    initialSwFilePath=\"boards/$Board/$UP_cxs\"
/>
</summary:AutoIntegrationRbsSummaryFile>" > ${installDir}/RbsSummaryFile.xml.nl
}
########### Write config_eqm_dummy.netconf used by ARM autointegration semi ###########
config_eqm_dummy_netconf() {
    echo "${progname}: Creating ${installDir}/config_eqm_dummy.netconf"
    if [ "$Cluster" = "yes" ] && [ "$Core" = "yes" ]; then
	echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
 <capabilities>
   <capability>urn:ietf:params:netconf:base:1.0</capability>
 </capabilities>
</hello>]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"1\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
 <edit-config>
   <config xmlns:xc=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
     <ManagedElement xmlns=\"urn:com:ericsson:ecim:ComTop\">
        <managedElementId>1</managedElementId>
        <Equipment>
           <equipmentId>1</equipmentId>
             <FieldReplaceableUnit>
               <fieldReplaceableUnitId>1</fieldReplaceableUnitId>
               <administrativeState>UNLOCKED</administrativeState>
               <userLabel>DU1</userLabel>
               <TnPort>
                 <tnPortId>TN_A</tnPortId>
               </TnPort>
               <DiPort>
                 <diPortId>IDL_A</diPortId>
               </DiPort>
               <SyncPort>
                 <syncPortId>1</syncPortId>
               </SyncPort>
             </FieldReplaceableUnit>
             <FieldReplaceableUnit>
               <fieldReplaceableUnitId>2</fieldReplaceableUnitId>
               <administrativeState>UNLOCKED</administrativeState>
               <userLabel>DU2</userLabel>
               <DiPort>
                 <diPortId>IDL_A</diPortId>
               </DiPort>
             </FieldReplaceableUnit>
         </Equipment>
     </ManagedElement>
   </config>
 </edit-config>
</rpc>
]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"2\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
    <target>
      <running />
    </target>
    <config xmlns:xc=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
      <ManagedElement xmlns=\"urn:com:ericsson:ecim:ComTop\">
        <managedElementId>1</managedElementId>
        <NodeSupport>
          <nodeSupportId>1</nodeSupportId>
          <MpClusterHandling>
            <mpClusterHandlingId>1</mpClusterHandlingId>
              <primaryCoreRef>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1</primaryCoreRef>
          </MpClusterHandling>

        </NodeSupport>

        <Equipment>
          <equipmentId>1</equipmentId>

<!--
    FieldReplaceableUnit=1 refs
-->
          <FieldReplaceableUnit>
            <fieldReplaceableUnitId>1</fieldReplaceableUnitId>
                <DiPort>
                  <diPortId>IDL_A</diPortId>
                </DiPort>
          </FieldReplaceableUnit>

<!--
    FieldReplaceableUnit=2 refs
-->
          <FieldReplaceableUnit>
            <fieldReplaceableUnitId>2</fieldReplaceableUnitId>
                <DiPort>
                <diPortId>IDL_A</diPortId>
                </DiPort>
          </FieldReplaceableUnit>

<!--
    DiLink=1 refs
-->
    <DiLink>
       <diLinkId>1</diLinkId>
         <diPortRef1>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,DiPort=IDL_A</diPortRef1>
         <diPortRef2>ManagedElement=1,Equipment=1,FieldReplaceableUnit=2,DiPort=IDL_A</diPortRef2>
     </DiLink>
</Equipment>

<!--
    Transport=1 refs
-->
        <Transport>
          <transportId>1</transportId>
          <Synchronization>
            <synchronizationId>1</synchronizationId>
            <FrequencySyncIO xc:operation=\"create\">
              <frequencySyncIOId>1</frequencySyncIOId>
              <encapsulation>ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,SyncPort=1</encapsulation>
            </FrequencySyncIO>
          </Synchronization>
        </Transport>

      </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>


<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"3\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
    <target>
      <running/>
    </target>
    <config xmlns:xc=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
    <ManagedElement>
      <managedElementId>1</managedElementId>
      <Transport>
        <transportId>1</transportId>
        <Synchronization>
          <synchronizationId>1</synchronizationId>
          <RadioEquipmentClock xc:operation=\"create\">
            <radioEquipmentClockId>1</radioEquipmentClockId>
            <minQualityLevel>
              <qualityLevelValueOptionI>PRC</qualityLevelValueOptionI>
              <qualityLevelValueOptionIII>UNK</qualityLevelValueOptionIII>
              <qualityLevelValueOptionII>STU</qualityLevelValueOptionII>
            </minQualityLevel>
          </RadioEquipmentClock>
        </Synchronization>
      </Transport>
    </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"4\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
    <target>
      <running/>
    </target>
    <config xmlns:xc=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
    <ManagedElement>
      <managedElementId>1</managedElementId>
      <Transport>
        <transportId>1</transportId>
        <Synchronization>
          <synchronizationId>1</synchronizationId>
          <RadioEquipmentClock>
            <radioEquipmentClockId>1</radioEquipmentClockId>
            <RadioEquipmentClockReference xc:operation=\"create\">
              <radioEquipmentClockReferenceId>1</radioEquipmentClockReferenceId>
              <encapsulation>ManagedElement=1,Transport=1,Synchronization=1,FrequencySyncIO=1</encapsulation>
              <adminQualityLevel>
                <qualityLevelValueOptionI>PRC</qualityLevelValueOptionI>
                <qualityLevelValueOptionIII>UNK</qualityLevelValueOptionIII>
                <qualityLevelValueOptionII>STU</qualityLevelValueOptionII>
              </adminQualityLevel>
              <priority>1</priority>
                    <administrativeState>UNLOCKED</administrativeState>
            </RadioEquipmentClockReference>
          </RadioEquipmentClock>
        </Synchronization>
      </Transport>
    </ManagedElement>
    </config>
  </edit-config>
</rpc>
]]>]]>

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rpc message-id=\"5\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <close-session/>
</rpc>]]>]]>" > ${installDir}/config_eqm_dummy.netconf
    else
	echo "Dummy file for test autointegration download from summaryfile" > ${installDir}/config_eqm_dummy.netconf
    fi
}

########### Write LKF.xml used by ARM autointegration semi ###########
LKF_xml() {
    if [ $RELEASE == "R3" ]; then
	licensingKeyFile="/proj/rcs/LKF_hands_off/licensingKeyFile_16A.xml"
    elif [ $RELEASE == "R4" ]; then
	licensingKeyFile="/proj/rcs/LKF_hands_off/licensingKeyFile_16A.xml"
    elif [ $RELEASE == "R5" ]; then
	licensingKeyFile="/proj/rcs/LKF_hands_off/licensingKeyFile_16B.xml"
    else
	licensingKeyFile="/proj/rcs/LKF_hands_off/licensingKeyFile_16B.xml"
    fi
    if [ -e ${licensingKeyFile} ]; then
	echo "${progname}: Copy $licensingKeyFile to ${installDir}/LKF.xml"
	( cp ${licensingKeyFile} ${installDir}/LKF.xml) ||error "Failed to copy [$licensingKeyFile.xml] to [${installDir}/LKF.xml]"
    else  # Fix for ipv6 HUB VDI in lab
	CClicensingKeyFile=`ls -tr $RCS_TOP/LMA/LMA_CNX9013077/test/suites/RCS* | tail -1`
	echo "${progname}: Copy ${CClicensingKeyFile} to ${installDir}/LKF.xml"
	( cp ${CClicensingKeyFile} ${installDir}/LKF.xml) ||error "Failed to copy [$CClicensingKeyFile.xml] to [${installDir}/LKF.xml]"
    fi
    chmod 664 ${installDir}/LKF.xml 2> /dev/null
}

########### Write netloader_mode.txt used by ARM autointegration semi ###########
netloader_mode_txt() {
    echo "${progname}: Creating ${installDir}/netloader_mode.txt"
    echo "# Ensure no new lines after value. (nl uses tail -n 1 to get value)
# Possible values: semi | lab
semi" > ${installDir}/netloader_mode.txt
}

########### Main ###########

# Used for debug or from other script/erlang
[ "_$1" = _call ] && {
        shift
eval $*
exit $?
}
 
if [ $# -lt 2 ]; then
    usage
else
    Node=$1
    LabelUrlFile="$2"
    shift;shift
fi

if echo $Node | grep "^rcf" > /dev/null; then
    IMAGE=${LabelUrlFile}
    if echo $LabelUrlFile | grep ^CXS101657_2-R > /dev/null; then  # VRCS
	IMAGE=${vrcsimage}@@/${LabelUrlFile}	
    fi
    echo "$RDE_TOP/tools/rcstgt/bin/vrcstprep.sh -p rcs-ci -l $Node -f VRCS -c $IMAGE -n KI10_rcs_ci_oam -n KI10_rcs_ci_traffic -w rcs_lab"
    $RDE_TOP/tools/rcstgt/bin/vrcstprep.sh -p rcs-ci -l $Node -f VRCS -c $IMAGE -n KI10_rcs_ci_oam -n KI10_rcs_ci_traffic -w rcs_lab
#    echo "$RDE_TOP/tools/rcstgt/bin/vrcstprep.sh -p rcs-ci -l $Node -f VRCS -c $LabelUrlFile -n KI10_rcs_ci_oam -n KI10_rcs_ci_traffic -w rcs_lab"
#    $RDE_TOP/tools/rcstgt/bin/vrcstprep.sh -p rcs-ci -l $Node -f VRCS -c $LabelUrlFile -n KI10_rcs_ci_oam -n KI10_rcs_ci_traffic -w rcs_lab
elif [ "$Node" = "only_load_image" ] ; then
#    $RDE_TOP/tools/rcstgt/bin/vrcstprep.sh $Node $LabelUrlFile rcs_lab
    echo "$RDE_TOP/tools/rcstgt/bin/vrcstprep.sh -p rcs-ci -c $LabelUrlFile"
    $RDE_TOP/tools/rcstgt/bin/vrcstprep.sh -p rcs-ci -c $LabelUrlFile
else
    STRIP_SIGNATURE=""
    OAMAP="no"
    IPV="default"
    while [ $# -gt 0 ]; do
	case "$1" in
            -no_strip_signature) STRIP_SIGNATURE="-no_strip_signature"; shift;;
            -oamap_ipv4) OAMAP="ipv4"; shift;;
            -oamap_ipv6) OAMAP="ipv6"; shift;;
            *)   usage;;
	esac
    done
    
    Booker=`cat $BOOKING_DIR/$Node`
    echo "${progname}: Verifying that $Node is booked by $USER"
    case $Booker in
	$USER) ;;
	*)
	    if [ -e $NODE_BOOKING_LIST ]; then # Fix for ipv6 HUB VDI in lab
		if [ "$(cat $NODE_BOOKING_LIST | grep $USER | grep $Node)" = "" ]; then
		    echo -n "${progname}: $1 booked by $Booker, do you want to continue (yes/no)? "
		    read answer
		    case $answer in
			yes) ;;
			*)   exit 1;;
		    esac
		fi
	    fi
    esac
    
    if echo $LabelUrlFile | grep ^CXS101549_6-R4 > /dev/null; then  # New EE TCU04 R4 label
	UP=${webServer}${tcu04package}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101549_6-R5 > /dev/null; then  # New EE TCU04 R5 label
	UP=${webServer}${tcu04package}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101549_6-R > /dev/null; then  # New EE TCU04 label
	UP=${webServer}${tcu04packageHALI}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101549_5-R4 > /dev/null; then  # New EE DUS52 R4 label
	UP=${webServer}${dus52package}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101549_5-R5 > /dev/null; then  # New EE DUS52 R5 label
	UP=${webServer}${dus52package}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101549_5-R > /dev/null; then  # New EE DUS52 HALI label
	UP=${webServer}${dus52packageHALI}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101549_8-R > /dev/null; then  # New EE DUS52 HALI split RCS label
	UP=${webServer}${dus52packageHALISplit}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep ^CXS101665_3-R > /dev/null; then  # BRCS R4 label
	UP=${webServer}${brcspackage}@@/${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep CXS101549_6-R > /dev/null; then   # New EE TCU04 CC
	UP=${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep CXS101549_5-R > /dev/null; then   # New EE DUS52 CC
	UP=${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    elif echo $LabelUrlFile | grep CXS101665_3-R > /dev/null; then   # BRCS CC
	UP=${LabelUrlFile}
	UP_cxs=`expr $UP : '.*/\(.*\)@@'`
    else
	UP=$LabelUrlFile
	UP_cxs=`basename $UP`  # Not clearcase
    fi
    
    if [ ! -f $stpsDir/$Node/config/stp.cfg ]; then
	error "$stpsDir/$Node/config/stp.cfg does not exist, wrong stp?"
    fi
    ClusterBoards=`sed -n "s/{'\(dus[0-9]\+\)',/\1/p" $stpsDir/$Node/config/stp.cfg`
    if [ `echo $ClusterBoards | wc -w` -lt 2 ]; then 
	Cluster="no"
	ClusterBoards=$Node
	echo "${progname}: Single board"
    else 
	Cluster="yes"
	echo -n "${progname}: Clustered node detected: "; echo $ClusterBoards
    fi
    
    Core="yes"
    for Board in $ClusterBoards
    do
	installDir=${tftpDir}$Board
	stp_cfg=${stpsDir}$Board/config/stp.cfg
	if [ ! -f $stp_cfg ]; then
	    error "$stp_cfg does not exist, wrong board?"
	fi
	echo "${progname}: Create ${installDir} if not existing"
	if [ ! -d "${installDir}" ]; then
	    mkdir -p "${installDir}" || error "Creation of Install Dir [${installDir}]Failed!"
	fi
	chmod 775 ${installDir} 2> /dev/null
	
	echo "${progname}: tftpPrep.sh $installDir $UP $STRIP_SIGNATURE"
	( tftpPrep.sh $installDir $UP $STRIP_SIGNATURE ) || error "Failed to run tftpPrep.sh"
#( /home/etxkols/tmp/rcstprep/tftpPrep.sh $installDir $UP $STRIP_SIGNATURE ) || error "Failed to run tftpPrep.sh"
	LMT_IP             # Board IP address
	RCS_CXP            # RCS-T_CXP9031274_4.cxp or RCS-DUS2_CXP9031275_3.cxp
	SECURE_BOARD       # "yes" or "no"
	UPTYPE             # "FULL" or "DUMMY"
	THIRD_NET_OCTET    # Third octet in boards network
	MW_OR_EE           # "MW" or "EE" lab
	RELEASE            # "R4" or "R5" or "R6"
	OAMAP              # "ipv4", "ipv6" or "no" 
	config_lab_sh
	config_initial_netconf
	RbsSummaryFile_xml
	RbsSummaryFile_xml_nl
	config_eqm_dummy_netconf
	LKF_xml
	netloader_mode_txt
	Core="no"
    done
    
    echo "${progname}: Downloading of UP and preparations for installation OK"
    
    exit 0
fi

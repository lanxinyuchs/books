#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	rcs_install.sh %
# %CCaseRev:	/main/R1A/R2A/R4A/R6A/1 %
# %CCaseDate:	2016-09-05 %
# Author:       etxkols
#
# Short description: Get files needed for installation on target and install
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
# main/1   2012-06-12 etxkols     Created
# R2A/1    2013-02-14 etxkols     Updated usage
# R2A/2    2013-02-22 etxkols     Updated usage
# R2A/3    2013-09-09 etxkols     If no UP is given, latest stable label is used
# R2A/4    2014-01-10 etxjovp     add --no-check-certificate
# R2A/5    2014-02-27 etxkols     Adaptions to autointegration semi for ARM
# R2A/6    2014-05-26 etxkols     Restructured EE
# R2A/7    2014-10-30 etxkols     Clean up
# R4A/3    2014-12-09 etxkols     Clean up
# R4A/4    2014-12-10 etxkols     Clean up
# R4A/5    2014-12-11 etxkols     Grrrrrr
# R4A/6    2016-01-22 etxkols     ipv6
# R4A/7    2016-02-09 etxkols     options -oamap_ipv4 and -oamap_ipv6
# R4A/8    2016-05-18 etxkols     VRCS
# R4A/9    2016-05-18 etxkols     No commontest install if VRCS
# R6A/1    2016-09-05 etxkols     GIT:ified
#----------------------------------------------------------------------
progname="`basename $0`"
stpsDir="/proj/rcs-tmp/stps/"
usage() {
   echo "Usage: ${progname} <Hw | Stp> [ [UP] | -oamap_ipv4 | -oamap_ipv6 ]
Usage: ${progname} <Hw | Stp>

<Hw>:        Board
<Stp>:       Stp
<UP>:        Optional. URL, Label or Filepath where to fetch \"UP\" from 
             If not given, latest stable RCS CXS is used
             If Label is given, UP.cxs@@/Label is downloaded.
             If URL is given, UP is downloaded from URL, ex node CI.
	     If FilePath is given, UP is copied from FilePath
-oamap_ipv4: Optional, board will be installed with ipv4 OamAccessPoint configured (not supported in RCS lab for secure boards in 16A)
-oamap_ipv6: Optional, board will be installed with ipv6 OamAccessPoint configured (not supported in 16A)

Description: Downloads UP to /proj/rcs-tmp/tftpboot/Hw and extracts/creates files necessary for installation and installs the Hw.

example: ${progname} tcu020                            # will install latest CXS
         ${progname} rcf100                            # will install latest image in cloud
         ${progname} tcu020 CXS101549_3-R2A2251        # will install CXS101549_3-R2A2251
         ${progname} dus004 https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs@@/CXS101549_1-R2A835
         ${progname} dus004 https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs
         ${progname} dus004 /vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs@@/CXS101549_1-R2A835
         ${progname} dus004 /vobs/rcs/delivery/RCP_CSX10179_1/RCP-T3_CXS101549_1/doc/19010/RCP-T3_CXS101549_1.cxs
         ${progname} tcu014 /vobs/rcs/delivery/RCP_CSX10179_1/RCP-TCU03_CXS101549_4/doc/19010/RCP-TCU03_CXS101549_4.cxs
         ${progname} dus004 https://rbs-g2-ci.rnd.ki.sw.ericsson.se/view/CI/view/CL1%20Delivery%20Check/view/FW%20-%20CXP9021691/job/DC-CXP9021691/lastSuccessfulBuild/artifact/DC-CXP9021691.tgz"
   exit 1
}

URL_OR_LABEL() {
    VRCS="no"
    if expr $Board : 'tcu[0-9]*' > /dev/null ; then
	BOARD_TYPE="TCU"
	Grep="TLabel:"
    elif expr $Board : 'dus3[0-9]*' > /dev/null ; then
	BOARD_TYPE="DUS"
	Grep="DUS2Label:"
    elif expr $Board : 'dus5[0-9]*' > /dev/null ; then
	BOARD_TYPE="DUS"
	Grep="DUS2Label:"
    elif expr $Board : 'rcf[0-9]*' > /dev/null ; then
	BOARD_TYPE="VRCS"
	VRCS="yes"
	Grep="VupLabel:"
    fi
    if [ $VRCS = "no" ]; then
	URL_OR_LABEL=`wget --no-check-certificate -q https://rbs-rde-ci.rnd.ki.sw.ericsson.se/view/stable_label/job/stable_prep4TestInfo.txt/lastSuccessfulBuild/artifact/prep4TestInfo.txt -O - | grep $Grep | awk -F ':' '{print $2}'`
    elif [ $VRCS = "yes" ]; then
	URL_OR_LABEL=`curl -k -s -u rcsci1:f27375777490410c2f5f8dd3c0e99458 https://fem007-eiffel002.rnd.ki.sw.ericsson.se:8443/jenkins/view/5G/job/create_lsv_CXS101654/lastSuccessfulBuild/artifact/prep4TestInfo.txt | grep $Grep | awk -F ':' '{print $2}'`
    fi
    echo "${progname}: No Label or URL given, using latest stable RCS label for $BOARD_TYPE $URL_OR_LABEL"
}

ClusterBoards=`sed -n "s/{'\(dus[0-9]\+\)',/\1/p" $stpsDir/$1/config/stp.cfg`
if [ `echo $ClusterBoards | wc -w` -lt 2 ]; then 
    Cluster="no"
    ClusterBoards=$1
    echo "${progname}: Single board detected"
else 
    Cluster="yes"
    echo -n "${progname}: Clustered node detected: "; echo $ClusterBoards
fi

if [ $# -eq 0 ]; then
    usage 
else
    case "$1" in
	[a-z][a-z]*[0-9][0-9]*)  
	    Stp=$1
	    if [ "$Cluster" = "yes" ]; then
		Board=`echo "$ClusterBoards" | head -1`
	    else
		Board=$Stp
	    fi
	    URL_OR_LABEL
	    shift;;
	 *) usage;;
    esac
fi

OAMAP=""
while [ $# -gt 0 ]; do
    case "$1" in
	-oamap_ipv4) OAMAP="-oamap_ipv4"; shift;;
	-oamap_ipv6) OAMAP="-oamap_ipv6"; shift;;
        /*)          URL_OR_LABEL=$1; shift;;
        https*)      URL_OR_LABEL=$1; shift;;
        CXS*)        URL_OR_LABEL=$1; shift;;
	*)           if [ -e $1 ]; then
	                 URL_OR_LABEL=$1
		         shift
		     else
		         usage
		     fi;;
    esac
done

if [ "$Cluster" = "no" ] && [ "$VRCS" = "no" ]; then
    if ! $RCT_TOP/test/bin/rct_add_stp.sh -stp $Board -nodes $Board; then exit 1; fi
fi

echo "${progname}: $RDE_TOP/tools/rcstgt/bin/rcstprep.sh $Stp $URL_OR_LABEL $OAMAP"

if rcstprep.sh $Stp $URL_OR_LABEL $OAMAP; then
#if $RDE_TOP/tools/rcstgt/bin/rcstprep.sh $Stp $URL_OR_LABEL $OAMAP; then
#if /home/etxkols/tmp/oam_access/rcstprep.sh $Stp $URL_OR_LABEL $OAMAP; then
    if [ "$VRCS" = "no" ]; then
	$RCT_TOP/test/bin/rct_run.sh -stp $Stp -suite $RCT_TOP/test/suites/install/install_dus_SUITE -case install
    fi
else
    exit 1
fi


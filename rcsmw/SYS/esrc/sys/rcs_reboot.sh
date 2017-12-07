#!/bin/sh
# ----------------------------------------------------------------------
# %CCaseFile:	rcs_reboot.sh %
# %CCaseRev:	/main/R2A/R3A/1 %
# %CCaseDate:	2015-02-05 %
# Author:	etxarnu/etxbjca
#
# Short description: Script used for rebooting the node in a controlled manner.
#                     
#
# ----------------------------------------------------------------------
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
# R2A/1  2014-04-02 etxarnu     First version in SYS
# R2A/2  2014-06-09 etxarnu     Corrected for arm-wr6
# R2A/3  2014-06-11 etxarnu     Take first erl_call if multiple found
# R3A/1  2015-02-05 etxarnu     Use sysColi board_restart cold 
#
#----------------------------------------------------------------------
#

ERL_CALL=`readlink -f /software/RCS*/OTP*_CXC1733859_*/otp-*/priv/tgt_*/lib/erl_interface-*/bin/erl_call | head -1`
$ERL_CALL -c rcs -sname sirpa -a 'sysColi board_restart [cold]' 


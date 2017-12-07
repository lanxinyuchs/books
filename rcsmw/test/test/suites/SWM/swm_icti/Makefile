# **********************************************************************
# %CCaseFile:	Makefile %
# %CCaseRev:	/main/R2A/11 %
# %CCaseDate:	2013-10-10 %
# Author:	erarafo
#
# Purpose: Building CXSes for test of upgrade.
#
# Invoked by: test/suites/SWM/bin/swm_icti.sh
#
# The following make-variables must be set by the invoker:
#
#   POSTFIX          s1v1 | s1v2 | s2
#
# Dependencies: /vobs/rcs/tools/RDE_LXA119945/bin/create_sig.sh
#
# **********************************************************************
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2013 All rights reserved.
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
# **********************************************************************
#
# Rev      Date       Name        What
# -----    -------    --------    --------------------------
# R2A/2    2013-08-08 erarafo     Description and copyright text added
# R2A/3    2013-08-14 erarafo     CXP and CXS created under /local/scratch
# R2A/5    2013-08-22 erarafo     CXC with multiple UE executables
# R2A/6    2013-08-26 erarafo     Commented lines deleted
# R2A/7    2013-08-26 erarafo     Common functions separately compiled
# R2A/8    2013-08-26 erarafo     Common functions in shared library
# R2A/9    2013-08-29 erarafo     Support for the DEMO MOM
# R2A/10   2013-10-10 erarafo     Cleanup
# **********************************************************************

help:
	@echo The source is your friend.


GCC_OPTS = -W -std=gnu99 -D_REENTRANT -g -O2 -m32 --sysroot=/ -lrt -lpthread

SAF_OPTS = \
	-I/vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/SAF_CXA11461/inc \
	-L/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/SAF/SAF_CXA11461/tgt_i686/lib32 \
	-lSafcImmCt \
	-lSafcCommon \
	-lSaImmOm \
	-lprotobuf-c


out/tgt_i686/bin/upgrade_demo: csrc/upgrade_demo.c
	@mkdir -p out/tgt_i686/bin
	@eval `/app/modules/0/bin/modulecmd bash add wrtools-i686` \
	&& i686-wrs-linux-gnu-gcc \
	$(GCC_OPTS) \
	$(SAF_OPTS) \
	-o $@ \
	$<
	@echo made: $@


out/tgt_i686/bin/ugt_$(POSTFIX): csrc/ugt_$(POSTFIX).c csrc/ugt_common.h out/tgt_i686/bin/libugt_common.so
	@mkdir -p out/tgt_i686/bin
	@eval `/app/modules/0/bin/modulecmd bash add wrtools-i686` \
	&& i686-wrs-linux-gnu-gcc \
	$(GCC_OPTS) \
	$(SAF_OPTS) \
	-Lout/tgt_i686/bin \
	-lugt_common \
	-o $@ \
	$<
	@echo made: $@


out/tgt_i686/bin/libugt_common.so: csrc/ugt_common.c csrc/ugt_common.h
	@mkdir -p out/tgt_i686/bin
	@eval `/app/modules/0/bin/modulecmd bash add wrtools-i686` \
	&& i686-wrs-linux-gnu-gcc \
	$(GCC_OPTS) \
	$(SAF_OPTS) \
	-shared \
	-c \
	-o $@ \
        $<
	@echo made: $@

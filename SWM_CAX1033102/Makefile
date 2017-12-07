# **********************************************************************
# %CCaseFile:	Makefile %
# %CCaseRev:	/main/R1A/R2A/R4A/R5A/2 %
# %CCaseDate:	2016-03-16 %
# %CCaseDocNo:  %
# Author:	etxbjca
#
# Short description: Makefile for SWM
# 
# **********************************************************************
# %CCaseTemplateFile:   Makefile.mk %
# %CCaseTemplateId: 51/002 01-LXA 119 334 Ux, Rev: /main/6 %
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
# **********************************************************************
#
# Rev        Date         Name        What
# -----      -------      --------    --------------------------
# R1A/2      2012-01-09   etxbjca     Created
# **********************************************************************
#

# Early include
include $(RDE_TOP)/build/common.mk

EXT_CXA_DIR := 	$(REL_DIR)/SWM_CXA11423
ESCRIPT :=	$(wildcard $(ERL_SRC)/*.escript)

# Publish/release target macros
CXC_TGT =	$(ESRC:$(ERL_SRC)/%.erl=$(APP_DIR)/ebin/%.beam) \
		$(MSRC:$(MDL_SRC)/%.xml=$(APP_DIR)/priv/model/%.xml) \
		$(ESCRIPT:$(ERL_SRC)/%.escript=$(APP_DIR)/priv/bin/%.escript) \
		$(APP_DIR)/priv/appdata/alarm.xml \
		$(APP_DIR)/priv/appdata/swm_coli_authorizations.xml \
		$(APP_DIR)/priv/appdata/swm_coli_registrations.xml \
		$(APP_DIR)/priv/bin/swm_sudo.sh

CXA_TGT =	$(MSRC:$(MDL_SRC)/%.xml=$(CXA_DIR)/inc/%.hrl) \
		$(EXT_CXA_DIR)/schema/CxpInfo.xsd \
		$(EXT_CXA_DIR)/schema/HwBaseline.xsd \
		$(EXT_CXA_DIR)/schema/HwCompatiblity.xsd \
		$(EXT_CXA_DIR)/schema/SwConfig.xsd \
		$(EXT_CXA_DIR)/model/RcsBrM.xml \
		$(EXT_CXA_DIR)/model/RcsSwIM.xml \
		$(EXT_CXA_DIR)/model/RcsSwM.xml \
		$(EXT_CXA_DIR)/doc/15519/UpIwd.doc

#release: all
#	mkdir -p ../out/base/ebin
#	mkdir -p ../out/base/priv/model
#	cp $(ERL_OUT)/*.beam ../out/base/ebin
#	cp $(MODEL_DIR)/*.xml ../out/base/priv/model
#	@echo "--> release of SWM; done"

#ssit: release
#	mv -f $(ERL_OUT)/*.beam /var/tmp/${USER}/RCS_ROOT/sw/active/rmw/base/ebin

# Late include
include $(RDE_TOP)/build/common2.mk

# **********************************************************************
# Author:	etxbjca
#
# Short description:
# Makefile for build of protobuf.
# **********************************************************************

# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2017 All rights reserved.
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
# Rev        Date       Name        What
# -----      -------    --------    --------------------------
# A          2017-03-21 etxbjca     Created
# **********************************************************************
#

include $(TOPDIR)/Makefile

#******************************************************************************
#   Define macros.
#
TOOL_ID :=	610
TOOL_TOP =	$(CURDIR)
TOOL_SRC_NO :=	$(shell ci_data.sh -f $(BLS) -a read -i $(TOOL_ID) -p 16 | awk -F: '{print $$ 1}')
TOOL_SRC_REV :=	$(shell ci_data.sh -f $(BLS) -a read -i $(TOOL_ID) -p 16 | awk -F: '{print $$ 2}')
TOOL_SRC_URL :=	$(shell ci_data.sh -f $(BLS) -a read -i $(TOOL_ID) -p 15)
TOOL_VER :=	$(shell ci_data.sh -f $(BLS) -a read -i $(TOOL_ID) -p 14)
TOOL_NAME :=	$(shell ci_data.sh -f $(BLS) -a read -i $(TOOL_ID) -p 7)
TOOL_CACHE :=	$(RCSMWCACHE)/tool/$(TOOL_NAME)-$(TOOL_VER)
TOOL_PREFIX =	$(TOOL_CACHE)/$(@:tool_%=%)/$(TGT)
TOOL_DIR :=	$(TOOL_NAME)-$(TOOL_VER)
TOOL_SRC :=	$(TOOL_DIR).zip

COMMON_RMS :=	OTP \
		protobuf-c

COMMON_ADDS :=	autoconf/2.69 \
		automake/1.9.6 \
		libtool/2.4.2

#******************************************************************************
#   Define conditional macros.
#

%_native :	TGT =			$(shell arch)
%_native :	CONFIGURE =	

#
# Target Definitions
#

build:		build_tool_native

build_tool_native:
		ExecProjName RCSDE "RCSMW/1.0" "module rm $(COMMON_RMS) ; module add $(COMMON_ADDS) gcc/4.3.2 && $(MAKE) tool_native"

build_tool_msrcs:
		ExecProjName RCSDE "RCSMW/1.0" "module rm $(COMMON_RMS) ; module add $(COMMON_ADDS) msrcs && $(MAKE) tool_msrcs"

build_tool_sim:
		ExecProjName RCSDE "RCSMW/1.0" "module rm $(COMMON_RMS) ; module add $(COMMON_ADDS) rcssim && $(MAKE) tool_sim"

build_tool_vrcs64:
		ExecProjName RCSDE "RCSMW/1.0" "module rm $(COMMON_RMS) ; module add $(COMMON_ADDS) vrcs64 && $(MAKE) tool_vrcs64"


release:	Makefile build
		ci_data.sh -f $(BLS) -a write -i $(TOOL_ID) -p 13 -c $(TOOL_CACHE)
		chmodAll.sh -p $(TOOL_CACHE)

tool_%:		Makefile $(TOOL_SRC)
		-@$(RM) -r $(TOOL_DIR) > /dev/null 2>&1
		unzip $(TOOL_SRC)
		cd $(TOOL_DIR); \
		./configure $(CONFIGURE) --prefix $(TOOL_PREFIX); \
		make -j $(PARALLEL_BUILDS); \
		make install
		$(RM) -r $(TOOL_DIR)

$(TOOL_SRC):
		wget -O $(TOOL_DIR).zip $(TOOL_SRC_URL)

#******************************************************************************
#   End Makefile.

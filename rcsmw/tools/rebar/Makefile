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

TOOL_ID :=	613
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

#******************************************************************************
#   Define conditional macros.
#

#
# Target Definitions
#

build:		build_tool_common

build_tool_common:
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add /app/otp/modules/otp_17 && $(MAKE) tool_common"

release:	Makefile build
		install -m 555 -D -p $(TOOL_DIR)/$(TOOL_NAME) $(TOOL_CACHE)/bin/$(TOOL_NAME)
		ci_data.sh -f $(BLS) -a write -i $(TOOL_ID) -p 13 -c $(TOOL_CACHE)
		chmodAll.sh -p $(TOOL_CACHE)
		$(RM)r $(TOOL_DIR)

tool_%:		Makefile $(TOOL_SRC)
		-@$(RM) -r $(TOOL_DIR) bin> /dev/null 2>&1
		unzip $(TOOL_SRC)
		cd $(TOOL_DIR); \
		./bootstrap

$(TOOL_SRC):
		wget -O $(TOOL_DIR).zip $(TOOL_SRC_URL)

#******************************************************************************
#   End Makefile.

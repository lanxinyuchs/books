# **********************************************************************
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
#

#------------------------------------------------------------
#  Check heading of the included "RCSbuild.mk" for makefile info
#------------------------------------------------------------

#******************************************************************************
#   Macros
#

.SECONDEXPANSION:

# -- Macros for push to ClearCase ------
TGTS :=			dus arm
OUT_DIR :=		out
OUT :=			$(OUT_DIR)
PRODUCTNAME :=		TBOX-SUITES
CXA_NUM :=		CXA1106478
CNX_NUM :=		CNX9013330
CAX_NUM :=		CAX1033705
CXC_NUM_NOSFX :=	CXC1737291
NO_CXC :=		yes
#----------------------------------------

# First inclusion of RCSbuild.mk - early definitions
RCS-TOOLS =	../rcs-tools
include $(RCS-TOOLS)/build/deliver.mk

#-------------------------------
# Per platform conditional macros
#-------------------------------

dus-% :			TGT = dus
arm-% :			TGT = arm


# --- End, per platform conditional macros
ALL_OUT_DIRS :=		$(TGTS:%=$(OUT)/%) 


#******************************************************************************
#   Targets
#

$(TGTS:%=%-install): $(OUT)/rct

clean:
	$(RM) -rf $(OUT)

$(OUT)/rct:
	for d in $(ALL_OUT_DIRS); do mkdir -p $$d; done
	@cp -r rct $(OUT)/

#******************************************************************************
#   End Makefile
#

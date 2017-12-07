# **********************************************************************

# Short description:
# Initiate environment, build and release of all or individual flavours of COMTE
#
# **********************************************************************
#
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
# Rev        Date         Name        What
# -----      -------      --------    --------------------------
# A          2017-05-05   etxbjca     Created
#
# **********************************************************************
#

include $(TOPDIR)/Makefile

#******************************************************************************
#   Define macros.
#

# COMTE Macros
COMTE_ID :=			605
COMTE_TOP :=			$(CURDIR)
COMTE_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_ID) -p 15)
COMTE_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_ID) -p 14)
COMTE_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_ID) -p 7)

#COMTE_LM_ID :=			138
#COMTE_LM_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_LM_ID) -p 2 | tr '/' '_')
#COMTE_LM_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_LM_ID) -p 6)
#COMTE_LM_NAME :=		$(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_LM_ID) -p 7)

#COMTE_LM_CACHE :=		$(RCSMWCACHE)/$(COMTE_LM_NAME)

#COMTE_LM =			$(COMTE_LM_CACHE)/$(COMTE_LM_REV)/$(COMTE_LM_NAME)_$(COMTE_LM_NO)

BASEDIR :=			$(COMTE_TOP)/comtebase
COMTE_MAIN =			$(BASEDIR)/comte
COMTE_VSN_FILE =		$(COMTE_TOP)/esrc/comte.version

#COMTE_IIF_ID :=                
#COMTE_IIF_NO :=                $(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_IIF_ID) -p 2 | tr '/' '_')
#COMTE_IIF_REV =                $(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_IIF_ID) -p 6)
#COMTE_IIF_NAME :=              $(shell ci_data.sh -f $(BLS) -a read -i $(COMTE_IIF_ID) -p 7)
#COMTE_IIF_CACHE :=             $(RCSMWCACHE)/$(COMTE_IIF_NAME)
#COMTE_IIF =                    $(COMTE_IIF_CACHE)/$(COMTE_IIF_REV)/$(COMTE_IIF_NAME)_$(COMTE_IIF_NO)
#COMTE_INC_DIR =		$(COMTE_IIF)/inc


#ARCH_CFLAGS =			-DCOM_TRANS_H=2 \
#				-DCOM_PM_MEAS_H=3 \
#				-DCOM_FM_H=4


#COMTE_EXPORTS =			\
#				COM_INC_DIR=$(COM_INC_DIR) \
#				DED_LD="$(DED_LD)" \
#				CC="$(CC)" \
#				CXX="$(CXX)" \
#				CFLAGS="$(CFLAGS)" \
#				CPPFLAGS="$(CPPFLAGS)" \
#				LDFLAGS="$(LDFLAGS)"

#******************************************************************************
#   Define conditional macros.
#

# msrcs
#%_msrcs :	DED_LD =	$(CC)

# vrcs64
#%_vrcs64 :	DED_LD =	$(CC)

# sim32
#%_sim32 :       CFLAGS  =  	-m32
#%_sim32 :       DED_LD =	$(CC) -m32


#******************************************************************************
#   Define targets.
#

default:	clone

#build:		clone genRevs default

clone:		clone_comte

import:		import_comte

#release:	build comte_release



build_comte_msrcs:
		ExecProjName RCSDE "RCSMW/1.0" "module add msrcs && $(MAKE) comte_msrcs"

build_comte_sim32:
		ExecProjName RCSDE "RCSMW/1.0" "module add sim32 && $(MAKE) comte_sim32"

build_comte_vrcs64:
		ExecProjName RCSDE "RCSMW/1.0" "module add vrcs64 && $(MAKE) comte_vrcs64"


# comte_release:	
# 		@printf "\nReleasing COMTE to $(BLS)/Nexus/InfoBank...\n"
# 		@date
#		chmodAll.sh -p $(COMTE_IIF)
#		deploy.sh -i $(COMTE_IIF_ID) -f $(BLS) -p $(COMTE_IIF_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0
#		deploy.sh -i $(COMTE_LM_ID) -f $(BLS) -p $(COMTE_LM_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0
#		deploy.sh -i $(COMTE_EIF_ID) -f $(BLS) -p $(COMTE_EIF_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0

#comte_publish:
# 		@printf "\nPublishing COMTE arch independent result, please wait...\n"
# 		@date
# 		-$(RM) -r $(COMTE_EIF)/inc; mkdir -p $(COMTE_EIF)/inc
# 		-$(RM) -r $(COMTE_IIF)/inc; mkdir -p $(COMTE_IIF)/inc
# 		install -p -D $(COMTEC_MAIN)/include/* $(COMTE_IIF)/$(TGT)/include
# 		install -p -D $(BASEDIR)/out/safc/$(TGT)/priv/lib/* $(COMTE_IIF)/$(TGT)/lib

import_comte:	clone
		install -p -D $(COMTE_MAIN)/src/*.[eh]rl $(COMTE_MAIN)/vsn.mk esrc
		install -p -D $(COMTE_MAIN)/include/* inc
		install -p -D $(COMTE_MAIN)/csrc/*.[chi]* csrc
		install -p -D $(COMTE_MAIN)/csrc/test/*.[ch] csrc/test
		install -p -D $(COMTE_MAIN)/csrc/test/cutest-1.5/* csrc/test/cutest-1.5
		install -p -D $(COMTE_MAIN)/priv/* priv

#comte_%:		
#		@printf "\nBuilding COMTE [$@],please wait...\n"
#		@date
#		export $(COMTE_EXPORTS); cd $(COMTE_MAIN) && git clean -dfx src csrc ; make compile
#		export $(COMTE_EXPORTS); cd $(COMTE_MAIN) && make RELEASE_DIR=$(BASEDIR)/out/comte release
#		install -d $(COMTE_LM)/$(@:comte_%=%)/priv/$(TGT) $(COMTE_LM)/$(@:comte_%=%)/ebin
#		install -d $(COMTE_IIF)/$(TGT)/include $(COMTE_IIF)/$(TGT)/lib
#		install -p -D $(COMTEC_MAIN)/include/* $(COMTE_IIF)/$(TGT)/include
#		install -p -D $(BASEDIR)/out/safc/$(TGT)/priv/lib/* $(COMTE_IIF)/$(TGT)/lib

#genRevs:	
#		@printf "\nRegistrates new R-states...\n"
#		@date
#		@set -x;for ID in $(COMTE_LM_ID) $(COMTE_IIF_ID); \
#		do \
#		  NO=`ci_data.sh -f $(BLS) -a read -i $${ID} -p 2`; \
#		  REV=`$(NEXTREV) -r $(RSTATE_BASE) -p $${NO}`; \
#		  ci_data.sh -f $(BASELINE) -a write -i $${ID} -p 6 -c $${REV}; \
#		  ci_data.sh -f $(BLS) -a write -i $${ID} -p 6 -c $${REV}; \
#		done

updateCacheLinks:
		@printf "\nUpdating binary cache links...\n"
		@date
		-cd $(COMTE_TOP) && rm -f include
		-$(RM) $(wildcard $(COMTE_TOP)/tgt_*)
		ln -s $(wildcard $(PROTOBUFC_ROOT:%=%)/*/tgt_*) $(COMTE_TOP)

clone_comte:	
		@printf "\nFetching COMTE source, please wait...\n"
		@date
		-@$(RM) -rf $(COMTE_MAIN) > /dev/null 2>&1
		@test -d $(BASEDIR) || mkdir -p $(BASEDIR)
		@$(GERRIT_CFG); \
		cd $(BASEDIR) && git clone $(COMTE_REPO) comte
		cd $(COMTE_MAIN) && git checkout $(COMTE_VER)
		cd $(COMTE_MAIN) && echo "`git rev-parse HEAD` $(notdir $(COMTE_MAIN))" > $(COMTE_VSN_FILE)
		cp $(COMTE_MAIN)/csrc/Makefile $(COMTE_MAIN)/csrc/Makefile.org
		sed 's/DED_LD = gcc//' $(COMTE_MAIN)/csrc/Makefile.org > $(COMTE_MAIN)/csrc/Makefile

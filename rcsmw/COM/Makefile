# **********************************************************************

# Short description:
# Initiate environment and build all or individual flavours of RCSMW
#
# **********************************************************************
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
# A          2017-02-22   etxbjca     Created
#
# **********************************************************************
#

include $(TOPDIR)/Makefile

#******************************************************************************
#   Define macros.
#

# COM Macros
COM_ID :=			600
COM_LM_ID :=			136
COM_IIF_ID :=			300
COM_EIF_ID :=			301
COM_SRC_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_ID) -p 2 | tr '/' '_')
COM_SRC_REV :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_ID) -p 6)
COM_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_ID) -p 15)
COM_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_ID) -p 14)
COM_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_ID) -p 7)

COM_LM_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_LM_ID) -p 2 | tr '/' '_')
COM_LM_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_LM_ID) -p 6)
COM_LM_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_LM_ID) -p 7)

COM_IIF_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_IIF_ID) -p 2 | tr '/' '_')
COM_IIF_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_IIF_ID) -p 6)
COM_IIF_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_IIF_ID) -p 7)

COM_EIF_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_EIF_ID) -p 2 | tr '/' '_')
COM_EIF_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_EIF_ID) -p 6)
COM_EIF_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(COM_EIF_ID) -p 7)

COM_LM_CACHE :=			$(RCSMWCACHE)/$(COM_LM_NAME)
COM_IIF_CACHE :=		$(RCSMWCACHE)/$(COM_IIF_NAME)
COM_EIF_CACHE :=		$(RCSMWCACHE)/$(COM_EIF_NAME)

COM_LM =			$(COM_LM_CACHE)/$(COM_LM_REV)/$(COM_LM_NAME)_$(COM_LM_NO)
COM_IIF =			$(COM_IIF_CACHE)/$(COM_IIF_REV)/$(COM_IIF_NAME)_$(COM_IIF_NO)
COM_EIF =			$(COM_EIF_CACHE)/$(COM_EIF_REV)/$(COM_EIF_NAME)_$(COM_EIF_NO)

COM_TOP :=			$(CURDIR)
BASEDIR :=			$(COM_TOP)/combase
COM_MAIN :=			$(BASEDIR)/$(notdir $(COM_REPO))
COM_VSN_FILE :=			$(BASEDIR)/$(notdir $(COM_REPO)).version

CMAKE_FILE_MSRCS :=		$(COM_TOP)/rcs-arm.cmake
CMAKE_FILE_SIM :=		$(COM_TOP)/rcs-sim64.cmake
CMAKE_FILE_SIM32 :=		$(COM_TOP)/rcs-sim32.cmake
CMAKE_FILE_VRCS64 :=		$(COM_TOP)/rcs-vrcs64.cmake

CMAKE_ARGS_MSRCS =		-DCOM_VERSION="$(COM_VER)" \
				-DSYSROOT=$(SDKTARGETSYSROOT) \
				-DBUILDFLAGS="--sysroot=$(SDKTARGETSYSROOT) $(CFLAGS)" \
				-DBUILDLDFLAGS="$(LDFLAGS)" \
				-DUSE_PREBUILT=off \
				-DCMAKE_TOOLCHAIN_FILE=$(CMAKE_FILE_MSRCS)

CMAKE_ARGS_SIM32 =		-DCOM_VERSION="$(COM_VER)" \
				-DSYSROOT=$(SDKTARGETSYSROOT) \
				-DBUILDFLAGS="--sysroot=$(SDKTARGETSYSROOT) $(CFLAGS)" \
				-DBUILDLDFLAGS="$(LDFLAGS)" \
				-DUSE_PREBUILT=off \
				-DCMAKE_TOOLCHAIN_FILE=$(CMAKE_FILE_SIM32)

CMAKE_ARGS_VRCS64 =		-DCOM_VERSION="$(COM_VER)" \
				-DSYSROOT=$(SDKTARGETSYSROOT) \
				-DBUILDFLAGS="--sysroot=$(SDKTARGETSYSROOT) $(CFLAGS)" \
				-DBUILDLDFLAGS="$(LDFLAGS)" \
				-DUSE_PREBUILT=off \
				-DCMAKE_TOOLCHAIN_FILE=$(CMAKE_FILE_VRCS64)


COM_MIBS :=			$(COM_MAIN)/model/instances/ComFmFmSecurityRules_mp.xml \
				$(COM_MAIN)/model/instances/ComLocalAuthorizationInstances_mp.xml \
				$(COM_MAIN)/model/instances/ComSysMSysMSecurityRules_mp.xml \
				$(COM_MAIN)/model/instances/ComTopTopSecurityRules_mp.xml


COM_INCS :=			ComCliApi*.h \
				ComMgmtSpi*.h \
				ComOamSpiTransaction*.h \
				ComMwSpiCrypto_1.h \
				ComMwSpiServiceIdentities_1.h \
				ComOamSpiModelRepository_1.h \
				ComOamSpiSecurityManagement_1.h \
				ComOamSpiServiceIdentities_1.h

COM_MAF_INCS :=			MafMgmtSpi*.h \
				MafMwSpiLog_1.h \
				MafMwSpiServiceIdentities_1.h \
				MafOamSpiTransaction*.h

#******************************************************************************
#   Define conditional macros.
#
%_msrcs  : 	ARCH_LDFLAGS =		
%_msrcs : 	CMAKE_ARGS =	$(CMAKE_ARGS_MSRCS)


%_vrcs64  : 	ARCH_LDFLAGS =		
%_vrcs64 : 	CMAKE_ARGS =	$(CMAKE_ARGS_VRCS64)


%_sim32 :	ARCH_LDFLAGS =	
%_sim32 : 	CMAKE_ARGS =	$(CMAKE_ARGS_SIM32)


#******************************************************************************
#   Define targets.
#


default:	build_com_sim32 build_com_msrcs build_com_vrcs64


build:		$(BASELINE) clone default


publish:	build publish_com



release:	publish release_com


build_com_sim32:
		ExecProjName RCSDE "RCSMW/1.0" "module add sim32 cmake/2.8.6 maven/3.1.0 j2sdk/1.6.0_45 && $(MAKE) com_sim32"

build_com_vrcs64:
		ExecProjName RCSDE "RCSMW/1.0" "module add vrcs64 cmake/2.8.6 maven/3.1.0 j2sdk/1.6.0_45 && $(MAKE) com_vrcs64"

build_com_msrcs:
		ExecProjName RCSDE "RCSMW/1.0" "module add msrcs cmake/2.8.6 maven/3.1.0 j2sdk/1.6.0_45 && $(MAKE) com_msrcs"


publish_com:	genRevs publish_com_sim32 publish_com_msrcs publish_com_vrcs64 publish_common updateCacheLinks


publish_com_sim32:
		ExecProjName RCSDE "RCSMW/1.0" "module add sim32 && $(MAKE) com_publish_sim32"

publish_com_vrcs64:
		ExecProjName RCSDE "RCSMW/1.0" "module add vrcs64 && $(MAKE) com_publish_vrcs64"

publish_com_msrcs:
		ExecProjName RCSDE "RCSMW/1.0" "module add msrcs && $(MAKE) com_publish_msrcs"


com_publish_%:	
		@printf "\nExpanding COM [$@] build result, please wait...\n"
		@date
		-$(RM) -r $(COM_LM)/$(TGT) ; mkdir -p $(COM_LM)/$(TGT)
		@set -x;cd  $(COM_LM)/$(TGT); \
		tar xf $(COM_MAIN)/$(BLDDIR)/dist/com*runtime*.tar.gz; \
		for file in *.tar.gz; \
		do \
		  tar xf $${file}; \
		  rm $${file} || true; \
		done; \
		cp -af $(COM_TOP)/patches/* ./
		chmodAll.sh -p $(COM_LM)/$(TGT)

com_%:	
		@printf "\nBuilding COM [$@], please wait...\n"
		@date
		git clean -dfx $(COM_MAIN)/$(BLDDIR)
		-mkdir -p $(COM_MAIN)/$(BLDDIR)
		cd $(COM_MAIN)/$(BLDDIR) && cmake $(CMAKE_ARGS) .. && make -j $(PARALLEL_BUILDS)


genRevs:	
		@printf "\nRegistrate new R-states...\n"
		@date
		@set -x;for ID in $(COM_LM_ID) $(COM_EIF_ID) $(COM_IIF_ID); \
		do \
		  NO=`ci_data.sh -f $(BLS) -a read -i $${ID} -p 2`; \
		  REV=`$(NEXTREV) -r $(RSTATE_BASE) -p $${NO}`; \
		  ci_data.sh -f $(BASELINE) -a write -i $${ID} -p 6 -c $${REV}; \
		  ci_data.sh -f $(BLS) -a write -i $${ID} -p 6 -c $${REV}; \
		done

updateCacheLinks:
		@printf "\nUpdating binary cache links...\n"
		@date
		-cd $(COM_TOP) && rm -f int_include ext_include
		cd $(COM_TOP) && ln -s $(COM_IIF) int_include
		cd $(COM_TOP) && ln -s $(COM_EIF) ext_include
		-$(RM) $(COM_TOP)/tgt_*
		ln -s $(wildcard $(COM_LM)/tgt_*) $(COM_TOP)

publish_common:
		@printf "\nPublishing COM arch independent result, please wait...\n"
		@date
		-$(RM) -r $(COM_LM)/mib; mkdir -p $(COM_LM)/mib
		-$(RM) -r $(COM_EIF)/inc; mkdir -p $(COM_EIF)/inc
		-$(RM) -r $(COM_EIF)/model; mkdir -p $(COM_EIF)/model
		-$(RM) -r $(COM_IIF)/inc; mkdir -p $(COM_IIF)/inc
		cp -p $(COM_MIBS) $(COM_LM)/mib
		chmodAll.sh -p $(COM_LM)/mib
		cp -r $(COM_TOP)/emx $(COM_EIF)
		cp $(COM_TOP)/inc/*.h $(COM_EIF)/inc
		@set -x;for file in $(COM_INCS); \
		do \
		  cp $(COM_MAIN)/inc/$${file} $(COM_EIF)/inc; \
		done
		@set -x;for file in $(COM_MAF_INCS); \
		do \
		  cp $(COM_MAIN)/3pp/src/maf/inc/$${file} $(COM_EIF)/inc; \
		done; \
		cp $(COM_MAIN)/model/*.xml $(COM_EIF)/model
		cp $(COM_MAIN)/model/mp.dtd $(COM_EIF)/model
		cp $(COM_MAIN)/build-arm32/tmp/installation/com/model_file_list.cfg $(COM_EIF)/model
		chmodAll.sh -p $(COM_EIF)
		cp $(COM_MAIN)/inc/* $(COM_IIF)/inc
		cp $(COM_MAIN)/3pp/src/maf/inc/* $(COM_IIF)/inc
		chmodAll.sh -p $(COM_IIF)


release_com:	
		@printf "\nReleasing to $(BASELINE)/Nexus/InfoBank...\n"
		@date
		deploy.sh -b $(BUDITCFG) -i $(COM_LM_ID) -f $(BLS) -e -p $(COM_LM_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0
		deploy.sh -b $(BUDITCFG) -i $(COM_IIF_ID) -f $(BLS) -e -p $(COM_IIF_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0
		deploy.sh -b $(BUDITCFG) -i $(COM_EIF_ID) -f $(BLS) -e -p $(COM_EIF_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0

clone:		$(BASELINE)
		@printf "\nFetching COM source, please wait...\n"
		@date
		@$(GERRIT_CFG)
		-$(RM)r $(BASEDIR)
		-mkdir -p $(BASEDIR)
		cd $(BASEDIR) && git clone $(COM_REPO)
		cd $(COM_MAIN) && git checkout $(COM_VER) && git submodule update --init
		cd $(COM_MAIN) && echo "`git rev-parse HEAD` $(notdir $(COM_MAIN))" > $(COM_VSN_FILE)

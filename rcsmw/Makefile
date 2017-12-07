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
# A          2016-12-15   etxbjca     Created
#
# **********************************************************************
#

#******************************************************************************
#   Define macros.
#
OUTDIR :=			out
UPDIR :=			$(TOPDIR)/$(OUTDIR)/UP
MPDTOP :=			$(TOPDIR)/tools/3pp/mpDtdParser_rev1
MPDSCHEMA :=			$(MPDTOP)/schemas
BLS_DIR :=			$(TOPDIR)/skeleton
BLS_FILE :=			CIdata.txt.skeleton
BLS :=				$(BLS_DIR)/$(BLS_FILE)
BASELINE :=			$(TOPDIR)/$(BLS_FILE:%.skeleton=%)
INIT_BASELINE :=		$(shell test -s $(BASELINE) || cp $(BLS) $(BASELINE))
BLDBASELINE :=			$(UPDIR)/baseline.txt
PARALLEL_BUILDS :=		12
TGTLIST :=			msrcs vrcs64 sim32
SRCFILTER :=			SVNFMD VNFC DIA
VSRCDIRS :=			$(shell cd $(TOPDIR) && ls -d1 * | egrep -v '[a-z,\.]'| grep 'CEC') $(shell cd $(TOPDIR) && ls -d1 * | egrep -v '[a-z,\.]|CEC|VRCS')
SRCDIRS :=			$(filter-out $(SRCFILTER),$(VSRCDIRS))
TSTSRCDIRS :=			$(shell cd $(TOPDIR) && ls -d1 test/* | grep -v '^test/[a-z,\.]')
BUDITCFG :=			/env/RCSDE/cnf/RCS_FEM025_ANY_project_settings.cnf

# 2bRemoved when xml-generation is implemented
SRCDIR :=			$(shell cd $(TOPDIR) && ls -d1 * | egrep -v '[a-z,\.]'| grep 'CEC') $(shell cd $(TOPDIR) && ls -d1 * | egrep -v '[a-z,\.]|CEC|VRCS') $(shell cd $(TOPDIR) && ls -d1 test/* | grep -v '^test/[a-z,\.]')

# R-state management		# for "sharp" R-state, use: rstatef_ci
NEXTREV :=			nextRev.sh
RSTATEF :=			$(NEXTREV)

RSTATE_ROOT_DIR :=		$(shell ci_data.sh -f $(BLS) -a read -i 0 -p 6)
RSTATE_BASE :=			$(shell ci_data.sh -f $(BLS) -a read -i 0 -p 2)
SLASHED :=			$(shell ci_data.sh -f $(BLS) -a read -i 0 -p 3)

ifeq ($(USER),rcsci1) # Use prel (P-states) outside CI-engine
RSTATETYPE :=			-r
endif
ifneq ($(SDC),) # SDC conditional parameters
RSTATETYPE :=			
OPT_EE_CLEVEL :=		-c '3 1 0'
endif
ifeq ($(RSTATETYPE),) # SDC conditional parameters
NEXTREV_FLAGS :=		-P
endif
NEXTREV_FLAGS +=		-r $(RSTATE_BASE) -b $(BASELINE) -p 
RSTATEF_FLAGS :=		$(NEXTREV_FLAGS)

# MW CXC data
CXC_ID :=			131
CXC_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(CXC_ID) -p 2)

# DUMMY CXC data
DCXC_ID :=			132
DCXC_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(DCXC_ID) -p 2)

ifeq ($(DEBUG),)
CXC_REV :=			$(shell $(NEXTREV) $(NEXTREV_FLAGS) $(CXC_NO))
DCXC_REV :=			$(shell $(NEXTREV) $(NEXTREV_FLAGS) $(DCXC_NO))
else
CXC_REV :=			$(shell echo "$(RSTATE_BASE)999" | tr 'R' 'P')
DCXC_REV :=			$(shell echo "$(RSTATE_BASE)888" | tr 'R' 'P')
endif

# Framework data
FRAMEWORK_ID :=			102
FRAMEWORK_NAME :=		$(shell ci_data.sh -f $(BLS) -a read -i $(FRAMEWORK_ID) -p 7)
FRAMEWORK_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(FRAMEWORK_ID) -p 2)
FRAMEWORK_REV =			$(shell $(NEXTREV) $(NEXTREV_FLAGS) $(FRAMEWORK_NO))

# System data
SYSTEM_ID :=			1
SYSTEM_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SYSTEM_ID) -p 7)
SYSTEM_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SYSTEM_ID) -p 2)
SYSTEM_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(SYSTEM_ID) -p 6)

# Enable Gerrit access as "rcsci1" for external repos
SSH_CFG_FILE =			$(HOME)/.ssh/config
GERRIT_CFG =			if [ "$(USER)" != "rcsci1" ]; \
				then \
			  	  if [ -s "$(SSH_CFG_FILE)" ]; \
			  	  then \
			  	    sed '/%rcsci1-gerrit_start%/,/%rcsci1-gerrit_end%/d' $(SSH_CFG_FILE) > $(SSH_CFG_FILE).tmp; \
				  fi; \
			  	  echo "Preparing SSH configuration for Gerrit access as \"rcsci1\"..."; \
			  	  ( sudo -u rcsci1 cat ~rcsci1/.ssh/config_skeleton ) | grep -vi 'your password' > $(SSH_CFG_FILE) && \
			  	  cat $(SSH_CFG_FILE).tmp >> $(SSH_CFG_FILE);\rm -f $(SSH_CFG_FILE).tmp || true; \
			  	  chmod 600 $(SSH_CFG_FILE) || true; \
			  	  ( sudo -u rcsci1 cat ~rcsci1/.ssh/rcsci1-gerrit_id ) > ~/.ssh/rcsci1-gerrit_id && chmod 600 ~/.ssh/rcsci1-gerrit_id && \
			  	  ( sudo -u rcsci1 cat ~rcsci1/.ssh/rcsci1-gerrit_id.pub ) > ~/.ssh/rcsci1-gerrit_id.pub; \
				fi


# Config Files
MSRCS_PROD_SKEL :=		skeleton/msrcs_product.py.skeleton
VRCS64_PROD_SKEL :=		skeleton/vrcs64_product.py.skeleton
SIM32_PROD_SKEL :=		skeleton/sim32_product.py.skeleton
MSRCS_PROD_FILE :=		tools/config/msrcs_product.py
VRCS64_PROD_FILE :=		tools/config/vrcs64_product.py
SIM32_PROD_FILE :=		tools/config/sim32_product.py
OSEMAIN :=			$(TOPDIR)/LMA/csrc/osemain.c
PROD_FILES :=			$(SIM32_PROD_FILE) $(VRCS64_PROD_FILE) $(MSRCS_PROD_FILE)

# RAN Baseline Data
RAN_BLID :=			50
FLOWCONTEXT :=			$(shell ci_data.sh -f $(BASELINE) -a read -i $(RAN_BLID) -p 11)

# "ALL" Data
ALL_ID_LIST :=			106
ALL_PROD_LIST =			$(foreach ID,$(ALL_ID_LIST),$(shell ci_data.sh -f $(BASELINE) -a read -i $(ID) -p 2 | tr '[:upper:]' '[:lower:]' | tr '/' '_'))
ALL_XML_LIST =			$(ALL_PROD_LIST:%=$(OUTDIR)/%.xml_tmpl) 

# CXP Data
CXP_ID_LIST =			$(foreach ID,$(ALL_ID_LIST),$(shell ci_data.sh -f $(BASELINE) -a read -i $(ID) -p 8))
CXP_PROD_LIST =			$(foreach ID,$(CXP_ID_LIST),$(shell ci_data.sh -f $(BASELINE) -a read -i $(ID) -p 2,14 | awk -F\| '{if ($$2!="external"){print $$1}}' | tr '[:upper:]' '[:lower:]' | tr '/' '_'))
CXP_XML_LIST =			$(CXP_PROD_LIST:%=$(OUTDIR)/%.xml_tmpl)

# UP Data
VRCS64_UPID :=			99
SIM32_UPID :=			103
MSRCS_UPID :=			104 105
UP_ID_LIST :=			$(SIM32_UPID) $(VRCS64_UPID) $(MSRCS_UPID)
UP_PROD_LIST =			$(foreach ID,$(UP_ID_LIST),$(shell ci_data.sh -f $(BASELINE) -a read -i $(ID) -p 2 | tr '[:upper:]' '[:lower:]' | tr '/' '_'))
UP_XML_LIST =			$(UP_PROD_LIST:%=$(OUTDIR)/%-up.xml_tmpl)

# XML Dynamic Skeletons
XML_SKEL_FILES =		$(ALL_XML_LIST) $(UP_XML_LIST) $(CXP_XML_LIST)

# GPB Dynamic Data
GPB_ID :=			612
GPB_ROOT :=			$(shell ci_data.sh -f $(BLS) -a read -i $(GPB_ID) -p 13)

# OTP Dynamic Data
OTP_LM_ID :=			130
OTP_LM_ROOT :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_LM_ID) -p 13)

# SAF Dynamic Data
SAF_IF_ID :=                    303
SAF_IF_ROOT :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAF_IF_ID) -p 13)

# HWMB Dynamic Data
HWMB_ID :=			108
HWMB_DIR =                   	$(shell ci_data.sh -f $(BASELINE) -a read -i $(HWMB_ID) -p 13)

# EE DUS2/DUS3 Dynamic Data
EE_SDK_ID_G2EE :=		147
EE_IF_ID_LIST_G2EE :=		$(EE_SDK_ID_G2EE)
EE_ARCH_G2EE :=			arm-wr6
EE_SDK_G2EE =			$(wildcard $(shell ci_data.sh -f $(BASELINE) -a read -i $(EE_SDK_ID_G2EE) -p 13))
EE_IF_LIST_G2EE =		$(foreach IF,$(EE_IF_ID_LIST_G2EE),$(shell ci_data.sh -f $(BASELINE) -a read -i $(IF) -p 13))

# EE SIM32 Dynamic Data
EE_SDK_ID_SIM32 :=		148
EE_IF_ID_LIST_SIM32 :=		$(EE_SDK_ID_SIM32)
EE_ARCH_SIM32 :=		i586-wrs-linux
EE_SDK_SIM32 =			$(wildcard $(shell ci_data.sh -f $(BASELINE) -a read -i $(EE_SDK_ID_SIM32) -p 13))
EE_IF_LIST_SIM32 =		$(foreach IF,$(EE_IF_ID_LIST_SIM32),$(shell ci_data.sh -f $(BASELINE) -a read -i $(IF) -p 13))

# EE VRCS64 Dynamic Data
EE_SDK_ID_VRCS64 :=		149
EE_IF_ID_LIST_VRCS64 :=		$(EE_SDK_ID_VRCS64)
EE_ARCH_VRCS64 :=		x86_64-wrlinuxsdk-linux
EE_SDK_VRCS64 =			$(wildcard $(shell ci_data.sh -f $(BASELINE) -a read -i $(EE_SDK_ID_VRCS64) -p 13))
EE_IF_LIST_VRCS64 =		$(foreach IF,$(EE_IF_ID_LIST_VRCS64),$(shell ci_data.sh -f $(BASELINE) -a read -i $(IF) -p 13))

# SIM CXP Dynamic Data
SIM_CXP_ID :=			117
SIM_CXP_NO :=                   $(shell ci_data.sh -f $(BLS) -a read -i $(SIM_CXP_ID) -p 2 | tr '/' '_')
SIM_CXP_NAME :=                 $(shell ci_data.sh -f $(BLS) -a read -i $(SIM_CXP_ID) -p 7)
SIM_CXP :=			$(SIM_CXP_NAME)_$(SIM_CXP_NO)

# EE SIM32 CXP Dynamic Data
EE_SIM32_CXP_ID :=		0133
EE_SIM32_CXP_NO :=              $(shell ci_data.sh -f $(BLS) -a read -i $(EE_SIM32_CXP_ID) -p 2 | tr '/' '_')
EE_SIM32_CXP_NAME :=            $(shell ci_data.sh -f $(BLS) -a read -i $(EE_SIM32_CXP_ID) -p 7)
EE_SIM32_CXP :=			$(EE_SIM32_CXP_NAME)_$(EE_SIM_CXP_NO)

# WAF ENV's
EE_SIM32_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i $(EE_SIM32_CXP_ID) -p 6)
EE_SIM32_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i $(EE_SIM32_CXP_ID) -p 13)
EE_VRCS64_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 133 -p 6)
EE_VRCS64_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 133 -p 13)
EE_DUS_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 122 -p 6)
EE_DUS_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 122 -p 13)
RCS_DUS2_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 115 -p 6)
RCS_DUS2_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 115 -p 13)
RCS_DUS3_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 120 -p 6)
RCS_DUS3_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 120 -p 13)
RCS_T_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 116 -p 6)
RCS_T_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 116 -p 13)
KATLA_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 111 -p 6)
KATLA_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 111 -p 13)
TIGER_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 113 -p 6)
TIGER_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 113 -p 13)
MTIGER_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 114 -p 6)
MTIGER_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 114 -p 13)
COBRA_REV =			$(shell ci_data.sh -f $(BASELINE) -a read -i 109 -p 6)
COBRA_URL =			file://$(shell ci_data.sh -f $(BASELINE) -a read -i 109 -p 13)

NEXUS_CONFIG :=			/home/rcsci1/nexus-credentials/rcsmw-uploader
NEXUS_BASE_URL :=		https://arm001-eiffel002.rnd.ki.sw.ericsson.se:8443/nexus/content/repositories/releases/com/ericsson

# C-Compiler Settings
CFLAGS +=			$(ARCH_CFLAGS) \
				-Wall\
				-pedantic\
				-fno-omit-frame-pointer\
				-funwind-tables\
				-g\
				-Wl,--allow-shlib-undefined,-unresolved-symbols=ignore-in-shared-libs,--no-undefined \
				-Os

CPPFLAGS +=			$(ARCH_CPPFLAGS) \
				$(addprefix -I,$(wildcard $(PROTOC)/*/$(TGT)/include)) \
				$(addprefix -I,$(filter-out $(wildcard $(OTP_LM_ROOT)/*_dummy/$(TGT)/usr/include),$(wildcard $(OTP_LM_ROOT)/*/$(TGT)/usr/include))) \
				$(addprefix -I,$(filter $(wildcard $(OTP_LM_ROOT)/*_dummy/$(TGT)/usr/include),$(wildcard $(OTP_LM_ROOT)/*/$(TGT)/usr/include)))

LDFLAGS	+=			$(ARCH_LDFLAGS)\
				$(addprefix -L,$(wildcard $(PROTOC)/*/$(TGT)/lib)) \
				$(addprefix -L,$(wildcard $(SAF_IF_ROOT)/$(TGT)/lib)) \
				$(addprefix -L,$(filter-out $(wildcard $(OTP_LM_ROOT)/*_dummy/$(TGT)/usr/lib),$(wildcard $(OTP_LM_ROOT)/*/$(TGT)/usr/lib))) \
				$(addprefix -L,$(filter $(wildcard $(OTP_LM_ROOT)/*_dummy/$(TGT)/usr/lib),$(wildcard $(OTP_LM_ROOT)/*/$(TGT)/usr/lib))) \
				-ldl

# Erlang Compiler Settings
ERL_LIBS =			$(SAFE_OTP_ROOT)/lib
ERL_FLAGS +=			$(addprefix -pa ,$(wildcard $(GPB_ROOT)/ebin))
ERLC_FLAGS +=			$(addprefix -I,$(wildcard $(OTP_ROOT)/lib/public_key-*/include)) \
				$(addprefix -I,$(wildcard $(GPB_ROOT)/include))


# Schema Settings
MP_DTD_PARSER_FLAGS +=		-I $(MPDTOP)/schemas

# WAF Export Settings
COMMON_EXPORTS =		\
				FRAMEWORK_NAME="$(FRAMEWORK_NAME)" \
				FRAMEWORK_NO="$(FRAMEWORK_NO)" \
				FRAMEWORK_REV="$(FRAMEWORK_REV)" \
				SYSTEM_NAME="$(SYSTEM_NAME)" \
				SYSTEM_NO="$(SYSTEM_NO)" \
				SYSTEM_REV="$(SYSTEM_REV)" \
				CFLAGS="$(CFLAGS)" \
				CPPFLAGS="$(CPPFLAGS)" \
				LDFLAGS="$(LDFLAGS)" \
				SRCDIR="$(SRCDIR)" \
				TOPDIR="$(TOPDIR)" \
				MP_DTD_PARSER_FLAGS="$(MP_DTD_PARSER_FLAGS)" \
				ERL_LIBS="$(ERL_LIBS)" \
				ERL_FLAGS="$(ERL_FLAGS)" \
				ERLC_FLAGS="$(ERLC_FLAGS)" \
				EE_REV=$(EE_REV) \
				EE_URL=$(EE_URL) \
				RCS_DUS2_REV=$(RCS_DUS2_REV) \
				RCS_DUS2_URL=$(RCS_DUS2_URL) \
				RCS_DUS3_REV=$(RCS_DUS3_REV) \
				RCS_DUS3_URL=$(RCS_DUS3_URL) \
				RCS_T_REV=$(RCS_T_REV) \
				RCS_T_URL=$(RCS_T_URL) \
				KATLA_REV=$(KATLA_REV) \
				KATLA_URL=$(KATLA_URL) \
				TIGER_REV=$(TIGER_REV) \
				TIGER_URL=$(TIGER_URL) \
				MTIGER_REV=$(MTIGER_REV) \
				MTIGER_URL=$(MTIGER_URL) \
				COBRA_REV=$(COBRA_REV) \
				COBRA_URL=$(COBRA_URL)

RSTATE_EXPORTS =		\
				RSTATE_BASE=$(RSTATE_BASE) \
				SLASHED=$(SLASHED) \
				RSTATE_ROOT_DIR=$(RSTATE_ROOT_DIR) \
				RSTATEF=$(RSTATEF) \
				RSTATEF_FLAGS="$(RSTATEF_FLAGS)"

DEPLOYCL :=			0
DELIVERCL :=			1

#******************************************************************************
#   Define conditional macros.
#

# native
%_native : 	ARCH_CFLAGS =	
%_native : 	ARCH_CPPFLAGS =	
%_native : 	ARCH_LDFLAGS =	

# msrcs/msrcs_dummy
%_msrcs_dummy \
%_msrcs \
msrcs :		EE_URL =	$(EE_DUS_URL)

%_msrcs_dummy \
%_msrcs \
msrcs :		EE_REV =	$(EE_DUS_REV)

# vrcs64
%_vrcs64_dummy \
%_vrcs64 \
vrcs64 :	EE_URL =	$(EE_VRCS64_URL)

%_vrcs64_dummy \
%_vrcs64 \
vrcs64 :	EE_REV =	$(EE_VRCS64_REV)

# sim32/sim32_dummy
%_sim32_dummy \
%_sim32 \
sim32 :		EE_URL =	$(EE_SIM32_URL)

%_sim32_dummy \
%_sim32 \
sim32 :		EE_REV =	$(EE_SIM32_REV)

sim32_install :	PKG =		--sim32

msrcs_install :	PKG =		--zip

vrcs64_install :PKG =		--img


%sim32_product.py :	UPID =	$(SIM32_UPID)
%vrcs64_product.py :	UPID =	$(VRCS64_UPID)
%msrcs_product.py :	UPID =	$(MSRCS_UPID)

#******************************************************************************
#   Define targets.
#

ifeq ($(CURDIR),$(TOPDIR))
default:	rcs_default

all:		rcs_all

build:		rcs_build

publish:	rcs_publish

clone:		rcs_clone
endif

rcs_default:
		ExecProjName RCSDE "RCSMW/1.0" "module add msrcs && waf -j $(PARALLEL_BUILDS)"
		ExecProjName RCSDE "RCSMW/1.0" "module add sim32 && waf -j $(PARALLEL_BUILDS)"
		ExecProjName RCSDE "RCSMW/1.0" "module add vrcs64 && waf -j $(PARALLEL_BUILDS)"

rcs_all:	pack_sim32 pack_msrcs pack_vrcs64

rcs_build:	build_sim32 build_msrcs build_vrcs64

rcs_publish:	full

package:	pack_sim32 pack_msrcs pack_vrcs64

full:		full_sim32 full_msrcs full_vrcs64

build_sim32:	prepare
		ExecProjName RCSDE "RCSMW/1.0" "module add sim32 && $(MAKE) -B sim32"

build_msrcs:	prepare
		ExecProjName RCSDE "RCSMW/1.0" "module add msrcs && $(MAKE) -B  msrcs"

build_vrcs64:	prepare
		ExecProjName RCSDE "RCSMW/1.0" "module add vrcs64 && $(MAKE) -B vrcs64"

pack_sim32:	build_sim32
		ExecProjName RCSDE "RCSMW/1.0" "module add sim32 && $(MAKE) -B sim32_install"

pack_msrcs:	build_msrcs
		ExecProjName RCSDE "RCSMW/1.0" "module add msrcs && $(MAKE) -B msrcs_install"

pack_vrcs64:	build_vrcs64
		ExecProjName RCSDE "RCSMW/1.0" "module add vrcs64 && $(MAKE) -B vrcs64_install"

full_sim32:	pack_sim32

full_msrcs:	pack_msrcs
		patchUP.sh -i "80 81 83" -f $(BASELINE) -o $(UPDIR)

full_vrcs64:	pack_vrcs64
		patchUP.sh -i "82 84 85 87" -f $(BASELINE) -o $(UPDIR)


%_install:	
		export $(RSTATE_EXPORTS) TOPDIR=$(TOPDIR); waf --vsn "$(CXC_REV)" --dvsn "$(DCXC_REV)" install --destdir $(OUTDIR)
	    	export $(RSTATE_EXPORTS) TOPDIR=$(TOPDIR); waf --vsn "$(CXC_REV)" --dvsn "$(DCXC_REV)" $(PKG) -j1
		cp -ax $(BLDDIR)/out/* $(UPDIR)
		reportRstates.sh -i $(BLDBASELINE) -f $(BASELINE) -o $(UPDIR)


$(TGTLIST):	
		git clean -dfx $(BLDDIR)
		@set -x;export $(RSTATE_EXPORTS) $(COMMON_EXPORTS); waf configure --vsn "$(CXC_REV)" --dvsn "$(DCXC_REV)" --hwmb "$(HWMB_DIR)" --prd "$@" --out "$(BLDDIR)" --prefix="" --tgt="$(TGT)" --lib="$(LIB)"
		@set -x;export $(RSTATE_EXPORTS) $(COMMON_EXPORTS); waf --vsn "$(CXC_REV)" --dvsn "$(DCXC_REV)" -j $(PARALLEL_BUILDS)


prep_com:
		+cd $(TOPDIR)/COM && make updateCacheLinks


prep_saf:
		+cd $(TOPDIR)/SAF && make updateCacheLinks


prep_otp:
		+cd $(TOPDIR)/OTP && make updateCacheLinks


$(BASELINE):	
		cp -f $(BLS) $@
		cacheManager.sh -i "70 71 73 72 74 75 77 $(HWMB_ID) 109 111 113 114 124 140" -f $@ -p $(RCSMWCACHE)
		cacheManager.sh -i "$(EE_IF_ID_LIST_G2EE) $(EE_IF_ID_LIST_SIM32) $(EE_IF_ID_LIST_VRCS64) $(EE_SIM32_CXP_ID) 115 116 120 133" -f $@ -p $(RCSMWCACHE) $(OPT_EE_CLEVEL)

prepare:	clean $(OSEMAIN) $(PROD_FILES) $(XML_SKEL_FILES) prep_com prep_saf prep_otp
		-@$(RM) -rf $(UPDIR) > /dev/null 2>&1
		@test -d $(UPDIR) || mkdir $(UPDIR)

%_product.py:	FORCE
		@if [ "$@" == "$(VRCS64_PROD_FILE)" ]; \
		then \
		  dirlist="$(VSRCDIRS):$(TSTSRCDIRS)"; \
		else \
		  dirlist="$(SRCDIRS):$(TSTSRCDIRS)"; \
		fi; \
		createProdPy.sh -a "$${dirlist}" -i $(BASELINE) -o $@ -u "$(UPID)"
		@echo " ...[$@] created successfully!"

%.xml_tmpl:	FORCE
		createXmlSkel.sh $(RSTATETYPE) -i $(BASELINE) -F "$(FRAMEWORK_NAME):$(FRAMEWORK_NO):$(FRAMEWORK_REV)" -S "$(SYSTEM_NAME):$(SYSTEM_NO):$(SYSTEM_REV)" -o $@
		@echo "Checking UP XML syntax [$@]..."
		@set -x;case "$(@F)" in \
		  *up.xml_tmpl)	xmllint  --schema SWM/schema/SwConfig.xsd --noout $@;; \
		  *)		xmllint  --schema SWM/schema/CxpInfo.xsd --noout $@;; \
		esac


$(OSEMAIN):		$(BASELINE)
			osemain=`echo $(wildcard $(shell ci_data.sh -f $(BASELINE) -a read -i $(EE_SDK_ID_G2EE) -p 13)/sysroots/*/usr/share/liblits/$(@F)) | head -1`; \
			cp -f $${osemain} $@


deploy:
		deploy.sh -i "410:3" -f $(BASELINE) -p $(UPDIR) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c $(DEPLOYCL)


deployALLCXS:
		dci_gen_ci_data_tab.sh $(BASELINE) > /dev/null
		deploy.sh -i $(ALL_ID_LIST) -f $(BASELINE) -p $(notdir $(BASELINE)) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c $(DEPLOYCL) -d
		deploy.sh -i $(ALL_ID_LIST) -f $(BASELINE) -p $(notdir $(BASELINE:%.txt=%.html)) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c $(DEPLOYCL)


deliver:
		deliver.sh -a $(ALL_ID_LIST) -r $(RAN_BLID) -f $(FLOWCONTEXT)  -i $(BASELINE)  -c $(DELIVERCL) -d


.PHONY:		package full deploy $(OSEMAIN) $(BASELINE)

.NOTPARALLEL:	prepare deploy deployALLCXS $(BASELINE)

FORCE:

clean:		FORCE
		-$(RM) -rf $(TOPDIR)/build-* > /dev/null 2>&1
		git clean -dfx $(OUTDIR)


# **********************************************************************

# Short description:
# Initiate environment and build all or individual flavours of OTP
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
# A          2017-03-07   etxbjca     Created
#
# **********************************************************************
#

include $(TOPDIR)/Makefile

#******************************************************************************
#   Define macros.
#

# SAF Macros
OTP_ID :=			601
SAFC_ID :=			603
SAFS_ID :=			604
SAF_TOP :=			$(CURDIR)
OTP_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 15)
OTP_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 14)
OTP_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 7)
SAFC_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFC_ID) -p 15)
SAFC_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFC_ID) -p 14)
SAFC_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFC_ID) -p 7)
SAFS_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFS_ID) -p 15)
SAFS_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFS_ID) -p 14)
SAFS_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFS_ID) -p 7)

BASEDIR :=			$(SAF_TOP)/safbase
OTP_MAIN =			$(BASEDIR)/$(notdir $(OTP_REPO))
SAFC_MAIN =			$(BASEDIR)/safc
SAFS_MAIN =			$(BASEDIR)/safs
SAFC_VSN_FILE =			$(SAF_TOP)/csrc/safc.version
SAFS_VSN_FILE =			$(SAF_TOP)/esrc/safs.version
SAF_BLD_DIR =			

# PROTOBUF-C Dynamic Data
PROTOBUFC_ID :=        		611
PROTOBUFC_ROOT :=       	$(shell ci_data.sh -f $(BLS) -a read -i $(PROTOBUFC_ID) -p 13)

# GPB Dynamic Data
GPB_ID :=			612
GPB_ROOT :=       		$(shell ci_data.sh -f $(BLS) -a read -i $(GPB_ID) -p 13)

# OTP Data
WITHOUT_APP :=			cosEvent \
				cosEventDomain \
				cosFileTransfer \
				cosNotification \
				cosProperty \
				cosTime \
				cosTransactions \
				dialyzer \
				diameter \
				erl_docgen \
				hipe \
				jinterface \
				megaco \
				orber \
				reltool \
				typer \
				wx \
				common_test \
				debugger \
				edoc \
				eunit \
				gs \
				ic  \
				parsetools \
				percept \
				safe \
				asn1 \
				compiler \
				crypto \
				eldap \
				erl_interface \
				erts \
				et \
				inets \
				kernel \
				mnesia \
				observer \
				os_mon \
				otp_mibs \
				public_key \
				runtime_tools \
				sasl \
				snmp \
				ssh \
				ssl \
				stdlib \
				syntax_tools \
				tools \
				xmerl

SAF_EXPORTS =			\
				ERL_TOP=$(OTP_MAIN) \
				V=1 \
				GPB_ROOT=$(GPB_ROOT) \
				PROTOBUFC_LIBDIR=$(PROTOBUFC_DIR)/lib \
				PROTOBUFC_INCDIR=$(PROTOBUFC_DIR)/include \
				SAF_SERVER=$(SAFS_MAIN) \
				TARGET=$(TARGET_PREFIX:%-=%)

CONFIGURE_FLAGS +=		--disable-sctp \
				--disable-hipe \
				$(WITHOUT_APP:%=--without-%)

#******************************************************************************
#   Define conditional macros.
#
%_msrcs :	ARCH_CPPFLAGS =		

%_msrcs : 	ARCH_LDFLAGS +=		\
					-Wno-narrowing \
					-Werror=strict-aliasing \
					-Werror=narrowing \
					-Wno-sizeof-pointer-memaccess \
					-Wno-strict-aliasing

%_msrcs :	PROTOBUFC_DIR =		$(PROTOBUFC_ROOT)/msrcs/tgt_arm-wr6


%_vrcs64 :	ARCH_CPPFLAGS =		

%_vrcs64 :	PROTOBUFC_DIR =		$(PROTOBUFC_ROOT)/vrcs64/tgt_x86_64


%_sim32 :	ARCH_CPPFLAGS =		

%_sim32 :	PROTOBUFC_DIR =		$(PROTOBUFC_ROOT)/sim32/tgt_i686


#******************************************************************************
#   Define targets.
#

default:	build_safs build_safc_msrcs build_safc_sim32 build_safc_vrcs64

build:		prep default

prep:		updateCacheLinks clone 

clone:		clone_otp clone_safs clone_safc


build_safs:
		ExecProjName RCSDE "RCSMW/1.0" "module add autoconf/2.69 protobuf msrcs && $(MAKE) safs_msrcs"

build_safc_sim:
		ExecProjName RCSDE "RCSMW/1.0" "module add autoconf/2.69 protobuf rcssim && $(MAKE) safc_sim"

build_safc_sim32:
		ExecProjName RCSDE "RCSMW/1.0" "module add autoconf/2.69 protobuf sim32 && $(MAKE) safc_sim32"

build_safc_vrcs64:
		ExecProjName RCSDE "RCSMW/1.0" "module add autoconf/2.69 protobuf vrcs64 && $(MAKE) safc_vrcs64"

build_safc_msrcs:
		ExecProjName RCSDE "RCSMW/1.0" "module add autoconf/2.69 protobuf msrcs && $(MAKE) safc_msrcs"

import:		prep
		install -p -D $(SAFC_MAIN)/c_src/*.[chv]* $(SAFC_MAIN)/include/* csrc
		install -p -D $(BASEDIR)/out/src/*.[ehv]* esrc

#
# Note! SAF is using OTP native build support and need by that a local, configured OTP source snapshot/setup 
#

safs_%:		
		@printf "\nBuilding SAFS [$@],please wait...\n"
		@date
		export $(SAF_EXPORTS); cd $(OTP_MAIN) && git clean -dfx ; ./otp_build autoconf; ./configure $(CONFIGURE_FLAGS)
		export $(SAF_EXPORTS); cd $(SAFS_MAIN) && git clean -dfx ; make
		export $(SAF_EXPORTS); cd $(SAFS_MAIN) && make RELSYSDIR=$(BASEDIR)/out release
safc_%:		
		@printf "\nBuilding SAFC [$@],please wait...\n"
		@date
		export $(SAF_EXPORTS); cd $(OTP_MAIN) && git clean -dfx ; ./otp_build autoconf; ./configure $(CONFIGURE_FLAGS)
		export $(SAF_EXPORTS); cd $(SAFC_MAIN) && git clean -dfx ; make
		export $(SAF_EXPORTS); cd $(SAFC_MAIN) && make RELSYSDIR=$(BASEDIR)/out/safc/$(TGT) release

updateCacheLinks:
		@printf "\nUpdating binary cache links...\n"
		@date
		-cd $(SAF_TOP) && rm -f include
		-$(RM) $(SAF_TOP)/tgt_*
		ln -s $(wildcard $(PROTOBUFC_ROOT:%=%)/*/tgt_*) $(SAF_TOP)

clone_otp:	
		@printf "\nFetching OTP source, please wait...\n"
		@date
		-@$(RM) -rf $(OTP_MAIN) > /dev/null 2>&1
		@test -d $(BASEDIR) || mkdir -p $(BASEDIR)
		@$(GERRIT_CFG); \
		cd $(BASEDIR) && git clone $(OTP_REPO)
		cd $(OTP_MAIN) && git checkout $(OTP_VER)

clone_safs:	
		@printf "\nFetching SAFS source, please wait...\n"
		@date
		-@$(RM) -rf $(SAFS_MAIN) > /dev/null 2>&1
		@$(GERRIT_CFG); \
		cd $(BASEDIR) && git clone $(SAFS_REPO) safs
		cd $(SAFS_MAIN) && git checkout $(SAFS_VER)
		cd $(SAFS_MAIN) && echo "`git rev-parse HEAD` $(notdir $(SAFS_MAIN))" > $(SAFS_VSN_FILE)

clone_safc:	
		@printf "\nFetching SAFC source, please wait...\n"
		@date
		-@$(RM) -rf $(SAFC_MAIN) > /dev/null 2>&1
		@$(GERRIT_CFG); \
		cd $(BASEDIR) && git clone $(SAFC_REPO) safc
		cd $(SAFC_MAIN) && git checkout $(SAFC_VER)
		cd $(SAFC_MAIN) && echo "`git rev-parse HEAD` $(notdir $(SAFC_MAIN))" > $(SAFC_VSN_FILE)

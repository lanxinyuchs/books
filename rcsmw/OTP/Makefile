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

# OTP Macros
OTP_TOP :=			$(CURDIR)

OTP_ID :=			601
SAFE_ID :=			602
OTP_SRC_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 16 | awk -F: '{print $$ 1}')
OTP_SRC_REV :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 16 | awk -F: '{print $$ 2}')
OTP_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 15)
OTP_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 14)
OTP_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_ID) -p 7)
SAFE_REPO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFE_ID) -p 15)
SAFE_VER :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFE_ID) -p 14)
SAFE_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAFE_ID) -p 7)

OTP_LM_ID :=			130
OTP_LM_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_LM_ID) -p 2 | tr '/' '_')
OTP_LM_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_LM_ID) -p 6)
OTP_LM_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(OTP_LM_ID) -p 7)
OTP_LM_CACHE :=			$(RCSMWCACHE)/$(OTP_LM_NAME)
OTP_LM =			$(OTP_LM_CACHE)/$(OTP_LM_REV)/$(OTP_LM_NAME)_$(OTP_LM_NO)

SAF_IF_ID :=			303		
SAF_IF_NO :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAF_IF_ID) -p 2 | tr '/' '_')
SAF_IF_REV =			$(shell ci_data.sh -f $(BLS) -a read -i $(SAF_IF_ID) -p 6)
SAF_IF_NAME :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAF_IF_ID) -p 7)
SAF_IF_CACHE :=			$(shell ci_data.sh -f $(BLS) -a read -i $(SAF_IF_ID) -p 13)

WITHOUT_APP_NATIVE :=		

WITHOUT_APP_COMMON :=		cosEvent \
				cosEventDomain \
				cosFileTransfer \
				cosNotification \
				cosProperty \
				cosTime \
				cosTransactions \
				dialyzer \
				diameter \
				edoc \
				eunit \
				erl_docgen \
				gs \
				hipe \
				ic \
				jinterface \
				megaco \
				orber \
				parsetools \
				percept \
				reltool \
				typer \
				wx

WITHOUT_APP_SIM_DUMMY :=	$(WITHOUT_APP_COMMON)

WITHOUT_APP_SIM :=		$(WITHOUT_APP_COMMON)

WITHOUT_APP_SIM32_DUMMY :=	$(WITHOUT_APP_COMMON) \
				common_test \
				debugger

WITHOUT_APP_SIM32 :=		$(WITHOUT_APP_COMMON) \
				common_test \
				debugger

WITHOUT_APP_MSRCS :=		$(WITHOUT_APP_COMMON) \
				common_test \
				debugger

WITHOUT_APP_MSRCS_DUMMY :=	$(WITHOUT_APP_MSRCS) \
				common_test \
				debugger

WITHOUT_APP_VRCS64 :=		$(WITHOUT_APP_MSRCS) \
				common_test \
				debugger

WITHOUT_APP_VRCS64_DUMMY :=	$(WITHOUT_APP_MSRCS) \
				common_test \
				debugger

BASEDIR :=			$(OTP_TOP)/otpbase
OTP_VSN_FILE =			$(OTP_MAIN)/$(notdir $(OTP_REPO)).version
SAFE_VSN_FILE =			$(SAFE_MAIN)/$(notdir $(SAFE_REPO)).version

OTP_EXPORTS =			\
				V=1 \
				ERL_TOP=$(OTP_MAIN) \
				PATH=$(BASEDIR)/bootstrap/$(notdir $(OTP_REPO))/bootstrap/bin:$(PATH) \
				CC="$(CC)" \
				CXX="$(CXX)" \
				CFLAGS="$(CFLAGS)" \
				CPPFLAGS="$(CPPFLAGS)" \
				LDFLAGS="$(LDFLAGS)"

SAF_DIR =			$(SAF_IF_CACHE)/$(TGT)

# Used modules
OPENSSL_MOD :=			openssl/1.0.1g
AUTOCONF_MOD :=			autoconf/2.69
ZLIB_MOD :=			zlib/1.2.8
NCURSES_MOD :=			ncurses/5.9
GCC_MOD :=			gcc/4.2.4

OPENSSL =			$(dir $(shell . /app/modules/0/init/sh && module show $(OPENSSL_MOD) -t 2>&1 | grep 'LD_LIBRARY_PATH' | cut -d ' ' -f3))

#******************************************************************************
#   Define conditional macros.
#

# bootstrap
%_bootstrap : 	CC =			gcc
%_bootstrap : 	CXX =			g++
%_bootstrap : 	CONFIGURE_FLAGS =	\
					--enable-bootstrap-only
%_bootstrap :	OTP_MAIN =		$(BASEDIR)/bootstrap/$(notdir $(OTP_REPO))
%_bootstrap :	SAFE_MAIN =		$(BASEDIR)/bootstrap/$(notdir $(SAFE_REPO))

# native
%_native : 	CC =			gcc
%_native : 	CXX =			g++
%_native : 	CONFIGURE_FLAGS =	\
					--disable-sctp \
					--with-ssl-zlib=$(zlibPATH) \
					--with-ssl-rpath=no \
					--with-ssl=$(OPENSSL) \
					$(WITHOUT_APP_NATIVE:%=--without-%)

%_native : 	TGT = 			$(shell arch)
%_native :	OTP_MAIN =		$(BASEDIR)/native/$(notdir $(OTP_REPO))
%_native :	SAFE_MAIN =		$(BASEDIR)/native/$(notdir $(SAFE_REPO))
%_native :	SAF_DIR =		$(SAF_IF_CACHE)/tgt_i686

# msrcs/msrcs_dummy
%_msrcs_dummy : CONFIGURE_FLAGS +=	--disable-sctp \
					--disable-hipe \
					--disable-amf \
					--disable-clm \
					--disable-smf \
					$(WITHOUT_APP_MSRCS_DUMMY:%=--without-%)

%_msrcs_dummy :	OTP_MAIN =		$(BASEDIR)/msrcs_dummy/$(notdir $(OTP_REPO))
%_msrcs_dummy :	SAFE_MAIN =		$(BASEDIR)/msrcs_dummy/$(notdir $(SAFE_REPO))

%_msrcs : 	CONFIGURE_FLAGS +=	--disable-sctp \
					--disable-hipe \
					$(WITHOUT_APP_MSRCS:%=--without-%)

%_msrcs :	OTP_MAIN =		$(BASEDIR)/msrcs/$(notdir $(OTP_REPO))
%_msrcs :	SAFE_MAIN =		$(BASEDIR)/msrcs/$(notdir $(SAFE_REPO))

# vrcs64/vrcs64_dummy
%_vrcs64_dummy : CONFIGURE_FLAGS +=	--disable-sctp \
					--disable-hipe \
					--disable-amf \
					--disable-clm \
					--disable-smf \
					$(WITHOUT_APP_VRCS64_DUMMY:%=--without-%)

%_vrcs64_dummy : OTP_MAIN =		$(BASEDIR)/vrcs64_dummy/$(notdir $(OTP_REPO))
%_vrcs64_dummy : SAFE_MAIN =		$(BASEDIR)/vrcs64_dummy/$(notdir $(SAFE_REPO))

%_vrcs64 :	CONFIGURE_FLAGS +=	--disable-sctp \
					--disable-hipe \
					$(WITHOUT_APP_VRCS64:%=--without-%)

%_vrcs64 :	OTP_MAIN =		$(BASEDIR)/vrcs64/$(notdir $(OTP_REPO))
%_vrcs64 :	SAFE_MAIN =		$(BASEDIR)/vrcs64/$(notdir $(SAFE_REPO))


# sim32/sim32_dummy
%_sim32_dummy :	CONFIGURE_FLAGS +=	--disable-sctp \
					--disable-hipe \
					--disable-amf \
					--disable-clm \
					--disable-smf \
					$(WITHOUT_APP_SIM32_DUMMY:%=--without-%)

%_sim32_dummy :	OTP_MAIN =		$(BASEDIR)/sim32_dummy/$(notdir $(OTP_REPO))

%_sim32_dummy :	SAFE_MAIN =		$(BASEDIR)/sim32_dummy/$(notdir $(SAFE_REPO))


%_sim32 :	CONFIGURE_FLAGS +=	--disable-sctp \
					--disable-hipe \
					$(WITHOUT_APP_SIM32:%=--without-%)

%_sim32  :	OTP_MAIN =		$(BASEDIR)/sim32/$(notdir $(OTP_REPO))

%_sim32 :	SAFE_MAIN =		$(BASEDIR)/sim32/$(notdir $(SAFE_REPO))

#******************************************************************************
#   Define targets.
#


default:	build_otp_msrcs build_otp_sim32 build_otp_vrcs64 build_otp_msrcs_dummy build_otp_sim32_dummy build_otp_vrcs64_dummy build_otp_native

clone:		clone_bootstrap clone_msrcs clone_native clone_sim32 clone_vrcs64 clone_msrcs_dummy clone_sim32_dummy clone_vrcs64_dummy

build:		$(BASELINE) bootstrap genRevs default 

release:	build otp_release updateCacheLinks

bootstrap:	build_otp_bootstrap


build_otp_bootstrap: clone_bootstrap
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP gcc; module add $(AUTOCONF_MOD) $(ZLIB_MOD) $(OPENSSL_MOD) $(NCURSES_MOD) $(GCC_MOD) && $(MAKE) otp_bootstrap"

build_otp_native: clone_native
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP gcc; module add $(AUTOCONF_MOD) $(ZLIB_MOD) $(OPENSSL_MOD) $(NCURSES_MOD) $(GCC_MOD) && $(MAKE) otp_native"

build_otp_sim32: clone_sim32
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add $(AUTOCONF_MOD) sim32 && $(MAKE) otp_sim32"

build_otp_sim32_dummy: clone_sim32_dummy
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add $(AUTOCONF_MOD) sim32 && $(MAKE) otp_sim32_dummy"

build_otp_vrcs64: clone_vrcs64
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add $(AUTOCONF_MOD) vrcs64 && $(MAKE) otp_vrcs64"

build_otp_vrcs64_dummy: clone_vrcs64_dummy
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add $(AUTOCONF_MOD) vrcs64 && $(MAKE) otp_vrcs64_dummy"

build_otp_msrcs: clone_msrcs
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add $(AUTOCONF_MOD) msrcs && $(MAKE) otp_msrcs"

build_otp_msrcs_dummy: clone_msrcs_dummy
		ExecProjName RCSDE "RCSMW/1.0" "module rm OTP ; module add $(AUTOCONF_MOD) msrcs && $(MAKE) otp_msrcs_dummy"

otp_release:	
		@printf "\nReleasing OTP to $(BLS)/Nexus/InfoBank...\n"
		@date
		chmodAll.sh -p $(OTP_LM) 
		ci_data.sh -f $(BLS) -a write -i $(OTP_ID) -p 13 -c $(OTP_LM)/native
		deploy.sh -b $(BUDITCFG) -i $(OTP_LM_ID) -f $(BLS) -e -p $(OTP_LM_CACHE) -u $(NEXUS_BASE_URL) -C $(NEXUS_CONFIG) -c 0

otp_bootstrap:	clone_bootstrap
		@printf "\nBuilding OTP [$@],please wait...\n"
		@date
		cd $(OTP_MAIN); \
		export ERL_TOP=$(OTP_MAIN); \
		./otp_build autoconf; \
		./configure $(CONFIGURE_FLAGS); \
		make -j $(PARALLEL_BUILDS)

otp_%:		
		@printf "\nBuilding OTP [$@],please wait...\n"
		@date
		-$(RM) -r $(dir $(OTP_MAIN))ssl $(OTP_MAIN)/lib/safe > /dev/null 2>&1
		-@set -x; \
		if echo "$@" | egrep '_dummy' > /dev/null 2>&1; \
		then \
		  cd $(OTP_MAIN)/lib && ln -s ../../$(notdir $(SAFE_MAIN))/safe; \
		  export SAF_DIR=$(SAF_DIR); \
		fi; \
		export $(OTP_EXPORTS); \
		if echo "$@" | egrep 'sim|msrcs|vrcs64' > /dev/null 2>&1; \
		then \
		  export erl_xcomp_sysroot=$(SDKTARGETSYSROOT); \
		fi; \
		cd $(OTP_MAIN) && ./otp_build autoconf; ./configure $(CONFIGURE_FLAGS) --prefix=$(OTP_LM)/$(@:otp_%=%)/$(TGT); \
		cd $(OTP_MAIN) && make -j $(PARALLEL_BUILDS)
		export $(OTP_EXPORTS); cd $(OTP_MAIN) && make RELEASE_ROOT=$(OTP_LM)/$(@:otp_%=%)/$(TGT) release
		export $(OTP_EXPORTS); cd $(OTP_LM)/$(@:otp_%=%)/$(TGT) && ./Install -cross -sasl $(OTP_LM)/$(@:otp_%=%)/$(TGT)
		if echo "$@" | egrep 'sim|msrcs|vrcs64' > /dev/null 2>&1; \
		then \
		  cp -af src/erl  $(OTP_LM)/$(@:otp_%=%)/$(TGT)/bin; \
		  cp -af src/erl  $(OTP_LM)/$(@:otp_%=%)/$(TGT)/erts-*/bin; \
		fi
		-test -d $(OTP_TOP)/patches && ( cd $(OTP_LM)/$(@:otp_%=%)/$(TGT); cp -af $(OTP_TOP)/patches/* ./ )


genRevs:	
		@printf "\nRegistrate new R-states...\n"
		@date
		@set -x;for ID in $(OTP_LM_ID); \
		do \
		  NO=`ci_data.sh -f $(BLS) -a read -i $${ID} -p 2`; \
		  REV=`$(NEXTREV) -r $(RSTATE_BASE) -p $${NO}`; \
		  ci_data.sh -f $(BASELINE) -a write -i $${ID} -p 6 -c $${REV}; \
		  ci_data.sh -f $(BLS) -a write -i $${ID} -p 6 -c $${REV}; \
		done


updateCacheLinks: 
		@printf "\nUpdating binary cache links...\n"
		@date
		-$(RM) $(wildcard $(OTP_TOP)/tgt_*) $(wildcard $(TOPDIR)/test/OTP/tgt_*)
		ln -s $(filter-out $(wildcard $(OTP_LM)/*_dummy/tgt_*),$(wildcard $(OTP_LM)/*/tgt_*)) $(OTP_TOP)
		ln -s $(filter $(wildcard $(OTP_LM)/*_dummy/tgt_*),$(wildcard $(OTP_LM)/*/tgt_*)) $(TOPDIR)/test/OTP


clone_%:	
		@printf "\nFetching OTP source [$@], please wait...\n"
		@date
		@$(GERRIT_CFG)
		@$(RM) -rf $(dir $(OTP_MAIN)) > /dev/null 2>&1
		@test -d $(dir $(OTP_MAIN)) || mkdir -p $(dir $(OTP_MAIN))
		cd $(dir $(OTP_MAIN)) && git clone $(OTP_REPO)
		cd $(OTP_MAIN) && git checkout $(OTP_VER) && git submodule update --init
		cd $(OTP_MAIN) && echo "`git rev-parse HEAD` $(notdir $(OTP_MAIN))" > $(OTP_VSN_FILE)
		cd $(dir $(OTP_MAIN)) && git clone $(SAFE_REPO)
		cd $(SAFE_MAIN) && git checkout $(SAFE_VER) && git submodule update --init
		cd $(SAFE_MAIN) && echo "`git rev-parse HEAD` $(notdir $(SAFE_MAIN))" > $(SAFE_VSN_FILE)

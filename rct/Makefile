# ----------------------------------------------------------------------
# (C) 2010-2016 Ericsson
#
# Makefile for RCS
# ----------------------------------------------------------------------
BOARD = null
REPEAT = 1
SECURE ?= no
FPGATEST ?= no
A10 ?= no
MEM_CFG ?= no
separator = -
RCTCASE ?=
CHK_INSTALL ?= true
EXEC_TESTS ?= false

RCT_PROPERTIES= properties/properties.cfg
REPO_ROOT=$(shell git rev-parse --show-toplevel)
BASELINE=""

# ----------------------------------------------
ifeq ($(findstring tcu4,$(BOARDS)),tcu4)
CXS_SFX = 5
FPGA_CXP = KATLA_CXP*
FPGA_FILE = katla
FPGA_CXC = CXC1737748
else ifeq ($(findstring tcu0,$(BOARDS)),tcu0)
CXS_SFX = 4
FPGA_CXP = TAIPAN_CXP*
FPGA_FILE = taipan
FPGA_CXC = CXC1735838
else
CXS_SFX = 5
FPGA_FILE = cobra
ifeq ($(A10),yes)
FPGA_CXP = COBRA_CXP102188
FPGA_CXC = CXC1738073
else
FPGA_CXP = COBRA_CXP102171
FPGA_CXC = CXC1735837
endif
endif

# Path to linux kernel in rcs-ee cxc
EE_NEW_KERNEL_PATH = RCSEE$(CXC_SFX)_CXC1737262_$(CXC_SFX)/rcsee$(CXC_SFX)/kernel/linux-arm.img
EE_NEW_BOOTFS_PATH = RCSEE$(CXC_SFX)_CXC1737262_$(CXC_SFX)/rcsee$(CXC_SFX)/bootfs/bootfs-arm.cpio.gz
FACTORY_LOADER_URL = https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/FL_LXA1191629/scripts/factoryloader_script.sh
FLTOOL_URL = https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/FL_LXA1191629/scripts/fltool
#----------------------------------------------

tcu0% : BOARD = ${shell echo $@ | grep -o 'tcu[0-9]\{3,\}'}
tcu0% : BOARD_TYPE = tcu
tcu0% : RCT_GROUP = tcu03
tcu0% : CXC_SFX = 5

tcu4% : BOARD = ${shell echo $@ | grep -o 'tcu[0-9]\{3,\}'}
tcu4% : BOARD_TYPE = tcu
tcu4% : RCT_GROUP = tcu04
tcu4% : CXC_SFX = 5

dus5% : BOARD = ${shell echo $@ | grep -o 'dus[0-9]\{3,\}'}
dus5% : BOARD_TYPE = dusX2
dus5% : RCT_GROUP = dus52
dus5% : CXC_SFX = 4

dus3% : BOARD = ${shell echo $@ | grep -o 'dus[0-9]\{3,\}'}
dus3% : BOARD_TYPE = dusX2
dus3% : RCT_GROUP = dus52
dus3% : CXC_SFX = 4

TOP=$(CURDIR)

# -- For rcs-aux squashfs -----
MODSETUP :=	. /usr/share/modules/init/sh; \
		module add /app/rbs/wrtools/modules/wrhosttools/Default
# ----------------------------

## New EE install
CREATESIG=/app/rbs/cs/V1/amd64_linux26/bin/create_sig.sh
######

$(addsuffix -cluster, $(BOARDS)):CCVIEWPATH=/repo/${USER}/view/${CCVIEW}.vws
$(addsuffix -cluster, $(BOARDS)):CCVIEW=${USER}_${HOST}_${BOARDS}_cluster_jenkins_rcs
$(addsuffix -cluster, $(BOARDS)):LOGDIR=/proj/rcs-tmp/stps/cluster/logs/$(USER)/$(shell date +"%Y-%m-%d")/
$(addsuffix -cluster, $(BOARDS)):STP=cluster
$(addsuffix -cluster, $(BOARDS)):SUITEDIR=suites
$(addsuffix -cluster, $(BOARDS)):NODES=$(subst $(separator), ,$(BOARDS))
$(addsuffix -cluster, $(BOARDS)):
	mkdir -p /repo/${USER}/view
	cleartool lsview ${CCVIEW} > /dev/null  2>&1 || \
		cleartool mkview -nsh -tag ${CCVIEW} -host `uname -n` \
			-hpath ${CCVIEWPATH} -gpath ${CCVIEWPATH} ${CCVIEWPATH}

	# If there is a running view_server proc, try to end it and sleep 20 seconds.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" && cleartool endview -server ${CCVIEW} && sleep 20 || true
	# If view_server proc is still running, try to kill it and sleep 20 seconds.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" && pkill -9 -u ${USER} -f "^view_server ${CCVIEWPATH}" && sleep 20 || true
	# Final check to see if view_server still exists.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" || true

	ExecProjName -v ${CCVIEW} RCSDE "1.0" "16B"
	ExecProjName -v ${CCVIEW} RCSDE "1.0" "\$$RCT_TOP/test/bin/rct_add_stp.sh -stp ${BOARDS} -nodes ${NODES}"
	mkdir -p $(LOGDIR)
	-ExecProjName -v ${CCVIEW} RCSDE "1.0" "\$$RCT_TOP/test/bin/rct_run.sh -stp ${BOARDS}  -dir ${SUITEDIR} -suite ${STP}_SUITE \
	-config ${RCT_PROPERTIES} -case -group ${STP} -noshell -logdir $(LOGDIR) -ct_hooks cth_surefire '[{path,\"${TOP}/report.xml\"}]' -pa $(CURDIR)/patches/"

	# Print out view_server proc  after test execution.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" || true
	# End view_server proc, wait 5 seconds to complete the operation.
	cleartool endview -server ${CCVIEW} && sleep 5
	# verify if view_server proc is ended or not.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" || true


$(addsuffix -lmbench, $(BOARDS)):
	rm -f lmbench-results.tar.gz
	rm -rf results
	rm -rf plots
	make ${BOARD}-rct F=- RCT_SUITE=lmbench_SUITE RCT_GROUP=$(RCT_GROUP) SUITEDIR=suites
	tar xvf lmbench-results.tar.gz
	mkdir plots
	for file in results/*.log ; do \
	 echo Fixing plots/$$(basename $${file%.*}.plot) ; \
	 if [ -s "$$file" ] ; then \
	  echo -n YVALUE= > plots/$$(basename $${file%.*}.plot) ; \
	  awk '{T+=$$1} END { print T/NR }' $${file} >> plots/$$(basename $${file%.*}.plot) ; \
         else \
	  echo "File is empty" ; \
	 fi ; \
	done


$(addsuffix -specificsuitetest, $(BOARDS) $(SUITE)):
	make ${BOARD}-rct F=- RCT_SUITE=$(SUITE) RCT_GROUP=$(RCT_GROUP) SUITEDIR=suites

$(addsuffix -specifictest, $(BOARDS) $(SUITE)):
	make ${BOARD}-rct F=- RCT_SUITE=$(SUITE)_SUITE RCT_GROUP=$(RCT_GROUP) SUITEDIR=suites


$(addsuffix -blackcxsinstall, $(BOARDS) $(BLACKCXS) $(RCTCASE) $(MEM_CFG) $(CHK_INSTALL)):
# Script for installing latest black cxs from new EE Jenkins machine for prewash of LSI deliveries
	@if [ -z "$(BOARD)" ]; then \
	echo "Error: BOARD is unset when building $@"; \
	false; \
	fi

	if [ -d "/proj/rcs-tmp/tftpboot/$(BOARD)" ]; then \
	cd /proj/rcs-tmp/tftpboot/$(BOARD) && rm -f *; \
	fi
	mkdir -p /proj/rcs-tmp/tftpboot/$(BOARD)

	/env/RCSDE/bin/rcstprep.sh $(BOARD) $(BLACKCXS)
	make ${BOARD}-rct RCT_SUITE=install_dus_SUITE RCT_CASE=$(RCTCASE) RCT_GROUP="" SUITEDIR="suites/install"

	if [ $(CHK_INSTALL) = "false" ]; then \
	echo "Skipping check after install suite";\
	elif [ $(MEM_CFG) = "fullup" ]; then \
	make ${BOARD}-rct RCT_SUITE=check_after_install_SUITE RCT_GROUP="" SUITEDIR="suites/install" RCT_PROPERTIES="properties/$(BOARD_TYPE)_full_up_mem_config.cfg" ; \
	else \
	make ${BOARD}-rct RCT_SUITE=check_after_install_SUITE RCT_GROUP="" SUITEDIR="suites/install" ; \
	fi ;

$(addsuffix -eeinstallfactorylxa, $(BOARDS) $(BLACKCXS) $(EXEC_TESTS) $(A10)):
# Script for installing latest factory cxs
	@if [ -z "$(BOARD)" ]; then \
	echo "Error: BOARD is unset when building $@"; \
	false;\
	fi
	if [ -d "/proj/rcs-tmp/tftpboot/$(BOARD)" ]; then \
	cd /proj/rcs-tmp/tftpboot/$(BOARD) && rm -f *; \
	fi
	mkdir -p /proj/rcs-tmp/tftpboot/$(BOARD)
	rm -rf pack
	mkdir -p pack/cxp/unpack

	# Get the version of FL for TCU03,TCU04 and DUS52/32 from RCS CI
	@$(eval LXA_VERSION:=$(shell cat $(REPO_ROOT)/baseline/factory_loader_version))
	@echo "Fetching LXA version $(LXA_VERSION)"
        # Unpack the factory LXA
	if [ $(RCT_GROUP) = tcu03 -o $(RCT_GROUP) = tcu04 ]; then \
	cd pack; wget -O FL_LXA1191629.zip https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/FL_LXA1191629/doc/19089/FL_LXA1191629.zip@@/${LXA_VERSION} ; \
	unzip FL_*.zip; \
	cd cxp;mv ../RCSEE-T_CXP*_[1-9].cxp ./ ;\
	fi;
	if [ $(RCT_GROUP) = dus52 ]; then \
	cd pack; wget -O FL_LXA1191629_DUS.zip https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/FL_LXA1191629/doc/19089/FL_LXA1191629_DUS.zip@@/${LXA_VERSION}; \
	unzip FL_*.zip; \
	cd cxp;mv ../RCSEE-DUS2_CXP*_[1-9].cxp ./ ;\
	fi;

	cd pack/cxp; chmod u+w RCSEE-*_CXP*_[1-9].cxp
	cd pack/cxp/unpack; tar -xf ../RCSEE-*_CXP*_[1-9].cxp
	cd pack/cxp/unpack; rm -rf RCSEE$(CXC_SFX)_CXC1737262_$(CXC_SFX)

	# Get the last Build from EE Jenkins to verify last EE build.
	cd pack/cxp/unpack; tar xf ../../../../delivery/RCSEE$(CXC_SFX)_CXC1737262_$(CXC_SFX).tar.gz
	cd pack/cxp/unpack; rm -rf RCSEE$(CXC_SFX)_CXC1737262_$(CXC_SFX)/dbg

	# Replace linux.img in LXA with the latest from RCS-EE (and remove signed header)
	cd pack/cxp/unpack; dd if=${EE_NEW_KERNEL_PATH} of=../../linux.img skip=1 bs=140

	# Strip & extract bootfs.cpio from RCS-EE, add a file and re-compress it.
	mkdir -p pack/bootfs/unpack
	cd pack/bootfs; dd if=../cxp/unpack/${EE_NEW_BOOTFS_PATH} of=bootfs.cpio.gz skip=1 bs=140
	cd pack/bootfs/unpack; gunzip -c ../bootfs.cpio.gz | cpio -id
	rm -f pack/bootfs/unpack/addons/bin/`basename ${FACTORY_LOADER_URL}`
	cd pack/bootfs/unpack; wget -O addons/bin/`basename ${FACTORY_LOADER_URL}` ${FACTORY_LOADER_URL}@@/${LXA_VERSION}; \
	wget -O addons/bin/`basename ${FLTOOL_URL}` ${FLTOOL_URL}@@/${LXA_VERSION};
	chmod 555 pack/bootfs/unpack/addons/bin/`basename ${FACTORY_LOADER_URL}`
	chmod 555 pack/bootfs/unpack/addons/bin/`basename ${FLTOOL_URL}`
	cd pack/bootfs/unpack; find . | cpio -o -H newc -R root:root | gzip -c > ../../bootfs.cpio.gz

	# Run create_sig to recreate the CXP containing the new CXC
	cd pack/cxp/unpack; $(CREATESIG) -f cxp*.xml -o ../RCSEE-*_CXP*_[1-9].cxp -x RCSEE$(CXC_SFX)_CXC1737262_$(CXC_SFX) .
	cd pack/cxp; mv RCSEE-*_CXP*_[1-9].cxp ..

	# Get version of NL
	@$(eval NL_VERSION:=$(shell cat $(REPO_ROOT)/baseline/network_loader_version))
	@echo "Fetching the Network loader version $(NL_VERSION)"
	cd pack; wget -O NL3_CXC1733874_3.tgz https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/OS_CRX901265/NL/NL3_CXC1733874_3/doc/19089/NL3_CXC1733874_3.tgz@@/${NL_VERSION}
	chmod 555 pack/NL3_CXC1733874_3.tgz
	mv pack/NL3_CXC1733874_3.tgz pack/NL.cxc

	cd pack;\cp -f *.* /proj/rcs-tmp/tftpboot/$(BOARD)/.

	#Extract the RCS CXS to fetch the FPGA CXP
	rm -rf fpga_pack
	mkdir -p fpga_pack/cxp/unpack
	if [ $(suffix $(BLACKCXS)) = ".zip" ]; then \
	cd fpga_pack/cxp; unzip $(BLACKCXS) ; \
	else \
	cd fpga_pack/cxp;tar xf $(BLACKCXS) ; \
	fi;
	cd fpga_pack/cxp; chmod 555 $(FPGA_CXP)_[1-9].cxp
	#Extract the FPGA CXP for rbf files
	cp fpga_pack/cxp/$(FPGA_CXP)_[1-9].cxp /proj/rcs-tmp/tftpboot/$(BOARD)/FPGA.cxp
	cd fpga_pack/cxp/unpack; tar xf ../$(FPGA_CXP)_[1-9].cxp
	cd fpga_pack/cxp/unpack; ar -x sw.ar
	$(MODSETUP); cd fpga_pack/cxp/unpack; unsquashfs sqfs.img
	chmod 555 fpga_pack/cxp/unpack/squashfs-root/$(FPGA_CXC)_[1-9]/bin/top_$(FPGA_FILE).periph.rbf
	chmod 755 /proj/rcs-tmp/tftpboot/$(BOARD)/*
	cp fpga_pack/cxp/unpack/squashfs-root/$(FPGA_CXC)_[1-9]/bin/top_$(FPGA_FILE).periph.rbf /proj/rcs-tmp/tftpboot/$(BOARD)/.

	rm -rf pack
	rm -rf fpga_pack

	make ${BOARD}-rct RCT_SUITE=install_factorylxa_SUITE RCT_GROUP="" SUITEDIR=suites

	# Execute factory tests if the switch EXEC_TESTS is true
	if [ $(EXEC_TESTS) = "true" ]; then \
	make ${BOARD}-rct RCT_SUITE=factory_SUITE RCT_GROUP=$(RCT_GROUP) SUITEDIR=suites ; \
	else \
	echo "No tests are requested."; \
	fi ;

$(addsuffix -installfactorylxa, $(BOARDS) $(BLACKCXS) $(EXEC_TESTS) $(A10)):
# Script for installing latest factory cxs
	@if [ -z "$(BOARD)" ]; then \
	echo "Error: BOARD is unset when building $@"; \
	false;\
	fi

	if [ -d "/proj/rcs-tmp/tftpboot/$(BOARD)" ]; then \
	cd /proj/rcs-tmp/tftpboot/$(BOARD) && rm -f *; \
	fi
	mkdir -p /proj/rcs-tmp/tftpboot/$(BOARD)
	rm -rf pack
	mkdir -p pack

	# Get the version of FL for TCU03 and DUS52/32 from RCS CI
	@$(eval LXA_VERSION:=$(shell cat $(REPO_ROOT)/baseline/factory_loader_version))
	@echo "Fetching LXA version $(LXA_VERSION)"

	if [ $(RCT_GROUP) = tcu03 -o $(RCT_GROUP) = tcu04 ]; then \
	cd pack; wget -O FL_LXA1191629.zip https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/FL_LXA1191629/doc/19089/FL_LXA1191629.zip@@/${LXA_VERSION} ; \
	fi;
	if [ $(RCT_GROUP) = dus52 ]; then \
	cd pack; wget -O FL_LXA1191629_DUS.zip https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/FL_LXA1191629/doc/19089/FL_LXA1191629_DUS.zip@@/${LXA_VERSION}; \
	fi;
	cd pack; unzip FL_*.zip

	# Get version of NL
	@$(eval NL_VERSION:=$(shell cat $(REPO_ROOT)/baseline/network_loader_version))
	@echo "Fetching the Network loader version $(NL_VERSION)"
	cd pack; wget -O NL3_CXC1733874_3.tgz https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/OS_CRX901265/NL/NL3_CXC1733874_3/doc/19089/NL3_CXC1733874_3.tgz@@/${NL_VERSION}
	chmod 555 pack/NL3_CXC1733874_3.tgz
	mv pack/NL3_CXC1733874_3.tgz pack/NL.cxc

	cd pack;\cp -f *.* /proj/rcs-tmp/tftpboot/$(BOARD)/.

	# Fetch the FPGA files from BLACKCXS
	rm -rf fpga_pack
	mkdir -p fpga_pack/cxp/unpack

	#Extract the RCS CXS to fetch the FPGA CXP
	if [ $(suffix $(BLACKCXS)) = ".zip" ]; then \
	echo "zip format detected" ; \
	cd fpga_pack/cxp; unzip $(BLACKCXS) ; \
	else \
	echo "cxs format detected" ; \
	cd fpga_pack/cxp; tar xf $(BLACKCXS) ; \
	fi;
	cd fpga_pack/cxp; chmod 555 $(FPGA_CXP)_[1-9].cxp
	#Extract the FPGA CXP for rbf files
	cp fpga_pack/cxp/$(FPGA_CXP)_[1-9].cxp /proj/rcs-tmp/tftpboot/$(BOARD)/FPGA.cxp
	cd fpga_pack/cxp/unpack; tar xf ../$(FPGA_CXP)_[1-9].cxp
	cd fpga_pack/cxp/unpack; ar -x sw.ar
	$(MODSETUP); cd fpga_pack/cxp/unpack; unsquashfs sqfs.img
	chmod 555 fpga_pack/cxp/unpack/squashfs-root/$(FPGA_CXC)_[1-9]/bin/top_$(FPGA_FILE).periph.rbf
	chmod 755 /proj/rcs-tmp/tftpboot/$(BOARD)/*
	cp fpga_pack/cxp/unpack/squashfs-root/$(FPGA_CXC)_[1-9]/bin/top_$(FPGA_FILE).periph.rbf /proj/rcs-tmp/tftpboot/$(BOARD)/.

	rm -rf pack
	rm -rf fpga_pack
	make ${BOARD}-rct RCT_SUITE=install_factorylxa_SUITE RCT_GROUP="" SUITEDIR=suites

	# Execute factory tests if the switch EXEC_TESTS is true
	if [ $(EXEC_TESTS) = "true" ]; then \
	make ${BOARD}-rct RCT_SUITE=factory_SUITE RCT_GROUP=$(RCT_GROUP) SUITEDIR=suites ; \
	else \
	echo "No tests are requested."; \
	fi ;

$(addsuffix -nlprepare, $(BOARDS) $(FPGATEST)):
# Script for preparing tftpdir stable WR6 cxs and calling nl verification suite
	@if [ -z "$(BOARD)" ]; then \
	echo "Error: BOARD is unset when building $@"; \
	false;\
	fi

	mkdir -p /proj/rcs-tmp/tftpboot/$(BOARD)
	mkdir -p pack

	# Recreate UP from the baseline

	echo "Recreate UP from the baseline"
	if [ $(FPGATEST) = no -a $(RCT_GROUP) = tcu03 ]; then \
	/env/rbsg2/bin/ncgen --zip --file $(REPO_ROOT)/baseline/tcu_baseline.xml;\
	fi;
	if [ $(FPGATEST) = no -a $(RCT_GROUP) = tcu04 ]; then \
	/env/rbsg2/bin/ncgen --zip --file $(REPO_ROOT)/baseline/tcu_baseline.xml;\
	fi;
	if [ $(FPGATEST) = no -a $(RCT_GROUP) = dus52 ]; then \
	/env/rbsg2/bin/ncgen --zip --file $(REPO_ROOT)/baseline/dusX2_baseline.xml;\
	fi;

	# Create a backup of FL.gz contents already available in tftpdir
	mkdir -p pack/tmp
	cd pack/tmp;cp -rf /proj/rcs-tmp/tftpboot/$(BOARD) .

	# Prepare the tftpdir with new CXS as selected above
	if [ $(FPGATEST) = yes ]; then \
	/env/RCSDE/bin/rcstprep.sh $(BOARD) `ls $(PWD)/delivery/EE_BLACK_*_fpgacl0.zip`;\
	else \
	/env/RCSDE/bin/rcstprep.sh $(BOARD) `ls $(PWD)/rct/EE_BLACK*.zip`;\
	fi;

	# Copy back the files backed up, to tftp dir. linux.img and bootfs gets updated with this FL.gz versions
	cd pack/tmp/$(BOARD);cp -f *.* /proj/rcs-tmp/tftpboot/$(BOARD)/.
	rm -rf ./pack
	make ${BOARD}-rct F=- RCT_SUITE=factory_nl_SUITE RCT_GROUP=$(RCT_GROUP) SUITEDIR=suites

	# Clean up the tftpdir
	if [ -d "/proj/rcs-tmp/tftpboot/$(BOARD)" ]; then \
	cd /proj/rcs-tmp/tftpboot/$(BOARD) && rm -f *; \
	fi

# Target to run Erlang based RCT framework for installing boards and running test suites.
# $F=- will ignore failures and $F= will abort in case of failure (used to stop subsequent test job if install fails).
$(addsuffix -rct, $(BOARDS)): CCVIEWPATH=/repo/${USER}/view/${CCVIEW}.vws
$(addsuffix -rct, $(BOARDS)): CCVIEW=${USER}_${HOST}_${BOARD}_localview2_jenkins_rcs
$(addsuffix -rct, $(BOARDS)): LOGDIR=/proj/rcs-tmp/stps/$(BOARD)/logs/$(USER)/$(shell date +"%Y-%m-%d")/
$(addsuffix -rct, $(BOARDS)):
	mkdir -p /repo/${USER}/view
	cleartool lsview ${CCVIEW} > /dev/null  2>&1 || \
		cleartool mkview -nsh -tag ${CCVIEW} -host `uname -n` \
			-hpath ${CCVIEWPATH} -gpath ${CCVIEWPATH} ${CCVIEWPATH}

	# If there is a running view_server proc, try to end it and sleep 20 seconds.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" && cleartool endview -server ${CCVIEW} && sleep 20 || true
	# If view_server proc is still running, try to kill it and sleep 20 seconds.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" && pkill -9 -u ${USER} -f "^view_server ${CCVIEWPATH}" && sleep 20 || true
	# Final check to see if view_server still exists.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" || true

	ExecProjName -v ${CCVIEW} RCSDE "1.0" "16B"
	ExecProjName -v ${CCVIEW} RCSDE "1.0" "\$$RCT_TOP/test/bin/rct_add_stp.sh ${BOARD}"
	mkdir -p $(LOGDIR)
	${F}ExecProjName -v ${CCVIEW} RCSDE "1.0" "\$$RCT_TOP/test/bin/rct_run.sh -stp ${BOARD} -dir ${SUITEDIR} -suite ${RCT_SUITE} \
	-config ${RCT_PROPERTIES} -case ${RCT_CASE} -group ${RCT_GROUP} -noshell -logdir $(LOGDIR) -ct_hooks cth_surefire '[{path,\"${TOP}/${RCT_SUITE}_report.xml\"}]' -pa $(CURDIR)/patches/ -repeat $(REPEAT)"

	# Print out view_server proc  after test execution.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" || true
	# End view_server proc, wait 5 seconds to complete the operation.
	cleartool endview -server ${CCVIEW} && sleep 5
	# verify if view_server proc is ended or not.
	pgrep -u ${USER} -l -f "^view_server ${CCVIEWPATH}" || true

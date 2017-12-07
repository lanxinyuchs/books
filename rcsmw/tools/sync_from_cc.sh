#!/bin/bash
DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
TOP=$(cd $DIR/.. && pwd)
CC=/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/ 
CCTEST=/vobs/rcs/test/RCT_CRX901275/

function ctLsToLog ()
{
   cleartool ls $* \
   | grep "@@" \
   | grep -v "/out/" \
   | grep -v "make.log" \
   >> tools/cc_sync.log
}

BLOCKS="\
   AIC \
   ALH \
   APPM \
   CCH \
   CEC \
   CERT \
   CLH \
   COI \
   COMSA \
   COMTE \
   ECOLI \
   EITC \
   EQS \
   FTPES \
   GMF \
   HWC \
   JSONE \
   LIH \
   LMA \
   OMC \
   OOT \
   PES \
   PMS \
   SAF \
   SWM \
   SYS \
   TIM"
TEST_BLOCKS="\
   FAKE\
   IFT"
TEST_LIB="\
   test/bin\
   test/lib/labb/esrc\
   test/lib/supp/esrc\
   test/lib/supp/csrc\
   test/lib/rtc-jsone/esrc\
   test/lib/rct-proxy/csrc\
   test/lib/rct-proxy/esrc\
   test/lib/rct-jsone/esrc\
   test/lib/cpulimit/csrc\
   test/lib/cpulimit/esrc\
   test/lib/saf-rpc/esrc\
   test/lib/rct-snmpmgr/esrc\
   test/lib/rct-snmpmgr/mibs\
   test/lib/lib-netconf/esrc\
   test/lib/lib-cli/esrc\
   test/lib/lib/esrc\
   test/lib/shell\
   test/lib/dialyzer\
   test/lib/saf-ift/esrc\
   test/lib/rcs-modump/esrc\
   test/lib/rcs-modump/bin\
   test/lib/rcs-sftpd/esrc\
   test/lib/rcs-sftpd/bin\
   test/lib/rcs-upinspect/bin\
   test/lib/rcs-upinspect/esrc\
   test/testspecs\
   test/suites/5G\
   test/suites/CM\
   test/suites/COM\
   test/suites/COMSA\
   test/suites/MEAS\
   test/suites/MISC\
   test/suites/NL\
   test/suites/NL/nl_SUITE_data\
   test/suites/NL/xml_files\
   test/suites/RAT\
   test/suites/RBS\
   test/suites/RESTART\
   test/suites/ROB\
   test/suites/SAF\
   test/suites/SWM\
   test/suites/SWM/bin\
   test/suites/SWM/swm_backup_SUITE_data\
   test/suites/SWM/swm_br_2_SUITE_data\
   test/suites/SWM/swm_icti\
   test/suites/SWM/swm_icti/csrc\
   test/suites/WEB\
   test/suites/XL\
   test/suites/install"

cd $TOP
rm tools/cc_sync.log

#Take care of LOG block, LOG/esrc/logWeb.erl and LOG/esrc/logWebIv1.erl MUST be kept in git (removed in CC)

echo LOG
mkdir -p LOG
mv LOG/esrc/logWeb*.erl .
rm -rf LOG/*
mkdir -p LOG/esrc
mv logWeb* LOG/esrc

   git checkout LOG/wscript_build
   cp -af $CC/LOG/LOG_CNX*/LOG_CAX*/* LOG/
   cp -af $CC/LOG/LOG_CNX*/doc/15519*/ LOG/doc
   cp -af $CC/LOG/LOG_CXA*/inc LOG/
   cp -af $CC/LOG/LOG_CNX*/test LOG/
   ctLsToLog $CC/LOG/LOG_CNX*/LOG_CAX*/*
   ctLsToLog $CC/LOG/LOG_CXA*/inc
   ctLsToLog $CC/LOG/LOG_CNX*/doc/15519*/
   ctLsToLog $CC/LOG/LOG_CNX*/test/*
   rm -rf LOG/Makefile
   rm -rf LOG/out
   rm -f LOG/make.log
   chmod -R u+r LOG/*
   chmod -R u+w LOG/*


# copy all files...
for b in $BLOCKS
do
   echo $b
   mkdir -p $b
   rm -rf $b/*
   git checkout $b/wscript_build
   cp -af $CC/$b/${b}_CNX*/${b}_CAX*/* $b/
   cp -af $CC/$b/${b}_CNX*/doc/15519*/ $b/doc
   cp -af $CC/$b/${b}_CXA*/inc $b/
   cp -af $CC/$b/${b}_CNX*/test $b/
   if [[ $b == 'HWC' ]]; then
     cp -af $CC/$b/${b}_CNX*/${b}_CAX*/xml $b/
     ctLsToLog $CC/$b/${b}_CNX*/${b}_CAX*/xml/*
   fi
   ctLsToLog $CC/$b/${b}_CNX*/${b}_CAX*/*
   ctLsToLog $CC/$b/${b}_CXA*/inc
   ctLsToLog $CC/$b/${b}_CNX*/doc/15519*/
   ctLsToLog $CC/$b/${b}_CNX*/test/*
   rm -rf $b/Makefile
   rm -rf $b/out
   rm -f $b/make.log
   chmod -R u+r $b/*
   chmod -R u+w $b/*
done


# copy all files... 
for b in $TEST_BLOCKS
do
   echo $b
   mkdir -p test/$b
   rm -rf test/$b/*
   git checkout test/$b/wscript_build
   cp -af $CCTEST/$b/${b}_CNX*/${b}_CAX*/* test/$b/
   cp -af $CCTEST/$b/${b}_CXA*/inc $b/
   ctLsToLog $CCTEST/$b/${b}_CNX*/${b}_CAX*/*
   ctLsToLog $CCTEST/$b/${b}_CXA*/inc
   rm -rf test/$b/Makefile
   rm -rf test/$b/out
   chmod -R u+r test/$b/*
   chmod -R u+w test/$b/*
done

# copy the test libraries
for d in $TEST_LIB
do
   rm -rf test/$d
   mkdir -p test/$d
   cp -f $CCTEST/$d/* test/$d/
   ctLsToLog $CCTEST/$d/
   chmod -R u+r test/$d/*
   chmod -R u+w test/$d/*
done
rm -rf test/test/lib/dialyzer/Makefile
rm -rf test/test/lib/rct-snmpmgr/mibs/*.bin

# get rid of empty directories:
find $out -type d -exec rmdir -p --ignore-fail-on-non-empty -v \{} \; &> /dev/null

# Small fixes:
rm LMA/csrc/osemain.c test/FAKE/csrc/osemain* test/IFT/csrc/osemain.c
rm SYS/csrc/com_ericsson_system_start_tp.c
rm -rf SWM/torrent
rm -rf SYS/old
rm -rf COMTE/comte
rm -rf COMTE/PATCHES

cp $CC/COMTE/COMTE_CNX*/COMTE_CAX*/comte/include/comte_log.hrl COMTE/inc/

#TODO: shell it be taken from SDK? or dynamically copied from there?
cp /vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/LIBLITS_CXA11428/share-wr6/liblits/osemain.c LMA/csrc/osemain.c
cp /vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/LIBLITS_CXA11428/share-wr6/liblits/osemain.c test/FAKE/csrc/osemain_restart_app.c
cp /vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/LIBLITS_CXA11428/share/liblits/osemain.c     test/FAKE/csrc/osemain_restart_app_sim.c
cp /vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/LIBLITS_CXA11428/share-wr6/liblits/osemain.c test/FAKE/csrc/osemain_tzii_example.c
cp /vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/LIBLITS_CXA11428/share/liblits/osemain.c     test/FAKE/csrc/osemain_tzii_example_sim.c
cp /vobs/rcs/delivery/RCP_CSX10179_1/RCPI_CXS101547/LIBLITS_CXA11428/share-wr6/liblits/osemain.c test/IFT/csrc/osemain.c

# Test specs:
mkdir -p test/specs
rm -rf test/specs/*
cp $CCTEST/test/jenkins/* test/specs/
ctLsToLog $CCTEST/test/jenkins/*

sed -i "s+/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/++"      test/specs/*
sed -i "s+/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/+test/+" test/specs/*
sed -i "s+[A-Z0-9_]*CNX[0-9]*/++"                           test/specs/*


cp /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/SYS/SYS_CNX9012620/SYS_CAX1033072/csrc/com_ericsson_system_start_tp.c SYS/csrc/com_ericsson_system_start_tp.c
chmod -R u+wr LMA/csrc/osemain.c SYS/csrc/com_ericsson_system_start_tp.c test/FAKE/csrc/osemain* test/IFT/csrc/osemain.c

# COM stuff
rm -rf COM/patches/
mkdir -p COM/emx
#mkdir -p COM/doc
cp /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/COM/COM_CXA11462/emx/* COM/emx
cp /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/COM/COM_CXA1105460/erlinc/* COM/inc
cp -a /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/COM/COM_CNX9012652/COM_CAX1033129/patches COM/


git checkout EQS/csrc/rhai-mmi.h
git checkout TIM/inc/cello_tzii.hrl
git checkout TIM/inc/cello_tzii_internal.hrl
git checkout TIM/inc/cello_tzii_sig.hrl
git checkout OTP/tgt_i686_32
git checkout test/OTP/tgt_i686_32
git checkout COM/tgt_i686_32
git checkout COM/ext_include
git checkout COM/int_include

git status

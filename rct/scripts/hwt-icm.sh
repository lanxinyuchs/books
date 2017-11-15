#!/bin/bash

SCRIPTDIR=$(dirname $0);
source $SCRIPTDIR/hwt-util.sh

BOARDTYPE=$(get_boardtype);
echo "$BOARDTYPE";

ICMTYPE=$(get_icmtype);
echo "$ICMTYPE";

shopt -s expand_aliases

TB_CMD="testbox --verbose 2"
echo $TB_CMD
alias tb=$TB_CMD


run_cmd(){
        cmd=$1
        arg2=$2
        arg3=$3
        arg4=$4
        arg5=$5
        arg6=$6
        arg7=$7
        arg8=$8
        arg9=$9

        echo "------------------------------------------------------------------"
        echo -e "testbox $cmd $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9"
        tb $cmd $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
        echo "------------------------------------------------------------------"
}

prbstest_npu_fpga(){
        link=$1
        pattern=$2

        run_cmd icmtrx_prbsinvert axm$link tx invert;
        run_cmd icmtrx_prbsinvert axm$link rx invert;
        run_cmd npuprbs_set tn$link tx prbs$pattern;
        run_cmd icmtrx_prbsgen axm$link 1 1 $pattern;	
        run_cmd npuprbs_set tn$link rx prbs$pattern;
        run_cmd icmtrx_prbsver axm$link 1 1 $pattern;	
        run_cmd npuprbs_status tn$link;
        run_cmd icmtrx_prbsstatus axm$link;
        run_cmd npuprbs_set tn$link rx off;
        run_cmd icmtrx_prbsver axm$link 0;
        run_cmd icmtrx_prbsgen axm$link 0;
        run_cmd npuprbs_set tn$link tx off;
}

prbstest_npu_external_tn(){
        link=$1
        pattern=$2
        speed=$3

        run_cmd npumac_reconf tn$link $speed;
        run_cmd npuprbs_set tn$link tx prbs$pattern;
        run_cmd npuprbs_set tn$link rx prbs$pattern;
        run_cmd npuprbs_status tn$link;
        run_cmd npuprbs_set tn$link rx off;
        run_cmd npuprbs_set tn$link tx off;
}

prbstest_fpga_external_tn(){
        link=$1
        pattern=$2

        run_cmd icmtrx_prbsgen tn$link 1 1 $pattern;
        run_cmd icmtrx_prbsver tn$link 1 1 $pattern;
        run_cmd icmtrx_prbsstatus tn$link 2;
        run_cmd icmtrx_prbsgen tn$link 0;
        run_cmd icmtrx_prbsver tn$link 0;
}

prbstest_fpga_external_ri(){
        link=$1
        pattern=$2
        speed=$3

        run_cmd icmtrx_setspeed ri$link $speed;
        run_cmd icmtrx_prbsgen ri$link 1 1 $pattern;
        run_cmd icmtrx_prbsver ri$link 1 1 $pattern;
        run_cmd icmtrx_prbsstatus ri$link;
        run_cmd icmtrx_prbsver ri$link 0 1 $pattern;
        run_cmd icmtrx_prbsgen ri$link 0 1 $pattern;
}

prbstest_fpga_phy(){
        link=$1
        pattern=$2

        # if we are running on Arria 10 FPGA we need to invert
        if [ "$ICMTYPE" == "ARRIA10" ]
                then
                run_cmd icmtrx_prbsinvert tn$link tx invert
                run_cmd icmtrx_prbsinvert tn$link rx invert
        fi
        run_cmd icmtrx_prbsgen tn$link 1 1 $pattern;
        run_cmd phy_prbsset tn$link prbs$pattern;
        run_cmd icmtrx_prbsver tn$link 1 1 $pattern;
        run_cmd phy_prbsstatus tn$link;
        run_cmd icmtrx_prbsstatus tn$link;
        run_cmd phy_prbsset tn$link off;
        run_cmd icmtrx_prbsver tn$link 0;
        run_cmd icmtrx_prbsgen tn$link 0;
        if [ "$ICMTYPE" == "5" ]
                then
                run_cmd icmtrx_prbsinvert tn$link tx normal
                run_cmd icmtrx_prbsinvert tn$link rx normal
        fi
}

tcu03_init() {
        echo "****init $BOARDTYPE****"
        run_cmd --verbose_level 2;
        run_cmd icm_load ssp;
        run_cmd icm_load cvp;
        sleep 2;
        run_cmd synth_init;
        run_cmd synth_status;
        run_cmd npu_load frog;
        run_cmd icm_status;
        run_cmd icmtrx_init;
        run_cmd icmtrx_status;
}


tcu03_test() {
        echo "****Run test on $BOARDTYPE****"
        echo "****Reset tests****"
        run_cmd tnport_setup tnj;
        run_cmd tnport_setup tnk;
        run_cmd tnport_setup tnl;
        run_cmd phy_rst_test tnj;
        run_cmd phy_rst_test tnk;
        run_cmd phy_rst_test tnl;

        run_cmd tnport_setup tnj;
        run_cmd tnport_setup tnk;
        run_cmd tnport_setup tnl;

        echo "****PHY time sync****"
        run_cmd phy_time_sync tnj;
        run_cmd phy_time_sync tnk;
        run_cmd phy_time_sync tnl;

        echo "****NPU TNA..D port (using loopback in AXM)****"
        run_cmd npumac_loop tna internal;
        prbstest_npu_external_tn a 7 gmac
        prbstest_npu_external_tn a 7 xgmac
        run_cmd npumac_loop tna no;

        run_cmd npumac_loop tnb internal;
        prbstest_npu_external_tn b 7 gmac
        prbstest_npu_external_tn b 7 xgmac
        run_cmd npumac_loop tnb no;

        run_cmd npumac_loop tnc internal;
        prbstest_npu_external_tn c 7 gmac
        prbstest_npu_external_tn c 7 xgmac
        run_cmd npumac_loop tnc no;

        run_cmd npumac_loop tnd internal;
        prbstest_npu_external_tn d 7 gmac
        prbstest_npu_external_tn d 7 xgmac
        run_cmd npumac_loop tnd no;

        echo "****NPU AXM-E to/from FPGA (1G + 10G mode)****"
        run_cmd icmtrx_setspeed axme 1GbE;
        run_cmd npumac_reconf tne GMAC;
        prbstest_npu_fpga e 15
        run_cmd icmtrx_setspeed axme 10GbE;
        run_cmd npumac_reconf tne XGMAC;
        prbstest_npu_fpga e 7

        echo "****FPGA TNE port (1G + 10G mode, using loopback in FPGA)****"
        run_cmd icmtrx_mode tne serial;
        run_cmd icmtrx_setspeed tne 1GbE;
        prbstest_fpga_external_tn e 10
        run_cmd icmtrx_setspeed tne 10GbE;
        prbstest_fpga_external_tn e 7
        run_cmd icmtrx_mode tne normal;

        echo "****NPU AXM-F..H to/from FPGA****"
        prbstest_npu_fpga f 15
        prbstest_npu_fpga g 15
        prbstest_npu_fpga h 15

        echo "****FPGA TNF..H port (using loopback in FPGA)****"
        run_cmd icmtrx_mode tnf serial;
        prbstest_fpga_external_tn f 10
        run_cmd icmtrx_mode tnf normal;

        run_cmd icmtrx_mode tng serial;
        prbstest_fpga_external_tn g 10
        run_cmd icmtrx_mode tng normal;

        run_cmd icmtrx_mode tnh serial;
        prbstest_fpga_external_tn h 10
        run_cmd icmtrx_mode tnh normal;

        echo "****NPU AXM-J..L to/from FPGA****"
        prbstest_npu_fpga j 15
        prbstest_npu_fpga k 15
        prbstest_npu_fpga l 15

        echo "****FPGA TNJ port to/from PHY****"
        run_cmd tnport_setup tnj;
        prbstest_fpga_phy j 15

        run_cmd tnport_setup tnk;
        prbstest_fpga_phy k 15

        run_cmd tnport_setup tnl;
        prbstest_fpga_phy l 15
}

tcu04_init() {
        echo "****init $BOARDTYPE****"
        run_cmd --verbose_level 2;
        run_cmd icm_load ssp;
        run_cmd icm_load cvp;
        sleep 2;
        run_cmd synth_init;
        run_cmd synth_status;
        run_cmd npu_load frog;
        run_cmd icm_status;
        #run_cmd icmtrx_init;
        #run_cmd icmtrx_status;
}

tcu04_test() {
        echo "****Run test on $BOARDTYPE****"
        echo "****Reset tests****"
        run_cmd tnport_setup tnk;
        run_cmd tnport_setup tnl;
        run_cmd tnport_setup tnm;
        run_cmd tnport_setup tnn;
        run_cmd phy_rst_test tnk;
        run_cmd phy_rst_test tnl;
        run_cmd phy_rst_test tnm;
        run_cmd phy_rst_test tnn;

        run_cmd tnport_setup tnk;
        run_cmd tnport_setup tnl;
        run_cmd tnport_setup tnm;
        run_cmd tnport_setup tnn;

        echo "****PHY time sync****"
        run_cmd phy_time_sync tnk;
        run_cmd phy_time_sync tnl;
        run_cmd phy_time_sync tnm;
        run_cmd phy_time_sync tnn;

        echo "****NPU TNA..N port (using loopback in AXM)****"
        run_cmd npumac_loop tna internal;
        prbstest_npu_external_tn a 7 gmac
        prbstest_npu_external_tn a 7 xgmac
        run_cmd npumac_loop tna no;

        run_cmd npumac_loop tnb internal;
        prbstest_npu_external_tn b 7 gmac
        prbstest_npu_external_tn b 7 xgmac
        run_cmd npumac_loop tnb no;

        run_cmd npumac_loop tnc internal;
        prbstest_npu_external_tn c 7 gmac
        prbstest_npu_external_tn c 7 xgmac
        run_cmd npumac_loop tnc no;

        run_cmd npumac_loop tnd internal;
        prbstest_npu_external_tn d 7 gmac
        prbstest_npu_external_tn d 7 xgmac
        run_cmd npumac_loop tnd no;

        run_cmd npumac_loop tne internal;
        prbstest_npu_external_tn e 7 gmac
        prbstest_npu_external_tn e 7 xgmac
        run_cmd npumac_loop tne no;

        run_cmd npumac_loop tnf internal;
        prbstest_npu_external_tn f 7 gmac
        run_cmd npumac_loop tnf no;

        run_cmd npumac_loop tng internal;
        prbstest_npu_external_tn g 7 gmac
        run_cmd npumac_loop tng no;

        run_cmd npumac_loop tnh internal;
        prbstest_npu_external_tn h 7 gmac
        run_cmd npumac_loop tnh no;

        run_cmd npumac_loop tnj internal;
        prbstest_npu_external_tn j 7 gmac
        run_cmd npumac_loop tnj no;

        run_cmd npumac_loop tnk internal;
        prbstest_npu_external_tn k 7 gmac
        run_cmd npumac_loop tnk no;

        run_cmd npumac_loop tnl internal;
        prbstest_npu_external_tn l 7 gmac
        run_cmd npumac_loop tnl no;

        run_cmd npumac_loop tnm internal;
        prbstest_npu_external_tn m 7 gmac
        run_cmd npumac_loop tnm no;

        run_cmd npumac_loop tnn internal;
        prbstest_npu_external_tn n 7 gmac
        run_cmd npumac_loop tnn no;
}

dusx2_init() {
        echo "****init $BOARDTYPE****"
        run_cmd --verbose_level 2;
        run_cmd icm_load ssp;
        run_cmd icm_load cvp;
        sleep 2;
        run_cmd synth_init;
        run_cmd synth_status;
        run_cmd icmtrx_pllrecal;
        run_cmd icmtrx_pllstatus;
        run_cmd npu_load frog;
        run_cmd icm_status;
        run_cmd icmtrx_init;
        run_cmd icmtrx_status;
}

dusx2_test() {
        echo "****Run test on $BOARDTYPE****"
        echo "****Reset tests****"
        run_cmd icmioexp_rst_test
        run_cmd tnport_setup tna
        run_cmd phy_rst_test tna
        run_cmd tnport_setup tna

        #Add this when Gureny-2 is released (and working)
        #echo "****PHY time sync****"
        #run_cmd phy_time_sync tna

        echo "****FPGA TNA port to/from TNA PHY****"
        run_cmd tnport_setup tna
        prbstest_fpga_phy a 15

        echo "****NPU AXM-A to/from FPGA (std mode)****"
        prbstest_npu_fpga a 15

        echo "****FPGA TNB port (1G + 10G mode, using loopback in FPGA)****"
        run_cmd icmtrx_mode tnb serial;
        run_cmd icmtrx_setspeed tnb 1GbE;
        prbstest_fpga_external_tn b 15
        run_cmd icmtrx_setspeed tnb 10GbE;
        prbstest_fpga_external_tn b 7
        run_cmd icmtrx_mode tnb normal;

        echo "****NPU AXM-B to/from FPGA (1G + 10G mode)****"
        run_cmd icmtrx_setspeed axmb 1GbE;
        run_cmd npumac_reconf tnb GMAC;
        prbstest_npu_fpga b 15
        run_cmd icmtrx_setspeed axmb 10GbE;
        run_cmd npumac_reconf tnb XGMAC;
        prbstest_npu_fpga b 7

        echo "****FPGA TNC port (1G + 10G mode, using loopback in FPGA)****"
        run_cmd icmtrx_mode tnc serial;
        run_cmd icmtrx_setspeed tnc 1GbE;
        prbstest_fpga_external_tn c 15
        run_cmd icmtrx_setspeed tnc 10GbE;
        prbstest_fpga_external_tn c 7
        run_cmd icmtrx_mode tnc normal;

        echo "****NPU AXM-C to/from FPGA (1G + 10G mode)****"
        run_cmd icmtrx_setspeed axmc 1GbE;
        run_cmd npumac_reconf tnc GMAC;
        prbstest_npu_fpga c 15
        run_cmd icmtrx_setspeed axmc 10GbE;
        run_cmd npumac_reconf tnc XGMAC;
        prbstest_npu_fpga c 7

        echo "****RI-0..5 (2.5G + 10.1G, using loopback in FPGA)****"
        run_cmd icmtrx_mode ri0 serial;
        run_cmd icmtrx_mode ri1 serial;
        run_cmd icmtrx_mode ri2 serial;
        run_cmd icmtrx_mode ri3 serial;
        run_cmd icmtrx_mode ri4 serial;
        run_cmd icmtrx_mode ri5 serial;
        prbstest_fpga_external_ri 0 7 2.5Gb
        prbstest_fpga_external_ri 1 7 2.5Gb
        prbstest_fpga_external_ri 2 7 2.5Gb
        prbstest_fpga_external_ri 3 7 2.5Gb
        prbstest_fpga_external_ri 4 7 2.5Gb
        prbstest_fpga_external_ri 5 7 2.5Gb
        prbstest_fpga_external_ri 0 7 10.1Gb
        prbstest_fpga_external_ri 1 7 10.1Gb
        prbstest_fpga_external_ri 2 7 10.1Gb
        prbstest_fpga_external_ri 3 7 10.1Gb
        prbstest_fpga_external_ri 4 7 10.1Gb
        prbstest_fpga_external_ri 5 7 10.1Gb
        run_cmd icmtrx_mode ri0 normal;
        run_cmd icmtrx_mode ri1 normal;
        run_cmd icmtrx_mode ri2 normal;
        run_cmd icmtrx_mode ri3 normal;
        run_cmd icmtrx_mode ri4 normal;
        run_cmd icmtrx_mode ri5 normal;

        echo "****AXM EA-0 port to/from FPGA (using loopback in FPGA)****"
        run_cmd icmtrx_prbsinvert ea tx invert;
        run_cmd icmtrx_prbsinvert ea rx invert;
        run_cmd npuprbs_set ea tx prbs31;
        run_cmd icmtrx_prbsgen ea 1 1 31;
        run_cmd npuprbs_set ea rx prbs31;
        run_cmd icmtrx_prbsver ea 1 1 31;
        ## It takes some time for the EA link to get ready!!
        sleep 2
        run_cmd icmtrx_prbsstatus ea 4;
        sleep 2
        run_cmd npuprbs_status ea;
        run_cmd icmtrx_prbsver ea 0;
        run_cmd npuprbs_set ea rx off;
        run_cmd npuprbs_set ea tx off;
        run_cmd icmtrx_prbsgen ea 0;
}

test_icmxio_cmd() {
        echo "-----------------------------------------------------------------"
        echo "--- test_icmxio_cmd ---"
        run_cmd icmxio_enable all 0
        echo -e "testbox icmxio_status"
        result=$(testbox icmxio_status)

        if [[ $result == *"en:1"* ]]
        then
                ## Replace TESTBOX_PASS with FAIL so Jenkins triggers on this!
                result=$(echo "$result" | sed 's/TESTBOX_PASS/TESTBOX_FAIL/g')
                echo "$result"
        else
                echo "$result"
        fi

        run_cmd icmxio_enable all 1
        echo -e "testbox icmxio_status"
        result=$(testbox icmxio_status)
        if [[ $result == *"en:0"* ]]
        then
                ## Replace TESTBOX_PASS with FAIL so Jenkins triggers on this!
                result=$(echo "$result" | sed 's/TESTBOX_PASS/TESTBOX_FAIL/g')
                echo "$result"
        else
                echo "$result"
        fi
        echo "-----------------------------------------------------------------"
}

run_and_verify() {
        cmd=$1

        echo "-----------------------------------------------------------------"
        echo "Test icm_rd/icm_wr"
        echo -e "testbox $1 $2 $3 0x00"
        testbox $1 $2 $3 0x00

        echo -e "testbox $1 $2 $3 $4"
        testbox $1 $2 $3 $4

        echo -e "testbox icm_rd $2 $3"
        result=$(testbox icm_rd $2 $3)

        if [[ $result == *"$4"* ]]
        then
                ## Replace TESTBOX_PASS with FAIL so Jenkins triggers on this!
                result=$(echo "$result" | sed 's/TESTBOX_PASS/TESTBOX_FAIL/g')
                echo "$result"
        else
                echo "$result"
        fi
        echo "-----------------------------------------------------------------"
}

icm_rd_wr_test() {
        run_and_verify icm_wr swri 0xb04 0xff
}

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
        if [ "$ICMTYPE" == "ARRIA10" ]; then
                echo "-- Running on board with Arria 10 FPGA --"
        else
                dusx2_init;
                dusx2_test;
        fi
        icm_rd_wr_test;
        run_cmd icmioexp_rst_test;
        test_icmxio_cmd;
fi

if [ "$BOARDTYPE" == "TCU0301" ]
then
        tcu03_init;
        tcu03_test;
        icm_rd_wr_test;
        #test_icmxio_cmd;
fi

if [ "$BOARDTYPE" == "TCU0401" ]
then
        tcu04_init;
        tcu04_test;
        icm_rd_wr_test;
       # test_icmxio_cmd;
fi


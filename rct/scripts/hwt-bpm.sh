#!/bin/bash

SCRIPTDIR=$(dirname $0);
source $SCRIPTDIR/hwt-util.sh

BOARDTYPE=$(get_boardtype);
echo "$BOARDTYPE";

ICMTYPE=$(get_icmtype);
echo "$ICMTYPE";


run_cmd(){
        cmd=$1

        echo "------------------------------------------------------------------"
        echo -e "testbox $cmd"
        testbox $cmd
        echo "------------------------------------------------------------------"
}

test_bpm()  {
        run_cmd "bpm_revision"
        run_cmd "bpm_power_status 0"
        run_cmd "bpm_pgood_status"
        run_cmd "bpm_syscond pgoods"
        run_cmd "bpm_pdstatus 0"
        #run_cmd "bpm_pdctrl 0 on"
        run_cmd "bpm_power"
        run_cmd "bpm_revision"
        run_cmd "bpm_log fault"
        #run_cmd "bpm_sysreset"
        run_cmd "bpm_stats"
        ##run_cmd "bpm_led_adc"
        #run_cmd "bpm_raw"
        run_cmd "bpm_alarms"
        ##run_cmd "bpm_smlsetvid 0 1"
        ##run_cmd "bpm_smlreadvid 0"
        ##run_cmd "bpm_smlctrl 0 keep"
        ##run_cmd "bpm_measv 0"
        #run_cmd "bpm_pdctrl 1 off"
        #run_cmd "bpm_pdctrl 1 on"
}

run_cmd "--clear all"

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
        test_bpm;
fi

if [ "$BOARDTYPE" == "TCU0301" ]
then
        test_bpm;
fi

if [ "$BOARDTYPE" == "TCU0401" ]
then
        test_bpm;
fi

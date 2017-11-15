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

test_maxload() {
        run_cmd "maxload_calc -s"
        run_cmd "maxload_calc -n 5 -t sec 30"
        run_cmd "maxload_calc -s",
        run_cmd "maxload_calc -b 1"
        run_cmd "maxload_calc -s"
        sleep 3
        run_cmd "maxload_calc -s"
}

test_misc()  {
        run_cmd "syntaocxo_temp_sensor"
        run_cmd "syntaocxo_checksum"
        run_cmd "syntaocxo_temp"
        run_cmd "syntaocxo_dump"
        run_cmd "syntaocxo_dump TN"
        run_cmd "syntaocxo_dump RBS"
        run_cmd "generate_freq"
        run_cmd "synth_init"
        run_cmd "tu_calibosc reset"
        run_cmd "tu_calibosc freq"
        run_cmd "tu_osccoeff orig"
        run_cmd "tu_osccoeff calib"

        run_cmd "synth_status"
        run_cmd "synth_check"
        run_cmd "synth_cfg RBS MOD"
        run_cmd "synth_freq TN get"
        run_cmd "synth_freq RBS get"
        run_cmd "synth_freqtest RBS 0 0"
        run_cmd "synth_freqtest TN 0 0"
        run_cmd "turef_status"
        run_cmd "generate_freq"
        run_cmd "tu_calibosc freq"
        run_cmd "tu_osccoeff orig"
        run_cmd "tuhwf_status RBS"
        run_cmd "tuhwf_status TN"
        run_cmd "syntamult_read 0x0006"
        run_cmd "hotspot_temp"
}

## Gpio(s) used in production
test_gpio() {
        run_cmd "gpio get 4"
        run_cmd "gpio get 5"
        run_cmd "gpio get 6"
        run_cmd "gpio get 12"
        run_cmd "gpio get 13"
        run_cmd "gpio get 14"
        run_cmd "gpio get 15"
        run_cmd "gpio get 23"
        run_cmd "gpio get 26"
        run_cmd "gpio get 27"
        run_cmd "gpio get 28"
        run_cmd "gpio get 29"
        run_cmd "gpio get 31"
        run_cmd "gpio set 4 out_high"
        run_cmd "gpio set 5 out_high"
        run_cmd "gpio set 6 out_high"
        run_cmd "gpio set 12 out_high"
        run_cmd "gpio set 13 out_high"
        run_cmd "gpio set 14 out_high"
        run_cmd "gpio set 15 out_high"
        run_cmd "gpio set 23 out_high"
        run_cmd "gpio set 26 out_high"
        run_cmd "gpio set 27 out_high"
        run_cmd "gpio set 28 out_high"
        run_cmd "gpio set 29 out_high"
        run_cmd "gpio set 31 out_high"
        run_cmd "gpio set 4 out_low"
        run_cmd "gpio set 5 out_low"
        run_cmd "gpio set 6 out_low"
        run_cmd "gpio set 12 out_low"
        run_cmd "gpio set 13 out_low"
        run_cmd "gpio set 14 out_low"
        run_cmd "gpio set 15 out_low"
        run_cmd "gpio set 23 out_low"
        run_cmd "gpio set 26 out_low"
        run_cmd "gpio set 27 out_low"
        run_cmd "gpio set 28 out_low"
        run_cmd "gpio set 29 out_low"
        run_cmd "gpio set 31 out_low"
}

run_cmd "--log_clear"
run_cmd "--clear all"

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
        test_misc;
        test_gpio;
        test_maxload;
fi

if [ "$BOARDTYPE" == "TCU0301" ]
then
        test_misc;
        test_gpio;
        test_maxload;
fi

if [ "$BOARDTYPE" == "TCU0401" ]
then
        test_misc;
        ## ToDo: Doesn't work on TCU04
        ##test_gpio;
        test_maxload;
fi



#!/bin/bash

echo "$0"

function get_boardtype() {
        BOARDTYPE=$(testbox boardinfo | grep "DUS 52\|DUS 32\|TCU 04\|TCU 03" | sed 's/Product name\://' | tr -d ' ')
        echo "$BOARDTYPE"
}

function get_icmtype() {
        STR=$(testbox icm_status | grep "FPGA FW version" | sed 's/FPGA FW version\: 0x//')
        TYPE=$(echo "${STR:0:1}")

        if [ "$TYPE" == "5" ]
        then
                ICMTYPE="ARRIA10"
                echo "$ICMTYPE"
        elif [ "$TYPE" == "6" ]
        then
                ICMTYPE="KATLA"
                echo "$ICMTYPE"
        elif [ "$TYPE" == "1" ]
        then
                ICMTYPE="STRATIX"
                echo "$ICMTYPE"
        elif [ "$TYPE" == "4" ]
        then
                ICMTYPE="TAIPAN"
                echo "$ICMTYPE"
        else
                ICMTYPE="UKNOWN"
                echo "$ICMTYPE"
        fi
}
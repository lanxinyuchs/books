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

run_cmd "--log_clear"
run_cmd "--clear all"

test_npu() {
		run_cmd "npu_load frog";
        run_cmd "npu_override";
        run_cmd "npu_temp";
        run_cmd "npusynce_select tna 1"
        run_cmd "npumac_enable tna 1 1"
}

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
        test_npu;
fi

if [ "$BOARDTYPE" == "TCU0301" ]
then
        test_npu;
fi

if [ "$BOARDTYPE" == "TCU0401" ]
then
        test_npu;
fi

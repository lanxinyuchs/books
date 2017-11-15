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

test_boardinfo() {
        run_cmd "boardinfo"
        run_cmd "version"
        run_cmd "icm_status"
        run_cmd "version uboot"
        run_cmd "version bpm"
        run_cmd "version dtb"
        run_cmd "version up"
        run_cmd "version ee"
        run_cmd "bpm_revision"
}

run_cmd --log_clear
run_cmd --clear all

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
        test_boardinfo;

elif [ "$BOARDTYPE" == "TCU0301" ]
then
        test_boardinfo;

elif [ "$BOARDTYPE" == "TCU0401" ]
then
        test_boardinfo;

fi



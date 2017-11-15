#!/bin/bash

SCRIPTDIR=$(dirname $0);
source $SCRIPTDIR/hwt-util.sh

BOARDTYPE=$(get_boardtype);
echo "$BOARDTYPE";

ICMTYPE=$(get_icmtype);
echo "$ICMTYPE";

run_cmd(){
  	echo "------------------------------------------------------------------"
  	echo -e "\033[1m$1\033[0m"
  	result=$($1)
  	echo "$result"
  	echo "------------------------------------------------------------------"
}

check_ret_value(){

	echo "------------------------------------------------------------------"
	result=$($1)
	echo -e "\033[1m$1\033[0m"

	if [[ $result == *"npucmem_test :: TESTBOX_PASS"* ]]
	then
	    result=$(echo "$result" | sed 's/TESTBOX_PASS/TESTBOX_FAIL/g')
	    echo "$result"
	else
	    result=$(echo "$result" | sed 's/TESTBOX_FAIL/TESTBOX_PASS/g')
	    echo "$result"
	fi
	echo "------------------------------------------------------------------"
}

run_cmd "testbox boardinfo"
run_cmd "testbox npu_load frog"
run_cmd "testbox dtbversion"



echo CMEM0
echo "## ECC SET TO GENERATE, DETECT AND CORRECT"
run_cmd "ncpWrite 0x8.0x9.0x48 0x4"
echo "## introduce double bit error"
run_cmd "ncpWrite 0x8.0x9.0x48 0x7"
## Replace TESTBOX_FAIL with TESTBOX_PASS so Jenkins doesn't trigger on this!
## Check that npucmem_test finds error, shoudl return FAIL 
check_ret_value "testbox npucmem_test -C 0 -e 1 -f 1 -m 0 -p 1"

run_cmd "ncpWrite 0x8.0x9.0x48 0x4"
echo "## introduce double bit error"
run_cmd "ncpWrite 0x8.0x9.0x48 0x7"
## Replace TESTBOX_FAIL with TESTBOX_PASS so Jenkins doesn't trigger on this!
## Check that npucmem_test finds error, shoudl return FAIL
check_ret_value "testbox npucmem_test -C 0 -e 1 -f 1 -m 0 -p 0"

echo "## Restore double bit error"
run_cmd "ncpWrite 0x8.0x9.0x48 0x0"
run_cmd "testbox npucmem_test -C 0 -e 1 -f 1 -m 0 -p 1"
run_cmd "testbox npucmem_test -C 0 -e 1 -f 1 -m 0 -p 0"



echo CMEM1
echo "## ECC SET TO GENERATE, DETECT AND CORRECT"
run_cmd "ncpWrite 0x9.0x9.0x48 0x4"
echo "## introduce double bit error"
run_cmd "ncpWrite 0x9.0x9.0x48 0x7"
## Replace TESTBOX_FAIL with TESTBOX_PASS so Jenkins doesn't trigger on this!
## Check that npucmem_test finds error, shoudl return FAIL
check_ret_value "testbox npucmem_test -C 1 -e 1 -f 1 -m 0 -p 1"
echo "## Restore single bit error"
run_cmd "ncpWrite 0x9.0x9.0x48 0x0"

echo "## ECC SET TO GENERATE, DETECT AND CORRECT"
run_cmd "ncpWrite 0x9.0x9.0x48 0x4"
echo "## introduce double bit error"
run_cmd "ncpWrite 0x9.0x9.0x48 0x7"
## Replace TESTBOX_FAIL with TESTBOX_PASS so Jenkins doesn't trigger on this!
## Check that npucmem_test finds error, shoudl return FAIL
check_ret_value "testbox npucmem_test -C 1 -e 1 -f 1 -m 0 -p 0"

echo "## Restore double bit error"
run_cmd "ncpWrite 0x9.0x9.0x48 0x0"
run_cmd "testbox npucmem_test -C 1 -e 1 -f 1 -m 0 -p 1"
run_cmd "testbox npucmem_test -C 1 -e 1 -f 1 -m 0 -p 0"

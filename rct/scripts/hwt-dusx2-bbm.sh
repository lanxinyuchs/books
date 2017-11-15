#!/bin/bash

SCRIPTDIR=$(dirname $0);
source $SCRIPTDIR/hwt-util.sh

BOARDTYPE=$(get_boardtype);
echo "$BOARDTYPE";

ICMTYPE=$(get_icmtype);
echo "$ICMTYPE";

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
        testbox $cmd $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9
        echo "------------------------------------------------------------------"
}

emca_status(){
	arg1=$1
	arg2=$2

	result=$(run_cmd emcaprbs_stats $arg1 $arg2 clear)
	echo "$result"
	result=$(run_cmd emcaprbs_stats $arg1 $arg2)
	echo "$result"
}

run_cmd --clear all
sleep 1
run_cmd emca_power off
run_cmd emca_power on
sleep 1
run_cmd npu_load frog
sleep 1
run_cmd icm_status

echo "---Do reset and init for emca(s) ---"
run_cmd emca_reset 1
run_cmd emca_reset 0
run_cmd emca_init
run_cmd emca_ethinit

emca_asic_tests() {
        emca=$1
        run_cmd emca_ddrtest $emca
        run_cmd emca_read $emca ddr 0x0 10
        run_cmd emca_spredix_test $emca
        run_cmd emca_xio_test $emca
        run_cmd emca_maxload $emca 0xffffffffffffffff
}

emca_test() {
        emca=$1
        if [ "$ICMTYPE" == "ARRIA10" ]
        then
        	run_cmd emca_avs $emca disable
        	run_cmd emca_avs $emca enable
        fi
        run_cmd emca_write $emca 0x10 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8
        run_cmd emca_read $emca cm 0x10 8
        run_cmd emca_resetdsps $emca
        run_cmd emca_bootdsps $emca 0xffffffffffffffff
        run_cmd emca_stopdsps $emca
}

if [ "$BOARDTYPE" == "DUS5201" ]
        then
        echo "---RUN TESTS ON $BOARDTYPE ---"
        sleep 1
        emca_asic_tests emca1
        emca_asic_tests emca2
        emca_asic_tests emca3
        emca_asic_tests emca4
        sleep 1
        emca_test emca1
        emca_test emca2
        emca_test emca3
        emca_test emca4
else
        echo "---RUN TESTS ON $BOARDTYPE ---"
        sleep 1
        emca_asic_tests emca3
        emca_asic_tests emca4
        sleep 1
        emca_test emca3
        emca_test emca4
fi

run_cmd icmtrx_power et0 1 q
run_cmd icmtrx_power et1 1 q
run_cmd icmtrx_power et2 1 q
run_cmd icmtrx_power et3 1 q
run_cmd icmtrx_power et4 1 q
run_cmd icmtrx_power et5 1 q
run_cmd icmtrx_power et6 1 q
run_cmd icmtrx_power et7 1 q

if [ "$BOARDTYPE" == "DUS5201" ]
then
	run_cmd emcaprbs_set emca1 et4 tx prbs11
	run_cmd emcaprbs_set emca2 et3 rx prbs11
	emca_status emca2 et3

	run_cmd emcaprbs_set emca1 et4 rx prbs58
	run_cmd emcaprbs_set emca2 et3 tx prbs58
	emca_status emca1 et4

	run_cmd emcaprbs_set emca1 et5 tx prbs31
	run_cmd emcaprbs_set emca2 et2 rx prbs31
	emca_status emca2 et2

	run_cmd emcaprbs_set emca1 et5 rx prbs58
	run_cmd emcaprbs_set emca2 et2 tx prbs58
	emca_status emca1 et5

	run_cmd emcaprbs_set emca1 et0 tx prbs31
	run_cmd emcaprbs_set emca3 et1 rx prbs31
	emca_status emca3 et1

	run_cmd emcaprbs_set emca1 et0 rx prbs7
	run_cmd emcaprbs_set emca3 et1 tx prbs7
	emca_status emca1 et0

	run_cmd emcaprbs_set emca1 et1 rx prbs7
	run_cmd emcaprbs_set emca3 et0 tx prbs7
	emca_status emca1 et1

	run_cmd emcaprbs_set emca1 et1 tx prbs31
	run_cmd emcaprbs_set emca3 et0 rx prbs31
	emca_status emca3 et0

	run_cmd emcaprbs_set emca1 et7 tx prbs31
	run_cmd emcaprbs_set emca4 et0 rx prbs31
	emca_status emca4 et0

	run_cmd emcaprbs_set emca1 et7 rx prbs11
	run_cmd emcaprbs_set emca4 et0 tx prbs11
	emca_status emca1 et7

	run_cmd emcaprbs_set emca1 et6 rx prbs7
	run_cmd emcaprbs_set emca4 et1 tx prbs7
	emca_status emca1 et6

	run_cmd emcaprbs_set emca1 et6 tx prbs23
	run_cmd emcaprbs_set emca4 et1 rx prbs23
	emca_status emca4 et1

	run_cmd emcaprbs_set emca2 et8 tx prbs31
	run_cmd emcaprbs_set emca3 et5 rx prbs31
	emca_status emca3 et5

	run_cmd emcaprbs_set emca2 et8 rx prbs7
	run_cmd emcaprbs_set emca3 et5 tx prbs7
	emca_status emca2 et8

	run_cmd emcaprbs_set emca2 et9 rx prbs7
	run_cmd emcaprbs_set emca3 et4 tx prbs7
	emca_status emca2 et9

	run_cmd emcaprbs_set emca2 et9 tx prbs31
	run_cmd emcaprbs_set emca3 et4 rx prbs31
	emca_status emca3 et4

	run_cmd emcaprbs_set emca2 et0 tx prbs31
	run_cmd emcaprbs_set emca4 et3 rx prbs31
	emca_status emca4 et3

	run_cmd emcaprbs_set emca2 et0 rx prbs11
	run_cmd emcaprbs_set emca4 et3 tx prbs11
	emca_status emca2 et0

	run_cmd emcaprbs_set emca2 et1 rx prbs7
	run_cmd emcaprbs_set emca4 et2 tx prbs7
	emca_status emca2 et1

	run_cmd emcaprbs_set emca2 et1 tx prbs23
	run_cmd emcaprbs_set emca4 et2 rx prbs23
	emca_status emca4 et2
fi

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
	run_cmd emcaprbs_set emca3 et2 tx prbs31
	run_cmd emcaprbs_set emca4 et5 rx prbs31
	emca_status emca4 et5

	run_cmd emcaprbs_set emca3 et2 rx prbs7
	run_cmd emcaprbs_set emca4 et5 tx prbs7
	emca_status emca3 et2

	if [ "$BOARDTYPE" == "DUS5201" ]
	then
		run_cmd emcaprbs_set emca3 et3 rx prbs7
		run_cmd emcaprbs_set emca4 et4 tx prbs7
		emca_status emca3 et3
	
		run_cmd emcaprbs_set emca3 et3 tx prbs31
		run_cmd emcaprbs_set emca4 et4 rx prbs31
		emca_status emca4 et4
	fi
fi

if [ "$BOARDTYPE" == "DUS5201" ]
then
	## XIO links
	run_cmd emcaprbs_set emca1 xtx4 tx prbs31
	run_cmd emcaprbs_set emca2 xrx9 rx prbs31
	emca_status emca2 xrx9

	run_cmd emcaprbs_set emca1 xtx5 tx prbs31
	run_cmd emcaprbs_set emca2 xrx8 rx prbs31
	emca_status emca2 xrx8

	run_cmd emcaprbs_set emca1 xtx6 tx prbs31
	run_cmd emcaprbs_set emca2 xrx7 rx prbs31
	emca_status emca2 xrx7

	run_cmd emcaprbs_set emca1 xtx7 tx prbs31
	run_cmd emcaprbs_set emca2 xrx6 rx prbs31
	emca_status emca2 xrx6

	run_cmd emcaprbs_set emca1 xtx8 tx prbs31
	run_cmd emcaprbs_set emca2 xrx5 rx prbs31
	emca_status emca2 xrx5

	run_cmd emcaprbs_set emca1 xtx9 tx prbs31
	run_cmd emcaprbs_set emca2 xrx4 rx prbs31
	emca_status emca2 xrx4
fi

if [ "$BOARDTYPE" == "DUS5201" ]
then
	## EMCA1
	run_cmd emcaprbs_set emca1 et8 tx prbs58
	run_cmd emcaprbs_set emca1 et8 rx prbs58
	run_cmd npuprbs_set bbm0 tx prbs58
	run_cmd npuprbs_set bbm0 rx prbs58
	emca_status emca1 et8
	run_cmd npuprbs_status bbm0

	run_cmd emcaprbs_set emca1 et9 tx prbs7
	run_cmd emcaprbs_set emca1 et9 rx prbs7
	run_cmd npuprbs_set bbm1 tx prbs7
	run_cmd npuprbs_set bbm1 rx prbs7
	emca_status emca1 et9
	run_cmd npuprbs_status bbm1

	## EMCA2
	run_cmd emcaprbs_set emca2 et6 tx prbs31
	run_cmd emcaprbs_set emca2 et6 rx prbs31
	run_cmd npuprbs_set bbm2 tx prbs31
	run_cmd npuprbs_set bbm2 rx prbs31
	emca_status emca2 et6
	run_cmd npuprbs_status bbm2

	run_cmd emcaprbs_set emca2 et7 tx prbs23
	run_cmd emcaprbs_set emca2 et7 rx prbs23
	run_cmd npuprbs_set bbm3 tx prbs23
	run_cmd npuprbs_set bbm3 rx prbs23
	emca_status emca2 et7
	run_cmd npuprbs_status bbm3
fi

if [ "$BOARDTYPE" == "DUS5201" ]
then
	run_cmd emcaprbs_set emca3 et9 tx prbs11
	run_cmd emcaprbs_set emca3 et9 rx prbs11
	run_cmd npuprbs_set bbm5 tx prbs11
	run_cmd npuprbs_set bbm5 rx prbs11
	emca_status emca3 et9
	run_cmd npuprbs_status bbm5

	run_cmd emcaprbs_set emca4 et9 tx prbs7
	run_cmd emcaprbs_set emca4 et9 rx prbs7
	run_cmd npuprbs_set bbm7 tx prbs7
	run_cmd npuprbs_set bbm7 rx prbs7
	emca_status emca4 et9
	run_cmd npuprbs_status bbm7
fi

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
	## EMCA3
	run_cmd emcaprbs_set emca3 et8 tx prbs9
	run_cmd emcaprbs_set emca3 et8 rx prbs9
	run_cmd npuprbs_set bbm4 tx prbs9
	run_cmd npuprbs_set bbm4 rx prbs9
	emca_status emca3 et8
	run_cmd npuprbs_status bbm4

	## EMCA4
	run_cmd emcaprbs_set emca4 et8 tx prbs15
	run_cmd emcaprbs_set emca4 et8 rx prbs15
	run_cmd npuprbs_set bbm6 tx prbs15
	run_cmd npuprbs_set bbm6 rx prbs15
	emca_status emca4 et8
	run_cmd npuprbs_status bbm6
fi

if [ "$BOARDTYPE" == "DUS5201" ]
then
	## EMCA1
	run_cmd icmtrx_prbsinvert et0 tx invert
	run_cmd icmtrx_prbsinvert et0 rx invert
	run_cmd emcaprbs_set emca1 et2 tx prbs7
	run_cmd emcaprbs_set emca1 et2 rx prbs7
	run_cmd icmtrx_prbsver et0 1 1 7
	run_cmd icmtrx_prbsgen et0 1 1 7
	emca_status emca1 et2
	run_cmd icmtrx_prbsstatus et0

	run_cmd icmtrx_prbsinvert et1 tx invert
	run_cmd icmtrx_prbsinvert et1 rx invert
	run_cmd emcaprbs_set emca1 et3 tx prbs7
	run_cmd emcaprbs_set emca1 et3 rx prbs7
	run_cmd icmtrx_prbsver et1 1 1 7
	run_cmd icmtrx_prbsgen et1 1 1 7
	emca_status emca1 et3
	run_cmd icmtrx_prbsstatus et1

	## XIO
	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xrx0 xrx1 xrx2 xrx3
		do
			run_cmd icmtrx_prbsinvert $trx rx invert
		done
	fi
	run_cmd emcaprbs_set emca1 xtx0 tx prbs7
	run_cmd icmtrx_prbsver xrx0 1 1 7
	run_cmd icmtrx_prbsstatus xrx0

	run_cmd emcaprbs_set emca1 xtx1 tx prbs7
	run_cmd icmtrx_prbsver xrx1 1 1 7
	run_cmd icmtrx_prbsstatus xrx1

	run_cmd emcaprbs_set emca1 xtx2 tx prbs7
	run_cmd icmtrx_prbsver xrx2 1 1 7
	run_cmd icmtrx_prbsstatus xrx2

	run_cmd emcaprbs_set emca1 xtx3 tx prbs7
	run_cmd icmtrx_prbsver xrx3 1 1 7
	run_cmd icmtrx_prbsstatus xrx3

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xtx0 xtx1 xtx2 xtx3 xtx4 xtx5 xtx6 xtx7 xtx8 xtx9
		do
			run_cmd icmtrx_prbsinvert $trx tx invert
		done
	fi
	run_cmd icmtrx_prbsgen xtx0 1 1 7
	run_cmd emcaprbs_set emca1 xrx0 rx prbs7
	emca_status emca1 xrx0

	run_cmd icmtrx_prbsgen xtx1 1 1 7
	run_cmd emcaprbs_set emca1 xrx1 rx prbs7
	emca_status emca1 xrx1

	run_cmd icmtrx_prbsgen xtx2 1 1 7
	run_cmd emcaprbs_set emca1 xrx2 rx prbs7
	emca_status emca1 xrx2

	run_cmd icmtrx_prbsgen xtx3 1 1 7
	run_cmd emcaprbs_set emca1 xrx3 rx prbs7
	emca_status emca1 xrx3

	run_cmd icmtrx_prbsgen xtx4 1 1 7
	run_cmd emcaprbs_set emca1 xrx4 rx prbs7
	emca_status emca1 xrx4

	run_cmd icmtrx_prbsgen xtx5 1 1 7
	run_cmd emcaprbs_set emca1 xrx5 rx prbs7
	emca_status emca1 xrx5

	run_cmd icmtrx_prbsgen xtx6 1 1 7
	run_cmd emcaprbs_set emca1 xrx6 rx prbs7
	emca_status emca1 xrx6

	run_cmd icmtrx_prbsgen xtx7 1 1 7
	run_cmd emcaprbs_set emca1 xrx7 rx prbs7
	emca_status emca1 xrx7

	run_cmd icmtrx_prbsgen xtx8 1 1 7
	run_cmd emcaprbs_set emca1 xrx8 rx prbs7
	emca_status emca1 xrx8

	run_cmd icmtrx_prbsgen xtx9 1 1 7
	run_cmd emcaprbs_set emca1 xrx9 rx prbs7
	emca_status emca1 xrx9

	## EMCA2
	run_cmd icmtrx_prbsinvert et2 tx invert
	run_cmd icmtrx_prbsinvert et2 rx invert
	run_cmd emcaprbs_set emca2 et4 tx prbs7
	run_cmd emcaprbs_set emca2 et4 rx prbs7
	run_cmd icmtrx_prbsver et2 1 1 7
	run_cmd icmtrx_prbsgen et2 1 1 7
	emca_status emca2 et4
	run_cmd icmtrx_prbsstatus et2

	run_cmd icmtrx_prbsinvert et3 tx invert
	run_cmd icmtrx_prbsinvert et3 rx invert
	run_cmd emcaprbs_set emca2 et5 tx prbs7
	run_cmd emcaprbs_set emca2 et5 rx prbs7
	run_cmd icmtrx_prbsver et3 1 1 7
	run_cmd icmtrx_prbsgen et3 1 1 7
	emca_status emca2 et5
	run_cmd icmtrx_prbsstatus et3

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xrx4 xrx5 xrx6 xrx7
		do
			run_cmd icmtrx_prbsinvert $trx rx invert
		done
	fi
	run_cmd emcaprbs_set emca2 xtx0 tx prbs7
	run_cmd icmtrx_prbsver xrx4 1 1 7
	run_cmd icmtrx_prbsstatus xrx4

	run_cmd emcaprbs_set emca2 xtx1 tx prbs7
	run_cmd icmtrx_prbsver xrx5 1 1 7
	run_cmd icmtrx_prbsstatus xrx5

	run_cmd emcaprbs_set emca2 xtx2 tx prbs7
	run_cmd icmtrx_prbsver xrx6 1 1 7
	run_cmd icmtrx_prbsstatus xrx6

	run_cmd emcaprbs_set emca2 xtx3 tx prbs7
	run_cmd icmtrx_prbsver xrx7 1 1 7
	run_cmd icmtrx_prbsstatus xrx7

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xtx10 xtx11 xtx12 xtx13
		do
			run_cmd icmtrx_prbsinvert $trx tx invert
		done
	fi
	run_cmd icmtrx_prbsgen xtx10 1 1 7
	run_cmd emcaprbs_set emca2 xrx0 rx prbs7
	emca_status emca2 xrx0

	run_cmd icmtrx_prbsgen xtx11 1 1 7
	run_cmd emcaprbs_set emca2 xrx1 rx prbs7
	emca_status emca2 xrx1

	run_cmd icmtrx_prbsgen xtx12 1 1 7
	run_cmd emcaprbs_set emca2 xrx2 rx prbs7
	emca_status emca2 xrx2

	run_cmd icmtrx_prbsgen xtx13 1 1 7
	run_cmd emcaprbs_set emca2 xrx3 rx prbs7
	emca_status emca2 xrx3
fi

if [ "$BOARDTYPE" == "DUS5201" -o "$BOARDTYPE" == "DUS3201" ]
then
	## EMCA3
	run_cmd icmtrx_prbsinvert et4 tx invert
	run_cmd icmtrx_prbsinvert et4 rx invert
	run_cmd emcaprbs_set emca3 et6 tx prbs7
	run_cmd emcaprbs_set emca3 et6 rx prbs7
	run_cmd icmtrx_prbsver et4 1 1 7
	run_cmd icmtrx_prbsgen et4 1 1 7
	emca_status emca3 et6
	run_cmd icmtrx_prbsstatus et4

	run_cmd icmtrx_prbsinvert et5 tx invert
	run_cmd icmtrx_prbsinvert et5 rx invert
	run_cmd emcaprbs_set emca3 et7 tx prbs7
	run_cmd emcaprbs_set emca3 et7 rx prbs7
	run_cmd icmtrx_prbsver et5 1 1 7
	run_cmd icmtrx_prbsgen et5 1 1 7
	emca_status emca3 et7
	run_cmd icmtrx_prbsstatus et5

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xrx8 xrx9 xrx10 xrx11 xrx12 xrx13 xrx14 xrx15 xrx16 xrx17
		do
			run_cmd icmtrx_prbsinvert $trx rx invert
		done
	fi
	run_cmd emcaprbs_set emca3 xtx0 tx prbs7
	run_cmd icmtrx_prbsver xrx8 1 1 7
	run_cmd icmtrx_prbsstatus xrx8

	run_cmd emcaprbs_set emca3 xtx1 tx prbs7
	run_cmd icmtrx_prbsver xrx9 1 1 7
	run_cmd icmtrx_prbsstatus xrx9

	run_cmd emcaprbs_set emca3 xtx2 tx prbs7
	run_cmd icmtrx_prbsver xrx10 1 1 7
	run_cmd icmtrx_prbsstatus xrx10

	run_cmd emcaprbs_set emca3 xtx3 tx prbs7
	run_cmd icmtrx_prbsver xrx11 1 1 7
	run_cmd icmtrx_prbsstatus xrx11

	if [ "$BOARDTYPE" == "DUS5201" ]
	then
		run_cmd emcaprbs_set emca3 xtx4 tx prbs7
		run_cmd icmtrx_prbsver xrx12 1 1 7
		run_cmd icmtrx_prbsstatus xrx12

		run_cmd emcaprbs_set emca3 xtx5 tx prbs7
		run_cmd icmtrx_prbsver xrx13 1 1 7
		run_cmd icmtrx_prbsstatus xrx13
	
		run_cmd emcaprbs_set emca3 xtx6 tx prbs7
		run_cmd icmtrx_prbsver xrx14 1 1 7
		run_cmd icmtrx_prbsstatus xrx14
	
		run_cmd emcaprbs_set emca3 xtx7 tx prbs7
		run_cmd icmtrx_prbsver xrx15 1 1 7
		run_cmd icmtrx_prbsstatus xrx15
	
		run_cmd emcaprbs_set emca3 xtx8 tx prbs7
		run_cmd icmtrx_prbsver xrx16 1 1 7
		run_cmd icmtrx_prbsstatus xrx16
	
		run_cmd emcaprbs_set emca3 xtx9 tx prbs7
		run_cmd icmtrx_prbsver xrx17 1 1 7
		run_cmd icmtrx_prbsstatus xrx17
	fi

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xtx14 xtx15 xtx16 xtx17 xtx18 xtx19
		do
			run_cmd icmtrx_prbsinvert $trx tx invert
		done
	fi
	run_cmd icmtrx_prbsgen xtx14 1 1 7
	run_cmd emcaprbs_set emca3 xrx0 rx prbs7
	emca_status emca3 xrx0

	run_cmd icmtrx_prbsgen xtx15 1 1 7
	run_cmd emcaprbs_set emca3 xrx1 rx prbs7
	emca_status emca3 xrx1

	run_cmd icmtrx_prbsgen xtx16 1 1 7
	run_cmd emcaprbs_set emca3 xrx2 rx prbs7
	emca_status emca3 xrx2

	run_cmd icmtrx_prbsgen xtx17 1 1 7
	run_cmd emcaprbs_set emca3 xrx3 rx prbs7
	emca_status emca3 xrx3

	run_cmd icmtrx_prbsgen xtx18 1 1 7
	run_cmd emcaprbs_set emca3 xrx4 rx prbs7
	emca_status emca3 xrx4

	run_cmd icmtrx_prbsgen xtx19 1 1 7
	run_cmd emcaprbs_set emca3 xrx5 rx prbs7
	emca_status emca3 xrx5

	## EMCA4
	run_cmd icmtrx_prbsinvert et6 tx invert
	run_cmd icmtrx_prbsinvert et6 rx invert
	run_cmd emcaprbs_set emca4 et6 tx prbs7
	run_cmd emcaprbs_set emca4 et6 rx prbs7
	run_cmd icmtrx_prbsver et6 1 1 7
	run_cmd icmtrx_prbsgen et6 1 1 7
	emca_status emca4 et6
	run_cmd icmtrx_prbsstatus et6

	run_cmd icmtrx_prbsinvert et7 tx invert
	run_cmd icmtrx_prbsinvert et7 rx invert
	run_cmd emcaprbs_set emca4 et7 tx prbs7
	run_cmd emcaprbs_set emca4 et7 rx prbs7
	run_cmd icmtrx_prbsver et7 1 1 7
	run_cmd icmtrx_prbsgen et7 1 1 7
	emca_status emca4 et7
	run_cmd icmtrx_prbsstatus et7

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xrx18 xrx19 xrx20 xrx21 xrx22
		do
			run_cmd icmtrx_prbsinvert $trx rx invert
		done
	fi
	run_cmd emcaprbs_set emca4 xtx0 tx prbs7
	run_cmd icmtrx_prbsver xrx18 1 1 7
	run_cmd icmtrx_prbsstatus xrx18

	run_cmd emcaprbs_set emca4 xtx1 tx prbs7
	run_cmd icmtrx_prbsver xrx19 1 1 7
	run_cmd icmtrx_prbsstatus xrx19

	run_cmd emcaprbs_set emca4 xtx2 tx prbs7
	run_cmd icmtrx_prbsver xrx20 1 1 7
	run_cmd icmtrx_prbsstatus xrx20

	run_cmd emcaprbs_set emca4 xtx3 tx prbs7
	run_cmd icmtrx_prbsver xrx21 1 1 7
	run_cmd icmtrx_prbsstatus xrx21

	run_cmd emcaprbs_set emca4 xtx4 tx prbs7
	run_cmd icmtrx_prbsver xrx22 1 1 7
	run_cmd icmtrx_prbsstatus xrx22

	if [ "$ICMTYPE" == "ARRIA10" ]
	then
		for trx in xtx20 xtx21 xtx22 xtx23
		do
			run_cmd icmtrx_prbsinvert $trx tx invert
		done
	fi
	run_cmd icmtrx_prbsgen xtx20 1 1 7
	run_cmd emcaprbs_set emca4 xrx0 rx prbs7
	emca_status emca4 xrx0

	run_cmd icmtrx_prbsgen xtx21 1 1 7
	run_cmd emcaprbs_set emca4 xrx1 rx prbs7
	emca_status emca4 xrx1

	run_cmd icmtrx_prbsgen xtx22 1 1 7
	run_cmd emcaprbs_set emca4 xrx2 rx prbs7
	emca_status emca4 xrx2

	run_cmd icmtrx_prbsgen xtx23 1 1 7
	run_cmd emcaprbs_set emca4 xrx3 rx prbs7
	emca_status emca4 xrx3
fi

run_cmd icmtrx_power et0 0 q
run_cmd icmtrx_power et1 0 q
run_cmd icmtrx_power et2 0 q
run_cmd icmtrx_power et3 0 q
run_cmd icmtrx_power et4 0 q
run_cmd icmtrx_power et5 0 q
run_cmd icmtrx_power et6 0 q
run_cmd icmtrx_power et7 0 q

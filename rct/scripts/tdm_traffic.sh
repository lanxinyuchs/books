#!/bin/bash
if [ "$1" == "" ]
then
    echo "usage: ./tdm_traffic.sh init internal |init external| start |  status  | stop "
    echo "failed"
    exit 0
fi


if [ "$1" == "init" ]
then

    echo "Configure tdm transceivers and enable tdm phy"
    testbox tdm_trx_configure normal
    testbox tdm_phy_enable
    testbox tdmphy_configure  0 0 E1
    testbox tdmphy_configure  0 1 E1
    testbox tdmphy_configure  0 2 E1
    testbox tdmphy_configure  0 3 E1
    testbox tdmphy_configure  1 0 E1
    testbox tdmphy_configure  1 1 E1
    testbox tdmphy_configure  1 2 E1
    testbox tdmphy_configure  1 3 E1
if [ "$2" == "internal" ]
then
    echo "Loopback the packets at tdm phy"
    testbox tdmphy_loopback   0 0 ddl
    testbox tdmphy_loopback   0 1 ddl
    testbox tdmphy_loopback   0 2 ddl
    testbox tdmphy_loopback   0 3 ddl
    testbox tdmphy_loopback   1 0 ddl
    testbox tdmphy_loopback   1 1 ddl
    testbox tdmphy_loopback   1 2 ddl
    testbox tdmphy_loopback   1 3 ddl
fi
if [ "$2" == "external" ]
then
    echo "Loopback the packets at tdm phy external"
    testbox tdmphy_loopback   0 0 off
    testbox tdmphy_loopback   0 1 off
    testbox tdmphy_loopback   0 2 off
    testbox tdmphy_loopback   0 3 off
    testbox tdmphy_loopback   1 0 off
    testbox tdmphy_loopback   1 1 off
    testbox tdmphy_loopback   1 2 off
    testbox tdmphy_loopback   1 3 off
fi
    echo "open the channels for each tdm port"
    testbox tdm_open_sc 0 0  1  7
    testbox tdm_open_sc 0 1  8 15
    testbox tdm_open_sc 0 2 16 23
    testbox tdm_open_sc 0 3 24 31
    testbox tdm_open_sc 1 0  1  7
    testbox tdm_open_sc 1 1  8 15
    testbox tdm_open_sc 1 2 16 23
    testbox tdm_open_sc 1 3 24 31
    testbox tdm_open_sc 2 0  1  7
    testbox tdm_open_sc 2 1  8 15
    testbox tdm_open_sc 2 2 16 23
    testbox tdm_open_sc 2 3 24 31
    testbox tdm_open_sc 3 0  1  7
    testbox tdm_open_sc 3 1  8 15
    testbox tdm_open_sc 3 2 16 23
    testbox tdm_open_sc 3 3 24 31
    testbox tdm_open_sc 4 0  1  7
    testbox tdm_open_sc 4 1  8 15
    testbox tdm_open_sc 4 2 16 23
    testbox tdm_open_sc 4 3 24 31
    testbox tdm_open_sc 5 0  1  7
    testbox tdm_open_sc 5 1  8 15
    testbox tdm_open_sc 5 2 16 23
    testbox tdm_open_sc 5 3 24 31
    testbox tdm_open_sc 6 0  1  7
    testbox tdm_open_sc 6 1  8 15
    testbox tdm_open_sc 6 2 16 23
    testbox tdm_open_sc 6 3 24 31
    testbox tdm_open_sc 7 0  1  7
    testbox tdm_open_sc 7 1  8 15
    testbox tdm_open_sc 7 2 16 23
    testbox tdm_open_sc 7 3 24 31
fi

if [ "$1" == "start" ]
then
echo "Load the npu frog to measure npu statistics"
testbox npu_load frog

echo "start receiving tdm traffic for each tdm port and channel"
testbox tdmtrf_rx start 0 0 -v -f
testbox tdmtrf_rx start 0 1 -v -f
testbox tdmtrf_rx start 0 2 -v -f
testbox tdmtrf_rx start 0 3 -v -f
testbox tdmtrf_rx start 1 0 -v -f
testbox tdmtrf_rx start 1 1 -v -f
testbox tdmtrf_rx start 1 2 -v -f
testbox tdmtrf_rx start 1 3 -v -f
testbox tdmtrf_rx start 2 0 -v -f
testbox tdmtrf_rx start 2 1 -v -f
testbox tdmtrf_rx start 2 2 -v -f
testbox tdmtrf_rx start 2 3 -v -f
testbox tdmtrf_rx start 3 0 -v -f
testbox tdmtrf_rx start 3 1 -v -f
testbox tdmtrf_rx start 3 2 -v -f
testbox tdmtrf_rx start 3 3 -v -f
testbox tdmtrf_rx start 4 0 -v -f
testbox tdmtrf_rx start 4 1 -v -f
testbox tdmtrf_rx start 4 2 -v -f
testbox tdmtrf_rx start 4 3 -v -f
testbox tdmtrf_rx start 5 0 -v -f
testbox tdmtrf_rx start 5 1 -v -f
testbox tdmtrf_rx start 5 2 -v -f
testbox tdmtrf_rx start 5 3 -v -f
testbox tdmtrf_rx start 6 0 -v -f
testbox tdmtrf_rx start 6 1 -v -f
testbox tdmtrf_rx start 6 2 -v -f
testbox tdmtrf_rx start 6 3 -v -f
testbox tdmtrf_rx start 7 0 -v -f
testbox tdmtrf_rx start 7 1 -v -f
testbox tdmtrf_rx start 7 2 -v -f
testbox tdmtrf_rx start 7 3 -v -f

echo "start sending tdm traffic to each channel of every tdm port"
testbox tdmtrf_tx start 10 100 100 0 0
testbox tdmtrf_tx start 10 100 100 0 1
testbox tdmtrf_tx start 10 100 100 0 2
testbox tdmtrf_tx start 10 100 100 0 3
testbox tdmtrf_tx start 10 100 100 1 0
testbox tdmtrf_tx start 10 100 100 1 1
testbox tdmtrf_tx start 10 100 100 1 2
testbox tdmtrf_tx start 10 100 100 1 3
testbox tdmtrf_tx start 10 100 100 2 0
testbox tdmtrf_tx start 10 100 100 2 1
testbox tdmtrf_tx start 10 100 100 2 2
testbox tdmtrf_tx start 10 100 100 2 3
testbox tdmtrf_tx start 10 100 100 3 0
testbox tdmtrf_tx start 10 100 100 3 1
testbox tdmtrf_tx start 10 100 100 3 2
testbox tdmtrf_tx start 10 100 100 3 3
testbox tdmtrf_tx start 10 100 100 4 0
testbox tdmtrf_tx start 10 100 100 4 1
testbox tdmtrf_tx start 10 100 100 4 2
testbox tdmtrf_tx start 10 100 100 4 3
testbox tdmtrf_tx start 10 100 100 5 0
testbox tdmtrf_tx start 10 100 100 5 1
testbox tdmtrf_tx start 10 100 100 5 2
testbox tdmtrf_tx start 10 100 100 5 3
testbox tdmtrf_tx start 10 100 100 6 0
testbox tdmtrf_tx start 10 100 100 6 1
testbox tdmtrf_tx start 10 100 100 6 2
testbox tdmtrf_tx start 10 100 100 6 3
testbox tdmtrf_tx start 10 100 100 7 0
testbox tdmtrf_tx start 10 100 100 7 1
testbox tdmtrf_tx start 10 100 100 7 2
testbox tdmtrf_tx start 10 100 100 7 3
fi

if [ "$1" == "status" ]
then
    echo "verifying tdm traffic statistics by getting stats from each counter(EA,TRX,ETH1 and TDMI)"
    testbox tdmtrf_stats ea all
    testbox tdm_stats trx
    testbox tdm_stats eth1g
    testbox tdm_stats tdmi
fi

if [ "$1" == "stop" ]
then
    echo "stopping tdm traffic from transmitter side"
    sleep 3
    testbox tdmtrf_tx stop 0 0
    testbox tdmtrf_tx stop 0 1
    testbox tdmtrf_tx stop 0 2
    testbox tdmtrf_tx stop 0 3
    testbox tdmtrf_tx stop 1 0
    testbox tdmtrf_tx stop 1 1
    testbox tdmtrf_tx stop 1 2
    testbox tdmtrf_tx stop 1 3
    testbox tdmtrf_tx stop 2 0
    testbox tdmtrf_tx stop 2 1
    testbox tdmtrf_tx stop 2 2
    testbox tdmtrf_tx stop 2 3
    testbox tdmtrf_tx stop 3 0
    testbox tdmtrf_tx stop 3 1
    testbox tdmtrf_tx stop 3 2
    testbox tdmtrf_tx stop 3 3
    testbox tdmtrf_tx stop 4 0
    testbox tdmtrf_tx stop 4 1
    testbox tdmtrf_tx stop 4 2
    testbox tdmtrf_tx stop 4 3
    testbox tdmtrf_tx stop 5 0
    testbox tdmtrf_tx stop 5 1
    testbox tdmtrf_tx stop 5 2
    testbox tdmtrf_tx stop 5 3
    testbox tdmtrf_tx stop 6 0
    testbox tdmtrf_tx stop 6 1
    testbox tdmtrf_tx stop 6 2
    testbox tdmtrf_tx stop 6 3
    testbox tdmtrf_tx stop 7 0
    testbox tdmtrf_tx stop 7 1 
    testbox tdmtrf_tx stop 7 2
    testbox tdmtrf_tx stop 7 3
    echo "sleep for 4seconds to allow receiver to get remaining packets"
    sleep 5
    echo "stopping tdm traffic from reciver side"
    testbox tdmtrf_rx stop 0 0
    testbox tdmtrf_rx stop 0 1
    testbox tdmtrf_rx stop 0 2
    testbox tdmtrf_rx stop 0 3
    testbox tdmtrf_rx stop 1 0
    testbox tdmtrf_rx stop 1 1
    testbox tdmtrf_rx stop 1 2
    testbox tdmtrf_rx stop 1 3
    testbox tdmtrf_rx stop 2 0
    testbox tdmtrf_rx stop 2 1
    testbox tdmtrf_rx stop 2 2
    testbox tdmtrf_rx stop 2 3
    testbox tdmtrf_rx stop 3 0
    testbox tdmtrf_rx stop 3 1
    testbox tdmtrf_rx stop 3 2
    testbox tdmtrf_rx stop 3 3
    testbox tdmtrf_rx stop 4 0
    testbox tdmtrf_rx stop 4 1
    testbox tdmtrf_rx stop 4 2
    testbox tdmtrf_rx stop 4 3
    testbox tdmtrf_rx stop 5 0
    testbox tdmtrf_rx stop 5 1
    testbox tdmtrf_rx stop 5 2
    testbox tdmtrf_rx stop 5 3
    testbox tdmtrf_rx stop 6 0
    testbox tdmtrf_rx stop 6 1
    testbox tdmtrf_rx stop 6 2
    testbox tdmtrf_rx stop 6 3
    testbox tdmtrf_rx stop 7 0
    testbox tdmtrf_rx stop 7 1
    testbox tdmtrf_rx stop 7 2
    testbox tdmtrf_rx stop 7 3
fi

#!/bin/bash
if [ "$1" == "" ] 
then
    echo "usage: ./traffic_loopback.sh <init <tcu03|tcu04|dus52> <internal|external> |  status <tcu03|tcu04|dus52> | stop <tcu03|tcu04|dus52> <internal|external> | clear <tcu03|tcu04> "
    echo "failed"
    exit 0
fi

if [ "$1" == "init" ]
then
    
    testbox --verbose_level 2   
    testbox --log_clear
    testbox --clear all
    sleep 2

    testbox npu_load frog
    sleep 5
    
    echo "Setting up the tn ports"
    testbox tnport_setup tna
    testbox tnport_setup tnb
    testbox tnport_setup tnc
    if [ "$2" == "tcu03" ]
    then
      testbox tnport_setup tnd
      testbox tnport_setup tne
      testbox tnport_setup tnf
      testbox tnport_setup tng
      testbox tnport_setup tnh
      testbox tnport_setup tnj
      testbox tnport_setup tnk
      testbox tnport_setup tnl
    fi 	
    if [ "$2" == "tcu04" ]
    then
      testbox tnport_setup tnd
      testbox tnport_setup tne
      testbox tnport_setup tnf
      testbox tnport_setup tng
      testbox tnport_setup tnh
      testbox tnport_setup tnj
      testbox tnport_setup tnk
      testbox tnport_setup tnl
      testbox tnport_setup tnm
      testbox tnport_setup tnn
    fi 	

	
    if [ "$3" == "internal" ]
    then
      echo "Enabling the internal loop back"

      testbox npumac_loop tna internal
      testbox npumac_loop tnb internal
      testbox npumac_loop tnc internal
      if [ "$2" == "tcu03" ]
      then
        testbox npumac_loop tnd internal
        testbox npumac_loop tne internal
        testbox npumac_loop tnf internal
        testbox npumac_loop tng internal
        testbox npumac_loop tnh internal
        testbox npumac_loop tnj internal
        testbox npumac_loop tnk internal
        testbox npumac_loop tnl internal
      fi
      if [ "$2" == "tcu04" ]
      then
        testbox npumac_loop tnd internal
        testbox npumac_loop tne internal
        testbox npumac_loop tnf internal
        testbox npumac_loop tng internal
        testbox npumac_loop tnh internal
        testbox npumac_loop tnj internal
        testbox npumac_loop tnk internal
        testbox npumac_loop tnl internal
        testbox npumac_loop tnm internal
        testbox npumac_loop tnn internal
      fi
    
    fi   
   
    if [ "$3" == "external" ]
    then
      
       if [ "$2" == "dus52" ]
       then  
         echo "phy_mode of tna set to loop mode"
         testbox phy_mode tna loop 
       fi
       
       
       if [ "$2" == "tcu03" ]
       then  
         echo "phy_mode of tnj, tnk, tnl set to loop mode"
         testbox phy_mode tnj loop 
         testbox phy_mode tnk loop
         testbox phy_mode tnl loop
       fi	

       if [ "$2" == "tcu04" ]
       then  
         echo "phy_mode of tnk, tnl, tnm, tnn set to loop mode"
         testbox phy_mode tnk loop 
         testbox phy_mode tnl loop
         testbox phy_mode tnm loop
         testbox phy_mode tnn loop
      fi	 
    fi    
   
    echo "Starting the transimission and reception "
    testbox nputrf_rxstart tna count
    testbox nputrf_rxstart tnb count
    testbox nputrf_rxstart tnc count
    
    sleep 1
    
    testbox nputrf_txstart tnc 2000 1000
    testbox nputrf_txstart tna 2000 1000
    testbox nputrf_txstart tnb 2000 1000
    
 
    if [ "$2" == "tcu03" ]
    then
      testbox nputrf_rxstart tnd count
      testbox nputrf_rxstart tne count
      testbox nputrf_rxstart tnf count
      testbox nputrf_rxstart tng count
      testbox nputrf_rxstart tnh count
      testbox nputrf_rxstart tnj count
      testbox nputrf_rxstart tnk count
      testbox nputrf_rxstart tnl count
      
      sleep 1
      
       testbox nputrf_txstart tnd 2000 1000
       testbox nputrf_txstart tne 2000 1000
       testbox nputrf_txstart tnf 2000 1000
       testbox nputrf_txstart tng 2000 1000
       testbox nputrf_txstart tnh 2000 1000
       testbox nputrf_txstart tnj 2000 1000
       testbox nputrf_txstart tnk 2000 1000
       testbox nputrf_txstart tnl 2000 1000
       
       
    fi

    if [ "$2" == "tcu04" ]
    then
      testbox nputrf_rxstart tnd count
      testbox nputrf_rxstart tne count
      testbox nputrf_rxstart tnf count
      testbox nputrf_rxstart tng count
      testbox nputrf_rxstart tnh count
      testbox nputrf_rxstart tnj count
      testbox nputrf_rxstart tnk count
      testbox nputrf_rxstart tnl count
      testbox nputrf_rxstart tnm count 
      testbox nputrf_rxstart tnn count
      
      sleep 1 
      
      testbox nputrf_txstart tnd 2000 1000
      testbox nputrf_txstart tne 2000 1000
      testbox nputrf_txstart tnf 2000 1000
      testbox nputrf_txstart tng 2000 1000
      testbox nputrf_txstart tnh 2000 1000
      testbox nputrf_txstart tnj 2000 1000
      testbox nputrf_txstart tnk 2000 1000
      testbox nputrf_txstart tnl 2000 1000
      testbox nputrf_txstart tnm 2000 1000
      testbox nputrf_txstart tnn 2000 1000
	
   	fi

    sleep 3
fi

## Status
if [ "$1" == "status" ]
then
## Displays the MAC statistics counters for the selected port

    echo "---- tna -------------"
    testbox nputrf_stats tna
    echo""
    echo "---- tnb -------------"
    testbox nputrf_stats tnb
    echo""
    echo "---- tnc -------------"
    testbox nputrf_stats tnc
    echo""
    if [ "$2" == "tcu03" ]
    then
      echo "---- tnd -------------"
      testbox nputrf_stats tnd
      echo""
      echo "---- tne -------------"
      testbox nputrf_stats tne
      echo""
      echo "---- tnf -------------"
      testbox nputrf_stats tnf
      echo""
      echo "---- tng -------------"
      testbox nputrf_stats tng
      echo""
      echo "---- tnh -------------"
      testbox nputrf_stats tnh
      echo""
      echo "---- tnj -------------"
      testbox nputrf_stats tnj
      echo""
      echo "---- tnk -------------"
      testbox nputrf_stats tnk
      echo""
      echo "---- tnl -------------"
      testbox nputrf_stats tnl
      echo""
    fi
    if [ "$2" == "tcu04" ]
    then
      echo "---- tnd -------------"
      testbox nputrf_stats tnd
      echo""
      echo "---- tne -------------"
      testbox nputrf_stats tne
      echo""
      echo "---- tnf -------------"
      testbox nputrf_stats tnf
      echo""
      echo "---- tng -------------"
      testbox nputrf_stats tng
      echo""
      echo "---- tnh -------------"
      testbox nputrf_stats tnh
      echo""
      echo "---- tnj -------------"
      testbox nputrf_stats tnj
      echo""
      echo "---- tnk -------------"
      ##testbox nputrf_stats tnk
      echo"tnk tnl tnm tnn are commented "
      ##echo "---- tnl -------------"
      ##testbox nputrf_stats tnl
      ##echo""
      ##echo "---- tnm -------------"
      ##testbox nputrf_stats tnm
      ## echo""
     ##echo "---- tnn -------------"
      ##testbox nputrf_stats tnn
      ##echo""
    fi    
fi

## Stop
if [ "$1" == "stop" ]
then
    echo "Stopping the transmission and reception of the packets"
      testbox nputrf_txstop all
      sleep 3
      testbox nputrf_rxstop all
    
    if [ "$3" == "internal" ]
    then

      echo "Disabling the internal loop back"

      testbox npumac_loop tna no
      testbox npumac_loop tnb no
      testbox npumac_loop tnc no
      if [ "$2" == "tcu03" ]
      then
        testbox npumac_loop tnd no
        testbox npumac_loop tne no
        testbox npumac_loop tnf no
        testbox npumac_loop tng no
        testbox npumac_loop tnh no
        testbox npumac_loop tnj no
        testbox npumac_loop tnk no
        testbox npumac_loop tnl no
      fi
      if [ "$2" == "tcu04" ]
      then
        testbox npumac_loop tnd no
        testbox npumac_loop tne no
        testbox npumac_loop tnf no
        testbox npumac_loop tng no
        testbox npumac_loop tnh no
        testbox npumac_loop tnj no
        testbox npumac_loop tnk no
        testbox npumac_loop tnl no
        testbox npumac_loop tnm no
        testbox npumac_loop tnn no
      fi   
    fi
    
    if [ "$3" == "external" ]
    then
       if [ "$2" == "dus52" ]
       then
       testbox phy_mode tna normal
       fi
       if [ "$2" == "tcu03" ]
       then  
         echo "phy_mode of tnj, tnk, tnl setback to normal mode"
         testbox phy_mode tnj normal
         testbox phy_mode tnk normal
         testbox phy_mode tnl normal
       fi	 
       if [ "$2" == "tcu04" ]
       then  
         echo "phy_mode of tnk, tnl, tnm, tnn setback to normal mode"
         testbox phy_mode tnk normal
         testbox phy_mode tnl normal
         testbox phy_mode tnm normal
         testbox phy_mode tnn normal
       fi	
    fi 

fi
## Clear
if [ "$1" == "clear" ]
then
       echo "Clearing the statistics"
    testbox nputrf_stats tna clear
    testbox nputrf_stats tnb clear
    testbox nputrf_stats tnc clear
    if [ "$2" == "tcu03" ]
    then
      testbox nputrf_stats tnd clear
      testbox nputrf_stats tne clear
      testbox nputrf_stats tnf clear
      testbox nputrf_stats tng clear
      testbox nputrf_stats tnh clear
      testbox nputrf_stats tnj clear
      testbox nputrf_stats tnk clear
      testbox nputrf_stats tnl clear
    fi	
    if [ "$2" == "tcu04" ]
    then
      testbox nputrf_stats tnd clear
      testbox nputrf_stats tne clear
      testbox nputrf_stats tnf clear
      testbox nputrf_stats tng clear
      testbox nputrf_stats tnh clear
      testbox nputrf_stats tnj clear
      testbox nputrf_stats tnk clear
      testbox nputrf_stats tnl clear
      testbox nputrf_stats tnm clear
      testbox nputrf_stats tnn clear
    fi	
fi


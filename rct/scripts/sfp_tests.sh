#!/bin/bash
if [ "$1" == "" ]
then
    echo "usage: ./sfp_tests.sh <tcu03|tcu04|dus52>"
    echo "failed"
    exit 0
fi
if [ "$1" == "tcu03" ]
then
testbox sfp_power tna on
testbox sfp_dump tna eeprom
testbox sfp_power tna off
testbox sfp_power tnb on
testbox sfp_dump tnb eeprom
testbox sfp_power tnb off
testbox sfp_power tnc on
testbox sfp_dump tnc eeprom
testbox sfp_power tnc off
testbox sfp_power tnd on
testbox sfp_dump tnd eeprom
testbox sfp_power tnd off
testbox sfp_power tne on
testbox sfp_dump tne eeprom
testbox sfp_power tne off
testbox sfp_power tnf on
testbox sfp_dump tnf eeprom
testbox sfp_power tnf off
testbox sfp_power tng on
testbox sfp_dump tng eeprom
testbox sfp_power tng off
testbox sfp_power tnh on
testbox sfp_dump tnh eeprom
testbox sfp_power tnh off
fi
if [ "$1" == "tcu04" ]
then
testbox sfp_power tna on
testbox sfp_dump tna eeprom
testbox sfp_power tna off
testbox sfp_power tnb on
testbox sfp_dump tnb eeprom
testbox sfp_power tnb off
testbox sfp_power tnc on
testbox sfp_dump tnc eeprom
testbox sfp_power tnc off
testbox sfp_power tnd on
testbox sfp_dump tnd eeprom
testbox sfp_power tnd off
testbox sfp_power tne on
testbox sfp_dump tne eeprom
testbox sfp_power tne off
testbox sfp_power tnf on
testbox sfp_dump tnf eeprom
testbox sfp_power tnf off
testbox sfp_power tng on
testbox sfp_dump tng eeprom
testbox sfp_power tng off
testbox sfp_power tnh on
testbox sfp_dump tnh eeprom
testbox sfp_power tnh off
fi
if [ "$1" == "dus52" ]
then
testbox sfp_power tnb on
testbox sfp_power tnb off
testbox sfp_power tnc on
testbox sfp_power tnc off

fi


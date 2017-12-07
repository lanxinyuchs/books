#! /bin/bash
## 
## %CCaseFile:	cxpMetaGen.sh %
## %CCaseRev:	/main/R2A/1 %
## %CCaseDate:	2013-08-16 %
## Author: <name>, <e-mail address>
## 
## Purpose: Generate a CXP metadata file
## 
## Dependencies: None. The output is valid XML but readability
## may be improved by filtering through `xmllint --format -'.
## 
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2013 All rights reserved.
## 
## The information in this document is the property of Ericsson.
## 
## Except as specifically authorized in writing by Ericsson, the 
## receiver of this document shall keep the information contained 
## herein confidential and shall protect the same in whole or in 
## part from disclosure and dissemination to third parties.
## 
## Disclosure and disseminations to the receivers employees shall 
## only be made on a strict need to know basis.
## %CCaseCopyrightEnd%
## 
## ----------------------------------------------------------------------
## 
## Revision history:
## 
## Rev        Date       Name        What
## -----      -------    --------    ------------------------------------
## 
## ----------------------------------------------------------------------

set -o nounset

# Script name
declare -r Script=cxpMetaGen.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  printf "Usage is: cxpMetaGen.sh OPTIONS...\n"
  printf "Options are:\n"
  printf "  -h                        this help\n"
  printf "  -N CxpProdName            CXP product name\n"
  printf "  -I CxpProdId              CXP product id\n"
  printf "  -V CxpProdVersion         CXP product version\n"
  printf "  -D CxpDate                CXP date\n"
  printf "\n"
  printf "  -X CxpName+CxpId+CxpVer+AppDataSpecs\n"
  printf "\n"
  printf "AppDataSpecs is a semicolon-separated set of relpath,filename\n"
  printf "specifications. Because of semicolons the entire argument\n"
  printf "needs to be quoted.\n"
  printf "\n"
  printf "Embedded space in options are not allowed. Pipe through\n"
  printf "'xmllint --format -' for proper formatting.\n"
  printf "\n"
}

# Print string to stderr and exit nonzero
function die() {
  printf "$Script: FATAL: $1\n" >&2
}

# Print string to stderr as info message
function info() {
  printf "$Script: INFO: $1\n" >&2
}

# Print string to stderr as warning
function warning() {
  printf "$Script: WARNING: $1\n" >&2
}



function xmlPrelude() {
  printf "%s\n" "<?xml version='1.0' encoding='ISO-8859-1'?>"
}

function product() {
  printf "<product name='%s' id='%s' version='%s'/>\n" $1 $2 $3
}

function date() {
  printf "<date>%s</date>\n" $1
}

function description() {
  printf "<description>none</description>\n"
}

function type() {
  printf "<type>none</type>\n"
}

function framework() {
  printf "<framework>\n"
  product RCPI CXS101547 R2A
  printf "</framework>\n"
}

function productCxc() {
  printf "<product name='%s' id='%s' version='%s'>\n" $1 $2 $3
  echo $4 \
  | tr ';' '\n' \
  | (while read U
       do
         echo $U \
         | (IFS=',' read P F
            printf "<appdata relpath='%s' file='%s'/>\n" $P $F
           )
       done
    )
  printf "</product>\n"
}


function contentinfo() {
  printf "<contentinfo>\n"
  for cxc in $CxcSpecs; do
    echo $cxc | (IFS='+' read N I V FF && productCxc $N $I $V $FF)
  done
  printf "</contentinfo>\n"
}

function configuration() {
  printf "%s\n" "<configuration>"
  product $CxpName $CxpId $CxpVersion
  date $CxpDate
  description
  type
  framework
  contentinfo
  printf "%s\n" "</configuration>"
}



########################################################################
# Execution begins here


declare OptionPatterns=""
OptionPatterns+="h"
OptionPatterns+="N:"
OptionPatterns+="I:"
OptionPatterns+="V:"
OptionPatterns+="D:"
OptionPatterns+="X:"


declare CxpName=undefined
declare CxpId=undefined
declare CxpVersion=undefined
declare CxpDate=undefined

declare CxpSpecs=""

while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    N)
      CxpName=$OPTARG;;
    I)
      CxpId=$OPTARG;;
    V)
      CxpVersion=$OPTARG;;
    D)
      CxpDate=$OPTARG;;
    X)
      CxcSpecs+=" $OPTARG";;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

xmlPrelude
configuration

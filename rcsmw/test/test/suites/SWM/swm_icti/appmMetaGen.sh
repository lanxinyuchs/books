#! /bin/bash
## 
## %CCaseFile:	appmMetaGen.sh %
## %CCaseRev:	/main/R2A/4 %
## %CCaseDate:	2013-08-26 %
## Author: <name>, <e-mail address>
## 
## Purpose: Generate an appm (LMHI) metadata file
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
## R2A/1      2013-08-22 erarafo     First version
## R2A/2      2013-08-23 erarafo     Target architecture as parameter
## R2A/3      2013-08-26 erarafo     Corrected handling of i386 and i686
## R2A/4      2013-08-26 erarafo     Shared library supported
## ----------------------------------------------------------------------


# Script name
declare -r Script=appmMetaGen.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  printf "Usage is: $Script OPTIONS...\n"
  printf "Options are:\n"
  printf "  -h                        this help\n"
  printf "  -P ProgramSpec            program specification\n"
  printf "\n"
  printf "The ProgramSpec option may be repeated. Each option is a colon-delimited\n"
  printf "aggregate of tag, name, id, arch, relpath.\n"
  printf "\n"
  printf "The tag is 'local' for an executable and 'shared_lib' for a shared library.\n"
  printf "\n"
  printf "Allowed architectures are: powerpc, armhf, i386 (i686 is allowed\n"
  printf "for backward compatibility).\n"
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


function loadmodule() { 
  for PS in $*; do
    echo $PS \
    | (IFS=':' read T N I A P
       case $A in
         powerpc|armhf|i386)
           true;;
         i686)          # allowed for backward compatibility
           true;;
         *)
           die "unknown target architecture: $A"
       esac
       printf "<loadmodule tag='$T' name='$N' id='$I' rev='R1A1' date='2013-06-20T00:00:00'>\n"
       printf "<file type='$A' relpath='$P'/>\n"
       printf "</loadmodule>\n"
      )
  done
}


function appdata() {
  local -r target=$1; shift

  printf "<appdata target='%s'>\n" $target
  loadmodule $*
  printf "</appdata>\n"
}



########################################################################
# Execution begins here


declare OptionPatterns=""
OptionPatterns+="h"
OptionPatterns+="N:"
OptionPatterns+="I:"
OptionPatterns+="P:"


declare ProgramSpecs=""

while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    P)
      ProgramSpecs+=" $OPTARG";;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

xmlPrelude
appdata appm $ProgramSpecs

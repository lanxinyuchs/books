#! /bin/bash
##
## %CCaseFile:	gmfMetaGen.sh %
## %CCaseRev:	/main/R2A/4 %
## %CCaseDate:	2013-10-28 %
## Author: <name>, <e-mail address>
##
## Purpose: Generate a MIM or IMM metadata file
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
## R2A/2      2013-08-22 erarafo     Corrected value of Script
## R2A/3      2013-09-05 erarafo     Added NONE feature for schema name
## R2A/4      2013-10-23 erarafo     Support for version.release.correction
## ----------------------------------------------------------------------


# Script name
declare -r Script=gmfMetaGen.sh

# Script directory
declare -r ScriptDir=`dirname $0`

function help() {
  printf "Usage is: cxsMetaGen.sh OPTIONS...\n"
  printf "Options are:\n"
  printf "  -h                        this help\n"
  printf "  -T Target                 gmfMim or gmfImm\n"
  printf "  -X MimInfoSpec            file+path+ifType\n"
  printf "  -Y ImmInfoSpec            file+path+fileType (no versioning)\n"
  printf "  -Z ImmInfoSpecExtended    file+path+fileType+schemaName+ver+rel+corr+from\n"
  printf "\n"
  printf "'corr' may be NONE or a non-negative integer\n"
  printf "\n"
  printf "'from' may be NONE or a list like 3.7,4,5\n"
  printf "\n"
  printf "Embedded space in options are not allowed. Pipe through\n"
  printf "'xmllint --format -' for proper formatting.\n"
  printf "\n"
  printf "For the -Z option a schemaName of NONE causes the schema name\n"
  printf "to be an empty string (for negative tests only).\n"
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


function fromVersions() {
  if [[ "$1" == NONE ]]; then
    printf "<fromVersions/>"
  else
    printf "<fromVersions>"
    echo $1 \
    | tr ',' '\n' \
    | (while read V; do
	 case $V in
           *.*.*)
             echo $V \
             | (IFS='.' read Ver Rel Corr
                printf "<fromVersion version='%s' release='%s' correction='%s'/>\n" $Ver $Rel $Corr
               )
             ;;
	   *.*)
	     echo $V \
             | (IFS='.' read Ver Rel
                printf "<fromVersion version='%s' release='%s'/>\n" $Ver $Rel
               )
	     ;;
	   *)
	     printf "<fromVersion version='%s'/>\n" $V
	 esac
       done
      )
    printf "</fromVersions>"
  fi
}


function infos() {
  case $Target in
    gmfMim)
      printf "<mimInfos>\n"
      echo "$MimInfos" \
      | tr ' ' '\n' \
      | (while read U; do
          printf "<mimInfo\n"
          echo "$U" \
          | (IFS='+' read F P T
             printf "file='%s' path='%s' ifType='%s'/>" "$F" "$P" "$T"
            )
         done
        )
      printf "</mimInfos>\n"
      ;;
    gmfImm)
      printf "<immInfos>\n"
      if [[ -n "$ImmInfos" ]]; then
	echo "$ImmInfos" \
	| tr ' ' '\n' \
	| (while read U; do
	     case "$U" in
	       Y@*)
		 printf "<immInfo\n"
		 echo "$U" \
		 | sed -e 's|^.@||' \
		 | (IFS='+' read F P T
		    printf "file='%s' path='%s' fileType='%s'/>" "$F" "$P" "$T"
		   )
	       ;;
	       Z@*)
		 printf "<immInfo\n"
		 echo "$U" \
		 | sed -e 's|^.@||' \
		 | (IFS='+' read F P T N V R C M
                    local schemaName
                    case "$N" in
                      NONE)
                        schemaName="";;
                      *)
                        schemaName="$N"
                    esac
                    case "$C" in
                      NONE)
		        printf "file='%s' path='%s' fileType='%s' schemaName='%s' version='%s' release='%s'>" \
                               "$F" "$P" "$T" "$schemaName" "$V" "$R";;
                      *)
		        printf "file='%s' path='%s' fileType='%s' schemaName='%s' version='%s' release='%s' correction='%s'>" \
                               "$F" "$P" "$T" "$schemaName" "$V" "$R" "$C"
                    esac
		    fromVersions $M
		   )
		 printf "</immInfo>\n"
	     esac
	   done
	  )
      fi
      printf "</immInfos>\n"
    ;;
    *)
      die "bad target: $Target"
  esac
}



function appdata() {
  printf "<appdata target='%s'>\n" $Target
  infos
  printf "</appdata>\n"
}



########################################################################
# Execution begins here


declare OptionPatterns=""
OptionPatterns+="h"
OptionPatterns+="T:"
OptionPatterns+="X:"
OptionPatterns+="Y:"
OptionPatterns+="Z:"


declare Target=undefined
declare MimInfos=""
declare ImmInfos=""

while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    T)
      Target=$OPTARG;;
    X)
      if [[ -z "$MimInfos" ]]; then
        MimInfos+="$OPTARG"
      else
        MimInfos+=" $OPTARG"
      fi
      ;;
    Y)
      if [[ -z "$ImmInfos" ]]; then
        ImmInfos+="Y@$OPTARG"
      else
        ImmInfos+=" Y@$OPTARG"
      fi
      ;;
    Z)
      if [[ -z "$ImmInfos" ]]; then
        ImmInfos+="Z@$OPTARG"
      else
        ImmInfos+=" Z@$OPTARG"
      fi
      ;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

xmlPrelude
appdata

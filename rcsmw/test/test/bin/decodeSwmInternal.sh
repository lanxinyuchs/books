#! /bin/bash
##
## %CCaseFile:	decodeSwmInternal.sh %
## %CCaseRev:	/main/R3A/3 %
## %CCaseDate:	2015-03-04 %
## Author: <name>, <e-mail address>
##
## Description: This script reads a SwmInternal log file. It
## will decode a block of ".. DB .. INFO" lines therein and
## print MO instances to standard output.
##
## The input file is expected to contain just one block of
## lines. A SwmInternal log file that has recorded multiple
## upgrade attempts will contain multiple blocks; the file
## must then be split into suitable chunks before being
## used as input to this script.
##
## The script expects Erlang to be present in the environment.
## If needed, type `module add erlang' before starting the
## script.
##
## Dependencies: None
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2015 All rights reserved.
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
## R3A/2      2015-03-04 erarafo     First version; also provided to EPETZEB
## R3A/3      2015-03-04 erarafo     Enhanced version
## ----------------------------------------------------------------------


declare -r Script=decodeSwmInternal.sh
declare -r InputDefault=/local/scratch/$USER/RCS_ROOT/rcs/log/SwmInternal/SwmInternal.1


function die() {
  printf "$Script: FATAL: $1\n" >&2
  exit 1
}

function warning() {
  printf "$Script: WARNING: $1\n" >&2
}

function info() {
  printf "$Script: INFO: $1\n" >&2
}


########################################################################
# Execution begins here

declare OptionPatterns=""
OptionPatterns+="h"
OptionPatterns+="f:"
OptionPatterns+="F"
OptionPatterns+="p"


function help() {
  cat <<EOF
Usage is: $Script [OPTIONS]
Options are:
  -h          this help
  -f FILE     take input from specified file
  -F          take input from $InputDefault
  -p          pretty format (not suitable for replay)
EOF
}


declare Input=""
declare Pretty=false
while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    f)
     Input=$OPTARG;;
    F)
     Input=$InputDefault;;
    p)
     Pretty=true;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

if [[ $# -gt 0 ]]; then
  die "extra arguments not allowed, try -h for help"
fi

if [[ -z "$Input" ]]; then
  die "no input specified, try -h for help"
fi

if [[ ! -r $Input ]]; then
  die "cannot read: $Input"
fi

declare -r TermsRaw=`mktemp /tmp/$USER-decode-SwmInternal-raw.XXXXXX`

sed \
  -e '/Z DB[ ][ ]*"INFO/!d' \
  -e 's|^.*INFO: ||' \
  -e 's|".*||' \
  $Input \
| xxd -r -p \
| gunzip \
>$TermsRaw

if [[ $Pretty == true ]]; then
  declare -r Escript=`mktemp /tmp/$USER-decode-SwmInternal-escript.XXXXXX`
  cat <<EOF >$Escript

    % do not remove the blank line above this one
    main([InFile]) ->
      {ok, Stream} = file:open(InFile, [read]),
      readAll(Stream).

    readAll(Stream) ->
      case io:read(Stream, "") of
	eof ->
	  ok;
	{ok, Term} ->
	  io:format("~p~n~n", [Term]),
	  readAll(Stream);
	Other ->
	  io:format("failed to read, reason: ~p", [Other])
      end.
EOF
  escript $Escript $TermsRaw
  rm $TermsRaw $Escript
else
  cat $TermsRaw
  rm $TermsRaw
fi

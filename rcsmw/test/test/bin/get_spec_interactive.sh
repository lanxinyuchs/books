#! /bin/bash
## %CCaseFile:	get_spec_interactive.sh %
## %CCaseRev:	/main/R2A/1 %
## %CCaseDate:	2013-12-02 %
## Author: erarafo, rabbe.fogelholm@ericsson.com
##
## Purpose: Interactively gather data and cast as a test spec.
## See the built-in help() function for details.
##
## Dependencies: None
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
## R2A/1      2013-12-02 erarafo     First version
## ----------------------------------------------------------------------


declare -r Script=get_spec_interactive.sh



# help
#
# Explains usage. This function should not be invoked in
# normal use, when this script is invoked from another script.

function help() {
  cat <<EOF
Usage is: $Script [OPTIONS]

Options are:
  -h       this help
  -w       format output as ct_run options
  -o FILE  output to file

The purpose of this script is to collect parameters required by
Common Test. The parameters collected are:

  a directory containing test suites
  a test suite from that directory
  a selection of test cases from that suite, or the word 'all'

The interactive session gets user input from stdin and writes
comsole messages to stderr. When done the result is written to
stdout.

By default the output is a test spec (an Erlang term). If the
-w option is given then the output is formatted as ct_run options.

The -o option can be used to direct output to a file. This is
not meaningful in combination with the -w option.

Advanced test specs referring to multiple suites or groups cannot
be created by this script.

EOF
}



# die MESSAGE
#
# Outputs an error message and exits with a nonzero code.

function die() {
  local -r text="$1"; shift
  printf "$Script: FATAL: $text\n" >&2
  exit 1
}



#complain MESSAGE
#
# Outputs a complaint to standard error.

function complain() {
  local -r text="$1"; shift
  printf "%s\n" "$text" >&2
}



# getString PROMPT [DEFAULT]
#
# Gets a string typed by the user.

function getString() {
  local -r prompt="$1"; shift
  if [[ $# -eq 0 ]]; then
    printf "%s: " "$prompt" >&2
    read result
    echo "$result"
  else
    local -r default="$1"; shift
    printf "%s [%s]: " "$prompt" "$default" >&2
    read result
    if [[ -z "$result" ]]; then
      echo "$default"
    else
      echo "$result"
    fi
  fi
}



# isPosInt NUMERAL
#
# Returns true if the given string is a positive integer.
# Leading zeros are not tolerated.

function isPosInt() {
  local -r word="$1"; shift
  case "$word" in
    0*)
      false;;
    *)
      [[ "$word" != "0" && -z "`echo $word | tr -d '0123456789'`" ]]
  esac
}



# selectWord INDEX W1 W2 ...
#
# Selects the word indicated by the given index. It is trusted
# that the index is not out of range. Indexing starts at 1.

function selectWord() {
  local -r k="$1"; shift
  local j=$((0))
  for w in $*; do
    j=$((j+1))
    if [[ $((k)) -eq $((j)) ]]; then
      echo $w
      break
    fi
  done
}



# multiSelect PROMPT W1 W2 ...
#
# Gets a selection of the given words.

function multiSelect() {
  local prompt="$1"; shift
  local number=$((0))
  local words=""
  for e in $*; do
    number=$((number+1))
    printf "%4d: %s\n" $number "$1" >&2
    words+=" $1"
    shift
  done
  printf "\n" >&2
  local isDone=false
  local result
  while [[ $isDone != true ]]; do
    result=""
    kk=`getString "$prompt" | tr ',' ' '`
    isDone=true
    for k in $kk; do
      if ! isPosInt $k; then
        complain "ill-formed number: $k"
        isDone=false
        break
      elif [[ $((k)) -gt $((number)) ]]; then
        complain "selection out of range: $k"
        isDone=false
        break
      fi
      x=`selectWord $k $words`
      result+=" $x"
    done
  done
  echo $result
}



# menuSelect PROMPT W1 W2 ...
#
# Gets one word selected from the given words.

function menuSelect() {
  local prompt="$1"; shift
  local selection=`multiSelect $prompt $*`
  if [[ `echo $selection | wc --words` -gt 1 ]]; then
    complain "multiple selection not supported"
    menuSelect "$prompt" $*
  else
    echo "$selection"
  fi


}



# getSourceDir
#
# Gets a directory; refuse to get a directory that has no
# Erlang source files.

function getSourceDir() {
  local dir
  local dirSpaceFree
  local xdir
  local isOkDir=false
  while [[ $isOkDir != true ]]; do
    printf "\nSpecify the directory containing the test suite:\n\n" >&2
    dir=`getString 'dir' '.'`
    dirSpaceFree=`echo "$dir" | tr -d ' \t'`
    if [[ "$dirSpaceFree" != "$dir" ]]; then
      complain "cannot handle directory name with embedded spaces: '$dir'"
    else
      xdir=`eval "echo $dir"`
      if [[ ! -d "$xdir" ]]; then
	complain "not a directory: $xdir"
      elif [[ `find "$xdir" -maxdepth 1 -mindepth 1 -type f -name '*.erl' | wc --lines` -eq 0 ]]; then
	complain "no .erl files in directory: $xdir"
      else
	isOkDir=true
      fi
    fi
  done
  echo $xdir
}



# isTestSuite DIR MODULE
#
# Returns true if the given module specifies a file that
# appears to be a test suite.

function isTestSuite() {
  local -r dir="$1"; shift
  local -r module="$1"; shift
  [[ -f "$dir/$module.erl" ]] \
  && grep --silent '^[ \t]*suite[ \t]*([ \t]*)' "$dir/$module.erl"
}



# getModule DIR
#
# Gets a test suite module selected from the Erlang sources
# in the given directory.

function getModule() {
  local dir="$1"; shift
  local module
  modules=`(cd $dir && ls -1d *.erl | sed -e 's|\.erl$||')`
  suites=""
  for m in $modules; do
    if isTestSuite $dir $m; then
      suites+=" $m"
    fi
  done
  printf "\nSelect a test suite:\n\n" >&2
  module=`menuSelect "select" $suites`
  echo $module
}



# getTestcases DIR MODULE
#
# Gets a selection of test cases from the given module.

function getTestcases() {
  local -r dir="$1"; shift
  local -r module="$1"; shift
  printf "\nSelect test cases:\n" >&2
  local cases=`cat $dir/$module.erl \
  | sed -e 's|%.*||' -e '/^[ \t]*-[ \t]*export[ \t]*(/,/)/!d' \
  | tr ',' '\n' \
  | tr -d ' \t.,[]()' \
  | sed \
    -e 's|-[ \t]*export||' \
    -e '/^[ \t]*$/d' \
    -e '/\/[ \t]*1[ \t]*$/!d' \
    -e 's|/[ \t]*1[ \t]*$||' \
    -e '/^init_per_suite$/d' \
    -e '/^init_per_group$/d' \
    -e '/^init_per_testcase$/d' \
    -e '/^end_per_suite$/d' \
    -e '/^end_per_group$/d' \
    -e '/^end_per_testcase$/d' \
    -e '/^group$/d'`

  cases+=" all"

  multiSelect 'select (one or more)' $cases
}



# createTestSpec DIR MODULE CASE1 CASE2 ...
#
# Outputs a test specification for the given arguments.

function createTestSpec() {
  local dir="$1"; shift
  local module="$1"; shift
  local cases="$1"; shift

  local commaList=""
  for c in `echo $cases | tr ',' ' '`; do
    if [[ -z "$commaList" ]]; then
      commaList="$c"
    else
      commaList+=", $c"
    fi
  done

  local listBegin
  local listEnd
  if [[ $cases == all ]]; then
    listBegin=''
    listEnd=''
  else
    listBegin='['
    listEnd=']'
  fi

  printf "{cases,\n %s%s%s,\n %s,\n %s%s%s\n}.\n" \
    '"' \
    `readlink -f $dir` \
    '"' \
    "$module" \
    "$listBegin" \
    "$commaList" \
    "$listEnd"
}



# displayResult DIR MODULE CASE1 CASE2 ...
#
# Displays ct_run options for the given arguments.

function displayResult() {
  local -r dir="$1"; shift
  local -r module="$1"; shift
  local -r cases="$1"; shift
  printf "\nThese options will be passed to ct_run:\n"
  printf "  -dir $dir\n"
  printf "  -suite $module\n"
  if [[ "$cases" != 'all' ]]; then
    printf "  -case $cases\n"
  fi
  printf "\n"
}



# outputResult DIR MODULE CASE1 CASE2 ...
#
# Outputs ct_run options for the given arguments.

function outputResult() {
  local -r dir="$1"; shift
  local -r module="$1"; shift
  local -r cases="$1"; shift
  printf "  -dir $dir\n"
  printf "  -suite $module\n"
  if [[ "$cases" != 'all' ]]; then
    printf "  -case $cases\n"
  fi
}



# isOk
#
# Returns true if a yes-like reply is typed, and
# false otherwise.

function isOk() {
  local reply=`getString 'Ok?' 'y'`
  case "$reply" in
   y|Y|yes|Yes|YES|ok|Ok|OK)
     true;;
   *)
     false
  esac
}


# Execution begins here

declare OutputFile="-"
while getopts ":ho:w" OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    o)
      OutputFile="$OPTARG";;
    w)
      WordSequence=true;;
    *)
      die "Unknown option, try -h for help"
  esac
done; shift $((OPTIND-1))

if [[ $# -gt 0 ]]; then
  die "arguments not allowed; try -h for help"
fi

if [[ $WordSequence == true && "$OutputFile" != "-" ]]; then
  die "options -w and -o are mutually exclusive"
fi


declare Dir
declare Module
while true; do
  Dir=`getSourceDir`
  Module=`getModule $Dir`
  Cases=`getTestcases $Dir $Module`

  if [[ $WordSequence == true ]]; then
    printf "Options for ct_run:\n\n" >&2
    displayResult $Dir $Module "$Cases" >&2
    printf "\n" >&2
  else
    printf "Test specification:\n\n" >&2
    createTestSpec $Dir $Module "$Cases" >&2
    printf "\n" >&2
  fi

  if ! isOk; then
    printf "\nStarting over.\n" >&2
  elif [[ $WordSequence == true ]]; then
    outputResult $Dir $Module "$Cases"
    break
  elif [[ "$OutputFile" == "-" ]]; then
    createTestSpec $Dir $Module "$Cases"
    break
  else
    createTestSpec $Dir $Module "$Cases" >"$OutputFile"
    break
  fi
done

#! /bin/bash
##
## %CCaseFile:	metaDialyzer.sh %
## %CCaseRev:	/main/R3A/2 %
## %CCaseDate:	2015-02-23 %
## Author: <name>, <e-mail address>
##
## Purpose: Analyze the Jenkins Dialyzer run and suppress certain well-known
## complaints that should not be reported. This script is used by
## metaDialyzer_SUITE in $RCT_TOP/test/suites/install.
##
## The script takes no arguments. It will analyze the highest-numbered run
## in the build history.
##
## Output is lines of text. The first line is 'ok' or 'failure: REASON'.
## The second line is the run number.
## The third line is the URL of the Jenkins page being analyzed.
## Subsequent lines, if any, are Dialyzer complaints (after filtering out
## unimportant complaints).
##
## This script is experimental and likely to be removed.
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
## R3A/1      2015-02-23 erarafo     First version
## R3A/2      2015-02-23 erarafo     Tuning
## ----------------------------------------------------------------------


declare -r JenkinsPrefix=https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job
declare -r DialyzerIndex=$JenkinsPrefix/run_dialyzer/

function die() {
  printf "failure: $1\n"
  exit
}


########################################################################
# Execution begins here


declare -r HighestRun=`
wget -q $DialyzerIndex --output-document=- \
| fmt -w 1 \
| sed \
    -e 's|^[ \t]*||' \
    -e '/href="\/job\/run_dialyzer\/[1-9][0-9]*\/console"/!d' \
    -e 's/href="\/job\/run_dialyzer\/\([1-9][0-9]*\)\/console".*/\1/' \
| sort -r -n \
| head -n 1
`
declare -r ExitCodeWget1=$?
if [[ $ExitCodeWget1 -ne 0 ]]; then
  printf "failure: could not find highest run number, exit code: %d\n" $ExitCodeWget1
  exit
fi

if [[ -z "$HighestRun" ]]; then
  printf "failure: could not find any run number\n"
  exit
fi

declare -r ConsoleOutput=$JenkinsPrefix/run_dialyzer/$HighestRun/consoleText


# Suppress known and harmless complaints here,
# in the '-e' options to 'sed'.

printf "%s\n" ok
printf "%s\n" $HighestRun
printf "%s\n" $ConsoleOutput
wget -q --output-document=- $ConsoleOutput \
| sed \
    -e '/^[a-zA-Z0-9_][a-zA-Z0-9_-]*\.[eh]rl:[1-9][0-9]*:/!d' \
    -e '/^leexinc.hrl:/d' \
    -e '/^PKCS-8.erl:/d'

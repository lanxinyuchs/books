#! /bin/bash
##
## %CCaseFile:	dialyzerCheck.sh %
## %CCaseRev:	/main/R3A/1 %
## %CCaseDate:	2015-02-23 %
## Author: <name>, <e-mail address>
##
## Purpose: Analyze the Jenkins Dialyzer run and ignore certain well-known
## and harmless complaints.
##
## The script reads standard input, which is expected to be plaintext
## output from a Jenkins dialyzer run. For an example of such input, see
## https://rbs-rde-ci.rnd.ki.sw.ericsson.se/job/run_dialyzer/7628/consoleText .
##
## No output is produced. Exit codes are:
##
##    0: dialyzer output is considered clean
##    1: dialyzer output has significant complaints
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
## ----------------------------------------------------------------------

declare -r Green=0
declare -r Red=1

# Suppress known and harmless complaints here,
# in the '-e' options to 'sed' following the
# very first one.

if [[ `sed \
         -e '/^[a-zA-Z0-9_][a-zA-Z0-9_-]*\.[eh]rl:[1-9][0-9]*:/!d' \
         -e '/^leexinc.hrl:/d' \
         -e '/^PKCS-8.erl:/d' \
       | wc --lines` -gt 0 ]]; then
  exit $Red
else
  exit $Green
fi

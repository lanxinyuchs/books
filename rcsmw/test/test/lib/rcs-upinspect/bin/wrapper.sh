#! /bin/bash
##
## %CCaseFile:	wrapper.sh %
## %CCaseRev:	/main/R5A/1 %
## %CCaseDate:	2016-02-11 %
## Author: <name>, <e-mail address>
##
## Purpose: Wrap program execution to collect stdout and stderr
##
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2016 All rights reserved.
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
## R5A/1      2016-02-11 erarafo     First version
## ----------------------------------------------------------------------

declare -r TempDir=/tmp


declare -r StdOut=`mktemp -u $TempDir/upInspect-$USER-XXXXXXXX`
declare -r StdErr=`mktemp -u $TempDir/upInspect-$USER-XXXXXXXX`


$* >$StdOut 2>$StdErr
echo "$?|$StdOut|$StdErr"

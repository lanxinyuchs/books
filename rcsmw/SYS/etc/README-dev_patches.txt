## %CCaseFile:	README-dev_patches.txt %
## %CCaseRev:	/main/R8A/R9A/R11A/1 %
## %CCaseDate:	2017-09-19 %
## Author: <name>, <e-mail address>
## Purpose: Explain use of this directory.
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
## Revision history:
##
## Rev        Date       Name        What
## -----      -------    --------    ------------------------------------
## R8A/1      2016-12-06 erarafo     In progress
## R8A/2      2017-01-19 erarafo     In progress
## R8A/3      2017-01-19 erarafo     In progress
## R9A/1      2017-01-24 erarafo     Reasonably complete
## R11A/1     2017-09-19 etxarnu     Some updates
## ----------------------------------------------------------------------
===end=of=header===do=not=edit=this=line===

Patches may be placed in this directory. The filename must be equal to the
name of the file that you want to override.

The types of files that can be patched are listed below.


ERLANG BINARIES

Erlang binaries (.beam files) can be patched.


EXECUTABLE PROGRAMS

Appllication binaries, scripts and shared libraries can be patched.

Copying a new version of a binary program to the dev_patches directory will not
be possible if the directory contains an executing version already. Stop the
program from the Erlang shell by typing `appmServer:stop_lm("myprog", 0).', then
do the copying, then restart with `appmServer:start_lm("myprog", 0).'.


SHARED LIBRARIES

Patched version of shared libraries (.so files) may be placed here. Patching will
be effective after a program restart.


ESI PROGRAMS AND SCRIPTS

Patched versions of ESI executables may be placed here. See the Load Module Handler
IWD for a description.


UPI PROGRAMS AND SCRIPTS

Patched versions of UPI executables may be placed here. See the Load Module Handler
IWD for a description of the UPI feature.


METADATA FILES

Patched versions of metadata files may be placed here. Since metadata files are
read at an early stage of intial start it may be necessary to inject the files
very quickly. For the upgrade case see further down. With the simulator the -P
option can be used to set up a patches directory before the initial start.

One can also reinstall the node by issuing the coli commands 

/misc/authlevel disabled
/sysm/sw -r

or from the erlang shell with
sysNetloader:coli_reinstall(0).


EXTERNAL COLI COMMAND EXECUTABLES

Patched versions of executables may be placed here. The ECOLI block launches
such executables.


PRODUCT INFO

A patched version of the prodinfo program may be placed here. This program is
launched from the EQS block.


BUTTON CONTROL

A patched version of the eqs_buttonctl program may be placed here. This program is
launched from the EQS block.


LED CONTROL

A patched version of the eqs_ledctl program may be placed here. This program is
launched from the EQS block.


WRAPPER SCRIPTS

Wrapper scripts can be used to start up application programs in special ways.
Suppose an application provides a program named fanControl and you want to run
it with an environment variable setting SPECIAL_TRACE=yes. Then create an executable
wrapper script 'fanControlWrapper' in this directory -

  #! /bin/sh
  Executable=$1; shift
  export SPECIAL_TRACE=yes
  exec $Executable "$@"

- plus a one-liner text file 'fanControl.appm' that links the registered program
name 'fanControl' to the wrapper script -

  {prefix, "/home/sirpa/dev_patches/fanControlWrapper"}.


WATCHDOG RELATED

A patched version of the kickwd program may be placed here. The appmWdKick module
starts this program.


SNMPD CONTROL SCRIPTS

Patched versions of the comea-snmp and kill_snmp.sh scripts may be placed here. The
scripts are called from the AIC block via functions in APPM.


RESTART HOOK

A patched version of restart_hook.sh may be placed here, overriding the script in
the SYS block. The script is executed at restart time. See appmServer for details.


RHAI RELATED

Patched versions of the sysread and set_bootptr programs may be placed here.


ITC LINK HANDLER

A patched version of itc_link_handler may be placed here. This program is launched
from the SYS block.


THE MAKE-RELEASE SCRIPT

A patched version of make_release.escript may be placed here. 
The purpose of the script is to prepare for the to-be-started Erlang VM.
This script is run at installations so one has to trigger it as described in 
the METADATA chapter above.

THE MIDDLEWARE START SCRIPT

A patched version of the middleware start script may be used. It is one of
start_rcs.sh  -- for rcssim
rcs_start     -- for target (ARM) nodes
start_vrcs.sh -- for cloud nodes


PATCHES FOR UPGRADE

To provide a patch that shall be effective after an upgrade restart, create the
directory rcs/swm/ug_patches and place the files there instead. RCS will put the
files in this directory before the restart.


------------------------------------------------------------------------
Please update this file as suitable, in the SYS block:
%CCaseFile:	README-dev_patches.txt %
%CCaseRev:	/main/R8A/R9A/R11A/1 %

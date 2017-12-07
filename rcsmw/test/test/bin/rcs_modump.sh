#! /bin/bash
##                                                           -*-Fundamental-*-
## %CCaseFile:	rcs_modump.sh %
## %CCaseRev:	/main/R4A/R5A/4 %
## %CCaseDate:	2016-03-22 %
## Author: <name>, <e-mail address>
##
## Purpose: Dump MO instances to file.
##
## Dependencies:
##        rcs_exec
##        mocount.escript
##
## The rcs_exec dependency is an ugly dependency in the sense
## that options to rcs_exec are collected here and passed to
## rcs_exec. Changes in rcs_exec may require that changes are
## made here as well.
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
## R4A/1      2015-06-07 erarafo     First version
## R4A/2      2015-06-18 erarafo     Debug option, MO Count option
## R4A/4      2015-06-23 erarafo     Added more subtrees
## R5A/1      2016-03-11 erarafo     Added the NodeBFunction=1 subtree
## R5A/2      2016-03-15 erarafo     Added -x option (re-analyze XML)
## R5A/3      2016-03-15 erarafo     Output file option
## R5A/4      2016-03-22 erarafo     Port option
## ----------------------------------------------------------------------


declare -r Script=rcs_modump.sh
declare -r ScriptDir=`dirname $0`

declare -r MoCountPath=$RDE_TOP/tools/mocount

declare -r SimInstDirDefault=/local/scratch/$USER
declare -r PasswordDefault=expert
declare -r UserDefault=expert
declare -r SubtreesDefault=TESTMOM
declare -r OutFileDefault='-'
declare -r NetconfPortDefault=830


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
OptionPatterns+="d"
OptionPatterns+="c"
OptionPatterns+="i:"
OptionPatterns+="a:"
OptionPatterns+="u:"
OptionPatterns+="p:"
OptionPatterns+="s:"
OptionPatterns+="x:"
OptionPatterns+="o:"
OptionPatterns+="P:"


function help() {
  cat <<EOF
Usage is: $Script [OPTIONS] [NODE]

where NODE is a node name such as 'dus5046', or 'sim' for the
simulator. The NODE cannot be specified if the -a option is used.

Options are:
  -s SUBTREES  comma-separated names of subtrees (defaults to $SubtreesDefault)
  -i DIR       simulator installation directory (defaults to /local/scratch/erarafo)
  -a HOST      alternatively specify the target host by name or IP address
  -P PORT      specify non-default port (default is $NetconfPortDefault)
  -u USER      user, defaults to '$UserDefault'
  -p PASSWORD  password, defaults to '$PasswordDefault'
  -x XML_FILE  re-analyze an existing XML file containing NETCONF responses
  -o FILE      output file, or '-' for standard output (the default)
  -c           run MO count
  -d           debug this script (the NETCONF XML response will be available)
  -h           this help

Known subtrees are as follows:

  ENBF            The ENodeBFunction=1 subtree
  EQUIPMENT       The Equipment=1 subtree
  EQSF            The EquipmentSupportFunction=1 subtree
  NBF             The NodeBFunction=1 subtree
  NODE_SUPPORT    The NodeSupport=1 subtree
  TRANSPORT       The Transport=1 subtree

  BRM             The SystemFunctions=1,BrM=1 subtree
  SWM             The SystemFunctions=1,SwM=1 subtree
  SYSM            The SystemFunctions=1,SysM=1 subtree
  ALARM_LIST      The active alarms
  ALARM_MODEL     The alarm types
  INVENT          Current software inventory

  ALL             The entire ManagedElement=1 tree

  TESTMOM         The TestRoot=1 subtree available in the DUMMY CXP
  NOBLE           The Helium=1 subtree available in the DUMMY CXP
  ERAT            The Erat=1 subtree available in the DUMMY CXP

EOF
}


declare Subtrees=$SubtreesDefault
declare SimInstDir=$SimInstDirDefault
declare Host=""
declare User=$UserDefault
declare Password=$PasswordDefault
declare Debug=false
declare MoCount=false
declare MoDumpXml=`mktemp -u /tmp/$USER-modump-XXXXXX`
declare RunNetconf=true
declare OutFile=$OutFileDefault
declare NetconfPort=$NetconfPortDefault
while getopts $OptionPatterns OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    c)
      MoCount=true;;
    d)
      Debug=true;;
    i)
      SimInstDir=$OPTARG;;
    a)
      Host=$OPTARG;;
    u)
      User=$OPTARG;;
    p)
      Password=$OPTARG;;
    s)
      Subtrees=$OPTARG;;
    x)
      RunNetconf=false
      MoDumpXml=$OPTARG;;
    o)
      OutFile=$OPTARG;;
    P)
      NetconfPort=$OPTARG;;
    *)
      die "Unknown option, try -h for help"
  esac
done

shift $((OPTIND - 1))

declare SpecCount=$((0))
declare HasAll=false
for spec in `echo $Subtrees | tr ',' ' '`; do
  if [[ "$spec" == ALL ]]; then
    HasAll=true
  fi
  SpecCount=$((SpecCount + 1))
done

if [[ $SpecCount -gt 1 && $HasAll == true ]]; then
  die "ALL cannot be combined with other subtree specifications"
fi


if [[ $RunNetconf == false ]]; then
  if [[ ! -r $MoDumpXml ]]; then
    die "cannot read: $MoDumpXml"
  fi
elif [[ -z "$Host" && $# -lt 1 ]]; then
  die "no node specified, try -h for help"
elif [[ -z "$Host" && $# -gt 1 ]]; then
  die "too many arguments, try -h for help"
elif [[ -n "$Host" && $# -gt 0 ]]; then
  die "NODE cannot be specified when -a used, try -h for help"
elif [[ -z "$Host" ]]; then
  declare -r Node=$1; shift
else
  declare -r Node=""
fi


declare BeamDir
if [[ -r $RCT_TOP/test/lib/rcs-modump/out/modump.beam ]]; then
  BeamDir=$RCT_TOP/test/lib/rcs-modump/out
elif [[ -r $RCT_TOP/test/release/rcs-modump/ebin/modump.beam ]]; then
  BeamDir=$RCT_TOP/test/release/rcs-modump/ebin
else
  die "cannot find: modump.beam; please run clearmake in $RCT_TOP/test/lib/rcs-modump and retry"
fi

if [[ $Debug == true ]]; then
  info "using beams from: $BeamDir"
fi



if [[ $RunNetconf == true ]]; then
  Opts="-i $SimInstDir -u $User -p $Password"
  if [[ -n "$Host" ]]; then
    Opts+=" -a $Host"
  fi

  if [[ $Debug == true ]]; then
    Opts+=" -v"
  fi

  declare SubtreesSpec=""
  for spec in `echo $Subtrees | tr ',' ' '`; do
    case "$spec" in
      ENBF)
	SubtreesSpec+="<ENodeBFunction/>";;
      EQUIPMENT)
	SubtreesSpec+="<Equipment/>";;
      EQSF)
	SubtreesSpec+="<EquipmentSupportFunction/>";;
      NBF)
	SubtreesSpec+="<NodeBFunction/>";;
      NODE_SUPPORT)
	SubtreesSpec+="<NodeSupport/>";;
      TRANSPORT)
	SubtreesSpec+="<Transport/>";;
      TESTMOM)
	SubtreesSpec+="<TestRoot/>";;
      NOBLE)
	SubtreesSpec+="<Helium/>";;
      ERAT)
	SubtreesSpec+="<Erat/>";;
      BRM)
	SubtreesSpec+="<SystemFunctions><BrM/></SystemFunctions>";;
      SWM)
	SubtreesSpec+="<SystemFunctions><SwM/></SystemFunctions>";;
      SYSM)
	SubtreesSpec+="<SystemFunctions><SysM/></SystemFunctions>";;
      ALARM_LIST)
	SubtreesSpec+="<SystemFunctions><Fm><FmAlarm/></Fm></SystemFunctions>";;
      ALARM_MODEL)
	SubtreesSpec+="<SystemFunctions><Fm><FmAlarmModel/></Fm></SystemFunctions>";;
      INVENT)
	SubtreesSpec+="<SystemFunctions><SwInventory/></SystemFunctions>";;
      ALL)
	SubtreesSpec+="";;
      *)
	die "unknown subtree spec: $spec, try -h for help"
    esac
  done

  if [[ $Debug == true ]]; then
    echo "invoking: rcs_exec -p $NetconfPort -f - -T 1200 $Opts $Node" >&2
  fi
  (if [[ $MoCount == true ]]; then \
     rcs_exec -P $NetconfPort -f - -T 1200 $Opts $Node | sed -e '/^<?xml version/d'; \
   elif [[ $MoCount == false ]]; then \
     rcs_exec -P $NetconfPort -f - -T 1200 $Opts $Node | sed -e '/<rpc-reply.*message-id="1"/,/<\/rpc-reply/!d'; \
   fi \
  ) <<EOF >$MoDumpXml
<?xml version="1.0" encoding="UTF-8"?>
<hello xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">
 <capabilities>
  <capability>urn:ietf:params:netconf:base:1.0</capability>
 </capabilities>
</hello>
]]>]]>

<?xml version="1.0" encoding="UTF-8"?>
<rpc message-id="1" xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">
 <get>

  <filter type="subtree">
   <ManagedElement xmlns="urn:com:ericsson:ecim:ComTop">
        $SubtreesSpec
        <!-- <ENodeBFunction/> -->
        <!-- <SystemFunctions><Fm><FmAlarm/></Fm></SystemFunctions> -->
        <!-- <SystemFunctions><HwInventory/></SystemFunctions> -->
        <!-- <TestRoot/> -->
        <!-- <Helium/> -->
        <!-- <Erat/> -->
        <!-- <SystemFunctions><Fm/></SystemFunctions> -->
   </ManagedElement>
  </filter>

 </get>
</rpc>
]]>]]>

<rpc message-id="2" xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">
  <close-session></close-session>
</rpc>
]]>]]>
EOF
fi


if [[ $MoCount == true ]]; then
  export PATH=$MoCountPath:$PATH
  mocount.escript $MoDumpXml
else
  erl -pa $BeamDir -noshell -run modump modump $MoDumpXml "$SubtreesSpec" "=$OutFile" -run init stop \
  | sed -e '1,/end_of_prelude/d'
fi

if [[ $RunNetconf == false ]]; then
  true
elif [[ $Debug == true ]]; then
  info "NETCONF output in: $MoDumpXml, please remove manually"
else
  rm $MoDumpXml
fi

#! /bin/bash
##
## %CCaseFile:	rcs_cpuload_monitor.sh %
## %CCaseRev:	/main/R2A/1 %
## %CCaseDate:	2014-05-06 %
## Author: <name>, <e-mail address>
##
## Purpose:
##
## Script that displays CPU load. The display is updated
## every second. Nice, kernel and idle time is not shown.
##
## An optional colour argument (red, green, blue, yellow)
## may be provided.
##
## TODO: Allow slower updates. Handle CPU time more
## accurately.
##
## Dependencies:
##
## %CCaseCopyrightBegin%
## Copyright (c) Ericsson AB 2014 All rights reserved.
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
## R2A/1      2014-05-06 erarafo     First version
## ----------------------------------------------------------------------




declare -r ScriptDir=`dirname $0`
declare -r Pgm=`readlink -f $ScriptDir/bottom`
declare -r Colours=(red green yellow blue magenta cyan)
declare -r Bar='#####################################################################################################'
declare -r Filler='-----------------------------------------------------------------------------------------------------'
declare -r Stat=/proc/stat


help() {
  echo "Options are:"
  echo "  -c `echo ${Colours[*]} | tr ' ' '|'`    specify color of load bars"
  echo "  -h                                       this help"
}


die() {
  echo "$Pgm: $1" >&2
  exit 1
}


isColour() {
  for c in ${Colours[*]}; do
    if [[ $c == "$1" ]]; then
      return
    fi
  done
  false
}


getColourCode() {
  case "$1" in
    red)
      echo 31;;
    green)
      echo 32;;
    yellow)
      echo 33;;
    blue)
      echo 34;;
    magenta)
      echo 35;;
    cyan)
      echo 36;;
    black)
      echo 30
  esac
}


Colour=black; while getopts ':hc:' OPT; do
  case "$OPT" in
    c)
      Colour="$OPTARG"
      if ! isColour "$Colour"; then
        die "unknown colour: $Colour; try -h for help"
      fi;;
    h)
      help
      exit;;
    *)
      die "unknown option, try -h for help"
  esac
done


shift $((OPTIND - 1))

if [[ $# -gt 0 ]]; then
  die "unknown argument(s): $*"
fi

if [[ ! -r $Stat ]]; then
  die "cannot read: $Stat"
fi

declare -r ColourCode=`getColourCode $Colour`

declare -a Ticks
declare -a OldTicks

declare j=0
declare Cpus=""
while [[ j -lt `grep -c '^processor' /proc/cpuinfo` ]]; do
  Cpus+=" $j"
  j=$((j+1))
done

for j in $Cpus; do
  OldTicks[$j]=0
done

declare FirstLap=true; while true; do
  Ticks=(`sed -e '/^cpu[0-9]/!d' -e 's|^[cpu0-9]* \([0-9]*\) .*|\1|' $Stat`)
  if [[ $FirstLap != true ]]; then
    clear
    printf "%s\n\n" '        | ------- 10 ------ 20 ------ 30 ------ 40 ------ 50 ------ 60 ------ 70 ------ 80 ------ 90 ------- |'
    for j in $Cpus; do
      Size=$((${Ticks[$j]} - ${OldTicks[$j]}))
      if [[ $Size -gt 100 ]]; then Size=100; fi
      Fill=$((100 - Size))
      printf 'CPU %2d: |\e[1;%dm%s\e[0m%s|\n\n' $j $ColourCode "${Bar:0:$Size}" "${Filler:0:$Fill}"
    done
    sleep 1
  fi

  OldTicks=(${Ticks[*]})
  FirstLap=false
done

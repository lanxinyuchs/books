#! /bin/bash
##
## %CCaseFile:	build.sh %
## %CCaseRev:	/main/R4A/3 %
## %CCaseDate:	2015-11-18 %
## Author: <name>, <e-mail address>
##
## Purpose: Stand-alone compile and link script, used for code example in IWD.
##
## Usage:
##
##     build.sh i686
##     build.sh arm-wr6
##
## Dependencies: Several CXAs.
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
## R4A/1      2015-11-15 erarafo     First version
## R4A/2      2015-11-15 erarafo     Comments added
## R4A/3      2015-11-17 erarafo     IWD Rev B version
## ----------------------------------------------------------------------


if [[ $# -ne 1 ]]; then
  echo "need argument: i686 or arm-wr6"
  exit 1
else
  case "$1" in
    arm-wr6)
      declare -r arch=arm-wr6;;
    i686)
      declare -r arch=i686;;
    *)
      echo "need argument: i686 or arm-wr6"
      exit 1
  esac
fi


cd $RCT_TOP/FAKE/FAKE_CNX*/FAKE_CAX*


rm -f \
  out/obj.$arch/tzii_example.o \
  out/obj.$arch/osemain_tzii_example.o \
  out/tgt_$arch/bin/tziiExample


source $RDE_TOP/bin/shModuleInit.sh

declare -r LocalPathTim=$RCS_TOP/TIM
declare -r LocalPathTri=$OS_TOP/TRI
declare -r LocalPathLit=$OS_TOP/LIBLITS
declare -r LocalPathItc=$OS_TOP/ITC
declare -r LocalPathNs=$OS_TOP/NS
declare -r LocalPathDis=$OS_TOP/DISTRO


if [[ $arch == 'arm-wr6' ]]; then

  module add wrtools-arm/1.1

  arm-wrs-linux-gnueabi-gcc \
      -c \
      -o out/obj.arm-wr6/tzii_example.o \
      -std=gnu99 \
      --sysroot=$LocalPathDis/DISTRO_CXA11448/arm-wr6 \
      -I$LocalPathItc/ITC_CXA11466/include-wr6 \
      -I$LocalPathLit/LIBLITS_CXA11428/include-wr6 \
      -I$LocalPathNs/NS_CXA11467/include-wr6 \
      -I$LocalPathTri/TRI_CXA11471/include-wr6 \
      -I$LocalPathTim/TIM_CXA114001/inc \
      csrc/tzii_example.c

  arm-wrs-linux-gnueabi-gcc \
      -c \
      -o out/obj.arm-wr6/osemain_tzii_example.o \
      -std=gnu99 \
      --sysroot=$LocalPathDis/DISTRO_CXA11448/arm-wr6 \
      -Icsrc/tzii_example.d \
      -I$LocalPathItc/ITC_CXA11466/include-wr6 \
      -I$LocalPathLit/LIBLITS_CXA11428/include-wr6 \
      -I$LocalPathNs/NS_CXA11467/include-wr6 \
      -I$LocalPathTri/TRI_CXA11471/include-wr6 \
      csrc/osemain_tzii_example.c

  arm-wrs-linux-gnueabi-gcc \
      -o out/tgt_arm-wr6/bin/tziiExample \
      --sysroot=$LocalPathDis/DISTRO_CXA11448/arm-wr6 \
      -L$LocalPathItc/ITC_CXA11466/arm-wr6/lib \
      -L$LocalPathLit/LIBLITS_CXA11428/arm-wr6/lib \
      -L$LocalPathNs/NS_CXA11467/arm-wr6/lib \
      -L$LocalPathTri/TRI_CXA11471/arm-wr6/lib \
      -L$LocalPathTim/TIM_CXA114001/tgt_arm-wr6/lib32 \
      -Wl,--wrap=main \
      -Wl,--no-undefined \
      -Wl,--allow-shlib-undefined \
      -Wl,-unresolved-symbols=ignore-in-shared-libs \
      -llits \
      -ltri \
      -litc \
      -ltzii \
      out/obj.arm-wr6/tzii_example.o \
      out/obj.arm-wr6/osemain_tzii_example.o \
 2>&1 | sed \
    -e '/warning: libcec.so, needed by .*\/libtzii.so, not found/d'

elif [[ $arch == 'i686' ]]; then

  module add wrtools-i686/1.0

  i686-wrs-linux-gnu-gcc \
      -c \
      -std=gnu99 \
      -m32 \
      --sysroot=/ \
      -I$LocalPathItc/ITC_CXA11466/include \
      -I$LocalPathLit/LIBLITS_CXA11428/include \
      -I$LocalPathTri/TRI_CXA11471/include \
      -I$LocalPathTim/TIM_CXA114001/inc \
      -o out/obj.i686/tzii_example.o \
      csrc/tzii_example.c

  i686-wrs-linux-gnu-gcc \
      -c \
      -std=gnu99 \
      -m32 \
      --sysroot=/ \
      -Icsrc/tzii_example.d \
      -I$LocalPathItc/ITC_CXA11466/include \
      -I$LocalPathLit/LIBLITS_CXA11428/include \
      -I$LocalPathTri/TRI_CXA11471/include \
      -I$LocalPathTim/TIM_CXA114001/inc \
      -o out/obj.i686/osemain_tzii_example.o \
      csrc/osemain_tzii_example.c

  i686-wrs-linux-gnu-gcc \
      -o out/tgt_i686/bin/tziiExample \
      --sysroot=/ \
      -Wl,--wrap=main \
      -Wl,--allow-shlib-undefined \
      -Wl,-unresolved-symbols=ignore-in-shared-libs \
      -Wl,--no-undefined \
      -L$LocalPathItc/ITC_CXA11466/x86/lib \
      -L$LocalPathLit/LIBLITS_CXA11428/x86/lib \
      -L$LocalPathTri/TRI_CXA11471/x86/lib \
      -L$LocalPathTim/TIM_CXA114001/tgt_i686/lib32 \
      -llits \
      -ltri \
      -litc \
      -ltzii \
      out/obj.i686/tzii_example.o \
      out/obj.i686/osemain_tzii_example.o \
  2>&1 | sed \
    -e '/warning: liblttng-ust.so.0, needed by .*\/libtri.so, not found/d' \
    -e '/warning: liburcu.so.1, needed by .*\/libtri.so, not found/d' \
    -e '/warning: liblttng-ctl.so.0, needed by .*\/libtri.so, not found/d' \
    -e '/warning: libcec.so, needed by .*\/libtzii.so, not found/d'
fi

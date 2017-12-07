
source ${MODULESHOME-/app/modules/0}/init/bash
module remove python
module remove qemu
module add python/2.7.6
module add git/2.8.1
module add gitlfs/1.4.4



if [[ -f /etc/SuSE-release ]]; then

    # Workaround for qemu issue (on SuSE) with missing libxenctrl
    patchlevel=$(grep PATCHLEVEL /etc/*release | gawk '{ print $3 }')

    case $patchlevel in
        2)
          qemu_ver="2.4.1-1"
          ;;
        3)
          qemu_ver="2.4.1"
          ;;
        4)
          qemu_ver="2.4.1-2"
          ;;
        *)
          qemu_ver="2.4.1"
          ;;
    esac
    echo "SuSE PATCHLEVEL = $patchlevel, setting qemu version to $qemu_ver"
elif [[ -f /etc/redhat-release ]]; then
    qemu_ver="2.4.1"
    cat /etc/redhat-release
    echo "setting qemu version to $qemu_ver"
else
    qemu_ver="2.4.1"
fi

module rm qemu
module add qemu/$qemu_ver

module add netcat

if [ -f "/etc/redhat-release" ]
then
    OTP_NATIVE=OTP-19.2.1-redhat-6.8-0
else
    OTP_NATIVE=OTP-19.2.1-native-0
fi

RCSMW3PP=/proj/5G_rcs/3pp/
OTP_ROOT=${OTP_ROOT:-$RCSMW3PP/OTP/$OTP_NATIVE}
OTP=$OTP_ROOT/lib/erlang
ERLGPB=$RCSMW3PP/erl_gpb_rev1/
PROTOC_C=$RCSMW3PP/proto-c_rev1/
MPD_PARSER=${TOPDIR}/tools/3pp/mpDtdParser_rev1
MKSQFS=$RCSMW3PP/bin

export MP_DTD_PARSER_FLAGS="-I $MPD_PARSER/schemas/"
export RDE_TOP=${TOPDIR}
export RCS_TOP=${TOPDIR}
export RCT_TOP=${TOPDIR}/test
export PATH=$OTP/bin:$PROTOC_C/bin:$MPD_PARSER:$MKSQFS:$PATH
export PATH=${TOPDIR}/tools/bin:$RCT_TOP/test/bin:$PATH
export PATH=$PATH:/env/RCSDE/bin


export ERL_FLAGS="-pa $ERLGPB/ebin"

export ERLC_FLAGS="${ERLC_FLAGS_EXTRA:-}\
 -I$OTP/lib/public_key-1.3/include/\
 -I$ERLGPB/include/"

export CPPFLAGS="\
 -I$PROTOC_C/include"

if [[ -d ${TOPDIR}/tools/git_template/hooks ]]; then
   # The following two line makes sure that existing files have correct permissions
   touch ${TOPDIR}/.git/hooks/*
   chmod 755 ${TOPDIR}/.git/hooks/*

   for file in $(git ls-tree --name-only -r origin/master -- ${TOPDIR}/tools/git_template/hooks); do
             git show origin/master:$file > ${TOPDIR}/.git/hooks/$(basename $file)
   done
fi


## UGLY FIX
# "git lfs install" must be run once per user
# Make sure it's not run for rcsci1, it gives error on redhet, not a clue why.
if [[ `whoami` != "rcsci1" ]]; then
    (cd $TOPDIR && git lfs install --force > /dev/null 2>&1)
fi

source $TOPDIR/tools/trim-path.sh $TOPDIR/tools/git-remote.sh

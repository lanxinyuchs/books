dus2_version=R7J30
if [[ -d /proj/5G_rcs/3pp/msrcs-ee/CXP9031275_4_${dus2_version} ]]; then
    OS=/proj/5G_rcs/3pp/msrcs-ee/CXP9031275_4_${dus2_version}
else
    sdk_install_dir=$(readlink -f $(dirname "${BASH_SOURCE[0]}")/..)/sdk-local-installdir/msrcs-ee
    OS=${sdk_install_dir}/CXP9031275_4_${dus2_version}
    $(dirname "${BASH_SOURCE[0]}")/bin/install-sdk.sh --rstate $dus2_version --target msrcs
fi
export SYSROOT=$OS/DISTRO/DISTRO_CXA11448/arm-wr6

export CROSS_COMPILE=arm-wrs-linux-gnueabi-
export ARCH=arm
export CONFIGURE_FLAGS="--host=arm-wrs-linux --target=arm-wrs-linux --host=arm-wrs-linux --build=x86_64-linux --with-libtool-sysroot=$SYSROOT"
export OECORE_DISTRO_VERSION="wr8.0.0.4"

source ${MODULESHOME-/app/modules/0}/init/bash
module use /app/rbs/modules
module add wrtools/8.0.0.4

export CC="arm-wrs-linux-gnueabi-gcc --sysroot=$SYSROOT"
export CXX="arm-wrs-linux-gnueabi-g++ --sysroot=$SYSROOT"
export CXX="arm-wrs-linux-gnueabi-g++ --sysroot=$SYSROOT"
export STRIP="arm-wrs-linux-gnueabi-strip"
export OBJCOPY="arm-wrs-linux-gnueabi-objcopy"

#Binary baseline for EE
source $(dirname "${BASH_SOURCE[0]}")/env-msrcs-ee-version.sh

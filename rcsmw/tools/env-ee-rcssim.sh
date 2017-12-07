export TOPDIR=$(readlink -f $(dirname "${BASH_SOURCE[0]}")/..)
dus2_version=R7J30
if [[ -d /proj/5G_rcs/3pp/msrcs-ee/CXP9031275_4_${dus2_version} ]]; then
    OS=/proj/5G_rcs/3pp/msrcs-ee/CXP9031275_4_${dus2_version}
else
    sdk_install_dir=${TOPDIR}/sdk-local-installdir/msrcs-ee
    OS=${sdk_install_dir}/CXP9031275_4_${dus2_version}
    if [[ ! -d $OS ]]; then
        $(dirname "${BASH_SOURCE[0]}")/bin/install-sdk.sh --rstate $dus2_version --target msrcs
    fi
fi
export WR_SYSROOT=$OS/DISTRO/DISTRO_CXA11448/i686
export SYSROOT=/
#export SYSROOT=$WR_SYSROOT

export ARCH=i686
export TARGET_SYS=${ARCH}-wrs-linux-gnu
#export CROSS_COMPILE=${TARGET_SYS}-
export CONFIGURE_FLAGS="--host=i686-pc-linux-gnu --target=${TARGET_SYS} --build=i686-pc-linux-gnu --with-libtool-sysroot=$SYSROOT"
export OECORE_DISTRO_VERSION="wr6.0.0.4"

#export CC="${CROSS_COMPILE}gcc --sysroot=$SYSROOT -L${WR_SYSROOT}/usr/lib"
#export CC="${CROSS_COMPILE}gcc --sysroot=$SYSROOT"
export CC="${CROSS_COMPILE}gcc --sysroot=$SYSROOT -L/usr/lib -L/lib"
export CXX="${CROSS_COMPILE}g++ --sysroot=$SYSROOT"
export STRIP="${CROSS_COMPILE}strip"
export OBJCOPY="${CROSS_COMPILE}objcopy"

#Binary baseline for EE
source ${TOPDIR}/tools/env-rcssim-ee-version.sh

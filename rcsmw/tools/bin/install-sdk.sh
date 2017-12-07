#!/usr/bin/env bash

# Install the RCS EE SDK to file system
#
# This should normally be called from a Jenkins job
#
#

set -euo pipefail
#set -x

# Print basic usage information
#

print_usage () {
    script=$(basename $0)
    echo "Usage: (two alternatives)"
    echo "  $script --packageurl URL [--installdir PATH] [--tmpdir PATH]"
    echo "  $script --rstate REV [--target {vrcs|brcs|msrcs}] [--arm-url URL] [--installdir PATH] [--tmpdir PATH]"
    echo "  $script --cl CONFIDENCE_LEVEL [--target {vrcs[64]|brcs|msrcs}] [--arm-url URL] [--installdir PATH] [--tmpdir PATH]"
    echo "Description: Fetch RCS EE SDK from ARM and install it into specified directory."
    echo "Options:"
    echo "  --help: show this help"
    echo "  --packageurl: URL to SDK on ARM"
    echo "  --rstate: r-state of the sdk" 
    echo "  --cl: use latest r-state with this confidence level of the sdk" 
    echo "  --target: the target for the EE, hence vrcs, vrcs64, brcs or msrcs (default: vrcs)"
    echo "  --installdir: the path to where to install SDK (default: local repo)"
    echo "  --tmpdir: where to put the fetched CXA (default: local repo)"
    echo "  --arm-url: URL to ARM (optional)"
}

# Declared variables

export TOPDIR=$(readlink -f $(dirname $0)/../..)
args=""
target="vrcs"
installdir="${TOPDIR}/sdk-local-installdir"
tmpdir="${TOPDIR}/.sdk-tmp"
arm_url="https://arm001-eiffel002.rnd.ki.sw.ericsson.se:8443/nexus/service/local/repositories/releases/content/com/ericsson"
packageurl=""
rstate=""
cl=""

# Parse options
#

handle_options () {

    # Program options
    GETOPT=$(getopt --options 'hm' --long help,rstate:,cl:,installdir:,target:,tmpdir:,arm-url:,packageurl: -n $0 -- "$@")

    if [[ $? != 0 ]] ; then echo "Error parsing arguments." >&2 ; exit 1 ; fi

    eval set -- "$GETOPT"

    while true ; do
        case "$1" in
          --help) print_usage; exit ;;
          --target) shift; target=$1;;
          --installdir) shift; installdir=$1;;
          --rstate) shift; rstate=$1;;
          --cl) shift; cl=$1;;
          --tmpdir) shift; tmpdir=$1;;
          --arm-url) shift; arm_url=$1;;
          --packageurl) shift; packageurl=$1;;
          --) shift ; break ;;
          *) echo "Internal error!" ; exit 1 ;;
        esac
        shift
    done

    # $@ is local since we are in a function. Copy it to the global variable.
    args="$@"
}


die() {
    echo "$@" 1>&2
    exit 1
}


# FIXME: Perhaps this should be fetched from an external config file
# Return the product number of the SDK
# Args: <target>

get_cxa_number() {
    local rv=$1
    local target=$2
    case "$target" in
      vrcs) eval "$rv=CXA11448_4" ;;
      vrcs64) eval "$rv=CXA2010033_1" ;;
      brcs) eval "$rv=CXA11448_5" ;;
      msrcs) eval "$rv=CXP9031275_4" ;;
    esac
}
get_target() {
    local rv=$1
    local cxa=$2
    case "$cxa" in
      CXA11448_4) eval "$rv=vrcs" ;;
      CXA2010033_1) eval "$rv=vrcs64" ;;
      CXA11448_5) eval "$rv=brcs" ;;
      CXP9031275_4) eval "$rv=msrcs" ;;
    esac
}

# Download all interfaces for the given product and unpack them in the
# given directory
# param 1: target directory
# param 2: product number
# param 3: product R-state
install_distro() {
    local number=$2
    local rstate=$3
    local target=$1/${number}_${rstate}
    if [[ -e $target ]]; then
        echo Skipping installation of already installed distro in $target
        return 0
    fi

    # Temporary code until product-structure.py is released in ciutils
    source ${MODULESHOME:-/app/module/0}/init/bash
    module use /app/plf_tools/bs_g2/python_modules/modulefiles
    module add ciutils/2.2.11
    export PYTHONPATH=${TOPDIR}/tools/lib:${PYTHONPATH}

    # Extract the GAV part of the product structure output
    products=$(product-structure.py $number $rstate\
               | grep " provides "\
               | cut -d' ' -f3\
               | tr [a-z] [A-Z]) # TODO remove 'tr' part when the name is correct in Eiffel
    mkdir -p $target
    pushd $target > /dev/null

    for product in $products; do
        # Skip the huge RCSEE interface, which is not needed for building?
        #if [[ $product = *:rcsee_* || $product = *:RCSEE_* ]]; then
        #    continue
        #fi
        local av=${product#*:}      # artifact:version
        local pname=${av%%_*}
        local prstate=${av##*:}
        local pnumber=${av#*_}
        pnumber=${pnumber%:*}
        local packageurl=$(${TOPDIR}/tools/bin/get_url_for_product_rev.sh $pnumber $prstate)
        local filename=$(basename "${packageurl}")
        wget --no-verbose "$packageurl"
        mkdir -p $pname
        tar xzf "$filename" -C $pname
        rm -f "$filename"
    done

    # Internal products
    for product in CNX9013327:RHAI_CXA1105840:R7J25; do
        local cnx=${product%%:*}
        local version=${product##*:}
        local artifact=${product#*:}
        artifact=${artifact%:*}
        local pname=${artifact%%_*}
        local pnumber=${artifact#*_}
        local packageurl="https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179@@/CSX10179-R7A01/OS_CRX901265@@/CRX901265-R7J01/RCSEE@@/${cnx}-${version}/${artifact}@@/${pnumber}-${version}/doc@@/${pnumber}-${version}/19010@@/${pnumber}-${version}/${artifact}.cxa@@/${pnumber}-${version}"
        local filename=$(basename "${packageurl}")
        wget --no-verbose "$packageurl"
        mkdir -p $pname
        tar xzf "$filename" -C $pname
        rm -f "$filename"
    done
}


# Main program.

if [[ $# -eq 0 ]]; then
    print_usage
    exit 0
fi
handle_options "$@"

RSTATE=$rstate
if [[ -z "${RSTATE-}" ]]; then
    RSTATE=$(echo ${packageurl##*-}| cut -d. -f1)
fi
PRODUCT=""
get_cxa_number "PRODUCT" "$target"
mkdir -p $installdir/$target-ee 

if [[ $target = msrcs ]]; then
    [[ ! $RSTATE ]] && die '--rstate is mandatory for msrcs'
    install_distro $installdir/$target-ee $PRODUCT $RSTATE
else
    mkdir -p $tmpdir
    cd $tmpdir
    if [[ ! $packageurl ]]; then
        if [[ ! $RSTATE ]]; then
            if [[ ! $cl ]]; then
                echo "ERROR: At least one of --rstate, --cl and --packageurl is mandatory" 1>&2
                exit 1
            fi
            set +e
            RSTATE=$(${TOPDIR}/tools/bin/get_latest_cxp.sh $PRODUCT $cl 2>/dev/null)
            if [[ "$?" != "0" ]]; then
                echo "Error: couldn't find $PRODUCT with CL=$cl" 1>&2
                exit 1
            fi
            set -e
        fi
        packageurl=$(${TOPDIR}/tools/bin/get_url_for_product_rev.sh $PRODUCT $RSTATE)
    fi
    wget $packageurl
    cxa=${packageurl##*/}
    tgz=$(tar tf $cxa | egrep "[^/]*/[^/]*.tar.gz")
    outdir=$(echo ${cxa%.cxa} | cut -d_ -f2-3|tr '-' '_')
    #get_target "target" ${outdir%_*}
    tar xf $cxa
    tar xzf $tgz
    ./wrlinux-*-sdk.sh -y -d $installdir/$target-ee/${outdir}
    cd -- >/dev/null 2>&1
    rm -rf $tmpdir
fi


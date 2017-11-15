#!/bin/bash

set -e

rcstestdir=$(readlink -f $(dirname $0)/..)

outdir=$PWD
rcseedir="/repo/$USER/rcs-yocto"
builddir="make/build"
baselinedir=${rcstestdir}/baseline
product=tcu
mw_latest=""
extended=""
up_suffix="_MINI"
fpgacl0=""
fpgacl0_suffix=""
debug=no
fl=""
up=""
new_up=""

CREATESIG=/app/rbs/cs/V1/amd64_linux26/bin/create_sig.sh

usage()
{
  echo "options:"
  echo "  -h   This text"
  echo "  -o   Output directory, default '.'"
  echo "  -r   rcs-yocto directory, default '/repo/$USER/rcs-yocto'"
  echo "  -p   product, dus|tcu, default 'tcu'"
  echo "  -e   build extended UP, by default will build minimal UP"
  echo "  -m   Use latest MW version, will use version from the baseline file if not specified"
  echo "  -u   Path to UP to patch (zip or gzip format), will be created from the baseline file if not specified"
  echo "  -t   The same as -u. Keept for backward compatibility"
  echo "  -f   Path to Factory Loader Package (LXA in zip or gzip tar format) to patch"
  echo "  -d   Save debug info,  default 'no'"
  echo "  -c   Build minimal UP to test CL0 FPGA's"
  echo "  -c -e  Build extended UP to test CL0 FPGA's"
  echo "  -b   Build directory"
  echo "  -y   baseline directory."
}

cleanup()
{
  echo "Cleaning up"
  rm -rf ${workdir}
  [ "$debug" == "no" ] && rm -rf $outdir/debug
}

tmp=$(getopt --options dmecho:r:b:p:u:t:f:y: --long help -- "$@")
eval set -- $tmp
while true ; do
    case "$1" in
	-d)
	    shift
	    debug=yes
	    ;;

	-o)
	    shift
	    if [ -d "${1}" ]; then
		outdir="$(readlink -f "${1}")"
	    elif [ -e ${1} ]; then
		echo "Will not overwrite $1, exiting"
		exit 1
	    elif [ -d $(dirname ${1}) ]; then
		outdir=$(readlink -f $(dirname ${1}))
		if [ "$(basename ${1})" = "$(basename ${1} .zip)" ]; then
		    new_up_zip=""
		else
		    new_up_zip=".zip"
		fi
		new_up=$(basename ${1} .zip)
	    else
		echo "Error: ${1} must be either an existing directory or"
		echo "       a non-existing file in an existing directory"
		exit 1
	    fi
	    shift
	    ;;

	-r)
	    shift
	    rcseedir=$(readlink -f "${1}")
	    shift
	    ;;

	-b)
	    shift
	    builddir=$(readlink -f "${1}")
	    shift
	    ;;

	-p)
	    shift
	    product="${1}"
	    shift
	    ;;

	-m)
	    mw_latest="true"
	    shift
	    ;;

	-e)
	    extended="extended_"
            up_suffix=""
	    shift
	    ;;

	-c)
	    fpgacl0="fpgacl0_"
            fpgacl0_suffix="_fpgacl0"
	    shift
	    ;;

	-u)
	    shift
	    up="${1}"
	    shift
	    ;;

	-t)
	    shift
	    up="${1}"
	    shift
	    ;;

	-f)
	    shift
	    fl="${1}"
	    shift
	    ;;

	-y)
	    shift
	    baselinedir=$(readlink -f "${1}")
	    shift
	    ;;

	-h|--help)
	    usage
	    exit
	    ;;

	--)
	    shift
	    break
	    ;;
    esac
done

OLDIFS=$IFS
IFS=','
for d in ${outdir},"Out" ${rcseedir},"rcs-yocto" ; do
    set $d
    if [ ! -d "${1}" ] ; then
	echo "$2 dir not found."
	exit
    fi
done
IFS=$OLDIFS

trap cleanup EXIT

[ ! -d /tmp/$USER ] && mkdir /tmp/$USER
workdir=`mktemp -d -p /tmp/$USER`
cd ${workdir}

[ -d ${outdir}/debug ] && rm -rf ${outdir}/debug
mkdir -p ${outdir}/debug
deldir="${builddir}/export/delivery"

if [ "${product}" == "tcu" ] ; then
    ee_cxc=RCSEE5_CXC1737262_5
    ee_cxp=RCSEE-T_CXP9025317_5
    ee_cxs=RCP-T_CXS101549_6
    ee_cxs_name=EE_BLACK_RCP-T
    cxp_id=CXP9031274_4
    bundle_cxp=RCS-T_CXP9031274_4
    bundle_xml=cxp9031274_4
    epath="RCSEE5_CXC1737262_5/rcsee5"
    baseline_file=${baselinedir}/tcu_${fpgacl0}${extended}baseline.xml
elif [ "${product}" == "dus" ] ; then
    ee_cxc=RCSEE4_CXC1737262_4
    ee_cxp=RCSEE-DUS2_CXP9025851_3
    ee_cxs=RCP-DUS2_CXS101549_5
    ee_cxs_name=EE_BLACK_DUS
    cxp_id=CXP9031275_3
    bundle_cxp=RCS-DUS2_CXP9031275_3
    bundle_xml=cxp9031275_3
    epath="RCSEE4_CXC1737262_4/rcsee4"
    baseline_file=${baselinedir}/dusX2_${fpgacl0}${extended}baseline.xml
else
    echo "Unsupported product: ${product}"
    exit 1
fi

file=""
mode=""
# Define script logic
# Patch UP if up provided
if [ ! -z "${up}" ] && [ -z "${fl}" ] ; then
    mode='up'
    file="${up}"
# Patch UP if nothing provided
elif [ -z "${up}" ] && [ -z "${fl}" ] ; then
    mode='up'
    file=""
# Patch FL if provided
elif [ -z "${up}" ] && [ ! -z "${fl}" ] ; then
    mode='fl'
    file="${fl}"
# We can do only one thing. Fail if both UP and FL provided
elif [ ! -z "${up}" ] && [ ! -z "${fl}" ] ; then
    echo 'You can not user -f and -u at the same time'
    exit 1
# We shouldn't get there
else
    echo 'internal logic error. This should never happen'
    exit 1
fi

echo "######################## Get ${mode} and extract it ########################"

# Unpack file provided by user
if [ ! -z "${file}" ] ; then
    echo "Use user provided ${mode} ${file}"
    # Is it a gzip file?
    if [ ! -z "`file ${file} | grep "gzip compressed data"`" ] ; then
        tar xf ${file}
    # Is it a zip file?
    elif [ ! -z "`file ${file} | grep "Zip archive data"`" ] ; then
        unzip ${file}
    else
        echo "Supported archive types are zip and gzip only"
        echo "${file} is: `file ${file}`"
        exit 1
    fi
# We don't have user provided UP or FL. Build UP from the baseline
elif [ "${mode}" == "up" ] && [ -z "${file}"] ; then
    if [ "${mw_latest}" == "true" ] ; then
        echo "Update baseline file in order to fetch latest MW version"
        # Path baseline file to get latest bundle CXP instead of one from the baseline
        latest_url='https\:\/\/rbs-rde-dev\.rnd\.ki\.sw\.ericsson\.se\/vobs\/rcs\/delivery\/RCP_CSX10179_1\/'${ee_cxs}'\/'${ee_cxp}'\/doc\/19010\/'${ee_cxp}'\.cxp\"'
        cp ${baseline_file} ${outdir}/debug/baseline_with_latest_mw.xml
        baseline_file=${outdir}/debug/baseline_with_latest_mw.xml
        sed -i 's/https\:\/\/rbs-rde-dev\.rnd\.ki\.sw\.ericsson\.se\/vobs\/rcs\/delivery\/RCP_CSX10179_1\/.*/'${latest_url}'/' ${baseline_file}
    fi
    echo "Create UP using baseline ${baseline_file}"
    /env/rbsg2/bin/ncgen --zip --file ${baseline_file}
    echo "Extract UP to ${workdir}"
    unzip *.zip
    rm *.zip
else
    echo 'internal logic error. This should never happen'
    exit 1
fi

echo "######################## Patch RCS EE CXP ########################"
echo "Extracting RCSEE CXP from bundle RCS cxp"

[ "$fl" == "" ] && tar xf $bundle_cxp.cxp $ee_cxp.cxp

# Update RCS EE CXP version
echo "Unpacking the original RCSEE CXP"
mkdir ee_unpack
cd ee_unpack
tar -xf ../${ee_cxp}.cxp

echo "Unpacking the EE CXC from : ${rcseedir}/${deldir}/${ee_cxc}.tar.gz"
rm -rf ${ee_cxc}
tar xf ${rcseedir}/${deldir}/${ee_cxc}.tar.gz
rm -rf ${ee_cxc}/dbg

echo "Modifying contents of EE CXP : ${ee_cxp}.cxp"
chmod u+w ../${ee_cxp}.cxp
$CREATESIG -f cxp*.xml -o ../${ee_cxp}.cxp -x ${ee_cxc} ${PWD} >  ${outdir}/debug/create_sig.log
cd ..

echo "######################## Recreate ${mode} using patched RCS EE CXP ########################"

if [ "${mode}" == "up" ] ; then
    echo "Replace $ee_cxp.cxp in $bundle_cxp.cxp"
    chmod u+w $bundle_cxp.cxp
    gunzip -S .cxp $bundle_cxp.cxp
    tar --delete -f $bundle_cxp $ee_cxp.cxp
    tar --update -f $bundle_cxp $ee_cxp.cxp
    rm $ee_cxp.cxp
    gzip -S .cxp $bundle_cxp
    new_up=${outdir}/${ee_cxs_name}${up_suffix}${fpgacl0_suffix}
    new_up_zip=".zip"
    echo "Packing ${new_up}${new_up_zip}"
    tar czf ${new_up} *.cxp *.xml
    mv ${new_up} ${new_up}${new_up_zip}
elif [ "${mode}" == "fl" ] ; then
    echo "Copy RCS-EE Linux kernel to LXA top level"
    rm -f linux.img
    security_stripper.sh ee_unpack/${epath}/kernel/linux-arm.img linux.img

    echo "Copy RCS-EE bootfs to LXA top level"
    mkdir tmp; mkdir tmp/old; mkdir tmp/new
    mv bootfs.cpio.gz tmp/.; cd tmp
    gunzip bootfs.cpio.gz; cd old; cpio -id < ../bootfs.cpio; cd ..
    rm -f bootfs*
    security_stripper.sh ../ee_unpack/${epath}/bootfs/bootfs-arm.cpio.gz bootfs.cpio.gz
    gunzip -q bootfs.cpio.gz || true; cd new; cpio -id < ../bootfs.cpio;
    cp -f ../old/addons/bin/factoryloader_script.sh addons/bin/.
    cp -f ../old/addons/bin/fltool addons/bin/.
    rm -f ../bootfs*; find . | cpio --quiet -o -H newc -R root:root -O ../../bootfs.cpio
    cd ../..; gzip bootfs.cpio; rm -rf tmp

    echo "Packing ${file}.new"
    # Should it be a gzip file?
    if [ ! -z "`file ${file} | grep "gzip compressed data"`" ] ; then
	tar --exclude='./ee_unpack' -zcf ${fl}.new .
    # Should it be a zip file?
    elif [ ! -z "`file ${file} | grep "Zip archive data"`" ] ; then
	rm -f ${fl}.new
        zip  ${fl}.new * --exclude ee_unpack/
    else
        echo "Supported archive types are zip and gzip only"
        echo "${file} is: `file ${file}`"
        exit 1
    fi
else
    echo 'internal logic error. This should never happen'
    exit 1
fi

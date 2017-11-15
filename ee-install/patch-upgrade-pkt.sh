#!/bin/bash

set -ex

rcseedir=""
cmd=$(basename $0)
tmpdir=$(mktemp -d ${TMPDIR:-/tmp}/patch-up.XXXXXX)
mkdir -p $tmpdir/{dst,src}

new_ee_version=default
new_up_version=default

CREATESIG=/app/rbs/cs/V1/amd64_linux26/bin/create_sig.sh

usage() {

    set +e +x

    echo "Patch ee cxp in upgrade package without breaking SWM upgrade verify"
    echo ""
    echo "$cmd -h|-r path-to-rcs-ee -d destination-UP -p prod [-v version] [-u version]"
    echo "     -r rcs-ee directory"
    echo "     -d path to UP for destination"
    echo "     -v define new RCSEE cxp version - default: use version from dest"
    echo "     -u define new UP version - default: use version from dest"
    echo "     -p product type, tcu|dus"
    echo "     -h print this text"

    set -ex
}

cleanup() {

    echo "$cmd exit"
    rm -rf $tmpdir
}

verify() {

    if [ ! -f $dst ]; then
	echo "-d destination not set"
	exit 1
    fi

    if [ "${prod}" == "tcu" ] ; then
	ee_cxc=RCSEE5_CXC1737262_5
	ee_cxp_id=CXP9025317_5
	ee_cxp_name=RCSEE-T
	bundle_cxp_id=CXP9031274_4
	bundle_cxp_name=RCS-T
	deldir=tcu-delivery
    elif [ "${prod}" == "dus" ] ; then
	ee_cxc=RCSEE4_CXC1737262_4
	ee_cxp_id=CXP9025851_3
	ee_cxp_name=RCSEE-DUS2
	bundle_cxp_id=CXP9031275_3
	bundle_cxp_name=RCS-DUS2
	deldir=dusX2-delivery
    else
	echo "Unsupported product: ${prod}"
	exit 1
    fi
    bcxp_xml=$(echo "${bundle_cxp_id}.xml" | tr '[:upper:]' '[:lower:]')
    bundle_cxp="${bundle_cxp_name}_${bundle_cxp_id}"
    ee_cxp="${ee_cxp_name}_${ee_cxp_id}"
    ecxp_xml=$(echo "${ee_cxp_id}.xml" | tr '[:upper:]' '[:lower:]')

    if [ ! -f ${rcseedir}/${deldir}/${ee_cxc}.tar.gz ]; then
	echo "${rcseedir}/${deldir}/${ee_cxc}.tar.gz - Not found"
	exit 1
    fi

    if tar -tf $dst | grep "\-up.xml" > /dev/null; then
	dst_type=up
    elif [ ! -z "`file ${dst} | grep "Zip archive data"`" ] ; then
	dst_type=up
    else
	echo "dst: ${dst} - Unsupported destination file format"
	exit 1
    fi
}

trap cleanup SIGHUP SIGINT SIGTERM EXIT

ee_version_from_dst() {
    local tmp

    cd  $tmpdir/dst

    if [ ! -z "`file ${dst} | grep "gzip compressed data"`" ] ; then
	tar -xf $dst
    elif [ ! -z "`file ${dst} | grep "Zip archive data"`" ] ; then
	unzip $dst
    else
        echo "Supported archive types are zip and gzip only"
        echo "${dst} is: `file ${dst}`"
        exit 1
    fi

    chmod u+w $bundle_cxp.cxp
    gunzip -S .cxp $bundle_cxp.cxp
    tar -xf $bundle_cxp $bcxp_xml

    tmp=$(grep "${ee_cxp_name}" $bcxp_xml)
    dst_up_vid=${tmp#*version=}
    dst_up_vid=${dst_up_vid//\"}
    dst_up_vid=${dst_up_vid//\>}
    dst_up_vid=${dst_up_vid//\/}
    rm $bcxp_xml
    tar -xf $bundle_cxp $ee_cxp.cxp
    chmod u+w $ee_cxp.cxp
    gunzip -S .cxp $ee_cxp.cxp
}

create_ee_cxp() {

    cd $tmpdir/src
    mkdir ee_unpack
    cd ee_unpack
    tar xf $tmpdir/dst/$ee_cxp
    rm $tmpdir/dst/$ee_cxp
    tar xf ${rcseedir}/${deldir}/${ee_cxc}.tar.gz
    rm -rf ${ee_cxc}/dbg
    if [ "$new_ee_version" != "default" ]; then
	sed -i "s/${dst_up_vid}/${new_ee_version}/" $ecxp_xml
    fi
    $CREATESIG -f $ecxp_xml -o ../${ee_cxp}.cxp -x ${ee_cxc} ${PWD}
    cd ..
    rm -rf ee_unpack
}

update_ee_dst() {
    local tmp
    local upxml
    local nmsinfo
    local up_prod_id

    cd  $tmpdir/dst
    tar --delete -f $bundle_cxp $ee_cxp.cxp
    cp ../src/$ee_cxp.cxp .
    tar --update -f $bundle_cxp $ee_cxp.cxp
    rm $ee_cxp.cxp
    if [ "$new_ee_version" != "default" ]; then
	tar -xf $bundle_cxp $bcxp_xml
	sed -i "s/\"${ee_cxp_id}\" version=\"${dst_up_vid}\"/\"${ee_cxp_id}\" version=\"${new_ee_version}\"/" $bcxp_xml
	tar --delete -f $bundle_cxp $bcxp_xml
	tar --update -f $bundle_cxp $bcxp_xml
	rm $bcxp_xml
    fi
    gzip -S .cxp $bundle_cxp

    if [ "$new_up_version" != "default" ]; then
	upxml=$(readlink -f ./*-up.xml)
	tmp=$(basename $upxml)
	up_prod_id=${tmp//\-up.xml}
	up_prod_id=$(echo $up_prod_id | tr '[:lower:]' '[:upper:]')
	tmp=$(grep "${up_prod_id}" $upxml)
	upv=${tmp#*version=}
	upv=${upv//\"}
	upv=${upv//\>}
	upv=${upv//\/}
	id=${tmp#*id=};
	id=${id//\ version*};
	id=${id//\"};
	sed -i "s/id=\"${id}\" version=\"${upv}\"/id=\"${id}\" version=\"${new_up_version}\"/" $upxml
	if [ -f ./nmsinfo.xml ]; then
	    nmsinfo=$(readlink -f ./nmsinfo.xml)
	    sed -i "s/${upv}/${new_up_version}/" $nmsinfo
	fi
    fi
    if [ ! -z "`file ${dst} | grep "gzip compressed data"`" ] ; then
	tar czf ${dst}.new *.cxp *.xml
    elif [ ! -z "`file ${dst} | grep "Zip archive data"`" ] ; then
	zip ${dst}.new *
    fi
}

while getopts "r:d:p:v:u:h" opt; do

    case $opt in
	r)
	    rcseedir=$(readlink -f $OPTARG)
	    ;;
	d)
	    dst=$OPTARG
	    ;;
	p)
	    prod=$OPTARG
	    ;;
	v)
	    new_ee_version=$OPTARG
	    ;;
	u)
	    new_up_version=$OPTARG
	    ;;
	h|?)
	    usage
	    exit 0
    esac
done

verify
ee_version_from_dst
create_ee_cxp
update_ee_dst

echo "all done"
exit 0

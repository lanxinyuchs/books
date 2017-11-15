#!/bin/sh

RED='\033[01;31m'

progname="`basename $0`"
PPC_cxp='RCS_CXP9021221_1.cxp'
ARM_cxp='RCS-ARM_CXP9021221_3.cxp'
EETCU03_cxp='RCSEE-TCU03_CXP9025317_3.cxp'
EEDUS52_cxp='RCSEE-DUS2_CXP9025851_3.cxp'
TCU03_cxp='RCS-TCU03_CXP9031274_3.cxp'
DUS2_cxp='RCS-DUS2_CXP9031275_3.cxp'

usage() {
    echo "Usage: ${progname} <target type>"
    echo "Example: ./check_signing.sh tcu03"
    exit 1 
}
check_signing()
{
   image_file=$1
    [ -r ${image_file} ] || error "$image_file unreadable"    
	set -- $(od  -An -ta -N 4 ${image_file}) || "Failed to read image: $image_file"
	case :${1}:${2}:${3}:${4}: in
	    :S:B:B:dc1:)
		echo "${progname}: Image $image_file is signed"
		;;
	    *)
		echo -e "${RED}${progname}: ERROR : Image $image_file is not signed \e[0m" 
		;;
	esac
}
if [ $# != 1 ]; then 
    usage
fi

rm -rf pack;
mkdir pack;

if [ $1 = dus52 ] ; then 
echo "Target type is not supported yet."
cd pack ; wget https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.cxs ;
tar xf *.cxs ;
tar xf $DUS2_cxp ;
tar xf $EEDUS52_cxp ;
NL_bootfs='NL3_CXC1733874_3/nl-*/priv/tgt_arm/pkg/bootfs.cpio.gz'
check_signing $NL_bootfs;
NL_kernel='NL3_CXC1733874_3/nl-*/priv/tgt_arm/pkg/linux.img'
check_signing $NL_kernel;
EE_kernel='RCSEE4_CXC1737262_4/rcsee4/kernel/linux-arm.img'
check_signing $EE_kernel ;
EE_bootfs='RCSEE4_CXC1737262_4/rcsee4/bootfs/bootfs-arm.cpio.gz'
check_signing $EE_bootfs ;
fi

if [ $1 = tcu03 ] ; then 
cd pack ; wget https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-TCU03_CXS101549_4/doc/19010/RCP-TCU03_CXS101549_4.cxs ;
tar xf *.cxs ;
tar xf $TCU03_cxp ;
tar xf $EETCU03_cxp ;
NL_bootfs='NL3_CXC1733874_3/nl-*/priv/tgt_arm/pkg/bootfs.cpio.gz'
check_signing $NL_bootfs;
NL_kernel='NL3_CXC1733874_3/nl-*/priv/tgt_arm/pkg/linux.img'
check_signing $NL_kernel;
EE_kernel='RCSEE3_CXC1737262_3/rcsee3/kernel/linux-arm.img'
check_signing $EE_kernel ;
EE_bootfs='RCSEE3_CXC1737262_3/rcsee3/bootfs/bootfs-arm.cpio.gz'
check_signing $EE_bootfs ;
fi;


	

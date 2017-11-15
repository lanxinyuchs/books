#!/bin/sh 

RED='\033[01;31m'
progname="`basename $0`"
BASELINE_PATH=$3"/../../../baseline"

if [ $1 = tcu03 ] ; then	
EE_maincxp='RCSEE-T_CXP9025317_5.cxp'		
EE='CXP9031274_4'		
EE_CXC='RCSEE5_CXC1737262_5'	
sqfs='RCSEE5_CXC1737262_5/rcsee5/rootfs'	
cxc_xml='cxp9025317_5.xml'	
cxp_xml='cxp9031274_4.xml'	
cxs_xml='cxs101549_4-up.xml'	
fi
if [ $1 = dus52 ] ; then	
EE_maincxp='RCSEE-DUS2_CXP9025851_3.cxp'		
EE='CXP9031275_3'
EE_CXC='RCSEE4_CXC1737262_4'		
sqfs='RCSEE4_CXC1737262_4/rcsee4/rootfs'
cxc_xml='cxp9025851_3.xml'	
cxp_xml='cxp9031275_3.xml'	
cxs_xml='cxs101549_4-up.xml'
fi

# /usr/bin/xpath not always available
which xpath > /dev/null
if [ $? -ne 0 ]; then
  xpath=`dirname $0`/xpath
  echo "Using local xpath: $xpath"
else
  xpath=xpath
fi

usage() {	
	echo "Usage: ${progname} <target type>"	
	echo "Example: ./cxs_badroothash.sh tcu03 sm/nosm"	
	exit 1 	
	}	
if [ $# != 3 ]; then 	
	usage	
fi	

rm -rf pck1;
mkdir pck1;

echo $BASELINE_PATH;
UP_VERSION=`cat ${BASELINE_PATH}/semisecure_test_up_version`;
if [ $1 = tcu03 ] ; then 	
   cd pck1 ; wget https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-T_CXS101549_6/RCS-T_CXP9031274_4/doc/19010/RCS-T_CXP9031274_4.cxp@@/${EE}-${UP_VERSION} ;	
fi
if [ $1 = dus52 ] ; then
   cd pck1 ; wget https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/RCS-DUS2_CXP9031275_3/doc/19010/RCS-DUS2_CXP9031275_3.cxp@@/${EE}-${UP_VERSION} ;
fi
	tar xf CXP* ;
	mkdir cxcs;		
		cd cxcs ; tar xf ../$EE_maincxp ;		
		mkdir ar;	
		cd ar ; ar -x ../sw.ar ;
		cp sqfs.img ../$sqfs/.	
		sed -i 's\b\c\g' arch-info.txt ;	
		if [ $2 = sm ] ; then	
			echo "Creating badsm along with badroothash."	
			rm -f arch-info.sm ;	
			curl -F file_in=@arch-info.txt -o arch-info.sm \https://platform-rcssign.rnd.ki.sw.ericsson.se/cgi-bin/callskh-prod.pl	
		else [ $2 = nosm ] ;	
			echo "Creating badroothash cxs."	
		fi	
		ar rs sw.ar * ;	
		rm -f ../sw.ar ; cp sw.ar ../../. ;	
		cd ../ ; rm -rf ar ;	
		mv $cxc_xml ../. ;	
		cd ../ ; 	
		rm -f $EE_maincxp ;		
		$3/create_sig.sh -f $cxc_xml -a sw.ar -x $EE_CXC -o $EE_maincxp cxcs	
	rm $3/../../$EE_maincxp ;
	cp $EE_maincxp $3/../../. ;	
	cd ../ ; rm -rf pck1 ;	

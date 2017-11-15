#!/bin/sh 

RED='\033[01;31m'
progname="`basename $0`"	
frumcxp_xml='cxp9024280_2.xml'
frumfilename='FRUM_CXP9024280_2.cxp'

# /usr/bin/xpath not always available
which xpath > /dev/null
if [ $? -ne 0 ]; then
  xpath=`dirname $0`/xpath
  echo "Using local xpath: $xpath"
else
  xpath=xpath
fi

usage() {	
	echo "Usage: ${progname} <target type> <node name>"	
	echo "Example: ./secure_cxs.sh tcu03 tcu022"	
	exit 1 	
	}	
if [ $# != 2 ]; then 	
	usage	
fi	
if [ $1 = tcu03 ] ; then	
	prod_num='2'
	tncxp_xml='cxp9022846_2.xml'
	tnname='TN-TCU03_CXP9022846_2_'	
	cxsname='EE_BLACK_RCP-T_MINI.zip'
	cxsxml='EE_BLACK_RCP-T_MINI-up.xml'
	name='TN-DUSG2'
	tnid='CXP9022846_2'
	frumid='CXP9024280_2'
	frumcxp_xml='cxp9024280_2.xml'
	frumfilename='FRUM_CXP9024280_2.cxp'
else [ $1 = dus52 ] ;	
	prod_num='10'
	tncxp_xml='cxp9022846_10.xml'
	tnname='TN-DUSG2_CXP9022846_10_'
	cxsname='EE_BLACK_DUS_MINI.zip'
	cxsxml='EE_BLACK_DUS_MINI-up.xml'
	name='TN-TCU03'
	tnid='CXP9022846_10'
	frumid='CXP9024280_4'
	frumcxp_xml='cxp9024280_4.xml'
	frumfilename='FRUM_CXP9024280_4.cxp'
fi


rm -rf pck1;	
mkdir pck1;

echo "first argument is $1"
echo "second argument is $2"

### To get FRUM CXP with confidence level 3
	
cat <<EOF > $PWD/frum_cxp_test.xml  
<pkgspec>
   />
  <product
   />
  <containers>
      <!-- FRUM -->
      <module
          id="$frumid"
          confidence_level="3"
      />
  </containers>
</pkgspec>

EOF

/env/rbsg2/bin/ifu --spec frum_cxp_test.xml --create ./tp

cd tp;
tar xf FRUM* ;
frum_product_ver=$($xpath $frumcxp_xml '/configuration/product/@version' 2> /dev/null | sed -e 's/ version="//' -e 's/"//')
if  [ -z "$frum_product_ver" ]; then
	 echo "Failed to parse frum xml: ver=$frum_product_ver"
  	 cleanup
  	 exit 1
fi
echo "version of FRUM CXP verion is $frum_product_ver"
cd .. ;
###### To get TN CXP with confidence level 3

cat <<EOF > $PWD/tn_cxp_test.xml  
<pkgspec>
   />
  <product
   />
  <containers>
      <!-- TN -->
      <module
          id="$tnid"
          confidence_level="3"
      />
  </containers>
</pkgspec>

EOF

/env/rbsg2/bin/ifu --spec tn_cxp_test.xml --create ./tmp


cd tmp ;
tar xf TN* ;
## Parse xml
tn_product_ver=$($xpath $tncxp_xml '/configuration/product/@version' 2> /dev/null | sed -e 's/ version="//' -e 's/"//')
if  [ -z "$tn_product_ver" ]; then
	echo "Failed to parse tn xml: ver=$tn_product_ver"
	cleanup
	exit 1
fi

echo "version of TN CXP is $tn_product_ver"
cd  .. ;
tnfilename=$tnname$tn_product_ver'.cxp' ;
echo "tnfilename is $tnfilename"
		
###Copy the prepared CXS from makefile eeinstall 
###Prepare new CXS by including FRUM, TN CXPs and removing DUMMY CXP to install secure boards.

cd pck1 ;  
#wget -O RCP-TCU03_CXS101549_4.cxs https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-TCU03_CXS101549_4/doc/19010/RCP-TCU03_CXS101549_4.cxs ;
#wget -O RCP-DUS2_CXS101549_5.cxs https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.cxs
cp ../../pack/EE_BLACK_*_MINI.zip . ;
tar xf 	$cxsname ;
rm -rf DUMMY-ARM_CXP9021691_3.cxp ;
rm -rf $cxsname ;
cp ../tmp/TN-*.cxp . ;
cp ../tp/FRUM*.cxp . ;
sed -i 's/DUMMY.*cxp/'$name'" id="'$tnid'" version="'$tn_product_ver'" filename="'$tnfilename'"\/>\n\t<product name="FRUM" id="'$frumid'" version="'$frum_product_ver'" filename="'$frumfilename'/g' $cxsxml ;
tar czf $cxsname * ;
cp  $cxsname ../../. ;
cd .. ; rm -rf tn_cxp_test.xml tmp tp pck1 ;
cd .. ;
echo "the directory is $PWD" ;
/env/RCSDE/bin/rcstprep.sh $2 $PWD/$cxsname -no_strip_signature ;
		



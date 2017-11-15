#!/bin/sh 

RED='\033[01;31m'
progname="`basename $0`"	

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
	echo "Example: ./fpga_cxs.sh tcu03 tcu022"	
	exit 1 	
	}	
if [ $# != 2 ]; then 	
	usage	
fi	

echo "Target type is $1"
echo "Node name is $2"

if [ $1 = tcu03 ] ; then 
cxs_xml='ee_black_rcp-tcu03_mini-up.xml'
cxs_filename='EE_BLACK_RCP-T_MINI.zip'
fpga_cxp_xml='cxp102172_2.xml'
fpga_prod_id='CXP102172_2'
fpga_name="TAIPAN"
elif [ $1 = dus52 ] ; then 
cxs_xml='ee_black_dus_mini-up.xml'
cxs_filename='EE_BLACK_DUS_MINI.zip'
fpga_cxp_xml='cxp102171_1.xml'
fpga_prod_id='CXP102171_1'
fpga_name="COBRA"
fi

echo "FPGA Product Id is $fpga_prod_id"	

cat <<EOF > $PWD/fpga_cxp_test.xml  
<pkgspec>
   <meta_data
      work_dir="out"
   />
   <containers>
      <!-- FPGA: Taipan or Cobra -->
      <module
          id="$fpga_prod_id"
          confidence_level="0"
      />
  </containers>
</pkgspec>

EOF

/env/rbsg2/bin/ifu --spec fpga_cxp_test.xml --create ./black

cd black ;
tar xf *.cxp ;

## Parse xml
fpga_prod_ver=$($xpath $fpga_cxp_xml '/configuration/product/@version' 2> /dev/null | sed -e 's/ version="//' -e 's/"//')
if  [ -z "$fpga_prod_ver" ]; then
    echo "Failed to parse xml: ver=$fpga_prod_ver"
    cleanup
    exit 1
fi

echo "FPGA Product Version is: $fpga_prod_ver"

fpga_filename="$fpga_prod_id"_"$fpga_prod_ver".cxp ;

echo "FPGA filename is: $fpga_filename" ;

mkdir -p ../white ; cd ../white ;

echo "Get CXS file"	
#if [ $1 = tcu03 ] ; then 	
#	wget -O $cxs_filename https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-TCU03_CXS101549_4/doc/19010/RCP-TCU03_CXS101549_4.cxs ;
#elif [ $1 = dus52 ] ; then 	
# 	wget -O $cxs_filename https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/delivery/RCP_CSX10179_1/RCP-DUS2_CXS101549_5/doc/19010/RCP-DUS2_CXS101549_5.cxs ; 	
#fi
cp ../../pack/EE_BLACK_*_MINI.zip . ;

echo "Unpack white CXS file"
tar xf 	$cxs_filename ;
rm -rf $cxs_filename ;

echo "Copy black FPGA CXP to white"
chmod -R g+w *$fpga_prod_id.cxp
rm -f *$fpga_prod_id.cxp
cp ../black/*.cxp . ;

echo "Update CXS xml file with black version"
sed -i 's/ id="'$fpga_prod_id'".*filename/ id="'$fpga_prod_id'" version="'$fpga_prod_ver'" filename/g' $cxs_xml ;

echo "Create CXS containing black FPGA CXP"
tar czf $cxs_filename * ;

echo "Copy CXS containing black FPGA CXP"
cp $cxs_filename ../../. ;

echo "Clean up temporary directories"
cd .. ; rm -rf black white ; rm fpga_cxp_test.xml ; cd .. ;

echo "the directory is $PWD" ;
/env/RCSDE/bin/rcstprep.sh $2 $PWD/$cxs_filename ;




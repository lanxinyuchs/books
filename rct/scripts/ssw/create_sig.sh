#!/bin/bash
# ----------------------------------------------------------------------
# %CCaseFile:	create_sig.sh %
# %CCaseRev:	/main/2 %
# %CCaseDate:	2013-03-27 %
# Author:       etxbjca 
#
# Short description:
# Create Container package for RBS CS based systems (based on squashfs)
#NOTE : It is a black version of original create_sig.sh for test purpose
# **********************************************************************
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2013 All rights reserved.
#
# The information in this document is the property of Ericsson.
#
# Except as specifically authorized in writing by Ericsson, the
# receiver of this document shall keep the information contained
# herein confidential and shall protect the same in whole or in
# part from disclosure and dissemination to third parties.
#
# Disclosure and disseminations to the receivers employees shall
# only be made on a strict need to know basis.
# %CCaseCopyrightEnd%
#
# **********************************************************************
#
# Rev      Date       Name        What
# -----    -------    --------    --------------------------
# main/1   2012-03-01 etxbjca     Created, imported from ABCDE
#
# **********************************************************************

pname="`basename $0`"
## Do not change file names below, dependencies exist!!!
sqfs_img="sqfs.img"
verity_img="verity.img"
arch_info="arch-info-tmp.txt"
arch_info2="arch-info.txt"
arch_info_sm=`basename $arch_info2 .txt`".sm"
arch_fname="sw.ar"
sqfs_img_hash_fname="sqfs.hash"
altmksqfs=""
noxattrs="-no-xattrs"
debug=0

# /usr/bin/xpath not always available
which xpath > /dev/null
if [ $? -ne 0 ]; then
  xpath=`dirname $0`/xpath
  echo "Using local xpath: $xpath"
else
  xpath=xpath
fi

# xml vars
prod_id=
outdir=
xmlfile=
myfile=
rcsee_cxc_name=

# use veritysetup or not
create_verity=yes
rcsee=no
rcsee_single_sqfs=no

# Signing
nocurl=false
sm=""
ssw_server="platform-rcssign.rnd.ki.sw.ericsson.se"
ssw_url_prod="https://$ssw_server/cgi-bin/callskh-prod.pl"
ssw_url_test="https://$ssw_server/cgi-bin/callskh.pl"
ssw_url=$ssw_url_prod
curl_opts="-sS"
which curl > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "WARNING: curl not found, will not be able to do software signing"
  nocurl=true
fi

usage()
{
    echo ""
    echo "USAGE:"
    echo "$pname [options...] [dirs/files...<input to sqfs, dont use wildcards>]"
    echo "  options:"
    echo "  -o               path to output cxp file name (mandatory)"
    echo "  -f               path to cxp xml file (mandatory)"
    echo "  -v               don't create verity hash tree image"
    echo "  -t               use test certificate when signing"
    echo "  -d               print debug info"
    echo " "
    echo "Example: "
    echo "         $pname -f /tmp/cxp542514.xml -o out/CXP165461.cxp /tmp/cxp"
}

cleanup()
{
    rm -rf "$tmp"
    rm -f $arch_name $sqfs_img_hash
    if [ "$outdir" != $(dirname $xmlfile) ]; then
	rm -f $outdir/$(basename $xmlfile)
    fi
}

# parse args
while getopts dtmvx:o:f:a: opt; do
    case $opt in
	o) out_cxpfile="$OPTARG"
	    ;;
	f) xmlfile="$(readlink -f $OPTARG)"
	    ;;
	a) myfile="$(readlink -f $OPTARG)"
	    ;;    
	v) create_verity=yes
	    ;;
	x) rcsee=yes;
	   rcsee_cxc_name="$OPTARG"
	    ;;
	m) altmksqfs=yes
	    ;;
	t) ssw_url=$ssw_url_test
	    ;;
	d) debug=1
	    ;;
	\?) echo "Invalid option: -$OPTARG"
	    usage
	    exit 1;;
    esac
done
shift $((OPTIND-1))

## Tools
MKSQUASHFS=/app/rbs/wrtools/tools-sdk-20140211/usr/sbin/mksquashfs
pdir=$(dirname $(readlink -f $0))
if [ "$altmksqfs" = yes ]; then
  if [ -x $pdir/mksquashfs ]; then
  # Use alternate mksquashfs located next to this prog, if found
    MKSQUASHFS=$pdir/mksquashfs
    noxattrs=""
  else
    echo "WARNING: Alternate mksquashfs requested but not found in $pdir"
  fi
  echo "Using $MKSQUASHFS"
fi
VERITYSETUP=/app/rbs/wrtools/tools-sdk-20140211/usr/bin/veritysetup

# Check that mandatory args are set
if [ -z "$xmlfile" ] || [ -z "$out_cxpfile" ]
then
    echo "Missing mandatory args, [ '-o' or '-f' ]"
    usage
    exit 1
fi
if [ ! -f "$xmlfile" ]; then
    echo "Specified xml file does not exist, $xmlfile"
    usage
    exit 1
fi
if [ "$rcs" == "yes" ] && ! [ ls "$@" | grep -q "$rcsee_cxc_name" ]; then
    echo "Cant find $rcsee_cxc_name in $@"
    usage
    exit 1
fi

outdir=$(dirname $out_cxpfile)
sqfs_input="$@"
# Check arguments
if [ -z "$sqfs_input" ]; then
    echo "Specify at least one file/dir for squashfs"
    usage
    exit 1
fi

# create outdir if does not exist
if [ ! -d "$outdir" ]
then
    mkdir -p $outdir
fi
outdir=$(readlink -f $outdir)

## Copy xml file to out for later packing
if [ "$outdir" != $(dirname $xmlfile) ]; then
    cp -f $xmlfile $outdir
fi
if [ "$outdir" != $(dirname $myfile) ]; then
    cp -f $myfile $outdir
fi

tmp=$(mktemp -d $outdir/.sig.XXXXXX)
ar_input="$tmp/$verity_img $tmp/$sqfs_img"

# path name of the output archive
arch_name=$outdir/$arch_fname

# output for hash
sqfs_img_hash=$outdir/$sqfs_img_hash_fname

## Parse xml
id=$($xpath $xmlfile '/configuration/product/@id' 2> /dev/null | sed -e 's/ id="//' -e 's/"//')
name=$($xpath $xmlfile '/configuration/product/@name' 2> /dev/null | sed -e 's/ name="//' -e 's/"//')
ver=$($xpath $xmlfile '/configuration/product/@version' 2> /dev/null | sed -e 's/ version="//' -e 's/"//')
if [ -z "$id" ] || [ -z "$name" ] || [ -z "$ver" ]; then
    echo "Failed to parse xml: id=$id, name=$name, ver=$ver"
    cleanup
    exit 1
fi
prod_id="$name"_"$id"_"$ver"
echo "- PARSED XML FILE: $prod_id"

## Verify input args that they exist
for dr in $sqfs_input; do
    if [ ! -e "$dr" ]; then
	echo "No such file/dir, $dr"
	usage
	cleanup
	exit 1
    fi
    # Prepare input folders for mksquashfs
    if [ -d "$dr" ]; then
	sqfs_input2=$sqfs_input2" $dr/*"
    else
	sqfs_input2=$sqfs_input2" $dr"
    fi
done

#append .xml to sqfs input
sqfs_input2=$sqfs_input2" $xmlfile"
sqfs_input2=$sqfs_input2" $myfile"

# create sqfs
if [ $rcsee != yes ]; then
  echo "- CREATING SQUASHED FILE SYSTEM"
  if ! $MKSQUASHFS $sqfs_input2 "$tmp/$sqfs_img" -noI -noD -force-uid 1000 -force-gid 1000 $noxattrs -info > /dev/null
  then
    echo "Squashfs creation failed"
    cleanup
    exit 1
  fi
else
  echo "- SQUASHED FILE SYSTEM ALREADY CREATED (RCSEE_CXC)"
    # Copy RCSEE_CXC to tmp for later tar:ing
  for cxc in $sqfs_input2; do
    if [ -d "$cxc" ]; then
      if ! cp -prf $cxc $tmp; then
	echo "Failed to copy $cxc"
	cleanup
	exit 1
      fi
    fi
  done

  ## Find path to images
  find_path=$(cd $tmp/$rcsee_cxc_name && find . -name rootfs -type d)
  path2img=$rcsee_cxc_name/${find_path#./}
  if [ $? -ne 0 ]; then
    echo "Did not find 'rootfs' under $tmp/$rcsee_cxc_name"
    cleanup
    exit 1
  fi

  if [ -e $tmp/$rcsee_cxc_name/$find_path/$sqfs_img ]; then
    echo "- CREATING RCSEE IMAGE WITH SINGLE SQFS"
    rcsee_single_sqfs=yes
    mv $tmp/$rcsee_cxc_name/$find_path/$sqfs_img $tmp
    # New structure: insert xml into sqfs.img
    chmod u+w $tmp/$sqfs_img
    $MKSQUASHFS $xmlfile $tmp/$sqfs_img $mksquashfs_opts
    if [ $? -ne 0 ]; then
      echo "WARNING: Couldn't add xml to rcsee rootfs: mksquashfs failed"
    else
      echo "Added xml to rootfs"
    fi
    chmod u-w $tmp/$sqfs_img
  else
    echo "- CREATING RCSEE IMAGE WITH MULTIPLE SQFS"
  fi
fi

if [ $rcsee != yes ] || [ $rcsee_single_sqfs == yes ]; then
# reserve space for hash tree
    dd if=/dev/zero of="$tmp/$verity_img" bs=1 count=4096 2> /dev/null

# create verity hash tree
    if  [ "$create_verity" == yes ]; then
	echo "- CREATING VERITY HASH TREE IMAGE"
	if ! $VERITYSETUP --hash=sha512 format "$tmp/$sqfs_img" "$tmp/$verity_img" > $tmp/.hash_result.$$; then
	    echo "Veritysetup failed"
	    cleanup
	    exit 1
	fi
	mv $tmp/.hash_result.$$ $sqfs_img_hash
    fi
else
# rcsee == yes
    rcsee_image_names="home rootfs sirpa testbox"
    for img in $rcsee_image_names
    do
	echo "- CREATING VERITY HASH TREE IMAGE ($img.sqsh)"
	dd if=/dev/zero of="$tmp/$path2img/$img.verity" bs=1 count=4096 2> /dev/null
	if ! $VERITYSETUP --hash=sha512 format "$tmp/$path2img/$img.sqsh" "$tmp/$path2img/$img.verity" > $tmp/$img.hash; then
	    echo "Veritysetup failed"
	    cleanup
	    exit 1
	fi
    done
fi

# Create arch_info file with needed information
#"prod_id=%s roothash=%s"
#" lvsize-verity=%d lvsize-sqfs=%d"
if [ $rcsee != yes  ] || [ $rcsee_single_sqfs == yes ]; then
    echo -n "prod-id=$prod_id" > $tmp/$arch_info
    echo -n " roothash=`grep -i \"root hash:\" $sqfs_img_hash | awk '{print $3}'`" >> $tmp/$arch_info
    echo -n " lvsize-verity=`stat -c %s $tmp/$verity_img`" >> $tmp/$arch_info
    echo -n " lvsize-sqfs=`stat -c %s $tmp/$sqfs_img`" >> $tmp/$arch_info
    input_size=$(du -csk $tmp/$sqfs_img $tmp/$verity_img $xmlfile | xargs | awk '{print$(NF-1)}')
    ## Add size for arch-info.txt
    cxp_size=$(( $input_size+4 ))
    echo " cxp_size=$cxp_size" >> $tmp/$arch_info

else
# rcsee == yes
    echo "prod-id=$prod_id" > $tmp/$arch_info
    for img in $rcsee_image_names
    do
	echo "roothash.$img=`grep -i \"root hash:\" $tmp/$img.hash | awk '{print $3}'`" >> $tmp/$arch_info
	echo "lvsize-verity.$img=`stat -c %s $tmp/$path2img/$img.verity`" >> $tmp/$arch_info
	echo "lvsize-sqfs.$img=`stat -c %s $tmp/$path2img/$img.sqsh`" >> $tmp/$arch_info
    done
    echo "rcsee_cxc_path=$path2img" >> $tmp/$arch_info

    # Remove temp hash files
    rm -f $tmp/*.hash

    input_size=$(du -csk $tmp | xargs | awk '{print$(NF-1)}')
    ## Add size for arch-info.txt
    cxp_size=$(( $input_size+4 ))
    echo "cxp_size=$cxp_size" >> $tmp/$arch_info
fi

## Pad with zeroes to even size, fixes a bug when extracting ar archive
## later on target
dd if=$tmp/$arch_info ibs=4k count=1 of=$tmp/$arch_info2 conv=sync 2> /dev/null

# Do software signing
if [ "$nocurl" = "false" ]; then
  [ $debug -eq 1 ] && ssw_url=${ssw_url}\?debug=1
  curl_cmd="curl $curl_opts -F file_in=@$tmp/$arch_info2 -o $tmp/$arch_info_sm $ssw_url"
  echo "- CREATING SIGNATURE MODULE"
  [ "$debug" -eq 1 ] && echo "$curl_cmd"
  $curl_cmd
  curl_ret=$?
  if [ $curl_ret -ne 0 ]; then
    echo "WARNING: curl returned $curl_ret, signing failed"
    sm=""
  fi
fi

if [ $rcsee != yes ]; then
    ar_input="$tmp/$arch_info2 $tmp/$arch_info_sm $ar_input"
    # Create archive with all of the files
    echo "- CREATING ARCHIVE from : $ar_input"
    if ! ar rs "$arch_name" $ar_input 2> /dev/null
    then
	echo "Failed to create an ar archive"
	cleanup
	exit 1
    fi
fi

## create cxp tgz
if [ $rcsee != yes ]; then
    echo "- CREATING CXP from : $arch_name $xmlfile"
    if ! tar -C $outdir -czf $out_cxpfile $(basename $arch_name) $(basename $xmlfile)
    then
	echo "Failed to create CXP tgz"
	cleanup
	exit 1
    fi
else
    cp -f $xmlfile $tmp
    cp -f $myfile $tmp
    # Include SSW certificates (if found) in archive
    certs=""
    if [ -d $1/$rcsee_cxc_name/rcsee*/certs ]; then
      certs=`find $1/$rcsee_cxc_name/rcsee*/certs -type f -print`
    else
      echo "No certificates in $rcsee_cxc_name"
    fi

    # Include the single squashfs with verity if it exists
    sqfs_files=""
    if [ $rcsee_single_sqfs == yes ]; then
	sqfs_files="$ar_input"
    fi

    # Place arch-info.txt into sw.ar to have a common way to extract it
    # this is a temp solution for now
    # in the future arch-info.txt should be placed under root for all cxp types
    #ar_cmd="ar rs $tmp/$arch_fname $tmp/$arch_info2 $tmp/$arch_info_sm $certs $sqfs_files"
   # [ "$debug" -eq 1 ] && echo "$ar_cmd"
   # $ar_cmd 2> /dev/null
   [ -e $tmp/$arch_info2 ] && rm -f $tmp/$arch_info2
    [ -e "$tmp/$arch_info_sm" ] && rm -f "$tmp/$arch_info_sm"
    [ -e $tmp/$arch_info ] && rm -f $tmp/$arch_info
    for afile in $sqfs_files; do
	[ -e $afile ] && rm -f $afile
    done
    # sort input to tar, place small files first
    tar_input=
    for item in `ls $tmp | xargs`; do
	if [ -d $item ]; then
	    tar_input="$tar_input $item"
	fi
    done
    for item in `ls $tmp | xargs`; do
	if [ ! -d $item ]; then	
	       tar_input="$item $tar_input"
	fi
    done
    echo "- CREATING RCSEE CXP from : $tar_input"
    if ! tar -C $tmp -czf $out_cxpfile $tar_input
    then
	echo "Failed to create RCS EE CXP tgz"
	cleanup
	exit 1
    fi
fi

echo "- DONE: $out_cxpfile"
cleanup
exit 0

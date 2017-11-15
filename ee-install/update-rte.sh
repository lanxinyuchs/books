#!/bin/bash

name=$(basename $0)
linuxdir=/repo/$USER/rcs-ee/rcs-linux-arm
rtedir=/repo/$USER/rcs-ee/rcs-rte
dryrun=yes
tmpdir=$(mktemp -d ${TMPDIR:-/tmp}/update-rte.XXXXXX})
mkdir -p $tmpdir
update=n

ncpdirs=(\
ncp_rte/src \
ncp_rte/src/api/cat \
ncp_rte/src/api/cfg_node/regs \
ncp_rte/src/api/common \
ncp_rte/src/api/common/regs \
ncp_rte/src/api/cow \
ncp_rte/src/api/cow/regs \
ncp_rte/src/api/eioa \
ncp_rte/src/api/eioa/regs \
ncp_rte/src/api/misc \
ncp_rte/src/api/mmb \
ncp_rte/src/api/mme \
ncp_rte/src/api/mpp/include \
ncp_rte/src/api/mpp/regs \
ncp_rte/src/api/mtm \
ncp_rte/src/api/nca \
ncp_rte/src/api/nca/linux_kernel/ace \
ncp_rte/src/api/nca/regs \
ncp_rte/src/api/nrcp \
ncp_rte/src/api/nrcp/regs \
ncp_rte/src/api/pab \
ncp_rte/src/api/pic \
ncp_rte/src/api/pll \
ncp_rte/src/api/sal \
ncp_rte/src/api/sal/linux \
ncp_rte/src/api/sal/arch/arm \
ncp_rte/src/api/sed \
ncp_rte/src/api/spp \
ncp_rte/src/api/spp/regs \
ncp_rte/src/api/sysmem/regs \
ncp_rte/src/api/task \
ncp_rte/src/api/task/raw \
ncp_rte/src/api/timer \
ncp_rte/src/api/timer/regs \
ncp_rte/src/api/vpm \
)

ncpsrc=( \
ncp_bus_ace.c \
ncp_task_api_entry.c \
ncp_task_config_acp34xx.c \
ncp_task_acp34xx.c \
ncp_task_common.c \
ncp_task_config_axm55xx.c \
ncp_task_axm55xx.c \
ncp_task_ncav2_raw.c \
ncp_task_raw.c \
ncp_dev.c \
ncp_dev_eioa.c \
ncp_dev_eioa_55xx.c \
ncp_dev_eioa_utils_55xx.c \
ncp_sal.c \
ncp_dev_intr.c \
ncp_eioa_mac.c \
ncp_dev_event.c \
ncp_dev_i2c.c \
ncp_dev_i2c_acp3400.c \
ncp_dev_i2c_axm5500.c \
ncp_dev_lbi.c \
ncp_eioa_epp_35xx.c \
ncp_eioa_epp_55xx.c \
ncp_eioa_ipp_35xx.c \
ncp_eioa_ipp_55xx.c \
ncp_eioa_mac_custom_phy.c \
ncp_eioa_mac_mdio_phy.c \
ncp_eioa_mac_phy_common.c \
ncp_eioa_mac_phy_common_55xx.c \
ncp_dev_cat.c \
ncp_dev_pka.c \
ncp_dev_eioa_35xx.c \
ncp_eioa_hss_55xx.c \
ncp_eioa_mac_phy_common_35xx.c \
ncp_dev_eioa_utils_35xx.c \
ncp_dev_linux_kernel.c \
ncp_dev_linux_kernel_axm55xx.c \
ncp_sal_linux.c \
ncp_config.c \
ncp_utils.c \
ncp_utils_io.c \
ncp_vpm.c \
ncp_status.c \
ncp_pka_timing.c \
)

ncpuapi=( \
ncp.h \
ncp_sal.h \
ncp_sal_types.h \
ncp_sal_target.h \
ncp_sal_linux-arma15.h \
ncp_sal_types_linux.h \
ncp_dev.h \
ncp_types.h \
ncp_status.h \
ncp_dev_config_regs.h \
ncp_task.h \
ncp_eioa.h \
ncp_bitmap.h \
ncp_eioa_hss_utils.h \
ncp_nodes.h \
ncp_sal_linux.h \
ncp_map.h \
ncp_config.h \
ncp_vpm.h \
ncp_regions.h \
ncp_spp_regions.h \
ncp_eioa_regions.h \
ncp_timer_regions.h \
ncp_pab_top_regions.h \
ncp_mpp_regions.h \
ncp_sed_regions.h \
ncp_pic_regions.h \
ncp_mtm_regions.h \
ncp_mme_regions.h \
ncp_mmb_regions.h \
ncp_nca_regions.h \
ncp_nrcp.h \
ncp_x1_pll_regions.h \
ncp_syscon_clk_regions.h \
ncp_axis_apb2ser2_regions.h \
ncp_axis_apb2ser3_regions.h \
ncp_axis_apb2ser_clk_3500_regions.h \
ncp_rsrc.h \
ncp_cow.h \
ncp_eioa_mac.h \
ncp_pka.h \
ncp_timer.h \
ncp_dev_linux.h \
ncp_pka_timing.h \
)

adkdirs=( \
netdriver/src \
netdriver/src/include \
)

adksrc=( \
adk_netd_interface.c \
adk_netd_gen_comm.c \
adk_netd_mgmt.c \
adk_netd_phy_tstamp.c \
adk_netd_vei_if.c \
adk_netd_mac.c \
adk_netd_main.c \
adk_netd_tstamp.c \
)

adkuapi=( \
adk_netd_interface.h \
adk_netd_gen_comm.h \
adk_netd_netlink.h \
)

adkkapi=( \
adk_netd_phy_tstamp.h \
)

linuxdirs_clean=( \
drivers/net/npu/ncp \
drivers/net/npu/ncp/arch \
drivers/net/npu/adk \
drivers/net/npu/adk/include \
include/linux/npu/regs \
include/uapi/linux/npu \
)

linuxdirs_copy=( \
drivers/net/npu/ncp \
drivers/net/npu/ncp/arch \
drivers/net/npu/adk \
drivers/net/npu/adk/include \
include/linux/npu \
include/linux/npu/regs \
include/uapi/linux/npu \
)

usage()
{
    echo "$name [options]"
    echo "options:"
    echo "  -d  Dry run, default '$dryrun'" 
    echo "  -h  This text"
    echo "  -l  rcs-linux-arm directory, default '$linuxdir'"
    echo "  -r  rcs-rte directory, default '$rtedir'"
}

cleanup()
{
    echo "$name exit"
    test -d $linuxdir || echo "$linuxdir ?"
    test -d $rtedir || echo "$rtedir ?"
    rm -rf $tmpdir
}

trap cleanup SIGHUP SIGINT SIGTERM EXIT

verify()
{
    local dirs=${#linuxdirs[*]}
    local dir cnt reply
    local ret=0
    local repo

    test -d $linuxdir || exit 1
    test -d $rtedir || exit 1
    pushd . > /dev/null
    cd $linuxdir
    repo=$(git rev-parse --show-toplevel 2> /dev/null)
    if [ "$repo" != "$linuxdir" ] ; then
	echo "$linuxdir is not a git repository"
	ret=1
    else
	for (( cnt=0; cnt<$dirs; cnt++ )) ; do
            dir=${linuxdirs[$cnt]}
	    if ! test -d $linuxdir/$dir ; then
		echo "$linuxdir/$dir : not found"
		ret=1
	    fi
	done
    fi
    git status --porcelain --untracked-files=no | grep 'M\|A\|D\|R\|C\|U' > /dev/null
    if [ $? -eq 0 ]; then
	read -p "$linuxdir is dirty - continue anyway? (y/n) " reply
	[ "$reply" != "y" ]  && ret=1
    fi
    cd $rtedir
    repo=$(git rev-parse --show-toplevel 2> /dev/null)
    if [ "$repo" != "$rtedir" ] ; then
	echo "$rtedir is not a git repository"
	ret=1
    fi
    git status --porcelain --untracked-files=no | grep 'M\|A\|D\|R\|C\|U' > /dev/null
    if [ $? -eq 0 ]; then
	read -p "$rtedir is dirty - continue anyway? (y/n) " reply
	[ "$reply" != "y" ]  && ret=1
    fi
    if [ ! -d $tmpdir ] ; then
	echo "workdir : $tmpdir is not a directory"
	ret=1
    fi
    popd > /dev/null
    [ $ret -eq 0 ] || exit 1
}

copy_ncp()
{
    local dirs=${#ncpdirs[*]}
    local srcs=${#ncpsrc[*]}
    local uapis=${#ncpuapi[*]}
    local warnings=0
    local cnt hcnt
    local dir src path tmp

    mkdir -p $tmpdir/drivers/net/npu/ncp/arch
    mkdir -p $tmpdir/include/uapi/linux/npu
    mkdir -p $tmpdir/include/linux/npu/regs

    echo -n "Copy ncp module sources..."
    for (( cnt=0; cnt<$srcs; cnt++ )) ; do
	src=${ncpsrc[$cnt]}
	path=$(find $rtedir/ncp_rte/ -name $src 2> /dev/null)
	tmp=$(basename $path)
	if [ "$tmp" != "$src" ] ; then
	    echo "WARNING - src not found : $src"
	    warnings=$((warnings+1))
	    continue
	fi
	cp $path $tmpdir/drivers/net/npu/ncp/.
    done
    echo "Done - $cnt sources copied"
    echo -n "Copy ncp module headers..."
    hcnt=0
    for (( cnt=0; cnt<$dirs; cnt++ )) ; do
        dir=${ncpdirs[$cnt]}
	tmp=$(basename $dir)
	for path in `ls $rtedir/$dir/*.h` ; do
	    hcnt=$((hcnt+1))
	    if [ "$tmp" == "regs" ] ; then
		cp $path $tmpdir/include/linux/npu/regs/.
	    elif [ "$tmp" == "arm" ] ; then
	        cp $path $tmpdir/drivers/net/npu/ncp/arch/.
	    else
		cp $path $tmpdir/drivers/net/npu/ncp/.
	    fi
	done
    done
    echo "Done - $hcnt headers copied"
    echo -n "Removing arch/os specific unused h-files..."
    # remove arch/os specific unused h-files
    rm $tmpdir/drivers/net/npu/ncp/*_vxworks-*.h
    rm $tmpdir/drivers/net/npu/ncp/*_ose-*.h
    rm $tmpdir/drivers/net/npu/ncp/*_wrl-*.h
    rm $tmpdir/drivers/net/npu/ncp/*ppc476.h
    rm $tmpdir/drivers/net/npu/ncp/*win32.h
    rm $tmpdir/drivers/net/npu/ncp/*x86.h
    rm $tmpdir/drivers/net/npu/ncp/*x86_64.h
    echo "Done"
    echo -n "Remove all h-files that contains a prorietary license note..."
    hcnt=0
    # remove all h-files that contains a prorietary license note
    for path in `find $tmpdir/drivers/net/npu/ncp -name '*.h'`; do 
	grep "PROPRIETARY NOTE" $path > /dev/null
	if [ $? -eq 0 ]; then 
	    hcnt=$((hcnt+1))
	    rm $path
	fi
    done
    for path in `find $tmpdir/include/linux/npu/regs -name '*.h'`; do 
	grep "PROPRIETARY NOTE" $path > /dev/null
	if [ $? -eq 0 ]; then 
	    hcnt=$((hcnt+1))
	    rm $path
	fi
    done
    echo "Done - $hcnt headers removed"
    echo -n "Move user API headers to include/uapi/linux/npu..."
    hcnt=0
    # move user API headers to include/uapi/linux/npu
    for (( cnt=0; cnt<$uapis; cnt++ )) ; do
	src=${ncpuapi[$cnt]}
        path=$(find $tmpdir/drivers/net/npu/ncp/ -name $src 2> /dev/null)
        tmp=$(basename $path 2> /dev/null)
        if [ "$tmp" != "$src" ] ; then
            echo "WARNING - user API hdr not found : $src"
	    warnings=$((warnings+1))
            continue
        fi
	hcnt=$((hcnt+1))
	mv $path $tmpdir/include/uapi/linux/npu/.
    done
    echo "Done - $hcnt headers moved"
    echo "$warnings WARNINGS"
}

copy_adk()
{
    local dirs=${#adkdirs[*]}
    local srcs=${#adksrc[*]}
    local uapis=${#adkuapi[*]}
    local kapis=${#adkkapi[*]}
    local warnings=0
    local cnt hcnt
    local dir src path tmp
    
    mkdir -p $tmpdir/drivers/net/npu/adk/include
    mkdir -p $tmpdir/include/uapi/linux/npu

    echo -n "Copy adkNetD module sources..."
    for (( cnt=0; cnt<$srcs; cnt++ )) ; do
	src=${adksrc[$cnt]}
	path=$(find $rtedir/netdriver/ -name $src 2> /dev/null)
	tmp=$(basename $path)
	if [ "$tmp" != "$src" ] ; then
	    echo "WARNING - src not found : $src"
	    warnings=$((warnings+1))
	    continue
	fi
	cp $path $tmpdir/drivers/net/npu/adk/.
    done
    echo "Done - $cnt sources copied"
    echo "Emulate adk make gen_hdr tgt - generat adk_netd_netlink.h"
    cat <<EOF > $rtedir/netdriver/src/include/adk_netd_netlink.h
#ifndef __ADK_NETD_NETLINK_H__
#define __ADK_NETD_NETLINK_H__
#define NETLINK_VEI_NETDEV  31
#define NETLINK_GEN_COMM_NETDEV 30
#endif
EOF
    echo -n "Copy adk module headers..."
    hcnt=0
    for (( cnt=0; cnt<$dirs; cnt++ )) ; do
        dir=${adkdirs[$cnt]}
	tmp=$(basename $dir)
	for path in `ls $rtedir/$dir/*.h` ; do
	    hcnt=$((hcnt+1))
	    if [ "$tmp" == "include" ] ; then
		cp $path $tmpdir/drivers/net/npu/adk/include/.
	    else
		cp $path $tmpdir/drivers/net/npu/adk/.
	    fi
	done
    done
    echo "Done - $hcnt headers copied"
    echo -n "Move user API headers to include/uapi/linux/npu..."
    hcnt=0
    # move user API headers to include/uapi/linux/npu
    for (( cnt=0; cnt<$uapis; cnt++ )) ; do
	src=${adkuapi[$cnt]}
        path=$(find $tmpdir/drivers/net/npu/adk/ -name $src 2> /dev/null)
        tmp=$(basename $path 2> /dev/null)
        if [ "$tmp" != "$src" ] ; then
            echo "WARNING - user API hdr not found : $src"
	    warnings=$((warnings+1))
            continue
        fi
	hcnt=$((hcnt+1))
        mv $path $tmpdir/include/uapi/linux/npu/.
    done
    echo "Done - $hcnt headers moved"
    echo "$warnings WARNINGS"
    echo -n "Move kernel API headers to include/linux/npu..."
    hcnt=0
    # move kernel API headers to include/linux/npu
    for (( cnt=0; cnt<$kapis; cnt++ )) ; do
	src=${adkkapi[$cnt]}
        path=$(find $tmpdir/drivers/net/npu/adk/ -name $src 2> /dev/null)
        tmp=$(basename $path 2> /dev/null)
        if [ "$tmp" != "$src" ] ; then
            echo "WARNING - kernel API hdr not found : $src"
	    warnings=$((warnings+1))
            continue
        fi
	hcnt=$((hcnt+1))
        mv $path $tmpdir/include/linux/npu/.
    done
    echo "Done - $hcnt headers moved"
    echo "$warnings WARNINGS"

}

update_linuxdir ()
{
    local dirs dir cnt

    dirs=${#linuxdirs_clean[*]}
    echo "Copy ncp and adk driver source to $linuxdir"
    for (( cnt=0; cnt<$dirs; cnt++ )) ; do
        dir=${linuxdirs_clean[$cnt]}
	 rm -f $linuxdir/$dir/*.[ch]
    done
    dirs=${#linuxdirs_copy[*]}
    for (( cnt=0; cnt<$dirs; cnt++ )) ; do
        dir=${linuxdirs_copy[$cnt]}
	[ -d $linuxdir/$dir ] || mkdir $linuxdir/$dir
	 cp $tmpdir/$dir/*.[ch] $linuxdir/$dir/
    done
    (cd $linuxdir && git status -s --porcelain | \
	while read mode path ; do
	    [ "$(dirname $path)" == "." ] && continue
	    [ "$mode" == "??" ] && mode="New untracked file"
	    [ "$mode" == "D" ] && mode="Deleted           "
	    [ "$mode" == "M" ] && mode="Modified          "
	    [ "$mode" == "A" ] && mode="Added             "
	    [ "$mode" == "R" ] && mode="Renamed           "
	    if test -e $tmpdir/$(dirname $path) ; then
		echo "$mode : $linuxdir/$path"
	    fi
	done) 
    echo "All done except the git add/rm commit stuff..."
}

while getopts "d:hl:r:" opt; do

    case $opt in
	d)
	    dryrun=$OPTARG
	    ;;
	h)
	    usage
	    exit 0
	    ;;
	l)
	    linuxdir=$OPTARG
	    ;;
	r)
	    rtedir=$OPTARG
	    ;;
	?)
	    usage
	    exit 1
	    ;;
	esac
done

verify
copy_ncp
copy_adk

[ "$dryrun" == "yes" ] && exit 0

read -p "Go ahead and update $linuxdir? (y/n) " update
[ "$update" == "y" ] && update_linuxdir

exit 0

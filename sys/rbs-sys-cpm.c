/*
 * RBS system CPM common
 *
 * Copyright 2013 Ericsson AB
 * Andrey Panteleev <andrey.xx.panteleev@ericsson.com>
 *
 * This program is free software; you can redistribute  it and/or modify it
 * under  the terms of  the GNU General  Public License as published by the
 * Free Software Foundation;  either version 2 of the  License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#define pr_fmt(fmt) "RBS Sys: " fmt

#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/reboot.h>
#include <linux/slab.h>
#include <linux/memblock.h>
#include <linux/mm.h>
#include <linux/vmalloc.h>
#include <linux/io.h>

#include <linux/rbs/rbs-fn.h>
#include <linux/rbs/rbs-sys.h>
#include <linux/of.h>
#include <linux/of_address.h>
#include <linux/of_fdt.h>

#include <asm/uaccess.h>
#include <asm/io.h>
#if defined(__arm__)
#include <asm/mach/map.h>
#endif

#include "rbs-sys-internal.h"
#include "rbs-sys-cpm.h"

#define PSTORE_SIZE             0x100000 /* 1Mb */

/* default mailbox location and size */
#define BOOT_MBOX_BASE		0x20000000
#define BOOT_MBOX_SIZE		0x00001000
/* default ramlog location and size */
#define BOOT_RAMLOG_BASE	0x20001000
#define BOOT_RAMLOG_SIZE	0x00010000

#define BOOT_MAGIC_MASK		0xffff0000

#define BOOT_REASON_POWERON	0xaffe1111
#define BOOT_REASON_WATCHDOG	0xaffe2222
#define BOOT_REASON_MMI_RESET	0xaffe4444
#define BOOT_REASON_BPM_SW	0xaffe6666
#define BOOT_REASON_COLD	0xaffe8888
#define BOOT_REASON_COLD_WT	0xaffeaaaa
#define BOOT_REASON_COLD_WET	0xaffecccc
#define BOOT_REASON_TEST	0xaffeeeee
#define BOOT_REASON_UNCR_ECCERR	0xaffedddd

#define BOOT_REQUEST_COLD	0xebba8888
#define BOOT_REQUEST_COLD_WT	0xebbaaaaa
#define BOOT_REQUEST_COLD_WET	0xebbacccc

#define BOOT_STARTED_CF		0xaffe3333
#define BOOT_STARTED_FB		0xaffe5555
#define BOOT_STARTED_NL_T2	0xaffe7777
#define BOOT_STARTED_NL_T3	0xaffe8888
#define BOOT_STARTED_UP		0xaffe9999

#define BOOT_UPGRADE_FLAG	0xebbaf1a9

#define BOOT_API_MAGIC		0xaffe0000

/* needed for uboot stage 2 */
#define PERSISTENT_DATA_4_REG       (0xf0)
#define COLD_W_TEST_IND       (0x00000001)
#define COLD_W_E_TEST_IND     (0x00000002)

/* lowest supported version */
#define BOOT_MINAPI_VERSION	0x1

struct boot_mailbox {
	__u32 api_version;	/* 0x00 */
	__u32 boot_count;	/* 0x04 */
	__u32 restart_request;	/* 0x08 */
	__u32 restart_reason;	/* 0x0c */
	__u64 fault;		/* 0x10 */
	__u64 timestamp;	/* 0x18 */
	__u32 boot_started;	/* 0x20 */
	__u32 poweron_time;	/* 0x24 */
	__u32 upgrade_flag;	/* 0x28 */
};

struct rbs_sys_cpm {
	struct boot_mailbox *mbox;
	__u32 mbox_version;
	phys_addr_t mbox_base;
	phys_addr_t ramlog_base;
	phys_addr_t ramlog_size;
};

static void __iomem *syscon_base;

static inline phys_addr_t __get_pmem_node_base(const char *area,
					       const char *region,
					       phys_addr_t base_default)
{
	const __be32 *pp;
	struct device_node *np;
	int sz;

	np = of_find_node_by_name(NULL, area);
	if (np) {
		np = of_parse_phandle(np, region, 0);
		if (!np)
			return base_default;

		pp = of_get_property(np, "reg", NULL);
		if (pp)
			return of_read_number(pp, of_n_addr_cells(np));
	} else {
		np = of_find_node_by_name(NULL, "persistent_mem_area");
		if (!np)
			return base_default;

		pp = of_get_property(np, "base", &sz);
		if (pp && sz == sizeof(*pp))
			return (phys_addr_t) be32_to_cpup(pp);
	}

	return base_default;
}

static inline void __get_pmem_node_location(const char *area,
					    const char *region,
					    const char *name,
					    phys_addr_t *base,
					    phys_addr_t *size,
					    phys_addr_t base_default,
					    phys_addr_t size_default)
{
	struct device_node *np;
	const __be32 *val;
	int nsize;
	phys_addr_t pmem_base;
	phys_addr_t node_off;

	pmem_base = __get_pmem_node_base(area, region, base_default);
	np = of_find_node_by_name(NULL, name);
	if (!np)
		goto out_def;

	val = of_get_property(np, "offset", &nsize);
	if (val && nsize == sizeof(*val))
		node_off = (phys_addr_t) be32_to_cpup(val);
	else
		goto out_def;

	val = of_get_property(np, "size", &nsize);
	if (val && nsize == sizeof(*val))
		*size = (phys_addr_t) be32_to_cpup(val);
	else
		goto out_def;

	*base = pmem_base + node_off;
	return;

out_def:
	*base = base_default;
	*size = size_default;
}

static inline void get_pmem_node_location(const char *rname,
					  const char *name,
					  phys_addr_t *base,
					  phys_addr_t *size,
					  phys_addr_t base_default,
					  phys_addr_t size_default)
{
	phys_addr_t base_mod, size_mod;
	int fake_pstore = 0;
	char area[64], region[64];

	strcpy(area, rname);
	strcat(area, "_area");
	strcpy(region, rname);
	strcat(region, "-region");

	if (!strcmp(name, "trace")) {
		/* Try fetching "pstore" area */
		__get_pmem_node_location(area,
					 region,
					 "pstore",
					 &base_mod,
					 &size_mod,
					 0,
					 0);

		if (base_mod == 0 && size_mod == 0)
			fake_pstore = 1;
	}

	if (fake_pstore) {
		__get_pmem_node_location(area,
					 region,
					 "trace",
					 base,
					 size,
					 base_default,
					 size_default);

		/* If there is no trace section in the DTB,
		   we should not steal a section for pstore */
		if (*base == base_default && *size == size_default)
			return;

		if (*size >= 4*PSTORE_SIZE)
			*size = *size - PSTORE_SIZE;
	} else {
		__get_pmem_node_location(area,
					 region,
					 name,
					 base,
					 size,
					 base_default,
					 size_default);
	}

	return;
}

static void __iomem *map_mailbox(phys_addr_t base, phys_addr_t size)
{
	size = PAGE_ALIGN(size);
	if (size > PAGE_SIZE) {
		pr_info("Invalid mailbox size\n");
		return NULL;
	}

	return ioremap_nocache(base, size);
}

int rbs_sys_cpm_board_init(struct rbs_sys *sys)
{
	struct device_node *syscon_np;
	struct rbs_sys_cpm *cpm;
	phys_addr_t mbox_size;

	syscon_np = of_find_compatible_node(NULL, NULL, "intel,axxia-syscon");
	if (!syscon_np)
		syscon_np = of_find_compatible_node(NULL, NULL, "lsi,axxia-syscon");

	if (!syscon_np)
		syscon_base = ioremap(0x2010030000, 0x1000);
	else
		syscon_base = of_iomap(syscon_np, 0);

	WARN_ON(!syscon_base);

	cpm = kmalloc(sizeof(*cpm), GFP_KERNEL);
	if (!cpm)
		return -ENOMEM;
	sys->board_data = cpm;

	get_pmem_node_location("boot_shared",
			       "mailbox",
			       &cpm->mbox_base, &mbox_size,
			       BOOT_MBOX_BASE, BOOT_MBOX_SIZE);
	pr_info("Mailbox base %llx, size %llu\n",
			cpm->mbox_base, mbox_size);
	get_pmem_node_location("boot_shared",
			       "ramlog",
			       &cpm->ramlog_base, &cpm->ramlog_size,
			       BOOT_RAMLOG_BASE, BOOT_RAMLOG_SIZE);
	pr_info("ramlog base %llx, size %llu\n",
			cpm->ramlog_base, cpm->ramlog_size);

	cpm->mbox = map_mailbox(cpm->mbox_base, mbox_size);
	if (!cpm->mbox) {
		pr_info("Unable to map boot mailbox\n");
		return 0;
	}

	/* check magic */
	if ((cpm->mbox->api_version & BOOT_MAGIC_MASK) != BOOT_API_MAGIC) {
		pr_info("Invalid boot mailbox magic (0x%x)\n",
				cpm->mbox->api_version);
		goto invalid_format;
	}

	/* check if mail box api version is supported */
	cpm->mbox_version = cpm->mbox->api_version & ~BOOT_MAGIC_MASK;
	if (cpm->mbox_version < BOOT_MINAPI_VERSION) {
		pr_info("Unsupported boot mailbox API version:0x%x\n",
				cpm->mbox_version);
		goto invalid_format;
	}

	pr_info("Boot mailbox API version: 0x%x\n",
			cpm->mbox_version);

	/* log faults */
	if (cpm->mbox->fault) {
		pr_info("u-boot fault indication: 0x%llx\n",
				cpm->mbox->fault);
	}

	return 0;

invalid_format:
	if (cpm->mbox)
		iounmap(cpm->mbox);
	cpm->mbox = NULL;
	cpm->mbox_version = 0;
	return 0;
}

void rbs_sys_cpm_board_set_restart(struct rbs_sys *sys,
		struct rbs_sys_restart_data *data)
{
	struct rbs_sys_cpm *cpm = sys->board_data;
	struct timespec ts;

	if (!cpm || !cpm->mbox)
		return;

	switch (data->type) {
	case RBS_SYSRESTART_COLD_WT:
		cpm->mbox->restart_request = BOOT_REQUEST_COLD_WT;

		/*
		  indicate "cold w test" in persistent data reg, needed
		  for U-Boot stage2 memory test.
		*/
		writel(COLD_W_TEST_IND, syscon_base + PERSISTENT_DATA_4_REG);
		break;
	case RBS_SYSRESTART_COLD_WET:
		cpm->mbox->restart_request = BOOT_REQUEST_COLD_WET;

		/*
		  indicate "cold w e test" in persistent data reg, needed
		  for U-Boot stage2 memory test.
		*/
		writel(COLD_W_E_TEST_IND, syscon_base + PERSISTENT_DATA_4_REG);
		break;
	default:
		cpm->mbox->restart_request = BOOT_REQUEST_COLD;
		break;
	}

	/* update timestamp */
	getnstimeofday(&ts);
	cpm->mbox->timestamp = (__u64) ts.tv_sec;

	/*
	  flush L3-cache not needed. Done by initiateRetentionReset()
	*/
}

int rbs_sys_cpm_board_get_restart(struct rbs_sys *sys,
		struct rbs_sys_restart_data *data)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	switch (cpm->mbox->restart_reason) {
	case BOOT_REASON_POWERON:
		data->type = RBS_SYSRESTART_POWERON;
		break;
	case BOOT_REASON_WATCHDOG:
		data->type = RBS_SYSRESTART_WATCHDOG;
		break;
	case BOOT_REASON_MMI_RESET:
		data->type = RBS_SYSRESTART_MMIRESET;
		break;
	case BOOT_REASON_BPM_SW:
		data->type = RBS_SYSRESTART_BPMSW;
		break;
	case BOOT_REASON_COLD_WT:
		data->type = RBS_SYSRESTART_COLD_WT;
		break;
	case BOOT_REASON_COLD_WET:
		data->type = RBS_SYSRESTART_COLD_WET;
		break;
	case BOOT_REASON_TEST:
		data->type = RBS_SYSRESTART_TEST;
		break;
	case BOOT_REASON_UNCR_ECCERR:
		data->type = RBS_SYSRESTART_ECC;
		break;
	case BOOT_REASON_COLD:
	default:
		data->type = RBS_SYSRESTART_COLD;
		break;
	}

	/* TODO: other fields of rbs_sys_restart_data are not used, remove */
	data->ecode = 0;
	data->extra1 = 0;
	data->extra2 = 0;

	return 0;
}

int rbs_sys_cpm_board_get_restart_reason(struct rbs_sys *sys,
					 unsigned int *reason)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	*reason = cpm->mbox->restart_reason;

	return 0;
}

int rbs_sys_cpm_board_get_fault(struct rbs_sys *sys,
				unsigned long long *fault)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	*fault = cpm->mbox->fault;

	return 0;
}

int rbs_sys_cpm_board_set_bootcount(struct rbs_sys *sys, unsigned int count)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	cpm->mbox->boot_count = count;
	return 0;
}

int rbs_sys_cpm_board_get_bootcount(struct rbs_sys *sys, unsigned int *count)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	*count = cpm->mbox->boot_count;
	return 0;
}

int rbs_sys_cpm_board_get_bootstarted(struct rbs_sys *sys, unsigned int *bs)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	switch (cpm->mbox->boot_started) {
	case BOOT_STARTED_CF:
		*bs = RBS_SYSBOOT_CF;
		break;
	case BOOT_STARTED_FB:
		*bs = RBS_SYSBOOT_FB;
		break;
	case BOOT_STARTED_NL_T2:
		*bs = RBS_SYSBOOT_NL;
		break;
	case BOOT_STARTED_NL_T3:
		*bs = RBS_SYSBOOT_NL3;
		break;
	case BOOT_STARTED_UP:
		*bs = RBS_SYSBOOT_UP;
		break;
	default:
		*bs = RBS_SYSBOOT_UN;
		break;
	}

	return 0;
}

int rbs_sys_cpm_board_mbox_map(struct rbs_sys *sys,
		struct vm_area_struct *vma)
{
	struct rbs_sys_cpm *cpm = sys->board_data;
	long length = vma->vm_end - vma->vm_start;
	phys_addr_t base;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	switch (vma->vm_pgoff) {
	case 0: /* MBOX */
		base = cpm->mbox_base;
		if (length > PAGE_SIZE)
			return -EINVAL;
		break;
	case 1: /* RAMLOG */
		base = cpm->ramlog_base;
		if (length > cpm->ramlog_size)
			return -EINVAL;
		break;
	default:
		return -ENOENT;
	}

	vma->vm_page_prot = pgprot_noncached(vma->vm_page_prot);

	return remap_pfn_range(vma, vma->vm_start,
			base >> PAGE_SHIFT, length,
			vma->vm_page_prot);
}

int rbs_sys_cpm_board_get_pmem_node_location(struct rbs_sys *sys,
					      const char *node,
					      phys_addr_t *base,
					      phys_addr_t *size)
{
        struct rbs_sys_cpm *cpm = sys->board_data;

        if (!cpm)
                return -ENOENT;

	get_pmem_node_location("persistent", node, base, size, 0, 0);

	if (*size == 0)
		return -ENOENT;

        return 0;
}

int rbs_sys_cpm_board_set_upgrade(struct rbs_sys *sys, int set)
{
	struct rbs_sys_cpm *cpm = sys->board_data;

	if (!cpm || !cpm->mbox)
		return -ENOENT;

	if (set)
		cpm->mbox->upgrade_flag = BOOT_UPGRADE_FLAG;
	else
		cpm->mbox->upgrade_flag = 0;

	return 0;
}

#ifdef __arm__
extern unsigned int ddr_shutdown_state(void);

int rbs_sys_cpm_getwdtrace(struct rbs_sys *sys, unsigned int *trace)
{
	*trace =  ddr_shutdown_state();
	return 0;
}
#endif

/* exported API set */
struct rbs_sys_bt_ops rbs_sys_cpm_bt_ops = {
	.init = rbs_sys_cpm_board_init,
	.set_restart = rbs_sys_cpm_board_set_restart,
	.get_restart = rbs_sys_cpm_board_get_restart,
	.set_bootcount = rbs_sys_cpm_board_set_bootcount,
	.get_bootcount = rbs_sys_cpm_board_get_bootcount,
	.get_bootstarted = rbs_sys_cpm_board_get_bootstarted,
	.set_upgrade = rbs_sys_cpm_board_set_upgrade,
	.mbox_map = rbs_sys_cpm_board_mbox_map,
	.get_pmem_node_location = rbs_sys_cpm_board_get_pmem_node_location,
#ifdef __arm__
	.get_wdtrace = rbs_sys_cpm_getwdtrace,
#endif
	.get_restart_reason = rbs_sys_cpm_board_get_restart_reason,
	.get_fault = rbs_sys_cpm_board_get_fault,
};

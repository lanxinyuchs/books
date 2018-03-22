/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2014 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <net/if.h>
#include <itc.h>
#include <uio_helper.h>
#include <pthread.h>
#include <time.h>
#include <sys/queue.h>
#include "rhd-i2c-if.h"
#include "rhd-common.h"
#include "conn-establish-helper.h"
#include "pinmux.h"
#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_i2c
#include "tpt_create.h"
#include "tpt.h"

#include "ma_mimo_backplaneRegisters.h"
#include "ma_mimo_trxmRegisters.h"
#include <semaphore.h>
#include <fcntl.h>

#define UIO_DEV_I2C    "fpga"
#define DAEMON_NAME    "rhd-i2cd"
#define I2C_SEM_NAME  "/i2c-lock"

#define BOARD_TYPE_BACKPLANE "BP"
#define BOARD_TYPE_TRXM      "TRXM"

#define MAX_MAILBOX_NUM    32
#define I2C_NOF_PORTS      3
/* I2C controller macro used with 7-bit addressing mode gives a maximum
 * of 128 slaves.
 */
#define MAX_I2C_SLAVE_UNITS 128

/* Size or depth, in bytes, of the I2C Master and Slave Data Buffer FIFO. */
#define DATA_BUFFER_FIFO_SIZE 4

#define _STRINGIFY(s)  #s
#define STRINGIFY(s)   _STRINGIFY(s)

#define UIO_DEV_NAME_LEN   sizeof(UIO_DEV_I2C) + \
	sizeof("trxm_")              /*e.g "trxm_fpga" or "bp_fpga"*/
#define MAILBOX_NAME_LEN   sizeof(RHD_I2C_MAILBOX) + \
	sizeof(STRINGIFY(I2C_NOF_PORTS)) + 1          /* e.g "rhd-i2c-0"*/
#define DAEMON_NAME_LEN    sizeof(DAEMON_NAME) + \
	sizeof(STRINGIFY(I2C_NOF_PORTS)) + 1          /* e.g "rhd-i2cd-0" */

#define EXIT_SIGNAL 0xdeadbeef

union itc_msg {
	uint32_t msgno;
	conn_any_msg_t any_msg;
	RHD_I2C_STRUCTS
};

struct conn_establish_msg_numbers  i2c_conn_messages = {
	I2C_CONN_ESTABLISH_REQ,
	I2C_CONN_ESTABLISH_CFM,
	I2C_CONN_ESTABLISH_REJ,
	I2C_CONN_DISCONNECT_REQ,
	I2C_CONN_DISCONNECT_CFM,
	I2C_CONN_DISCONNECT_REJ,
	I2C_CONN_MONITOR_FWD
};

typedef enum {
	IDLE,
	READ,
	READ_SUB,
	READ_LAST,
	WRITE,
	WRITE_SUB,
	WRITE_LAST
} i2c_state;

struct i2c_reg_s {
    volatile uint32_t* irqReg;             /* I2C_MASTER_IRQ  */
    volatile uint32_t* sda2Reg;            /* I2C_MASTER_SDA2REG */
    volatile uint32_t* addrReg;            /* I2C_MASTER_ADDR */
    volatile uint32_t* ctrlReg;            /* I2C_MASTER_CTRL */
    volatile uint32_t* lengthRndAddrReg;   /* I2C_MASTER_LENGTH_RND_ADDR */
    volatile uint32_t* dataReg;            /* I2C_MASTER_DATA */
    volatile uint32_t* wdataReg;           /* I2C_MASTER_WDATA */
};

struct i2c_read_write_s {
	uint32_t addr;                 /* I2C address */
	uint32_t sub_addr;             /* Sub address within I2C device */
	uint8_t sub_addr_size;         /* The sub address size */
	uint32_t rem_len;              /* Remaining length to read/write */
	uint32_t read_len;             /* Current read length. */
	bool sub_write;
	int return_value;
	uint8_t *data_ptr;
	i2c_state state;
	bool is_last_sequence;
#ifdef CHAIN_WRITE
	uint32_t data_len;         /* Total length write */
	bool is_first_sequence;
#endif

};

struct uio_map {
	UIO_HANDLE_ handle;
	void *base;
	uint32_t num_of_users;
};

struct i2c_block {
	uint32_t port_id;
	struct uio_map uio_map;
	itc_mbox_id_t sender_mbox;
        sem_t *lock;
	struct i2c_reg_s reg;
	struct i2c_read_write_s rw_info;
};

struct i2c_master_reg
{
	uint32_t master_irq;
	uint32_t master_sda2reg;
	uint32_t master_addr;
	uint32_t master_ctrl;
	uint32_t master_len_and_addr;
	uint32_t master_data;
	uint32_t master_wdata;
};

struct i2c_master_field
{
	uint32_t irq_i2c_valid;
	uint32_t sda2reg_7_0;
	uint32_t addr_7_0;
	uint32_t ctrl_rwn;
	uint32_t ctrl_mode;
	uint32_t ctrl_dev_17_11;
	uint32_t ctrl_line_7_1;
	uint32_t ctrl_start;
	uint32_t len_and_addr_15_8;
	uint32_t len_and_addr_7_0;
	uint32_t data_read_31_0;
	uint32_t wdata_31_0;
};

struct server_ref {
	uint32_t server_reference;
	LIST_ENTRY(server_ref) server_refs;      /* List. */
};

LIST_HEAD(server_ref_head, server_ref) head =
        LIST_HEAD_INITIALIZER(head);

/*
 * Local Variable Definition
 */
static itc_mbox_id_t i2c_mbox = ITC_NO_ID;
static char *daemon_name = NULL;


static struct i2c_block i2c_;
static struct i2c_master_reg master_reg;
static struct i2c_master_field master_field;


/*
 * Local Function Declaration
 */
static void print_usage(void);
static void extract_readbyte(void);
static bool i2c_register_address_and_field_set(void);
static void i2c_initRegs(void);
static int i2c_init(uint32_t i2c_port);
static void read_byte(void);
static void write_byte(void);
static bool wait_complete(void);
static void handle_i2c_readsub_req(union itc_msg *rec_msg,
                                   uint32_t client_ref);
static void handle_i2c_writesub_req(union itc_msg *rec_msg,
                                    uint32_t client_ref);
static void handle_i2c_read_req(union itc_msg *rec_msg, uint32_t client_ref);
static void handle_i2c_write_req(union itc_msg *rec_msg, uint32_t client_ref);
static bool i2c_status_check(void);
static void read_messages(conn_server_handle_t handle);
static void remove_hw_dependencies_and_server_ref(uint32_t server_reference);

/*
 * MACROS
 */
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/*
 * Function print_usage
 */
static void print_usage(void)
{
	printf("Usage: rhd-i2c [options] <i2c port>\n\n"
	       "Where <i2c port> is less than %d\n\n"
	       "Options:\n"
	       "    -h        Display usage information (this message).\n"
	       "    -d        Daemonize the program.\n\n", I2C_NOF_PORTS);
}


#ifdef DEBUG
/* Below is only for debug
 * It should not be enable in the delivery.
 */
static void debug_trace()
{
	struct i2c_block *i2c = &i2c_;
	TPT_TRACE(4, STR("i2c_block: state 0x%x", i2c->rw_info.state));
	TPT_TRACE(4, STR("i2c_block: uio_handle %p", i2c->uio_map.handle));
	TPT_TRACE(4, STR("i2c_block: mmap_base 0x%x",
	                 (uintptr_t)i2c->uio_map.base));
	TPT_TRACE(4, STR("i2c_block: addr 0x%x", i2c->rw_info.addr));
	TPT_TRACE(4, STR("i2c_block: sub_addr 0x%x", i2c->rw_info.sub_addr));
	TPT_TRACE(4, STR("i2c_block: sub_addr_size 0x%x",
	                 i2c->rw_info.sub_addr_size));
	TPT_TRACE(4, STR("i2c_block: rem_len 0x%x", i2c->rw_info.rem_len));
	TPT_TRACE(4, STR("i2c_block: read_len 0x%x", i2c->rw_info.read_len));
	TPT_TRACE(4, STR("i2c_block: sub_write 0x%x", i2c->rw_info.sub_write));
	TPT_TRACE(4, STR("i2c_block: return_value 0x%x",
	                 i2c->rw_info.return_value));
	TPT_TRACE(4, STR("i2c_block: is_last_sequence 0x%x",
	                 i2c->rw_info.is_last_sequence));
#ifdef CHAIN_WRITE
	TPT_TRACE(4, STR("i2c_block: is_first_sequence 0x%x",
	                 i2c->rw_info.is_first_sequence));
	TPT_TRACE(4, STR("i2c_block: data_len 0x%x", i2c->rw_info.data_len));
#endif
	TPT_TRACE(4, STR("i2c_block: sender mbox 0x%x", i2c->sender_mbox));
	TPT_TRACE(4, STR("i2c_block: register (address:value):"
                     "irqReg  (%p:0x%x) "
	                 "ctrlReg (%p:0x%x) "
	                 "addrReg (%p:0x%x) "
	                 "lengthRndAddrReg (%p:0x%x) "
	                 "dataReg (%p:0x%x) "
	                 "wdataReg (%p:0x%x) ",
	                 (void *)i2c->reg.irqReg, *i2c->reg.irqReg,
	                 (void *)i2c->reg.ctrlReg, *i2c->reg.ctrlReg,
	                 (void *)i2c->reg.addrReg, *i2c->reg.addrReg,
	                 (void *)i2c->reg.lengthRndAddrReg, *i2c->reg.lengthRndAddrReg,
	                 (void *)i2c->reg.dataReg, *i2c->reg.dataReg,
	                 (void *)i2c->reg.wdataReg, *i2c->reg.wdataReg));
}
#endif


/***
 * Function extract_readbyte
 *
 ***/
static void extract_readbyte(void)
{
	struct i2c_block *i2c = &i2c_;
	uint32_t no_of_bytes, data_rec, shift;
	uint32_t i;

	/* Get number of transfered bytes */
	no_of_bytes = i2c->rw_info.read_len;

	if(no_of_bytes > DATA_BUFFER_FIFO_SIZE) {
		/* Fatal error */
		TPT_ERROR(STR("Invalid number of transfered bytes (0x%x)",
		              no_of_bytes));
		return;
	}

	data_rec = *i2c->reg.dataReg;
	TPT_TRACE(3, STR("extract_readbyte data_rec=0x%x", data_rec));

	shift = 24;
	for(i = 0; i < no_of_bytes; i++) {
		/* First byte is received in 8MSBs.
		 * Mask out one byte at the time from the
		 * received buffer and store it in the signal buffer.
		 */
		i2c->rw_info.data_ptr[i2c->rw_info.return_value] =
		        (uint8_t) ((data_rec >> shift) & 0xFF);
		shift -= 8;
		i2c->rw_info.return_value++;
	}
}
/***
 * Function i2c_register_address_and_field_set
 *
 ***/
static bool i2c_register_address_and_field_set(void)
{
        char *boardtype;

        boardtype = getenv("SYS_BOARD_TYPE");
        if(boardtype == NULL)
        {
            TPT_ERROR("Can't get SYS_BOARD_TYPE!");
            return false;
        }
        if(strcmp(boardtype,BOARD_TYPE_BACKPLANE) == 0)
        {
                master_reg.master_addr =         MM_B_I2C_MASTER_ADDR;
		master_reg.master_ctrl =         MM_B_I2C_MASTER_CTRL;
		master_reg.master_data =         MM_B_I2C_MASTER_DATA;
		master_reg.master_irq =          MM_B_I2C_MASTER_IRQ;
		master_reg.master_len_and_addr = MM_B_I2C_MASTER_LENGTH_RND_ADDR;
		master_reg.master_sda2reg =      MM_B_I2C_MASTER_SDA2REG;
		master_reg.master_wdata =        MM_B_I2C_MASTER_WDATA;
		master_field.addr_7_0 =          MM_B_I2C_MASTER_ADDR_I2C_MASTER_ADDR_7_0;
		master_field.ctrl_dev_17_11 =    MM_B_I2C_MASTER_CTRL_I2C_MASTER_DEV_17_11;
		master_field.ctrl_line_7_1 =     MM_B_I2C_MASTER_CTRL_I2C_MASTER_LINE_7_1;
		master_field.ctrl_mode =         MM_B_I2C_MASTER_CTRL_I2C_MASTER_MODE;
		master_field.ctrl_rwn =          MM_B_I2C_MASTER_CTRL_I2C_MASTER_RWN;
		master_field.ctrl_start =        MM_B_I2C_MASTER_CTRL_I2C_MASTER_START;
		master_field.data_read_31_0 =    MM_B_I2C_MASTER_DATA_I2C_DATA_READ_31_0;
		master_field.irq_i2c_valid =     MM_B_I2C_MASTER_IRQ_I2C_MASTER_VALID;
		master_field.len_and_addr_15_8 = MM_B_I2C_MASTER_LENGTH_RND_ADDR_I2C_LENGTH_15_8;
		master_field.len_and_addr_7_0 =  MM_B_I2C_MASTER_LENGTH_RND_ADDR_I2C_RND_ADDR_7_0;
		master_field.sda2reg_7_0 =       MM_B_I2C_MASTER_SDA2REG_I2C_MASTER_SDA2REG_7_0;
		master_field.wdata_31_0 =        MM_B_I2C_MASTER_WDATA_I2C_WDATA_31_0;
                return true;
	}
	else if(strcmp(boardtype,BOARD_TYPE_TRXM) == 0)
        {
	        master_reg.master_addr =         MM_T_I2C_MASTER_ADDR;
		master_reg.master_ctrl =         MM_T_I2C_MASTER_CTRL;
		master_reg.master_data =         MM_T_I2C_MASTER_DATA;
		master_reg.master_irq =          MM_T_I2C_MASTER_IRQ;
		master_reg.master_len_and_addr = MM_T_I2C_MASTER_LENGTH_RND_ADDR;
		master_reg.master_sda2reg =      MM_T_I2C_MASTER_SDA2REG;
		master_reg.master_wdata =        MM_T_I2C_MASTER_WDATA;
		master_field.addr_7_0 =          MM_T_I2C_MASTER_ADDR_I2C_MASTER_ADDR_7_0;
		master_field.ctrl_dev_17_11 =    MM_T_I2C_MASTER_CTRL_I2C_MASTER_DEV_17_11;
		master_field.ctrl_line_7_1 =     MM_T_I2C_MASTER_CTRL_I2C_MASTER_LINE_7_1;
		master_field.ctrl_mode =         MM_T_I2C_MASTER_CTRL_I2C_MASTER_MODE;
		master_field.ctrl_rwn =          MM_T_I2C_MASTER_CTRL_I2C_MASTER_RWN;
		master_field.ctrl_start =        MM_T_I2C_MASTER_CTRL_I2C_MASTER_START;
		master_field.data_read_31_0 =    MM_T_I2C_MASTER_DATA_I2C_DATA_READ_31_0;
		master_field.irq_i2c_valid =     MM_T_I2C_MASTER_IRQ_I2C_MASTER_VALID;
		master_field.len_and_addr_15_8 = MM_T_I2C_MASTER_LENGTH_RND_ADDR_I2C_LENGTH_15_8;
		master_field.len_and_addr_7_0 =  MM_T_I2C_MASTER_LENGTH_RND_ADDR_I2C_RND_ADDR_7_0;
		master_field.sda2reg_7_0 =       MM_T_I2C_MASTER_SDA2REG_I2C_MASTER_SDA2REG_7_0;
		master_field.wdata_31_0 =        MM_T_I2C_MASTER_WDATA_I2C_WDATA_31_0;
                return true;
	}else
        {
           TPT_ERROR("Unknow SYS_BOARD_TYPE!");
           return false;
        }
}

/**
 * Function i2c_initRegs
 * Init i2c registers
 */
static void i2c_initRegs(void)
{
	struct i2c_block *i2c = &i2c_;

	/* Store pointers to I2C registers */
	i2c->reg.irqReg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                             master_reg.master_irq * 4);
	i2c->reg.sda2Reg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              master_reg.master_sda2reg * 4);
	i2c->reg.addrReg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              master_reg.master_addr * 4);
	i2c->reg.ctrlReg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              master_reg.master_ctrl * 4);
	i2c->reg.lengthRndAddrReg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              master_reg.master_len_and_addr * 4);
	i2c->reg.dataReg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                                     master_reg.master_data * 4);
	i2c->reg.wdataReg = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                                     master_reg.master_wdata * 4);
}

/**
 * Function i2c_init
 * Initialize i2c_block
 */
static int i2c_init(uint32_t i2c_port)
{
	struct i2c_block *i2c = &i2c_;
	char mailboxname[MAILBOX_NAME_LEN];

	memset(i2c, 0, sizeof( struct i2c_block));

	i2c->port_id = i2c_port;

    i2c->lock = sem_open(I2C_SEM_NAME, O_CREAT, 0600, 1);
    if( i2c->lock == SEM_FAILED){
        TPT_ERROR(STR("sem_open failed with error %d",
		              errno));
		abort();
    }

    /* Initialize ITC */
	itc_init(MAX_MAILBOX_NUM, ITC_MALLOC, NULL, ITC_NO_NAMESPACE, 0);

	if (snprintf(mailboxname, sizeof(mailboxname), "%s_%d", RHD_I2C_MAILBOX,
	             i2c_port) < 0) {
		TPT_ERROR(STR("snprintf mailbox %s_%d error",
		              RHD_I2C_MAILBOX, i2c_port));
		return -EFAULT;
	}
	/* Create our mailbox. */
	i2c_mbox = itc_create_mailbox(mailboxname, 0);
	if (i2c_mbox == ITC_NO_ID)
		return -EFAULT;

	LIST_INIT(&head);

    if(i2c_register_address_and_field_set())
	return 0;
    else
        return -EFAULT;
}


/**
 * Function read_byte
 *
 */
static void read_byte(void)
{
	struct i2c_block *i2c = &i2c_;

	TPT_TRACE(1, STR("%s called!", __func__));

    if(i2c->rw_info.state == READ_SUB){
        /*write sub-address to lengthRndAddrReg [7:0]*/
        *i2c->reg.lengthRndAddrReg = i2c->rw_info.sub_addr & master_field.len_and_addr_7_0;
    }

    if(i2c->rw_info.rem_len > DATA_BUFFER_FIFO_SIZE)
    {
	i2c->rw_info.read_len = DATA_BUFFER_FIFO_SIZE;
	i2c->rw_info.rem_len -= DATA_BUFFER_FIFO_SIZE;

	i2c->rw_info.is_last_sequence = false;
        if(i2c->rw_info.state == READ_SUB){
            i2c->rw_info.sub_addr += DATA_BUFFER_FIFO_SIZE;
        }
     } else if(i2c->rw_info.rem_len > 0){
	i2c->rw_info.read_len = i2c->rw_info.rem_len;
	i2c->rw_info.rem_len = 0;

        i2c->rw_info.is_last_sequence = true;
        i2c->rw_info.state = READ_LAST;
     }
    /* write read-byte-number to lengthRndAddrReg bit[15:8] */
    *i2c->reg.lengthRndAddrReg &= ~master_field.len_and_addr_15_8;
    *i2c->reg.lengthRndAddrReg |= (i2c->rw_info.read_len<< 8) & master_field.len_and_addr_15_8;
    TPT_TRACE(3, STR("lengthRndAddrReg with value=0x%x", *i2c->reg.lengthRndAddrReg));

    /* write start of ctrlReg bit 0 */
    *i2c->reg.ctrlReg |= master_field.ctrl_start;

#ifdef DEBUG
	debug_trace();
#endif
}

/**
 * Function write_byte
 */
static void write_byte(void)
{
	struct i2c_block *i2c = &i2c_;
	uint32_t write_len;

	TPT_TRACE(1, STR("%s called!", __func__));

    if(i2c->rw_info.state == WRITE_SUB){
        /* write device-internal adress to lengthRndAddrReg bit[7:0] */
        *i2c->reg.lengthRndAddrReg = i2c->rw_info.sub_addr & master_field.len_and_addr_7_0;
    }
	if (i2c->rw_info.rem_len > DATA_BUFFER_FIFO_SIZE) {
		write_len = DATA_BUFFER_FIFO_SIZE;
		i2c->rw_info.rem_len -= DATA_BUFFER_FIFO_SIZE;

		i2c->rw_info.is_last_sequence = false;
        if(i2c->rw_info.state == WRITE_SUB){
            i2c->rw_info.sub_addr += DATA_BUFFER_FIFO_SIZE;
        }
	} else {
		write_len = i2c->rw_info.rem_len;
		i2c->rw_info.rem_len = 0;

		if(write_len > 4 || write_len == 0) {
			TPT_ERROR(STR("Invalid length (0x%x)", write_len));
			return;
		}
		i2c->rw_info.is_last_sequence = true;
        i2c->rw_info.state = WRITE_LAST;
	}

    /*write number of write byte to lengthRndAddrReg bit[15:8]
	number of write byte (less than 3)
	0, 1, 2, 3 means 1 byte, 2 bytes, 3 bytes and 4 bytes*/
	*i2c->reg.lengthRndAddrReg &= ~master_field.len_and_addr_15_8;
	*i2c->reg.lengthRndAddrReg |= ((write_len - 1) << 8) & master_field.len_and_addr_15_8;
	TPT_TRACE(3, STR("lengthRndAddrReg with value=%x", *i2c->reg.lengthRndAddrReg));

    /* Put data to fifo. The first byte transfer to 0:7 */
    uint32_t  shift = 24, i;
    uint32_t  data_tmp = 0, data_to_fifo = 0;
	for(i = 0; i < write_len; i++)
	{
        data_tmp = (uint32_t)
		i2c->rw_info.data_ptr[i2c->rw_info.return_value++];
		data_to_fifo = data_to_fifo | (data_tmp << shift);
		shift -= 8;
	}
	TPT_TRACE(1, STR("Write data:0x%x", data_to_fifo));

	/* write DATA to wdataReg [31:0] */
	*i2c->reg.wdataReg = data_to_fifo;
	TPT_TRACE(3, STR("wdataReg with value =0x%x", *i2c->reg.wdataReg));

	/*write start bit of ctrlReg bit 0 */
	*i2c->reg.ctrlReg |= master_field.ctrl_start;
	TPT_TRACE(3, STR("CtrlReg with value=%x", *i2c->reg.ctrlReg));

#ifdef DEBUG
	debug_trace();
#endif
}
/**
 * Function wait_complete
 */
static bool wait_complete(void)
{
	struct i2c_block *i2c = &i2c_;
	int32_t counter = 0;

    /*keep polling the interrupt valid value */
    while(*i2c->reg.irqReg != master_field.irq_i2c_valid)
    {
	  usleep(10);  //sleep 10 us
      counter++;

      if(counter == 50){  //TODO: timeout after 0.5 msec?
		  i2c->rw_info.return_value = I2C_ERROR_TIMEOUT;
		  return false;
	  }
    }

    /* reset ctrlReg start bit [0]*/
    *i2c->reg.ctrlReg &= ~master_field.ctrl_start;
	return true;
}

/**
 * Function handle_i2c_readsub_req
 */
static void handle_i2c_readsub_req(union itc_msg *rec_msg, uint32_t client_ref)
{
	struct i2c_block *i2c = &i2c_;
	union itc_msg *send_msg = NULL;
	bool complete_ret;
	int sem_ret;

	if (itc_size(rec_msg) < sizeof(struct i2c_read_sub_req)) {
		TPT_ERROR("corrupt size of I2C read sub req message.");
		send_msg = itc_alloc(sizeof(struct i2c_read_sub_rej),
		                     RHD_I2C_READ_SUB_REJ);
		send_msg->read_sub_rej.error_code = I2C_ERROR_OTHER;
		send_msg->any_msg.connection_ref = client_ref;
		itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);
		return;
	}

	sem_ret = sem_wait(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_wait failed with error %d",
		              sem_ret));
		abort();
	}

	memset(&i2c->rw_info, 0, sizeof(struct i2c_read_write_s));

	send_msg = itc_alloc(MAX(sizeof(struct i2c_read_sub_rej),
	                         offsetof(struct i2c_read_sub_cfm, data) +
	                         rec_msg->read_sub_req.length), RHD_I2C_READ_SUB_REJ);

	if(rec_msg->read_sub_req.address >=  MAX_I2C_SLAVE_UNITS) {
		TPT_ERROR(STR("I2C slave address 0x%x is out of range",
		              rec_msg->read_sub_req.address));
		i2c->rw_info.return_value = I2C_ERROR_ILLEGAL_ADDRESS;
		goto handle_i2c_readsub_req_end;
	}

	if(rec_msg->read_sub_req.sub_address_size >
	    sizeof(uint32_t) / sizeof(uint8_t)) {
		TPT_ERROR(STR("sub address size 0x%x is out of range",
		              rec_msg->read_sub_req.sub_address_size));
		i2c->rw_info.return_value = I2C_ERROR_OTHER;
		goto handle_i2c_readsub_req_end;
	}

    i2c->rw_info.addr = rec_msg->read_sub_req.address;
	i2c->rw_info.rem_len  = rec_msg->read_sub_req.length;
	i2c->rw_info.sub_addr = rec_msg->read_sub_req.sub_address;
	i2c->rw_info.sub_addr_size = rec_msg->read_sub_req.sub_address_size;
	i2c->rw_info.data_ptr = send_msg->read_sub_cfm.data;
	i2c->rw_info.is_last_sequence = false;
    i2c->rw_info.state = READ_SUB;

    /* Write 0 to I2C buff read address (not used) */
    *i2c->reg.addrReg = 0;
	/* Load the portId into ctrlReg bits 7:1 */
    *i2c->reg.ctrlReg = ( i2c->port_id << 1) & master_field.ctrl_line_7_1;
    /* ... and the address into the ctrlReg bits 17:11. */
    *i2c->reg.ctrlReg |= (i2c->rw_info.addr << 11) & master_field.ctrl_dev_17_11;
    /*... and the RWN mode into the ctrlReg bit 22. '1': read mode en*/
    *i2c->reg.ctrlReg |= master_field.ctrl_rwn;
    /*... and the Mode into ctrlReg bit  21. '0': random mode*/
    *i2c->reg.ctrlReg &= ~master_field.ctrl_mode;

#ifdef DEBUG
	debug_trace();
#endif

     while(i2c->rw_info.is_last_sequence != true)
     {
         read_byte();
         complete_ret = wait_complete();

         if(complete_ret != true)
            break;
         /* transfer completed */
         usleep(5);/* tBUF is min 4.7 us */
         extract_readbyte();
     }

	if((complete_ret == true) &&
	    (i2c->rw_info.state == READ_LAST)) {
		send_msg->read_sub_cfm.msgno = RHD_I2C_READ_SUB_CFM;
		send_msg->read_sub_cfm.length = i2c->rw_info.return_value;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_READ_SUB_CFM");
	}

handle_i2c_readsub_req_end:
	if(send_msg->read_sub_rej.msgno == RHD_I2C_READ_SUB_REJ) {
		if(i2c->rw_info.return_value == 0) {
			i2c->rw_info.return_value = I2C_ERROR_OTHER;
		}
		send_msg->read_sub_rej.error_code = i2c->rw_info.return_value;
		i2c_initRegs();
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_READ_SUB_REJ");
	}

	i2c->rw_info.state = IDLE;

	send_msg->any_msg.connection_ref = client_ref;
	send_msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);

        sem_ret = sem_post(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_post failed with error %d",
		              sem_ret));
		abort();
	}
	return;
}

/**
 * Function handle_i2c_writesub_req
 */
static void handle_i2c_writesub_req(union itc_msg *rec_msg,
                                    uint32_t client_ref)
{
	struct i2c_block *i2c = &i2c_;
	union itc_msg *send_msg = NULL;
	bool complete_ret;
	int sem_ret;

	if (itc_size(rec_msg) <
	    (offsetof(struct i2c_write_sub_req, data) + 1)) {
		TPT_ERROR("corrupt size of I2C write sub req message.");
		send_msg = itc_alloc(sizeof(struct i2c_write_sub_rej),
		                     RHD_I2C_WRITE_SUB_REJ);
		send_msg->write_sub_rej.error_code = I2C_ERROR_OTHER;
		send_msg->any_msg.connection_ref = client_ref;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_WRITE_SUB_REJ");
		itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);
		return;
	}

   	sem_ret = sem_wait(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_wait failed with error %d",
		              sem_ret));
		abort();
	}

	memset(&i2c->rw_info, 0, sizeof(struct i2c_read_write_s));

	send_msg = itc_alloc(MAX(sizeof(struct i2c_write_sub_rej),
	                         sizeof(struct i2c_write_sub_cfm)),
	                     RHD_I2C_WRITE_SUB_REJ);

	if(rec_msg->write_sub_req.address >=  MAX_I2C_SLAVE_UNITS) {
		TPT_ERROR(STR("I2C slave address 0x%x is out of range",
		              rec_msg->write_sub_req.address));
		i2c->rw_info.return_value = I2C_ERROR_ILLEGAL_ADDRESS;
		goto handle_i2c_writesub_req_end;
	}
	if(rec_msg->write_sub_req.sub_address_size >
	    sizeof(uint32_t) / sizeof(uint8_t)) {
		TPT_ERROR(STR("sub address size 0x%x is out of range",
		              rec_msg->write_sub_req.sub_address_size));
		i2c->rw_info.return_value = I2C_ERROR_OTHER;
		goto handle_i2c_writesub_req_end;
	}

	i2c->rw_info.addr = rec_msg->write_sub_req.address;
	i2c->rw_info.rem_len  = rec_msg->write_sub_req.length;
	i2c->rw_info.sub_addr = rec_msg->write_sub_req.sub_address;
	i2c->rw_info.sub_addr_size = rec_msg->write_sub_req.sub_address_size;
	i2c->rw_info.data_ptr = rec_msg->write_sub_req.data;
	i2c->rw_info.is_last_sequence = false;
	if(i2c->rw_info.sub_addr_size != 0) {
		i2c->rw_info.sub_write = true;
	}
#ifdef CHAIN_WRITE
	i2c->rw_info.data_len = i2c->rw_info.rem_len;
	i2c->rw_info.is_first_sequence = true;
#endif
	i2c->rw_info.state = WRITE_SUB;

    /* Load the portId into ctrlReg bits 7:1 */
    *i2c->reg.ctrlReg = (i2c->port_id << 1) & master_field.ctrl_line_7_1;
    /* ... and the address into the ctrlReg bits 17:11. */
    *i2c->reg.ctrlReg |= (i2c->rw_info.addr << 11) & master_field.ctrl_dev_17_11;
    /*... and the RWN mode into the ctrlReg bit 22 with '0': write mode en*/
    *i2c->reg.ctrlReg &= ~master_field.ctrl_rwn;
    /*... and the Mode into ctrlReg bit  21 with '0': random mode*/
    *i2c->reg.ctrlReg &= ~master_field.ctrl_mode;

#ifdef DEBUG
	debug_trace();
#endif
    while(i2c->rw_info.is_last_sequence != true)
    {
        write_byte();
        complete_ret = wait_complete();

        if(complete_ret != true)
           break;
    }


	if(complete_ret == true &&
	    i2c->rw_info.state == WRITE_LAST) {
		send_msg->write_sub_cfm.msgno = RHD_I2C_WRITE_SUB_CFM;
		send_msg->write_sub_cfm.length = i2c->rw_info.return_value;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_WRITE_SUB_CFM");
	}

handle_i2c_writesub_req_end:
	if(send_msg->write_sub_rej.msgno == RHD_I2C_WRITE_SUB_REJ) {
		if(i2c->rw_info.return_value == 0) {
			i2c->rw_info.return_value = I2C_ERROR_OTHER;
		}
		send_msg->write_sub_rej.error_code = i2c->rw_info.return_value;
		i2c_initRegs();
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_WRITE_SUB_REJ");
	}

	i2c->rw_info.state = IDLE;

	send_msg->any_msg.connection_ref = client_ref;
	send_msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);

    sem_ret = sem_post(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_post failed with error %d",
		              sem_ret));
		abort();
	}

}

/**
 * Function handle_i2c_read_req
 */
static void handle_i2c_read_req(union itc_msg *rec_msg, uint32_t client_ref)
{
	struct i2c_block *i2c = &i2c_;
	union itc_msg *send_msg = NULL;
	bool complete_ret = false;
	int sem_ret;

	if (itc_size(rec_msg) < sizeof(struct i2c_read_req)) {
		TPT_ERROR("corrupt size of I2C read req message.");
		send_msg = itc_alloc(sizeof(struct i2c_read_rej),
		                     RHD_I2C_READ_REJ);
		send_msg->read_rej.error_code = I2C_ERROR_OTHER;
		send_msg->any_msg.connection_ref = client_ref;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_READ_REJ");
		itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);
		return;
	}

    sem_ret = sem_wait(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_wait failed with error %d",
		              sem_ret));
		abort();
	}


	memset(&i2c->rw_info, 0, sizeof(struct i2c_read_write_s));

	send_msg = itc_alloc(MAX(sizeof(struct i2c_read_rej),
	                         offsetof(struct i2c_read_cfm, data) +
	                         rec_msg->read_req.length), RHD_I2C_READ_REJ);

	if(rec_msg->read_req.address >=  MAX_I2C_SLAVE_UNITS) {
		TPT_ERROR(STR("I2C slave address 0x%x is out of range",
		              rec_msg->read_req.address));
		i2c->rw_info.return_value = I2C_ERROR_ILLEGAL_ADDRESS;
		goto handle_i2c_read_req_end;
	}

	i2c->rw_info.addr = rec_msg->read_req.address;
	i2c->rw_info.rem_len  = rec_msg->read_req.length;
	i2c->rw_info.is_last_sequence = false;
	i2c->rw_info.data_ptr = send_msg->read_cfm.data;
	i2c->rw_info.state = READ;

    /* Load the portId into ctrlReg bits 7:1 */
	*i2c->reg.ctrlReg = ( i2c->port_id << 1) & master_field.ctrl_line_7_1;
    /* ... and the address into the ctrlReg bits 17:11. */
    *i2c->reg.ctrlReg |= (i2c->rw_info.addr << 11) & master_field.ctrl_dev_17_11;
    /*... and the RWN mode into the ctrlReg bit 22. '1': read mode en*/
    *i2c->reg.ctrlReg |= master_field.ctrl_rwn;
    /*... and the Mode into ctrlReg bit  21. '1': current mode*/
    *i2c->reg.ctrlReg |= master_field.ctrl_mode;

#ifdef DEBUG
	debug_trace();
#endif

    while(i2c->rw_info.is_last_sequence != true)
    {
        read_byte();
        complete_ret = wait_complete();

        if(complete_ret != true)
           break;
        /* transfer completed */
        usleep(5);/* tBUF is min 4.7 us */
        extract_readbyte();
    }

	if((complete_ret == true) &&
	    (i2c->rw_info.state == READ_LAST)) {
		send_msg->read_cfm.msgno = RHD_I2C_READ_CFM;
		send_msg->read_cfm.length = i2c->rw_info.return_value;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_READ_CFM");

	}

handle_i2c_read_req_end:
	if(send_msg->read_rej.msgno == RHD_I2C_READ_REJ) {
		if(i2c->rw_info.return_value == 0) {
			i2c->rw_info.return_value = I2C_ERROR_OTHER;
		}
		send_msg->read_rej.error_code = i2c->rw_info.return_value;
		i2c_initRegs();
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_READ_REJ");
	}

	i2c->rw_info.state = IDLE;

	send_msg->any_msg.connection_ref = client_ref;
	send_msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);

    sem_ret = sem_post(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_post failed with error %d",
		              sem_ret));
		abort();
	}

}

/**
 * Function handle_i2c_write_req
 */
static void handle_i2c_write_req(union itc_msg *rec_msg, uint32_t client_ref)
{
	struct i2c_block *i2c = &i2c_;
	union itc_msg *send_msg = NULL;
	bool complete_ret = false;
	int sem_ret;

	if (itc_size(rec_msg) < (offsetof(struct i2c_write_req, data) + 1)) {
		TPT_ERROR("corrupt size of I2C write req message.");
		send_msg = itc_alloc(sizeof(struct i2c_write_rej),
		                     RHD_I2C_WRITE_REJ);
		send_msg->write_rej.error_code = I2C_ERROR_OTHER;
		send_msg->any_msg.connection_ref = client_ref;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_WRITE_REJ");
		itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);
		return;
	}

	sem_ret = sem_wait(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_wait failed with error %d",
		              sem_ret));
		abort();
	}
	memset(&i2c->rw_info, 0, sizeof(struct i2c_read_write_s));

	send_msg = itc_alloc(MAX(sizeof(struct i2c_write_rej),
	                         sizeof(struct i2c_write_cfm)),
	                     RHD_I2C_WRITE_REJ);

	if(rec_msg->write_req.address >=  MAX_I2C_SLAVE_UNITS) {
		TPT_ERROR(STR("I2C slave address 0x%x is out of range",
		              rec_msg->write_req.address));
		i2c->rw_info.return_value = I2C_ERROR_ILLEGAL_ADDRESS;
		goto handle_i2c_write_req_end;
	}

	i2c->rw_info.addr = rec_msg->write_req.address;
	i2c->rw_info.rem_len  = rec_msg->write_req.length;
	i2c->rw_info.data_ptr = rec_msg->write_req.data;
	i2c->rw_info.is_last_sequence = false;
#ifdef CHAIN_WRITE
	i2c->rw_info.data_len = i2c->rw_info.rem_len;
	i2c->rw_info.is_first_sequence = true;
#endif
	i2c->rw_info.state = WRITE;

    /* Load the portId into ctrlReg bits 7:1 */
    *i2c->reg.ctrlReg = (i2c->port_id << 1) & master_field.ctrl_line_7_1;
    /* ... and the address into the ctrlReg bits 17:11. */
    *i2c->reg.ctrlReg |= (i2c->rw_info.addr << 11) & master_field.ctrl_dev_17_11;
    /*... and the RWN mode into the ctrlReg bit 22 with '0': write mode en*/
    *i2c->reg.ctrlReg &= ~master_field.ctrl_rwn;
    /*... and the Mode into ctrlReg bit 21 with '1': current mode*/
    *i2c->reg.ctrlReg |= master_field.ctrl_mode;

#ifdef DEBUG
	debug_trace();
#endif
    while(i2c->rw_info.is_last_sequence != true)
    {
        write_byte();
        complete_ret = wait_complete();

        if(complete_ret != true)
           break;
    }

	if(complete_ret == true &&
	    i2c->rw_info.state == WRITE_LAST) {
		send_msg->write_cfm.msgno = RHD_I2C_WRITE_CFM;
		send_msg->write_cfm.length = i2c->rw_info.return_value;
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_WRITE_CFM");
	}

handle_i2c_write_req_end:
	if(send_msg->write_rej.msgno == RHD_I2C_WRITE_REJ) {
		if(i2c->rw_info.return_value == 0) {
			i2c->rw_info.return_value = I2C_ERROR_OTHER;
		}
		send_msg->write_rej.error_code = i2c->rw_info.return_value;
		i2c_initRegs();
		TPT_SEND_SIG(send_msg->msgno,
		             i2c->sender_mbox,
		             "Send RHD_I2C_WRITE_REJ");

	}

	i2c->rw_info.state = IDLE;

	send_msg->any_msg.connection_ref = client_ref;
	send_msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);

    sem_ret = sem_post(i2c->lock);
	if(sem_ret != 0) {
		TPT_ERROR(STR("sem_post failed with error %d",
		              sem_ret));
		abort();
	}

}

static bool is_server_ref_exist_in_list(uint32_t server_reference)
{
	struct server_ref *ref;

	LIST_FOREACH(ref, &head, server_refs) {
		if(server_reference == ref->server_reference) {
			return true;
		}
	}
	return false;
}

static bool add_server_ref_into_list(uint32_t server_reference,
                                     uint32_t *num_of_users)
{
	struct server_ref *ref;
	if(is_server_ref_exist_in_list(server_reference) == false) {
		ref = calloc(1, sizeof(struct server_ref));
		if(ref == NULL) {
			TPT_ERROR(STR("calloc ref failed %d",
			              sizeof(struct server_ref)));
			return false;
		}
		ref->server_reference = server_reference;
		LIST_INSERT_HEAD(&head, ref, server_refs);
		(*num_of_users)++;
	}
	return true;
}

static bool set_uio_map_and_irq(void)
{
	char uiodevname[UIO_DEV_NAME_LEN];
	struct i2c_block *i2c = &i2c_;
        char *boardtype;

        boardtype = getenv("SYS_BOARD_TYPE");
        if(boardtype == NULL)
        {
            TPT_ERROR("Can't get SYS_BOARD_TYPE!");
            return false;
        }
        if(strcmp(boardtype,BOARD_TYPE_BACKPLANE) == 0)
        {
	    snprintf(uiodevname, sizeof(uiodevname),
	             "bp_%s", UIO_DEV_I2C);
        }else if(strcmp(boardtype,BOARD_TYPE_TRXM) == 0)
        {
           snprintf(uiodevname, sizeof(uiodevname),
                     "trxm_%s", UIO_DEV_I2C);
        }else
        {
           TPT_ERROR("Unknow SYS_BOARD_TYPE!");
           return false;
        }

	i2c->uio_map.handle = uio_open(uiodevname);
	if (i2c->uio_map.handle == UIO_OPEN_FAILED) {
		TPT_ERROR("Failed to open uio");
		return false;
	}

	i2c->uio_map.base = uio_mmap(i2c->uio_map.handle);

	if (i2c->uio_map.base == MAP_FAILED) {
		i2c->uio_map.base = NULL;
		TPT_ERROR("Failed to peform UIO memory mapping");
		goto set_uio_map_and_irq_err;
	}

	return true;

set_uio_map_and_irq_err:

	uio_close(i2c->uio_map.handle);

	return false;
}

static void handle_i2c_open_req(union itc_msg *rec_msg,
                                struct conn_client_info client_info)
{
	struct i2c_block *i2c = &i2c_;
	union itc_msg *send_msg = NULL;


	send_msg = itc_alloc(MAX(sizeof(struct i2c_open_rej),
	                         sizeof(struct i2c_open_cfm)),
	                     RHD_I2C_OPEN_REJ);

	if (itc_size(rec_msg) < sizeof(struct i2c_open_req)) {
		TPT_ERROR("corrupt size of I2C open req message.");
		goto handle_i2c_open_req_end;
	}

	if(i2c->uio_map.num_of_users == 0) {

		if(set_uio_map_and_irq() == false) {
			goto handle_i2c_open_req_end;
		}
		i2c_initRegs(); /* enable i2c interrupt */
	}

#ifdef DEBUG
	debug_trace();
#endif

	if(add_server_ref_into_list(client_info.server_ref,
	                            &i2c->uio_map.num_of_users) == true) {
		send_msg->open_cfm.msgno = RHD_I2C_OPEN_CFM;
	}

handle_i2c_open_req_end:
	send_msg->any_msg.connection_ref = client_info.client_ref;
	send_msg->any_msg.procedure_ref = rec_msg->any_msg.procedure_ref;
	itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);
}

/**
 * Function i2c_status_check
 */
static bool i2c_status_check(void)
{
	struct i2c_block *i2c = &i2c_;
	if(i2c->rw_info.state != IDLE) {
		TPT_ERROR(STR("i2c status is not at the IDLE status."));
		i2c->rw_info.state = IDLE;
		return false;
	}

	return true;
}

/**
 * Function read_messages
 * Start reading received messages through ITC and handle them.
 */
static void read_messages(conn_server_handle_t handle)
{
	union itc_msg *msg;
	struct i2c_block *i2c = &i2c_;
	struct conn_client_info client_info;

	for (;;) {
		msg = itc_receive(ITC_NOFILTER, ITC_NO_TMO, ITC_FROM_ALL);

		if (msg->msgno == EXIT_SIGNAL) {
			TPT_INFO(STR("%s exiting as ordered", daemon_name));
			itc_free(&msg);
			return;
		}
		if(i2c_status_check() == false) {
			/* FIX ME, it should be continue or skip */
			itc_free(&msg);
			continue;
		}

		/*Handle CONN_ESTABLISH... messages (and messages from
		 unknown clients.*/
		if(!conn_check_client(handle, &msg, &client_info))
			continue;

		i2c->sender_mbox = client_info.sender;
		switch (msg->msgno) {
		case RHD_I2C_OPEN_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_I2C_OPEN_REQ from 0x%x.",
			                client_info.sender));
			handle_i2c_open_req(msg, client_info);
			break;
		}
		case RHD_I2C_WRITE_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_I2C_WRITE_REQ from 0x%x.",
			                client_info.sender));
			handle_i2c_write_req(msg, client_info.client_ref);
			break;
		}
		case RHD_I2C_READ_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_I2C_READ_REQ from 0x%x.",
			                client_info.sender));
			handle_i2c_read_req(msg, client_info.client_ref);
			break;
		}
		case RHD_I2C_WRITE_SUB_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_I2C_WRITE_SUB_REQ "
			                "from 0x%x.",
			                client_info.sender));
			handle_i2c_writesub_req(msg, client_info.client_ref);
			break;
		}
		case RHD_I2C_READ_SUB_REQ: {
			TPT_REC_SIG(msg->msgno,
			            STR("RHD_I2C_READ_SUB_REQ"
			                " from sender: 0x%x.",
			                client_info.sender));
			handle_i2c_readsub_req(msg, client_info.client_ref);
			break;
		}
		default:
			TPT_ERROR(STR("Receive Unexpected message: 0x%x.",
				      msg->msgno));
			break;
		}
		itc_free(&msg);
	}
}



static void remove_uio_map_and_unreserve_pinmux(void)
{
	struct i2c_block *i2c = &i2c_;

	i2c->uio_map.num_of_users--;
	if(i2c->uio_map.num_of_users == 0) {
		uio_munmap (i2c->uio_map.handle);
		uio_close(i2c->uio_map.handle);
	}
}

static void remove_hw_dependencies_and_server_ref(uint32_t server_reference)
{
	struct server_ref *ref, *next_ref;

	for(ref = LIST_FIRST(&head); ref != NULL; ref = next_ref) {
		next_ref = LIST_NEXT(ref, server_refs);
		if(server_reference == ref->server_reference) {
			remove_uio_map_and_unreserve_pinmux();
			LIST_REMOVE(ref, server_refs);
			free(ref);
		}
	}
	return;
}


/**
 * Function client_disconnect
 */
static void client_disconnect(struct conn_client_info *client_info)
{

	TPT_TRACE(1, STR("Client with mailbox 0x%08x, server_ref 0x%08x and "
	                 "client_ref 0x%08x has %s.",
	                 client_info->connected_mailbox,
	                 client_info->server_ref,
	                 client_info->client_ref,
	                 client_info->state ==
	                 CONN_ESTABLISH_STATUS_DISCONNECTING ?
	                 "disconnected." : "died (or forgot to disconnect.)"));

	remove_hw_dependencies_and_server_ref(client_info->server_ref);

}

/**
 * Function conn_server_init
 */
static conn_server_handle_t conn_server_init(void)
{
	conn_server_handle_t handle;
	struct conn_event_callbacks cb = { NULL, client_disconnect,
		       client_disconnect, NULL
	};
	uint32_t supported_versions[] = {I2C_SERVER_VERSIONS};
	int conn_result = conn_establish_server_init( &handle,
	                  sizeof(supported_versions) /
	                  sizeof(supported_versions[0]),
	                  supported_versions,
	                  &i2c_conn_messages, 0, &cb);
	if( conn_result != CONN_INIT_OK) {
		TPT_ERROR("Initalization of "
		          "conn_establish mechanism failed.");
		return NULL;
	}

	return handle;
}

/**
 * Function exit_handler
 */
static void exit_handler(int sig)
{
	union itc_msg *msg;

	TPT_INFO(STR("Receive signal %d, terminating", sig));
	msg = itc_alloc(sizeof(uint32_t), EXIT_SIGNAL);
	itc_send(&msg, i2c_mbox, ITC_MY_MBOX);
}

/**
 * Main function
 * start the rhd_i2c daemon
 */
int main(int argc, char **argv)
{
	static char short_options[] = "hd";
	int daemonize = 0;
	uint32_t i2c_port;
	int32_t ret = 0;
	char *endptr;
	void *handle;
	int c;

	while ((c = getopt(argc, argv, short_options)) != -1) {
		switch (c) {
		case 'h':
			print_usage();
			goto main_end;
		case 'd':
			daemonize = 1;
			break;
		default:
			print_usage();
			ret = -EINVAL;
			goto main_end;
		}
	}

	if (argc - optind != 1) {
		printf("No port supplied to rhd-i2c\n");
		print_usage();
		ret = -EINVAL;
		goto main_end;
	}

	i2c_port = strtol(argv[argc - 1], &endptr, 10);
	if (*endptr != '\0' || i2c_port >= I2C_NOF_PORTS) {
		printf("Invalid i2c port: %s\n", argv[argc - 1]);
		print_usage();
		ret = -EINVAL;
		goto main_end;
	}

	daemon_name = malloc(DAEMON_NAME_LEN);
	if (!daemon_name) {
		printf("malloc: failed\n");
		ret = -ENOMEM;
		goto main_end;
	}

	snprintf(daemon_name, DAEMON_NAME_LEN, "%s-%d",
	         DAEMON_NAME, i2c_port);

	if (rhd_try_lock(daemon_name)) {
		printf("failed to obtain lock: %s\n", daemon_name);
		ret = -EFAULT;
		goto main_end;
	}

	if (!daemonize || !daemon(0, 0)) {

		TPT_INFO(STR("Starting %s %s",
		             daemonize ? "daemon" : "foreground process",
		             daemon_name));

		handle = conn_server_init();

		if (!i2c_init(i2c_port) && (handle != NULL)) {
			/* Start processing ITC messages.
			 * No return.
			 */
			if (signal(SIGTERM, exit_handler) == SIG_ERR) {
				TPT_ERROR(STR("Failed to install signal"
				              " exit handler"));
				exit(1);
			}
			read_messages(handle);

		} else {
			TPT_ERROR(STR("Failed to intialize i2c%d", i2c_port));
			ret = -EFAULT;
		}
	} else {
		TPT_ERROR(STR("Failed to start daemon %s", daemon_name));
		ret = -EFAULT;
	}

main_end:
	if(daemon_name != NULL) {
		free(daemon_name);
	}

	return ret;

}

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

#if 0
#define CHAIN_WRITE
#endif


#define TRACEPOINT_PROVIDER     com_ericsson_xcs_rhd_i2c
#include "tpt_create.h"
#include "tpt.h"

#define XENON_I2C_CTRL (0x0)
#define XENON_I2C_DIV  (0x4)
#define XENON_I2C_ADDR (0x8)
#define XENON_I2C_DATA (0xC)
#define XENON_I2C_CMD  (0x10)

#define XENON_I2C_STATUS       (0x14)
#define XENON_I2C_STATUS_TRAP  (0x18)
#define XENON_I2C_STATUS_MASK  (0x1C)
#define XENON_I2C_STATUS_FORCE (0x20)
#define XENON_I2C_STATUS_DB    (0x24)
#define XENON_I2C_STATUS_TRIG  (0x28)

#define XENON_I2C_COUNT  (0x3C)

/* Register values */
#define CTRL_CHAIN (0x200)
#define CTRL_HB    (0x100) /* Hold Bus */

#define CTRL_1BYTE (0x00)
#define CTRL_2BYTE (0x10)
#define CTRL_3BYTE (0x20)
#define CTRL_4BYTE (0x30)

#define CTRL_WRITE (0x0)
#define CTRL_READ  (0x2)

#define CMD_START  (0x2)
#define CMD_RESET  (0x1)

#define STATUS_HLT  (0x40)
#define STATUS_IXF  (0x20)
#define STATUS_ANAK (0x08)
#define STATUS_MDF  (0x04)
#define STATUS_MDH  (0x02)
#define STATUS_MTC  (0x01)

/* AXI bus frequence is 245MHz,
 * e.g. DIV_100_KHZ value is round_up(245MHZ/100KHZ)
 */
#define DIV_100_KHZ (0x99A)
#define DIV_400_KHZ (0x267)

#define UIO_DEV_I2C    "i2c"
#define DAEMON_NAME    "rhd-i2cd"

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
	sizeof(STRINGIFY(I2C_NOF_PORTS))              /*e.g "i2c0"*/
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

typedef enum {
	I2C_EVENT_ILLEGAL_HW_INTR,             /* I2C HW related event. */
	I2C_EVENT_TRANSFER_ABORTED,            /* I2C HW related event. */
	I2C_EVENT_MASTER_TRANSFER_COMPLETE     /* I2C HW related event. */
} i2c_event;

struct i2c_reg_s {
	volatile uint32_t *ctrl;         /* I2C_X_CTRL */
	volatile uint32_t *div;          /* I2C_X_DIV */
	volatile uint32_t *addr;         /* I2C_X_ADDR */
	volatile uint32_t *data;         /* I2C_X_DATA */
	volatile uint32_t *cmd;          /* I2C_X_CMD */
	volatile uint32_t *status_trap;  /* I2C_X_STATUS_TRAP */
	volatile uint32_t *status_mask;  /* I2C_X_STATUS_MASK */
	volatile uint32_t *count;        /* I2C_X_TRANS_COUNT */
};

struct i2c_read_write_s {
	uint32_t addr;                 /* I2C address */
	uint32_t sub_addr;             /* Sub address within I2C device */
	uint8_t sub_addr_size;         /* The sub address size */
	uint32_t rem_len;              /* Remaining length to write */
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
	pinmux_handle_t pinmux_handle;
	itc_mbox_id_t sender_mbox;
	pthread_mutex_t lock;
	pthread_cond_t complete;
	struct i2c_reg_s reg;
	struct i2c_read_write_s rw_info;
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
static const uint32_t pin_number[3][2] = {{331, 332},  /* port0: GPIO_3_52, GPIO_3_53 */
                                          {333, 334},  /* port1: GPIO_3_54, GPIO_3_55 */
                                          {335, 336}   /* port3: GPIO_3_56, GPIO_3_57 */
};
static struct i2c_block i2c_;

/*
 * Local Function Declaration
 */
static void print_usage(void);
static void extract_readbyte(void);
static void handle_interrupt_event(i2c_event event,
                                   struct i2c_block *i2c);
static void i2c_int_notifier(__attribute__((unused)) void *data);
static void i2c_reset(void);
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
#define CTRL_BYTE(n) \
	((n) == 1 ? CTRL_1BYTE : \
	 ((n) == 2 ? CTRL_2BYTE : \
	  (n) == 3 ? CTRL_3BYTE : CTRL_4BYTE))


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
	TPT_TRACE(4, STR("i2c_block: uio_handle 0x%x", i2c->uio_map.handle));
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
	                 "ctrl (0x%x:0x%x) "
	                 "div (0x%x:0x%x), addr (0x%x:0x%x), data (0x%x:0x%x), "
	                 "cmd (0x%x:0x%x), status_trap (0x%x:0x%x), "
	                 "status_mask (0x%x:0x%x), count (0x%x:0x%x)",
	                 i2c->reg.ctrl, *i2c->reg.ctrl,
	                 i2c->reg.div, *i2c->reg.div,
	                 i2c->reg.addr, *i2c->reg.addr,
	                 i2c->reg.data, *i2c->reg.data,
	                 i2c->reg.cmd, *i2c->reg.cmd,
	                 i2c->reg.status_trap, *i2c->reg.status_trap,
	                 i2c->reg.status_mask, *i2c->reg.status_mask,
	                 i2c->reg.count, *i2c->reg.count));


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

	data_rec = *i2c->reg.data;

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
 * Function handle_interrupt_event
 *
 ***/
static void handle_interrupt_event(i2c_event event, struct i2c_block *i2c)
{
	int pthread_ret;
	switch(event) {
	case I2C_EVENT_TRANSFER_ABORTED:
		pthread_ret = pthread_cond_signal(&i2c->complete);
		if(pthread_ret != 0) {
			TPT_ERROR(STR("pthread_cond_signal return error %d",
			              pthread_ret));
			abort();
		}
		break;

	case I2C_EVENT_MASTER_TRANSFER_COMPLETE: {
		usleep(5);/* tBUF is min 4.7 us */
		if (i2c->rw_info.is_last_sequence == true) {
			if (i2c->rw_info.state == READ) {
				/* MASTER_READ_TRANSFER_COMPLETE */
				i2c->rw_info.state = READ_LAST;
				extract_readbyte();
				TPT_TRACE(3, "last read transfer done");
				pthread_ret =
				        pthread_cond_signal(&i2c->complete);
				if(pthread_ret != 0) {
					TPT_ERROR(STR("pthread_cond_signal "
					              "return error %d",
					              pthread_ret));
					abort();
				}

			} else if (i2c->rw_info.state == WRITE_SUB ||
			           i2c->rw_info.state == WRITE) {
				/* MASTER_WRITE_TRANSFER_COMPLETE */
				i2c->rw_info.state = WRITE_LAST;
				TPT_TRACE(3, "last write transfer done");
				pthread_ret =
				        pthread_cond_signal(&i2c->complete);
				if(pthread_ret != 0) {
					TPT_ERROR(STR("pthread_cond_signal "
					              "return error %d",
					              pthread_ret));
					abort();
				}
			}
		} else {
			if (i2c->rw_info.state == READ_SUB) {
				/* Subaddress is written, start reading. */
				i2c->rw_info.state = READ;
				read_byte();
			} else if (i2c->rw_info.state == READ) {
				/* Continue read */
				extract_readbyte();
				read_byte();
			} else if (i2c->rw_info.state == WRITE_SUB ||
			           i2c->rw_info.state == WRITE) {
				/* Continue write */
				i2c->rw_info.state = WRITE;
				write_byte();
			}
		}
	}
	break;

	case I2C_EVENT_ILLEGAL_HW_INTR:
		/* receive spurious interrupt
		 * disable reset since we assume hw should not have this issue,
		 * but if the hw is crappy, then it needs to be enabled. */
		/*i2c_reset();*/
		break;

	default: /* This should not happen. */
		TPT_ERROR("Illegal event");
		break;
	}
}


/**
 * Function i2c_int_notifier
 * The interrupt handler for I2C.
 */
static void i2c_int_notifier(__attribute__((unused)) void *data)
{
	struct i2c_block *i2c = &i2c_;
	uint32_t status_trap;
	int pthread_ret;

	pthread_ret = pthread_mutex_lock(&i2c->lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
		abort();
	}

	status_trap = *(i2c->reg.status_trap);

	/* Clear status trap */
	*(i2c->reg.status_trap) = status_trap;

	TPT_TRACE(3, STR("---> hwInterrupt status_trap=0x%x", status_trap));

	if (i2c->rw_info.state == IDLE) {
		/* The I2C macro should only cause an interrupt
		 * during a master transfer,
		 * i.e. when it is busy.
		 * receive spurious interrupt
		 */
		TPT_ERROR("An I2C Controller interrupt occurred in IDLE state");
		handle_interrupt_event(I2C_EVENT_ILLEGAL_HW_INTR, i2c);
		goto i2c_int_notifier_end;
	}

	if (!status_trap) {
		/* This should not happen. */
		TPT_ERROR("An unknown I2C Controller macro interrupt occurred");
		goto i2c_int_notifier_end;

	}
#ifdef CHAIN_WRITE
	if ((status_trap & STATUS_MDH) && ( *i2c->reg.ctrl & CTRL_CHAIN)) {
		TPT_TRACE(3, "MDH interrupt");
		*i2c->reg.status_trap &= ~STATUS_MDH;
		handle_interrupt_event(I2C_EVENT_MASTER_TRANSFER_COMPLETE, i2c);

	} else if(status_trap & STATUS_ANAK) {
#else
	if (status_trap & STATUS_ANAK) {
#endif
		/* If address non-Acknowledged. */
		TPT_TRACE(3, STR("Slave address non-Acknowledged, "
		                 "address 0x%02x, statusTrap 0x%02x",
		                 i2c->rw_info.addr, status_trap));
		i2c->rw_info.return_value = I2C_ERROR_ADDRESS_NON_ACKNOWLEDGED;
		handle_interrupt_event(I2C_EVENT_TRANSFER_ABORTED, i2c);

	} else if (status_trap & STATUS_IXF) {
		/* Incomplete transfer */
		TPT_TRACE(3, STR("Incomplete transfer, address 0x%02x, "
		                 "statusTrap 0x%02x",
		                 i2c->rw_info.addr, status_trap));
		i2c->rw_info.return_value = I2C_ERROR_INCOMPLETE_TRANSFER;
		handle_interrupt_event(I2C_EVENT_TRANSFER_ABORTED, i2c);
	} else {
		/* Master transfer is completed. */
		handle_interrupt_event(I2C_EVENT_MASTER_TRANSFER_COMPLETE, i2c);
	}

i2c_int_notifier_end:
	pthread_ret = pthread_mutex_unlock(&i2c->lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
		abort();
	}
	uio_enable_irq(i2c->uio_map.handle);
	return;
}

/**
 * Function i2c_reset
 * Reset i2c
 */
static void i2c_reset(void)
{
	struct i2c_block *i2c = &i2c_;
	/* Reset */
	*(i2c->reg.cmd) = CMD_RESET;
}

/**
 * Function i2c_initRegs
 * Init i2c registers
 */
static void i2c_initRegs(void)
{
	struct i2c_block *i2c = &i2c_;

	/* Store pointers to I2C registers */
	i2c->reg.ctrl = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              XENON_I2C_CTRL);
	i2c->reg.div  = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              XENON_I2C_DIV);
	i2c->reg.addr = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              XENON_I2C_ADDR);
	i2c->reg.data = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              XENON_I2C_DATA);
	i2c->reg.cmd  = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                              XENON_I2C_CMD);

	i2c->reg.status_trap = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                                     XENON_I2C_STATUS_TRAP);
	i2c->reg.status_mask = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                                     XENON_I2C_STATUS_MASK);

	i2c->reg.count = (uint32_t *) (((uintptr_t)i2c->uio_map.base) +
	                               XENON_I2C_COUNT);

	/* Reset the I2C controller */
	i2c_reset();

	/* Set divisor. */
	*(i2c->reg.div) = DIV_100_KHZ;

	/* Clear TRAP register (all bits). */
	*(i2c->reg.status_trap) = ~0;

	/* Set MASK register */
	*(i2c->reg.status_mask) = STATUS_IXF | STATUS_ANAK | STATUS_MTC;

	/* Set the initial state of the driver. */
	i2c->rw_info.state = IDLE;

}

/**
 * Function i2c_init
 * Initialize i2c_block
 */
static int i2c_init(uint32_t i2c_port)
{
	struct i2c_block *i2c = &i2c_;
	char mailboxname[MAILBOX_NAME_LEN];
	pthread_condattr_t cattr;
	int pthread_ret;

	memset(i2c, 0, sizeof( struct i2c_block));

	i2c->port_id = i2c_port;
	pthread_ret = pthread_mutex_init(&i2c->lock, NULL);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_init failed with error %d",
		              pthread_ret));
		abort();
	}
	pthread_ret = pthread_condattr_init(&cattr);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_condattr_init failed with error %d",
		              pthread_ret));
		abort();
	}

	pthread_condattr_setclock(&cattr, CLOCK_MONOTONIC);

	pthread_ret = pthread_cond_init(&i2c->complete, &cattr);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_cond_init failed with error %d",
		              pthread_ret));
		abort();
	}
	pthread_condattr_destroy(&cattr);

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

	return 0;


}


/**
 * Function read_byte
 *
 */
static void read_byte(void)
{
	struct i2c_block *i2c = &i2c_;

	if (i2c->rw_info.rem_len > DATA_BUFFER_FIFO_SIZE) {
		i2c->rw_info.read_len = DATA_BUFFER_FIFO_SIZE;
		i2c->rw_info.rem_len -= DATA_BUFFER_FIFO_SIZE;

		/* The transfer is one in a sequence of transfers. */
		*i2c->reg.ctrl = (CTRL_4BYTE | CTRL_READ);
		i2c->rw_info.is_last_sequence = false;
	} else {
		i2c->rw_info.read_len = i2c->rw_info.rem_len;
		i2c->rw_info.rem_len = 0;

		if(i2c->rw_info.read_len > 4 ||
		    i2c->rw_info.read_len == 0) {
			TPT_ERROR(STR("Invalid length 0x%x",
			              i2c->rw_info.read_len));
			return;
		}

		*i2c->reg.ctrl = CTRL_BYTE(i2c->rw_info.read_len) | CTRL_READ;
		/* This transfer is the last in a sequence of transfers or
		 * the first and only transfer.
		 */
		i2c->rw_info.is_last_sequence = true;
	}

	/* Start transfer */
	*i2c->reg.cmd = CMD_START;

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

#ifdef CHAIN_WRITE
	/* data_len is more than 4 bytes,
	 * use chain write and MDH interrupt */
	if((i2c->rw_info.data_len > DATA_BUFFER_FIFO_SIZE)
	    && i2c->rw_info.is_first_sequence) {
		*i2c->reg.status_mask &= 0;
		*i2c->reg.status_mask |= STATUS_MDH | STATUS_MTC | STATUS_ANAK;

	}
#endif
	if (i2c->rw_info.rem_len > DATA_BUFFER_FIFO_SIZE) {
		write_len = DATA_BUFFER_FIFO_SIZE;
		i2c->rw_info.rem_len -= DATA_BUFFER_FIFO_SIZE;

#ifdef CHAIN_WRITE
		/* The transfer use chain write
		 * when it's not the last in a sequence of transfers. */
		*i2c->reg.ctrl = (CTRL_4BYTE | CTRL_WRITE | CTRL_CHAIN);
#else
		/* The transfer is one in a sequence of transfers. */
		*i2c->reg.ctrl = (CTRL_4BYTE | CTRL_WRITE);
#endif
		i2c->rw_info.is_last_sequence = false;
	} else {
		write_len = i2c->rw_info.rem_len;
		i2c->rw_info.rem_len = 0;

#ifdef CHAIN_WRITE
		/* Disable MDH interrupt when the left length is less than
		 * 4 bytes(last in a sequence or only one transfer)*/
		*i2c->reg.status_mask &= ~STATUS_MDH;

		/* Fix me?
		 * Do we need to use IXF interrupt.
		 * i2c->reg.status_mask |= STATUS_IXF;
		 */
#endif
		if(write_len > 4 || write_len == 0) {
			TPT_ERROR(STR("Invalid length (0x%x)", write_len));
			return;
		}

		/* Number of bytes to write is written to I2C_CMD */
		*i2c->reg.ctrl = CTRL_BYTE(write_len) | CTRL_WRITE;

		/* This transfer is the last in a sequence of transfers or
		 * the first and only transfer.
		 */
		i2c->rw_info.is_last_sequence = true;
	}

	{
		/* Put data in FIFO.
		 * The first bits to transfer are 0:7 (bit 0 is MSB).
		 */

		uint32_t  shift = 24, i;
		uint32_t  data_tmp = 0, data_to_fifo = 0;

		if(i2c->rw_info.sub_write == true) {
			/* Put data in FIFO, bits 0:7 (bit 0 is MSB) */
			shift = 32 - 8 * i2c->rw_info.sub_addr_size;
			data_to_fifo = (i2c->rw_info.sub_addr << shift);
			shift -= 8;
			write_len -= i2c->rw_info.sub_addr_size;
			i2c->rw_info.sub_write = false;
		}

		for(i = 0; i < write_len; i++) {
			data_tmp = (uint32_t)
			           i2c->rw_info.data_ptr[i2c->rw_info.return_value++];
			data_to_fifo = data_to_fifo | (data_tmp << shift);
			shift -= 8;
		}

		TPT_TRACE(3, STR("Write data: 0x%x, ctrl_reg 0x%x",
		                 data_to_fifo, *i2c->reg.ctrl));

		*i2c->reg.data = data_to_fifo;
	}

#ifdef CHAIN_WRITE
	/* Only the first in a sequence of transfer enable start transfer.*/
	if(i2c->rw_info.is_first_sequence == true) {
		/* Start transfer */
		*i2c->reg.cmd = CMD_START;
		i2c->rw_info.is_first_sequence = false;
	}

#else
	/* Start transfer */
	*i2c->reg.cmd = CMD_START;
#endif

#ifdef DEBUG
	debug_trace();
#endif
}
/**
 * Function wait_complete
 */
static bool wait_complete(void)
{
	struct timespec ts;
	struct i2c_block *i2c = &i2c_;
	int pthread_ret;

	clock_gettime(CLOCK_MONOTONIC, &ts);
	ts.tv_sec += 1;
	/* wait for 1second time out */
	pthread_ret = pthread_cond_timedwait(&i2c->complete, &i2c->lock, &ts);
	if(pthread_ret == ETIMEDOUT) {
		i2c->rw_info.return_value = I2C_ERROR_TIMEOUT;
		TPT_ERROR("pthread_cond_timedwait: timed out after 1 second");
		return false;
	} else if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_cond_timedwait: "
		              "Other error with error %d",
		              pthread_ret));
		abort();
	}

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
	int pthread_ret;

	if (itc_size(rec_msg) < sizeof(struct i2c_read_sub_req)) {
		TPT_ERROR("corrupt size of I2C read sub req message.");
		send_msg = itc_alloc(sizeof(struct i2c_read_sub_rej),
		                     RHD_I2C_READ_SUB_REJ);
		send_msg->read_sub_rej.error_code = I2C_ERROR_OTHER;
		send_msg->any_msg.connection_ref = client_ref;
		itc_send(&send_msg, i2c->sender_mbox, ITC_MY_MBOX);
		return;
	}

	pthread_ret = pthread_mutex_lock(&i2c->lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
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

	/* Load the 7 bit slave address into bits 24:30 (bit 31 is LSB) */
	*i2c->reg.addr = i2c->rw_info.addr << 1;

	if(i2c->rw_info.sub_addr_size == 0) {
		/* normal read */
		i2c->rw_info.state = READ;
		read_byte();
	} else {
		/* sub read */
		/* Put sub address in FIFO, bits 0:7 (bit 0 is MSB) */
		uint8_t shift = 32 - 8 * i2c->rw_info.sub_addr_size;
		*i2c->reg.data = (i2c->rw_info.sub_addr << shift);
		*i2c->reg.ctrl = CTRL_BYTE(i2c->rw_info.sub_addr_size) | CTRL_WRITE;

		i2c->rw_info.state = READ_SUB;

		*i2c->reg.cmd = CMD_START;
	}

#ifdef DEBUG
	debug_trace();
#endif

	complete_ret = wait_complete();


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

	pthread_ret = pthread_mutex_unlock(&i2c->lock);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
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
	int pthread_ret;

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

	pthread_ret = pthread_mutex_lock(&i2c->lock);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
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
	i2c->rw_info.rem_len  = rec_msg->write_sub_req.length +
	                        rec_msg->write_sub_req.sub_address_size; /* include sub address  */
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

	/* Load the 7 bit slave address into bits 24:30 */
	*i2c->reg.addr = i2c->rw_info.addr << 1;

	i2c->rw_info.state = WRITE_SUB;

#ifdef DEBUG
	debug_trace();
#endif

	write_byte();

	complete_ret = wait_complete();

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

	pthread_ret = pthread_mutex_unlock(&i2c->lock);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
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
	int pthread_ret;

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

	pthread_ret = pthread_mutex_lock(&i2c->lock);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
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

	/* Load the 7 bit slave address into bits 24:30 */
	*i2c->reg.addr = i2c->rw_info.addr << 1;

	i2c->rw_info.state = READ;

#ifdef DEBUG
	debug_trace();
#endif

	read_byte();

	complete_ret = wait_complete();

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

	pthread_ret = pthread_mutex_unlock(&i2c->lock);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
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
	int pthread_ret;

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
	pthread_ret = pthread_mutex_lock(&i2c->lock);
	if(pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_lock failed with error %d",
		              pthread_ret));
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
	/* Load the 7 bit slave address into bits 24:30 */
	*i2c->reg.addr = i2c->rw_info.addr << 1;

	i2c->rw_info.state = WRITE;

#ifdef DEBUG
	debug_trace();
#endif

	write_byte();

	complete_ret = wait_complete();

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

	pthread_ret = pthread_mutex_unlock(&i2c->lock);
	if (pthread_ret != 0) {
		TPT_ERROR(STR("pthread_mutex_unlock failed with error %d",
		              pthread_ret));
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

	snprintf(uiodevname, sizeof(uiodevname),
	         "%s%d", UIO_DEV_I2C, i2c->port_id);

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

	uio_disable_irq(i2c->uio_map.handle);

	/**
	 * Set the notifier for the interrupts.
	 */
	if (uio_irq_set_notifier(i2c->uio_map.handle, i2c_int_notifier, NULL)) {
		TPT_ERROR("Failed to set UIO interrupt notifier");
		goto set_uio_map_and_irq_err;
	}
	TPT_TRACE(1, "UIO interrupt notifier set.");

	/* start interrupt handler */
	if (uio_bind_irq(i2c->uio_map.handle)) {
		TPT_ERROR("Unable to start UIO interrupt handler");
		goto set_uio_map_and_irq_err;
	}
	TPT_TRACE(1, "UIO interrupt handler started");

	uio_enable_irq(i2c->uio_map.handle);

	return true;

set_uio_map_and_irq_err:

	uio_close(i2c->uio_map.handle);

	return false;
}

static bool unreserve_pinmux(pinmux_handle_t pinmux_handle)
{
	pinmux_status_t status;
	struct i2c_block *i2c = &i2c_;

	status = pinmux_unreserve(pinmux_handle);
	if(status != PINMUX_STATUS_SUCCESS) {
		TPT_ERROR(STR("pinmux unreserve i2c port %d pins failed"
		              " with status %d",
		              i2c->port_id, status));
		return false;
	}
	return true;
}

static bool set_pinmux(void)
{
	struct i2c_block *i2c = &i2c_;
	pinmux_handle_t pinmux_handle = NULL;
	pinmux_status_t status;
	status = pinmux_reserve(sizeof(pin_number[i2c->port_id]) / sizeof(
	                                pin_number[i2c->port_id][0]),
	                        pin_number[i2c->port_id],
	                        &pinmux_handle);

	if(status != PINMUX_STATUS_SUCCESS) {
		TPT_ERROR(STR("pinmux reserve i2c port%d pins failed "
		              "with status %d",
		              i2c->port_id, status));
		return false;
	}

	status = pinmux_set_func(pinmux_handle,
	                         sizeof(pin_number[i2c->port_id]) /
	                         sizeof(pin_number[i2c->port_id][0]),
	                         pin_number[i2c->port_id],
	                         PINMUX_FUNC_TYPE_ALTF1);

	if(status != PINMUX_STATUS_SUCCESS) {
		TPT_ERROR(STR("pinmux set i2c port%d pins func failed "
		              "with status %d",
		              i2c->port_id, status));
		unreserve_pinmux(pinmux_handle);
		return false;
	}

	i2c->pinmux_handle = pinmux_handle;

	return true;
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

		if(set_pinmux() == false) {
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
		unreserve_pinmux(i2c->pinmux_handle);
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

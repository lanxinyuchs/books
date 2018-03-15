#include <stdint.h>
#include <string.h>
#include <tlnh_cmi.h>

#include "dp-cfg.h"
#include "mpu-api.h"
#include "shmem-cm.h"
#include "lnh-api.h"


#define ENABLE_INTERRUPTS() asm("cpsie i")
#define DISABLE_INTERRUPTS() asm("cpsid i")

struct irq_regs {
	uint32_t ISER[8];         /* interrupt set enable register */
	uint32_t RESERVED0[24];
	uint32_t ICER[8];         /* interrupt clear enable register */
	uint32_t RSERVED1[24];
	uint32_t ISPR[8];         /* interrupt set pending register */
	uint32_t RESERVED2[24];
	uint32_t ICPR[8];         /* interrupt clear pending register */
	uint32_t RESERVED3[24];
	uint32_t IABR[8];         /* interrupt active bit register */
	uint32_t RESERVED4[56];
	uint8_t  IP[240];         /* interrupt priority register */
	uint32_t RESERVED5[644];
	uint32_t STIR;            /* software trigger interrupt register */
};

#define MBOX_REGBASE      0x80008000
#define DP_DATA_RAM_START 0x20000
#define MBOX_FIRST_IRQ    32
#define MBOX_FIRST_ISR    48

#define MBOX_ISR_SET(mbox_id, isr) {                                   \
	*(((isr_vector*) 0) + (MBOX_FIRST_ISR + (mbox_id))) = (isr);   \
}

#define ENABLE_IRQ(irq) {                                              \
    irq_regsp->ISER[(uint32_t)((int32_t)irq) >> 5] =                   \
        (uint32_t)(1 << ((uint32_t)((int32_t)irq) & (uint32_t)0x1f));  \
}

#define DISABLE_IRQ(irq) {                                                    \
	irq_regsp->ICER[((uint32_t)(irq) >> 5)] = (1 << ((uint32_t)(irq) & 0x1f));\
}

#define IRQ_BASE (0xe000e000UL + 0x0100UL)

static volatile struct irq_regs * irq_regsp = (struct irq_regs *) IRQ_BASE;

static int rx_irq;
static int tx_irq;

static struct shmem_cm_fifo fifo_rx;
static struct shmem_cm_fifo fifo_tx;


static void transmit(uint32_t sid,
                     uint32_t srcAddr, uint32_t dstAddr,
                     uint32_t size, void *data)
{
	struct shmem_cm_msg_data msg = {
		.type = SHMEM_CM_MSG_TYPE_DATA,
		.src = srcAddr,
		.dst = dstAddr,
		.size = size
	};

	if (shmem_cm_fifo_enqueue(&fifo_tx,
	                          &msg, sizeof(msg),
	                          data, size) != 0) {
		/* TODO: Call error handler from here!!! */
	}
}

static void handle_connect(uint32_t version)
{
	static const struct shmem_cm_msg_ack msg_type_ack = {
		SHMEM_CM_MSG_TYPE_ACK
	};
	static const struct shmem_cm_msg_nack msg_type_nack = {
		SHMEM_CM_MSG_TYPE_NACK,
		SHMEM_CM_MSG_VERSION
	};

	if (version == SHMEM_CM_MSG_VERSION) {
		shmem_cm_fifo_enqueue(&fifo_tx,
		                      &msg_type_ack, sizeof(msg_type_ack), 0, 0);
		tlnhConnected(0, transmit);
	} else {
		shmem_cm_fifo_enqueue(&fifo_tx,
		                      &msg_type_nack, sizeof(msg_type_nack), 0, 0);
	}
}

static void handle_disconnect(void)
{
	/* TODO: Call error handler from here!!! */
}

static void handle_ping(void)
{
	static const uint32_t msg_type = SHMEM_CM_MSG_TYPE_PONG;

	shmem_cm_fifo_enqueue(&fifo_tx, &msg_type, sizeof(msg_type), 0, 0);
}

static void upcall_rx(void *object, uint32_t *data, uint32_t size)
{
	union shmem_cm_msg *cm_msg = (union shmem_cm_msg*) data;

	switch (cm_msg->type) {

	case SHMEM_CM_MSG_TYPE_DATA:
		tlnhDeliver(cm_msg->data.src,
		            cm_msg->data.dst,
		            cm_msg->data.size,
		            cm_msg->data.data);
		break;

	case SHMEM_CM_MSG_TYPE_PING:
		handle_ping();
		break;

	case SHMEM_CM_MSG_TYPE_CONN:
		handle_connect(cm_msg->conn.version);
		break;

	case SHMEM_CM_MSG_TYPE_DISC:
		handle_disconnect();
		break;

	default:
		break;
	}

	return;
}

#ifdef XSIM_WORKAROUND
static void delay(int num)
{
	volatile int i = 0;
	while (i++ < num) {}
}
#endif

static void lnh_isr_rx(void)
{
#ifdef XSIM_WORKAROUND
	/* Workaround for SRAM not always being updated in xsim, being investigated */
	delay(2000000);
#endif
	shmem_cm_fifo_rx(&fifo_rx);
}

static void lnh_isr_tx(void)
{
	shmem_cm_fifo_tx(&fifo_tx);
}

typedef void(*isr_vector)(void);

static void __attribute__((interrupt("IRQ"))) rx_isr()
{
	DISABLE_INTERRUPTS();
	lnh_isr_rx();
	ENABLE_INTERRUPTS();
}

static void __attribute__((interrupt("IRQ"))) tx_isr()
{
	DISABLE_INTERRUPTS();
	lnh_isr_tx();
	ENABLE_INTERRUPTS();
}

struct config_mpu {
	int region;
	uint32_t start;
	uint64_t size;
	uint32_t attr;
};

int lnh_init(void)
{
	struct dp_cfg volatile *cfg = (struct dp_cfg *)
	                                  (DP_DATA_RAM_START + DP_CFG_OFFSET);

	struct config_mpu mpu_configs[] = {
		{ /* First 1GB */
			0,
			0x00000000,
			1024 * 1024 * 1024,
			MPU_RASR_AP_NO_ACCESS | MPU_RASR_XN
		},
		{ /* DP CODE IRAM */
			1,
			0x00000000,
			128 * 1024,
			MPU_RASR_TEXCB_OIWTNWA | MPU_RASR_AP_FULL_ACCESS
		},
		{ /* DP DATA IRAM */
			2,
			0x00020000,
			64 * 1024,
			MPU_RASR_TEXCB_OIWTNWA | MPU_RASR_AP_FULL_ACCESS
		},
		{ /* DDR */
			3,
			0x40000000,
			1024 * 1024 * 1024,
			MPU_RASR_AP_NO_ACCESS | MPU_RASR_XN
		},
		{ /* SRAM */
			4,
			0x86000000,
			256 * 1024,
			MPU_RASR_TEXCB_OIWTNWA | MPU_RASR_SHAREABLE | MPU_RASR_AP_FULL_ACCESS
		},
		{ /* Peripheral registers */
			5,
			0x80000000,
			1024 * 1024 * 1024,
			MPU_RASR_TEXCB_SO | MPU_RASR_XN | MPU_RASR_AP_FULL_ACCESS
		},
		{ /* CPU subsystem */
			6,
			0xf0000000,
			256 * 1024,
			MPU_RASR_AP_NO_ACCESS | MPU_RASR_XN
		}
	};

	/* Configure the mpu */
	for (int i = 0; i < sizeof(mpu_configs) / sizeof(mpu_configs[0]); i++) {
		struct config_mpu *m = &mpu_configs[i];
		mpu_configure_region(m->region, m->start, m->size, m->attr);
	}

	/* Enable the mpu */
	mpu_enable();

	/* Initiate Tiny Link handler */
	tlnhInit();

	tx_irq = MBOX_FIRST_IRQ + cfg->link_cfg.tx_mbox_id;
	rx_irq = MBOX_FIRST_IRQ + cfg->link_cfg.rx_mbox_id;

	/* Put the mailbox ISRs pointers in the interrupt vector. */
	MBOX_ISR_SET(cfg->link_cfg.rx_mbox_id, rx_isr);
	MBOX_ISR_SET(cfg->link_cfg.tx_mbox_id, tx_isr);

	/* Configure RX FIFO */
	shmem_cm_fifo_init(&fifo_rx,
	                   NULL, &upcall_rx,
	                   (void*) MBOX_REGBASE + cfg->link_cfg.rx_mbox_id * 4,
	                   (uint32_t*) cfg->link_cfg.rx_shmem_start,
	                   cfg->link_cfg.rx_shmem_size / 4);

	/* Configure TX FIFO */
	shmem_cm_fifo_init(&fifo_tx,
	                   NULL, NULL,
	                   (void*) MBOX_REGBASE + cfg->link_cfg.tx_mbox_id * 4,
	                   (uint32_t*) cfg->link_cfg.tx_shmem_start,
	                   cfg->link_cfg.tx_shmem_size / 4);

	ENABLE_IRQ(rx_irq);
	ENABLE_IRQ(tx_irq);

	return 0;
}

#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "shmem-cm.h"

/* Only support traces on the CPU side for now */
#ifdef __linux__
#define TRACEPOINT_PROVIDER com_ericsson_shmem_cm
#include <tpt_create.h>
#include <tpt.h>
#else
#define TPT_INFO(s)
#define TPT_ERROR(s)
#define TPT_TRACE(group, s)
#endif

/* FIFO Control words */
#define SHMEM_CM_FIFO_WORD_WAIT     0x00000000
#define SHMEM_CM_FIFO_WORD_WRAP     0xffffffff

#define SHMEM_CM_FIFO_IRQ_GEN_RX(fifo)  \
	do { *(volatile uint32_t*) (fifo)->mbox = 0xffffffff; } while(0)

#define SHMEM_CM_FIFO_IRQ_GEN_TX(fifo)  \
	do { volatile uint32_t e = *((fifo)->mbox); (void) e; } while(0)

void shmem_cm_fifo_init(struct shmem_cm_fifo *fifo,
                        void *object, shmem_cm_fifo_uc uc,
                        uint32_t *mbox, uint32_t *start, uint32_t size)
{
	fifo->object = object;
	fifo->uc = uc;
	fifo->mbox = mbox;
	fifo->head = (uint32_t*) start;
	fifo->start = start + 1;
	fifo->end = size - 1;
}

void shmem_cm_fifo_tx(struct shmem_cm_fifo *fifo)
{
	/* If remote side has more to read then generate RX interrupt */
	if (fifo->start[*fifo->head] != SHMEM_CM_FIFO_WORD_WAIT) {
		SHMEM_CM_FIFO_IRQ_GEN_RX(fifo);
	}

	if (fifo->uc != NULL) {
		fifo->uc(fifo->object, NULL, 0);
	}
}

void shmem_cm_fifo_rx(struct shmem_cm_fifo *fifo)
{
	/* While we are not to wait for more... */
	while (fifo->start[*fifo->head] != SHMEM_CM_FIFO_WORD_WAIT) {

		/* FIFO wrap-around, continue reception from start */
		if (fifo->start[*fifo->head] == SHMEM_CM_FIFO_WORD_WRAP) {
			*fifo->head = 0;
			continue;
		}

		/* Receive data from FIFO */
		fifo->uc(fifo->object,
		         &fifo->start[(*fifo->head) + 1],
		         fifo->start[*fifo->head] - 1);
		*fifo->head += fifo->start[*fifo->head];
	}

	SHMEM_CM_FIFO_IRQ_GEN_TX(fifo);
}

int shmem_cm_fifo_enqueue(struct shmem_cm_fifo *fifo,
                          const void *data0, uint32_t size0,
                          const void *data1, uint32_t size1)
{
	uint32_t  head, *wrap = NULL;
	int	  free0, free1;
	int	  words = 1 + ((size0 + size1 + 3) & ~3) / 4;

	/* Check for free memory in FIFO. */
	head = *fifo->head;
	if (fifo->write >= head) {
		free0 = fifo->end - fifo->write;
		free1 = head;
	} else {
		free0 = head - fifo->write;
		free1 = 0;
	}
	if (words >= free0) {
		if (words >= free1) {
			return -1; /* Ugh, no more space. */
		} else {
			wrap = &fifo->start[fifo->write];
			fifo->write = 0;
		}
	}

	/* Copy data0 */
	if (size0 > 0) {
		memcpy(&fifo->start[fifo->write + 1], data0, size0);
	}

	/* Copy data1 */
	if (size1 > 0) {
		memcpy(((char*)&fifo->start[fifo->write + 1]) + size0, data1, size1);
	}

	/* Insert WAIT */
	fifo->start[fifo->write + words] = SHMEM_CM_FIFO_WORD_WAIT;

	/* Insert SIZE */
	fifo->start[fifo->write] = words;

	/* Insert WRAP if needed */
	if (wrap != NULL) {
		*wrap = SHMEM_CM_FIFO_WORD_WRAP;
	}

	/* Update the write pointer */
	fifo->write += words;

	/* Generate RX interrupt */
	SHMEM_CM_FIFO_IRQ_GEN_RX(fifo);

	return 0;
}

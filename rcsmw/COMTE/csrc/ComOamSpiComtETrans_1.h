#ifndef ComOamSpiComtETrans_1_h
#define ComOamSpiComtETrans_1_h

/**
 * TODO: Can't have this include here, seems like I've made a mess of includes...
 * #include <ComOamSpiTransactionalResource_1.h>
 */
#include <pthread.h>

// Forward declare for usage in Component
typedef struct comte_trans comte_trans_t;
typedef struct comte_client_connection comte_client_con_t;

#include "ComOamSpiComtEComponent_1.h"
#include "ComtEComm_1.h"


struct comte_client_connection {
	MafOamSpiTransactionHandleT transId;
	comte_con_handle_t connection;
	comte_client_con_t* next_trans;
};

/* --------------------------------------------------------------- */
/* TransactionalResource version                                   */
/* --------------------------------------------------------------- */
#define POPULATE_TRANS_IF_DEF(trans)                                    \
    trans->base.base.componentName = OAM_COMPONENT_NAME;                \
    trans->base.base.interfaceName = ComtETransResourceT_IfName;        \
    trans->base.base.interfaceVersion = ComtETransResourceT_IfVsn;      \
    trans->base.join = join;                                            \
    trans->base.prepare = prepare;                                      \
    trans->base.commit = commit;                                        \
    trans->base.abort = abort_transaction;                              \
    trans->base.finish = finish;                                        \
    trans->client = NULL;                                               \
    pthread_rwlock_init(&trans->cc_rwlock, NULL);



#ifndef COM_TRANS_H
#define COM_TRANS_H 1
#endif

#if COM_TRANS_H < 2
#include <MafOamSpiTransaction_1.h>
#include <MafOamSpiTransactionalResource_1.h>

#define ComtETransaction_Id MafOamSpiTransaction_1Id
typedef MafOamSpiTransaction_1T ComtETransactionT;
typedef MafOamSpiTransactionalResource_1T ComtETransResourceT;
#define ComtETransResourceT_IfName MafOamSpiTransactionalResource_1Id.interfaceName
#define ComtETransResourceT_IfVsn MafOamSpiTransactionalResource_1Id.interfaceVersion
#define POPULATE_TRANS_IF(trans)                \
    POPULATE_TRANS_IF_DEF(trans);

#else
/* Use newer SPIs */
#include <MafOamSpiTransaction_2.h>
#include <MafOamSpiTransactionalResource_2.h>

#define ComtETransaction_Id MafOamSpiTransaction_2Id
typedef MafOamSpiTransaction_2T ComtETransactionT;
typedef MafOamSpiTransactionalResource_2T ComtETransResourceT;
#define ComtETransResourceT_IfName MafOamSpiTransactionalResource_2Id.interfaceName
#define ComtETransResourceT_IfVsn MafOamSpiTransactionalResource_2Id.interfaceVersion
#define POPULATE_TRANS_IF(trans)                \
    POPULATE_TRANS_IF_DEF(trans);               \
    trans->base.validate = validate;
#endif



struct comte_trans {
	ComtETransResourceT base;
	comte_client_con_t* client;
	pthread_rwlock_t cc_rwlock;
};

MafReturnT comte_transactional_resource_create(comte_oam_component_t* component);
MafReturnT comte_transactional_resource_destroy(comte_oam_component_t* component);

#endif

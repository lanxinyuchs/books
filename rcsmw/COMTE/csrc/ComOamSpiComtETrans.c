/**
 * @author Lukas Larsson
 * @created 2011-03-01
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <MafOamSpiServiceIdentities_1.h>
#include <MafMwSpiServiceIdentities_1.h>
#include <MafOamSpiTransactionMaster_1.h>
#include "ComOamSpiComtETrans_1.h"
#include "ComMwSpiComtEComponent_1.h"
#include "ComtEUtils_1.h"
#include "ComtEComm_1.h"
#include "ComOamSpiComtEConverter_1.h"

static comte_oam_component_t* trans_component = NULL;

static MafReturnT join(MafOamSpiTransactionHandleT txHandle) {
	ENTER();
        MafReturnT res = MafFailure;

        /* Start checking for transaction */
        comte_client_con_t* client_con = find_client_con(
            trans_component->trans, txHandle);
	if (client_con != NULL) {
            ERROR("transaction id already exists!");
            return res;
	}

	MafMgmtSpiInterface_1T* implementation = NULL;
	res = trans_component->portal->getInterface(
            ComtETransaction_Id, &implementation);
	if (res != MafOk) {
            ERROR("join Transaction getInterface failed");
            return res;
	}

        ComtETransactionT * transaction =
			(ComtETransactionT*) implementation;

	res =
            transaction->registerParticipant(txHandle,
                                             &trans_component->trans->base);
	if (res != MafOk) {
		ERROR("join Transaction registerParticipant failed");
		return res;
	}
	INFO("join Transaction OK");

	client_con = comte_malloc(sizeof(comte_client_con_t));
	client_con->transId = txHandle;
	client_con->connection.quiet = 0;
	res = comte_connect(trans_component->config->comte_ip,
                            trans_component->config->comte_port,
                            &client_con->connection);
	if (res != MafOk) {
		ERROR("connect failed");
		comte_free(client_con);
		return res;
	}
	INFO("connected");

	comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
	res = encode_join((unsigned long) txHandle, buff);
	if (res != MafOk) {
		comte_free(buff);
		comte_free(client_con);
		return res;
	}

	res = comte_send_recv(buff, &client_con->connection);
	if (res != MafOk) {
		comte_free(buff);
		comte_free(client_con);
		return res;
	}

	res = decode_join(buff);
	comte_free(buff->buff);
	comte_free(buff);

	if (res != MafOk)
		ERROR("handshake failed");

	pthread_rwlock_wrlock(&trans_component->trans->cc_rwlock);

	client_con->next_trans = trans_component->trans->client;
	trans_component->trans->client = client_con;

	pthread_rwlock_unlock(&trans_component->trans->cc_rwlock);

	INFO("handshake OK");

	res = comte_mem_list_create(txHandle);

	LEAVE();
	return res;
}

static MafReturnT prepare(MafOamSpiTransactionHandleT txHandle) {
	ENTER();
	MafReturnT res = MafFailure;

        comte_client_con_t* client_con = find_client_con(
            trans_component->trans, txHandle);
        if (!client_con) {
            ERROR("transaction with id %lu was not found!", txHandle);
            return res;
	}

	comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
	res = encode_prepare((unsigned long) txHandle, buff);
	if (res != MafOk) {
            comte_free(buff);
            return res;
	}


	res = comte_send_recv(buff, &client_con->connection);
	if (res != MafOk) {
            comte_free(buff);
            return res;
	}

	res = decode_prepare(buff);
	comte_free(buff->buff);
	comte_free(buff);

	if (res != MafOk) {
		res = MafPrepareFailed;
		INFO("prepare failed");
	}
	LEAVE();
	return res;
}

static MafReturnT commit(MafOamSpiTransactionHandleT txHandle) {
	ENTER();
	MafReturnT res = MafFailure;

        comte_client_con_t* client_con = find_client_con(
			trans_component->trans, txHandle);
        if (!client_con) {
            ERROR("transaction with id %lu was not found!", txHandle);
            return res;
	}

	comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
	res = encode_commit((unsigned long) txHandle, buff);
	if (res != MafOk) {
            comte_free(buff);
            return res;
	}

	res = comte_send_recv(buff, &client_con->connection);
	if (res != MafOk) {
		comte_free(buff);
		return res;
	}

	res = decode_commit(buff);
	comte_free(buff->buff);
	comte_free(buff);

	if (res != MafOk)
		ERROR("commit failed");
	LEAVE();
	return res;
}

static MafReturnT abort_transaction(MafOamSpiTransactionHandleT txHandle) {
	ENTER();
	MafReturnT res = MafFailure;

	comte_client_con_t* client_con = find_client_con(
			trans_component->trans, txHandle);

	// If no client con exists, then the failure probably
	// happened because the erlang side was not up.
        if (!client_con) {
            ERROR("transaction with id %lu was not found!", txHandle);
            return res;
	}

	comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
	res = encode_abort_transaction((unsigned long) txHandle, buff);
	if (res != MafOk) {
		comte_free(buff);
		return res;
	}

	res = comte_send_recv(buff, &client_con->connection);
	if (res != MafOk) {
		comte_free(buff);
		return res;
	}

	res = decode_abort_transaction(buff);
	comte_free(buff->buff);
	comte_free(buff);

	if (res != MafOk)
		ERROR("abort_transaction failed");
	LEAVE();
	return res;
}

static MafReturnT finish(MafOamSpiTransactionHandleT txHandle) {
	ENTER();
	MafReturnT res = comte_mem_list_free(txHandle);

	comte_client_con_t* client_con = find_client_con(
			trans_component->trans, txHandle);

	// If no client con exists, then the failure probably
	// happened because the erlang side was not up.
        if (!client_con) {
            ERROR("transaction with id %lu was not found!", txHandle);
            return res;
         }

        comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
	res = encode_finish((unsigned long) txHandle, buff);
	if (res != MafOk) {
		comte_free(buff);
		return res;
	}

	res = comte_send_recv(buff, &client_con->connection);
	if (res != MafOk) {
		comte_free(buff);

		pthread_rwlock_wrlock(&trans_component->trans->cc_rwlock);
		trans_component->trans->client =
				remove_client_con(trans_component->trans->client, txHandle);
		pthread_rwlock_unlock(&trans_component->trans->cc_rwlock);

		return res;
	}

	res = decode_finish(buff);
	comte_free(buff->buff);
	comte_free(buff);

	if (res != MafOk)
		ERROR("finish failed");

	comte_disconnect(&client_con->connection);

	pthread_rwlock_wrlock(&trans_component->trans->cc_rwlock);
	trans_component->trans->client =
			remove_client_con(trans_component->trans->client, txHandle);
	pthread_rwlock_unlock(&trans_component->trans->cc_rwlock);

	LEAVE();
	return res;
}


/* Applicable for TransactionalResource_2 */
#if COM_TRANS_H > 1
static MafReturnT validate(MafOamSpiTransactionHandleT txHandle,
                           bool *result) {
    ENTER();
    MafReturnT res = MafFailure;

    comte_client_con_t* client_con = find_client_con(
        trans_component->trans, txHandle);
    if (!client_con) {
        ERROR("transaction with id %lu was not found!", txHandle);
        return res;
    }

    comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t));
    res = encode_validate((unsigned long) txHandle, buff);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = comte_send_recv(buff, &client_con->connection);
    if (res != MafOk) {
        comte_free(buff);
        return res;
    }

    res = decode_validate(buff, result);
    INFO("Validate transaction %lu -> %d, %d",
		 txHandle, res, (int) (*result));

    comte_free(buff->buff);
    comte_free(buff);

    LEAVE();
    return res;
}
#endif

MafReturnT comte_transactional_resource_create(comte_oam_component_t* comp) {

	MafReturnT res = MafOk;

	trans_component = comp;
	trans_component->trans = comte_malloc(sizeof(comte_trans_t));
	comte_trans_t* trans = trans_component->trans;

        /* Transactional resource version dependent */
        POPULATE_TRANS_IF(trans);

	return res;
}

MafReturnT comte_transactional_resource_destroy(comte_oam_component_t* comp) {

	MafReturnT res = MafOk;
	pthread_rwlock_destroy(&comp->trans->cc_rwlock);
	comte_free(comp->trans->client);
	comte_free(comp->trans);

	return res;
}


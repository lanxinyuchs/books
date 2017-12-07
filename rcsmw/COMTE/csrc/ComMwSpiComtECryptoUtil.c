/** 
 * @author Magnus Lidén
 * @created 2013-02-01
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <MafMwSpiServiceIdentities_1.h>
#include "ComMwSpiComtEComponent_1.h"
#include "ComMwSpiComtECryptoUtil_1.h"
#include "ComtEUtils_1.h"
#include "ComtEComm_1.h"
#include "ComMwSpiComtEConverter_1.h"

comte_mw_component_t* cu_component = NULL;


MafReturnT comte_encrypt (const char* string, char** result)
{
    ENTER();
   
    MafReturnT res = MafOk;
    comte_con_handle_t connection;

    connection.quiet = 0;
    res = comte_connect(cu_component->config->comte_ip, cu_component->config->comte_port, &connection);
    if (res != MafOk) {
        ERROR("connect failed");
        return res;
    }

    comte_buff_t buff;
    res = encode_encrypt(string, &buff);
    if (res != MafOk) {
        ERROR("encode failed");
        comte_disconnect(&connection);
        return res;
    }

    res = comte_send_recv(&buff, &connection);
    if (res != MafOk) {
        return res;
    }
    
    res = decode_encrypt(&buff, result);
    if (res != MafOk) {
        ERROR("decode failed");
        comte_free(buff.buff);
        comte_disconnect(&connection);
        return res;
    }
    
    comte_free(buff.buff);
    comte_disconnect(&connection);

    LEAVE();
    return MafOk;
}

MafReturnT comte_decrypt(const char* encryptedString, char** result)
{
    ENTER();
    
    MafReturnT res = MafOk;
    comte_con_handle_t connection;

    connection.quiet = 0;
    res = comte_connect(cu_component->config->comte_ip, cu_component->config->comte_port, &connection);
    if (res != MafOk) {
        ERROR("connect failed");
        return res;
    }

    comte_buff_t buff;
    res = encode_decrypt(encryptedString, &buff);
    if (res != MafOk) {
        ERROR("encode failed");
        comte_disconnect(&connection);
        return res;
    }

    res = comte_send_recv(&buff, &connection);
    if (res != MafOk) {
        return res;
    }
    
    res = decode_decrypt(&buff, result);
    if (res != MafOk) {
        ERROR("decode failed");
        comte_free(buff.buff);
        comte_disconnect(&connection);
        return res;
    }
    
    comte_free(buff.buff);
    comte_disconnect(&connection);

    LEAVE();
    return MafOk;
}

MafReturnT comte_crypto_util_create(comte_mw_component_t* comp) {

	MafReturnT res = MafOk;

	cu_component = comp;
	cu_component->cu = comte_malloc(sizeof(comte_crypto_util_t));
	comte_crypto_util_t* cu = cu_component->cu;

	cu->base.base.componentName = MW_COMPONENT_NAME;
	cu->base.base.interfaceName = MafMwSpiCrypto_1Id.interfaceName;
	cu->base.base.interfaceVersion =
            MafMwSpiCrypto_1Id.interfaceVersion;

	cu->base.encrypt = comte_encrypt;
	cu->base.decrypt = comte_decrypt;

	return res;
}

MafReturnT comte_crypto_util_destroy(comte_mw_component_t* comp) {

	MafReturnT res = MafOk;
	comte_free(comp->cu);
	return res;
}

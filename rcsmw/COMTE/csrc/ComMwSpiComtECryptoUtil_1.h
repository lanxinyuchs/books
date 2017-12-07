#ifndef ComMwSpiComtECryptoUtil_1_h
#define ComMwSpiComtECryptoUtil_1_h

#include <MafMwSpiCrypto_1.h>

typedef struct comte_cu comte_crypto_util_t;

#include "ComMwSpiComtEComponent_1.h"

struct comte_cu {
	MafMwSpiCrypto_1T base;
};

MafReturnT comte_crypto_util_create(comte_mw_component_t* component);
MafReturnT comte_crypto_util_destroy(comte_mw_component_t* component);

MafReturnT comte_encrypt (const char* string, char** result);
MafReturnT comte_decrypt (const char* encryptedString, char** result);

#endif

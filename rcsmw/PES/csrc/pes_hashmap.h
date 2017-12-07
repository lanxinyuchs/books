#ifndef PES_HASHMAP_H_
#define PES_HASHMAP_H_
/* ----------------------------------------------------------------------
 * %CCaseFile:	pes_hashmap.h %
 * %CCaseRev:	/main/R3A/1 %
 * %CCaseDate:	2014-11-12 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * Thread-safe hashmap functionality for the PES C interface.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.h %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2014 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date         Name        What
 * -----      -------      --------    --------------------------
 * R3A/1      2014-11-10   eolaand     Created
 * ----------------------------------------------------------------------
 */


#include <stdbool.h>

/*
 * Maximum number of keys that can be in use at a
 * certain time.
 */
#define PES_HASHMAP_SIZE 100

#define PES_HASHMAP_OK                       1
#define PES_HASHMAP_UNSPECIFIC_FAILURE       2
#define PES_HASHMAP_MAX_KEYS_EXCEEDED        3
#define PES_HASHMAP_OVERWRITE                4
#define PES_HASHMAP_OUT_OF_MEMORY            5
#define PES_HASHMAP_CLEAR_NONEXISTENT_ENTRY  6
#define PES_HASHMAP_CLEAR_ENTRY_TWICE        7

/*
 * Convenience function that calls createKey and uses
 * the obtained key to insert the given value. The caller
 * must check the returned code. For PES_HASHMAP_OK the
 * location pointed to by the 'result' argument will receive
 * a pointer to a null-terminated array of characters
 * which is a key to the stored value.
 */
int peiMap_createKeyAndPut(char const *prefix, void const *value, const void **result);

/*
 * Looks up a map entry for the given key. NULL is returned
 * if the key is not in the map (either it has never been in
 * the map, or it has been cleared).
 */
void *peiMap_get(char const *key);

/*
 * Clears the given key. The key is put on the reuse list.
 * The map entry is not removed, but it is set to NULL, thus
 * a subsequent get() call will return NULL for the key.
 * Returns PES_HASHMAP_OK when successful (that is, the entry
 * was actually found and cleared).
 */
int peiMap_clear(char const * key);

#endif /* PES_HASHMAP_H_ */

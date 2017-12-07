/* ----------------------------------------------------------------------
 * %CCaseFile:	pes_hashmap.c %
 * %CCaseRev:	/main/R3A/1 %
 * %CCaseDate:	2014-11-12 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description:
 * A thread-safe hashmap utility with bounded memory usage. The amount of
 * heap memory consumed is proportional to the maximum number of keys that
 * are handed out at some particular time (it is assumed that keys are
 * created with the 'createKeyAndPut' function). Keys that are no longer
 * used should be "handed back" with the 'clear' function and will then be
 * reused in subsequent 'createKeyAndPut' invocations.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
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
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R3A/1      2014-11-10 eolaand     Created
 * ----------------------------------------------------------------------
 */




#define _GNU_SOURCE
#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <search.h>
#include <assert.h>
#include <pthread.h>



#define THREADSAFE 1


// The SELF_TEST feature interacts with THREADSAFE;
// do not enable both at the same time!
// Fix it later if there is time
#define SELF_TEST 0
#define SELF_TEST_VERBOSE 0

#if SELF_TEST
#include <string.h>
#endif

#include "pes_hashmap.h"



#if THREADSAFE
#define ZERO(expr) assert(0 == (expr))
#define LOCK_WRLOCK(lock) ZERO(pthread_rwlock_wrlock(lock))
#define LOCK_UNLOCK(lock) ZERO(pthread_rwlock_unlock(lock))
#else
#define LOCK_WRLOCK(lock) ;
#define LOCK_UNLOCK(lock) ;
#endif

/*
 * This struct is used for holding a collection of
 * keys that are available for reuse.
 */
typedef struct {
	const char *key;
	void *tail;
} keyChain;

/*
 * Static non-public data. The use
 * of static data implies that only one thread at
 * a time may access the public functions. This
 * is accomplished with the map_lock.
 */
static int keyNumber = 0;
static keyChain *usedKeys = NULL;
static bool isMapInitialized = false;
static struct hsearch_data map = {.table = NULL};
static int entryCount = 0;

#if THREADSAFE
static pthread_rwlock_t map_lock = PTHREAD_RWLOCK_INITIALIZER;
#endif


static int threadUnsafeCreateKey(char const *prefix, const char **result);

static int threadUnsafePut(char const *key, void const *value);



/*
 * Returns a pointer to the internal data structure
 * that implements the map. This function is not for
 * public use.
 */
static struct hsearch_data *getMap();


#if SELF_TEST
/*
 * Test a large number of initialize and finalize actions.
 * The function returns true when successful.
 */
bool selfTestHeavy();

/*
 * Returns the number of map entries, including
 * cleared entries. This number will reflect the
 * maximum number of simultaneously active applications.
 */
int getEntryCount();


/*
 * This test does not clutter the map a lot, the number of
 * map entries created will not exceed the number of AppRecord
 * instances.
 */

/*
 * Represents an application that performs initialize
 * and finalize repeatedly. Time is represented as an
 * integer starting at 0. The first initialize happens
 * when t == delay. Subsequent initialize happen at
 * t == delay + N*period. Finalize happens at
 * t == delay + N*period + uptime, where uptime < period.
 */
typedef struct {
	char *name;
	int delay;
	int period;
	int uptime;
	char *handle;
} AppRecord;

/*
 * Returns the number of map entries, including
 * cleared entries. This number will reflect the
 * maximum number of simultaneously active applications.
 */
int getEntryCount() {
	return entryCount;
}


/*
 * Test a large number of initialize and finalize actions.
 * The function ought to return true..
 *
 * The test runs quickly; on an Intel host each timetick
 * takes about 0.1 microsecond.
 */
bool selfTestHeavy() {

	int const numberOfLaps = 100000;
	int const numberOfApps = 6;

#if SELF_TEST_VERBOSE
	printf("entered selfTestHeavy, apps: %d, timeticks: %d\n", numberOfApps, numberOfLaps);
#endif

	AppRecord apps[] = {
			{"a1", 30, 31, 23, NULL},
			{"a2", 22, 17, 15, NULL},
			{"a3", 33, 13, 11, NULL},
			{"a4", 37, 33, 30, NULL},
			{"a5", 41, 30, 23, NULL},
			{"a6", 43, 35, 3, NULL}
	};

	// Let apps insert and retract themselves
	// in the maps in a random fashion


	int t; for (t = 0; t < numberOfLaps; t++) {
		int k; for (k = 0; k < numberOfApps; k++) {
			if (t >= apps[k].delay) {
				int const u = (t - apps[k].delay) % apps[k].period;
				if (u == 0) {
				        char *key;
					int r = threadUnsafeCreateKey("test", &key);
					if (threadUnsafePut(key, apps[k].name) != PES_HASHMAP_OK) {
						return false;
					}
					apps[k].handle = key;
#if SELF_TEST_VERBOSE
					printf("%d   up: %s using %s\n", t, apps[k].name, key);
#endif
				}
				else if (u == apps[k].uptime) {
					char *key = apps[k].handle;
					if (clear(key) == 0) {
						return false;
					}
					apps[k].handle = NULL;
#if SELF_TEST_VERBOSE
					printf("%d down: %s dropping %s\n", t, apps[k].name, key);
#endif
				}
			}
		}

		// Check that all active apps can be retrieved
		// in the map, every now and then
		if ((t % 20) == 0) {
			int m; for (m = 0; m < numberOfApps; m++) {
				if (apps[m].handle != NULL) {
					if (strcmp(get(apps[m].handle), apps[m].name) != 0) {
						return false;
					}
				}
			}

			// check that the unused keys do not have active
			// map entries
			keyChain *p; for (p = usedKeys; p != NULL; p = (keyChain *)p->tail) {
				if (get(p->key) != NULL) {
					return false;
				}
			}
		}
	}

	// At this point the number of insertions into the map should
	// equal the number of apps
	if (getEntryCount() != numberOfApps) {
		return false;
	}

	return true;
}

#endif



int peiMap_createKeyAndPut(char const *prefix, void const *value, const void **result) {
	LOCK_WRLOCK(&map_lock);
	char *key;
	int r = threadUnsafeCreateKey(prefix, (const char **)&key);
	if (r != PES_HASHMAP_OK) {
	    LOCK_UNLOCK(&map_lock);
	                    *result = NULL;
	                    return r;
	}
	else {
	    int r = threadUnsafePut(key, value);
	    if (r != PES_HASHMAP_OK) {
		LOCK_UNLOCK(&map_lock);
		*result = NULL;
		return r;
	    }
	    else {
		LOCK_UNLOCK(&map_lock);
		*result = (char *)key;
		return PES_HASHMAP_OK;
	    }
	}
}


/*
 * Returns a key formed by the given prefix, a hyphen
 * and a decimal integer. The key is guaranteed to
 * be safe to use in a subsequent put operation. If
 * unsuccessful, NULL is returned.
 */
int threadUnsafeCreateKey(char const *prefix, const char **result) {
	if (usedKeys != NULL) {
		*result = usedKeys->key;
		keyChain *remainingKeys = (keyChain *)usedKeys->tail;
		free(usedKeys);
		usedKeys = remainingKeys;
		return PES_HASHMAP_OK;
	}
	else {
		keyNumber++;
		if (asprintf((char **)result, "%s-%d", prefix, keyNumber) == -1) {
			return PES_HASHMAP_OUT_OF_MEMORY;
		}
		else {
			return PES_HASHMAP_OK;
		}
	}
}



/*
 * Looks up a map entry for the given key. NULL is returned
 * if the key is not present.
 */
void *peiMap_get(char const *key) {
	LOCK_WRLOCK(&map_lock);
	struct hsearch_data * const map = getMap();
	if (map == NULL) {
		LOCK_UNLOCK(&map_lock);
		return NULL;
	}
	else {
		ENTRY wrappedKey = {.key = (char *)key};
		ENTRY *result;

		if (hsearch_r(wrappedKey, FIND, &result, map) == 0) {
			LOCK_UNLOCK(&map_lock);
			return NULL;
		}
		else if (result->data == NULL) {
			LOCK_UNLOCK(&map_lock);
			return NULL;
		}
		else {
			LOCK_UNLOCK(&map_lock);
			return result->data;
		}
	}
}

/*
 * Adds another entry to the map. It is assumed that
 * the key is not already in use. Returns true when successful.
 * An attempt to overwrite an entry is considered an error.
 */
int threadUnsafePut(char const *key, void const *value) {
	struct hsearch_data * const map = getMap();
	if (map == NULL) {
		return PES_HASHMAP_UNSPECIFIC_FAILURE;
	}

	ENTRY wrappedKeyValue = {.key = (char *)key, .data = (void *)value};
	ENTRY *result;

	if (hsearch_r(wrappedKeyValue, FIND, &result, map) == 0) {
		// no such key yet
	        if (entryCount == PES_HASHMAP_SIZE) {
	            return PES_HASHMAP_MAX_KEYS_EXCEEDED;
	        }
	        else if (hsearch_r(wrappedKeyValue, ENTER, &result, map) == 0) {
		        // entry could not be created
			return PES_HASHMAP_UNSPECIFIC_FAILURE;
		}
		else {
		        // entry created
			entryCount++;
			return PES_HASHMAP_OK;
		}
	}
	else if (result->data == NULL) {
	        // key exists and may be reused
		result->data = (void *)value;
		return PES_HASHMAP_OK;
	}
	else {
	        // key exists but is in use
		return PES_HASHMAP_OVERWRITE;
	}
}


/*
 * Clears the given key. The key is put on the reuse list.
 * The map entry is not removed, but it is set to null.
 */
int peiMap_clear(char const * key) {
	LOCK_WRLOCK(&map_lock);

	struct hsearch_data * const map = getMap();
	if (map == NULL) {
		LOCK_UNLOCK(&map_lock);
		return PES_HASHMAP_UNSPECIFIC_FAILURE;
	}
	else {
		ENTRY wrappedKey = {.key = (char *)key};
		ENTRY *result;

		if (hsearch_r(wrappedKey, FIND, &result, map) == 0) {
			// trying to clear non-existent entry
			LOCK_UNLOCK(&map_lock);
			return PES_HASHMAP_CLEAR_NONEXISTENT_ENTRY;
		}
		else if (result->data == NULL) {
			// trying to clear an entry that was already cleared
			LOCK_UNLOCK(&map_lock);
			return PES_HASHMAP_CLEAR_ENTRY_TWICE;
		}
		else {
			result->data = NULL;
			keyChain *newKeyChain = (keyChain *)malloc(sizeof(keyChain));
			newKeyChain->tail = usedKeys;
			newKeyChain->key = result->key;
			usedKeys = newKeyChain;
			LOCK_UNLOCK(&map_lock);
			return PES_HASHMAP_OK;
		}
	}
}

/*
 * Returns a pointer to the internal data structure
 * that implements the map. This function is not for
 * public use.
 */
struct hsearch_data *getMap() {
	if (! isMapInitialized) {
		if (hcreate_r(PES_HASHMAP_SIZE, &map) == 0) {
			return NULL;
		}
		isMapInitialized = true;
#if SELF_TEST
		if (selfTestHeavy() == 0) {
			return NULL;
		}
#endif
	}
	return &map;
}

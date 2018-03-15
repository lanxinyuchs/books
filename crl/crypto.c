#include <stdlib.h>
#include <pthread.h>
#include <openssl/evp.h>
#include <openssl/crypto.h>

/* This mutex list will be initialized in crypto_init and freed in
 * crypto_destroy.
 */
static pthread_mutex_t *locks = NULL;

static void 
locking_function(int mode, int index, const char *file, int line)
{
   if (mode & CRYPTO_LOCK) {
      if (pthread_mutex_lock(&locks[index]) != 0) {
	 /* FIX: exit? */
	 return;
      }
   } else {
      if (pthread_mutex_unlock(&locks[index]) != 0) {
	 /* FIX: exit? */
	 return;
      }
   }
}
 
static unsigned long 
id_function()
{
   return (unsigned long)pthread_self();
}

/* This function should be called before any OpenSSL functionality is
 * used and it should only be called once.
 *
 * The best option is probably to add the following to the
 * osemain.con file:
 *
 * START_OSE_HOOK1(crypto_init)
 */
void 
crypto_init()
{
   int i;
   int num_locks;

   num_locks = CRYPTO_num_locks();
   locks = malloc(num_locks * sizeof(pthread_mutex_t));

   if (!locks) {
      /* FIX: exit? */
      return;
   }

   for (i = 0; i < num_locks; ++i) {
      if (pthread_mutex_init(&locks[i], NULL) != 0) {
	 /* FIX: exit? */
	 return;
      }
   }

   CRYPTO_set_id_callback(id_function);
   CRYPTO_set_locking_callback(locking_function);
   OpenSSL_add_all_algorithms();
}

/* This function can be called before the application finishes and
 * should only be called once.
 */
void
crypto_destroy()
{
   int i;
   int num_locks;

   if (locks == NULL) {
      return;
   }

   CRYPTO_set_id_callback(NULL);
   CRYPTO_set_locking_callback(NULL);
   num_locks = CRYPTO_num_locks();
   
   for (i = 0;  i < num_locks; ++i) {
      (void)pthread_mutex_destroy(&locks[i]);
   }
   
   free(locks);
}

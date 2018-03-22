#include <string.h>
#include <openssl/bio.h>
#include <openssl/evp.h>
#include "ose_spi/checksum.h"

int 
ose_base64_encode(unsigned char *dest, const void *src, int length)
{
   BIO *b64 = NULL, *mem = NULL, *bio = NULL;
   char *edata = NULL;
   int num = 0;
   
   b64 = BIO_new(BIO_f_base64());
   
   if (NULL == b64) {
      goto fail;
   }

   BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
   
   mem = BIO_new(BIO_s_mem());
   
   if (NULL == mem) {
      goto fail;
   }
   
   bio = BIO_push(b64, mem);   
   
   if (BIO_write(bio, src, length) <= 0) {
      goto fail;
   }

   if (BIO_flush(bio) != 1) {
      goto fail;
   }

   num = BIO_get_mem_data(bio, &edata);
   memcpy(dest, edata, num);
   BIO_free_all(bio);
   return num;
   
  fail:
   
   if (b64) {
      BIO_free(b64);
   }

   if (mem) {
      BIO_free(mem);
   }

   return 0;	
}

int 
ose_base64_decode(void *dest, const unsigned char *src, int length)
{
   BIO *b64 = NULL, *mem = NULL, *bio = NULL;
   int num = 0;
   
   b64 = BIO_new(BIO_f_base64());
   
   if (NULL == b64) {
      goto fail;
   }

   BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
   mem = BIO_new_mem_buf((void *)src, length);	
   bio = BIO_push(b64, mem);
   num = BIO_read(bio, dest, length);

   if (num < 0) {
      goto fail;
   }

   BIO_free_all(bio);
   return num;

  fail:

   if (b64) {
      BIO_free(b64);
   }

   if (mem) {
      BIO_free(mem);
   }

   return 0;	
}

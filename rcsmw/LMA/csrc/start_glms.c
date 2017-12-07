#include <stdlib.h>
#include "glmsadpi/glms.h"
#include <pthread.h>

static void *glms_thread(void *data)
{
  (void)data;
  runGlms();

  return NULL;
}

void start_glms(void) 
{
  pthread_t tid;
  int ret;

  ret = pthread_create(&tid, NULL, glms_thread, NULL);
  if(ret != 0)
    abort();
}

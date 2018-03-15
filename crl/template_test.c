
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <time.h>

#define TRACEPOINT_DEFINE
#include "com_ericsson_my_provider.h"

static uint32_t testTraceMacros(void)
{
 
  uint32_t result = 0;
  char bus_send_data[10]= {0,1,2,3,4,5,6,7,8,9};
  char bus_rec_data[10]= {10,11,12,13,14,15,16,17,18,19};

  MY_PROVIDER_ENTER("testTraceMacros");
  MY_PROVIDER_ERROR("test of error macro");

  MY_PROVIDER_INFO("test of info macro");

  MY_PROVIDER_TRACE(1,"test of trace macro");
  MY_PROVIDER_TRACE(2,"test of trace macro");
  MY_PROVIDER_TRACE(3,"test of trace macro");
  MY_PROVIDER_TRACE(4,"test of trace macro");
  MY_PROVIDER_TRACE(5,"test of trace macro");
  MY_PROVIDER_TRACE(6,"test of trace macro");
  MY_PROVIDER_TRACE(7,"test of trace macro");
  MY_PROVIDER_TRACE(8,"test of trace macro");
  MY_PROVIDER_TRACE(9,"test of trace macro");

  MY_PROVIDER_TRACE_STATE("test of state macro");

  MY_PROVIDER_BUS_SEND("test of bus send macro",bus_send_data , 10);
  MY_PROVIDER_BUS_RECEIVE("test of bus recieve macro",bus_rec_data , 10);
  MY_PROVIDER_PARAM("test of param macro"); 
  MY_PROVIDER_RETURN result;

}


int main(int argc, char **argv)
{
  if ( testTraceMacros() == 0)
    fprintf(stderr, "Test of my_provider template traces and macors end\n");
  else
    fprintf(stderr, "Something went wrong in the testing of my_provider template\n");


	return 0;
}

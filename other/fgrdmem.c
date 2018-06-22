/* ---------------------------------------------------------------------------
 *
 * @ Ericsson AB 2017 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */

#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <uio_helper.h>


#define FPGA_UIO_DEV_NAME "fpga_dbg"
#define FPGA_MEM_LEN_MAX 0xA00000
#define FPGA_MEM_TOTAL_SIZE 128*1024*1024

#define AC_UIO_DEV_NAME "ac_mem"
#define AC_MEM_LEN_MAX 0x800000
#define AC_MEM_TOTAL_SIZE 8*1024*1024

void usage(void)
{
	printf("\n/*****************************************************************************************************\n");
	printf("usage: ./fgrdmem  [ac/fg]  [offset]  [length] \n");
	printf("For AC read,the max length is 8M one time,total length is 8M\n");
	printf("For fpga read,the mac length is 10M one time,total length is 128M\n");
	printf("Please mind the boundary,if offset plus length beyond the total size,the invalid addr will be cut off\n");
	printf("/*****************************************************************************************************\n");
	exit(0);
}

int main(int argc, char **argv)
{
	void * _uio_handle = NULL;
	void * _mmap_base = NULL;
	unsigned int baseAddressU32;
	char devname[64];
	unsigned int offset = 0;
	unsigned int read_len = 1;
	unsigned int i = 0;
	unsigned int max_len;
	unsigned int total_size,cut_size = 0;
	volatile unsigned int *p = NULL;

	if(argc < 2 )
	{
		printf("the arguments is wrong \n");
		usage();
	}

	memset(devname,0,sizeof(devname));

	if( !strncmp( argv[1], "ac", 2 ))
	{
		strcpy(devname,AC_UIO_DEV_NAME);
		max_len = AC_MEM_LEN_MAX;
		total_size = AC_MEM_TOTAL_SIZE;
		printf("For AC Read Mapped Memory to debug ...\n");
	}
	else if( !strncmp( argv[1], "fg", 2 ))
	{
		strcpy(devname,FPGA_UIO_DEV_NAME);
		max_len = FPGA_MEM_LEN_MAX;
		total_size = FPGA_MEM_TOTAL_SIZE;
		printf("For FPGA Read Mapped Memory to debug ...\n");
	}
	else
		usage();
	printf("total size is 0x%x( %dM )\n",total_size,total_size/(1024*1024));

	_uio_handle = (void *)uio_open(devname);


	if (_uio_handle == (UIO_HANDLE_) - 1)
	{
		printf("uio_open funciton return error\n");
		return 0;
	}
	_mmap_base = uio_mmap(_uio_handle);
	if (_mmap_base == MAP_FAILED)
	{
		_mmap_base = NULL;
		printf("uio_mmap funciton return error\n");
		uio_close(_uio_handle);
		return 0;
	}

	baseAddressU32  = (unsigned int)_mmap_base;

	if(argv[2] != NULL)
	{
		offset = strtoul(argv[2], NULL, 16);

		if(argv[3] != NULL)
		{
			printf("Start to read from offset 0x%x, ", offset);
			read_len = strtoul(argv[3], NULL, 16) ;
			printf("Read length 0x%x \n\n", read_len);
		}
		else
		{
			printf("Read from offset 0x%x ,default length 4 byte\n", offset);
		}
	}
	else
	{
		printf("Read from the start,offset 0x%x ,default length 4 byte\n", offset);
	}

	printf("baseAddressU32 : 0x%x ,offset :0x%x \n",baseAddressU32,offset);
	printf("read offset to: 0x%x \n",baseAddressU32 + offset );

	if(read_len <= max_len)
	{
		if( total_size < offset + read_len ){
			cut_size = offset + read_len - total_size;
			read_len -= cut_size;
			printf("beyond the total size %d ,read length is be cut off to 0x%x\n",total_size/(1024*1024),read_len);
		}
		i = 0;
		for(p =(unsigned int *)(baseAddressU32 +offset) ; p < (unsigned int *)(baseAddressU32 + offset + read_len); p++)
		{
			if(!(i%4)){
				printf("\n");
				printf("%p : ",p);
			}
			printf("0x%08x ",*p);
			i++;
		}
	}
	else
	{
		printf("The length is over boundary and MAX value is 0x%x.\n ", max_len);
	}

	printf("\n\nRead Done!\n");

	uio_close(_uio_handle);
	return 0;
}


/**
 * Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 * information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson, the receiver
 * of this document shall keep the information contained herein
 * confidential and shall protect the same in whole or in part from
 * disclosure and dissemination to third parties. Disclosure and
 * disseminations to the receiver's employees shall only be made on
 * a strict need to know basis.
 */

#ifndef _DEBUG_PRINTF_H_
#define _DEBUG_PRINTF_H_
#ifdef __cplusplus
extern "C" {
#endif

#ifdef  DEBUG
#    define HERE  printf("Here: %s;%d\n",__func__,__LINE__)
#    define FUNC  printf("Func: %s;%d\n",__func__,__LINE__)
#    define DBGF(format, ...) printf("Debug %s;%d: " format "\n",    \
                                     __func__,__LINE__, __VA_ARGS__)
#    define DBG(text) printf("Debug %s;%d: %s\n",       \
                             __func__,__LINE__, text)
#undef syslog
#define syslog(type, ...)                            \
	do {                                            \
		printf("syslog: "#type " " __VA_ARGS__);       \
		printf("\n");                           \
	} while(0)
#else
#    define HERE
#    define FUNC
#    define DBGF(format, ...)
#    define DBG(text)
#endif

#ifdef __cplusplus
}
#endif
#endif /* _DEBUG_PRINTF_H_ */

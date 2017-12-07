#ifndef ComtEComm_1_h
#define ComtEComm_1_h

#include <netdb.h>
#include <MafMgmtSpiCommon.h>

typedef struct comte_buff {
	uint32_t size;
	uint8_t* buff;
} comte_buff_t;

typedef struct comte_con_handle {
	int sockfd;
	int quiet;
} comte_con_handle_t;


MafReturnT comte_connect(char* host, char *port, comte_con_handle_t* con_handle);
MafReturnT comte_send(comte_buff_t* buff, comte_con_handle_t* con_handle);
MafReturnT comte_recv(comte_buff_t* buff, comte_con_handle_t* con_handle);
void comte_recv_close(comte_con_handle_t* con_handle);

MafReturnT comte_send_recv(comte_buff_t* buff, comte_con_handle_t* con_handle);
MafReturnT comte_disconnect(comte_con_handle_t* con_handle);
MafReturnT comte_connect_send_recv
(char *hostname, char *portno, comte_buff_t* buff, int quiet);


MafReturnT comte_listen(char *addr, char *portno, comte_con_handle_t *listen_handle);
MafReturnT comte_accept(comte_con_handle_t* listen_handle,
		comte_con_handle_t* con_handle);

#define GENERIC_BERT_RPC(comteConfig, encodeFunction, decodeFunction) \
	comte_buff_t* buff = comte_malloc(sizeof(comte_buff_t)); \
	if (buff == NULL) return MafFailure; \
	MafReturnT res = encodeFunction; \
	if (res != MafOk) { \
		ERROR("encode failed!"); \
		comte_free(buff); \
		return res; \
	} \
	res = comte_connect_send_recv(comteConfig->comte_ip, comteConfig->comte_port, buff, 0); \
	if (res != MafOk) { \
		ERROR("comte_rl_send_recv failed!"); \
		if (buff->buff != NULL) comte_free(buff->buff);	\
		comte_free(buff); \
		return res; \
	} \
	res = decodeFunction; \
	if (buff->buff != NULL) comte_free(buff->buff);	\
	comte_free(buff); \
	return res;

#endif

#ifndef ComtEUtils_1_h
#define ComtEUtils_1_h


#include <MafOamSpiManagedObject_3.h>
#include <MafMwSpiLog_1.h>
#include <MafMwSpiTrace_1.h>
#include <MafMgmtSpiCommon.h>
#include <MafMgmtSpiThreadContext_2.h>

// Forward declare for use in Oam and Component
typedef struct comte_config comte_config_t;

#include "ComtEComm_1.h"
#include "ComOamSpiComtETrans_1.h"
#include "bert.h"

#define MW_SA_LOG_SEV_TRACE 8

#define comte_tree_find tfind
#define comte_tree_search tsearch
#define comte_tree_delete tdelete
#define comte_tree_destroy tdestroy

#define comte_malloc malloc
#define comte_calloc calloc
#define comte_realloc realloc
#define comte_free free


#define catom create_atom
#define ctuple create_stuple
#define clist create_list
#define csint create_sint
#define clint create_lint
#define csbig create_sbig
#define cstring(s) create_binary(strlen(s),(uint8_t*)s)
#define cbool create_boolean
#define cbinary(len, bin) create_binary(len, (uint8_t*)bin)

#define COMTE_CALLBACK "comte_com_interface"


#define INFO(...) do { comte_log_printf(__FILE__, __func__, __LINE__, MW_SA_LOG_SEV_INFO, 1, __VA_ARGS__ ); } while(0)
#define WARNING(...) do { comte_log_printf(__FILE__, __func__, __LINE__, MW_SA_LOG_SEV_WARNING, 1, __VA_ARGS__ ); } while(0)
#define ERROR(...) do { printf(__VA_ARGS__ ); printf(" %s\n",__func__); comte_log_printf(__FILE__, __func__, __LINE__, MW_SA_LOG_SEV_ERROR, 1, __VA_ARGS__ ); } while(0)

#define TRACE(group) do comte_trace_wrapper_printf(__FILE__, __func__, __LINE__, (group), 0); while(0)
#define TRACE_ARGS(group, ...) do comte_trace_wrapper_printf(__FILE__, __func__, __LINE__, (group), 1, __VA_ARGS__ ); while(0)
#define ENTER() do comte_trace_wrapper_printf(__FILE__, __func__, __LINE__, 9, 2); while(0)
#define ENTER_ARGS(...)	do comte_trace_wrapper_printf(__FILE__, __func__, __LINE__, 9, 3, __VA_ARGS__ ); while(0)
#define LEAVE() do comte_trace_wrapper_printf(__FILE__, __func__, __LINE__, 10, 4); while(0)
#define LEAVE_ARGS(exit_code, ...)	do comte_trace_wrapper_printf(__FILE__, __func__, __LINE__, 10, 5, (exit_code), __VA_ARGS__ ); while(0)



typedef struct log_prio_ {
    uint16_t facility;
    uint16_t severity;
} log_prio_t;


typedef struct comte_log_priorities_ {
    size_t prio_count;
    log_prio_t *prio_arr;
} comte_log_priorities_t;

typedef struct comte_enabled_oam_comps_ {
    short alarm_producer;
    short cm_event_producer;
    short pm_event_producer;
} comte_enabled_oam_comps_t;


// Config types and functions
struct comte_config {
    char *comte_port;
    char *comte_ip;
    char *com_port;
    char *com_ip;
    uint8_t start_mw;
    comte_enabled_oam_comps_t *comte_enabled_oam_comps;
    char *com_log_dir;
    uint32_t com_log_size;
    uint32_t com_log_rotation;
    uint8_t comte_logging;
    MwSpiSeverityT comte_logging_level;
    uint8_t com_audit_logging;
    MwSpiSeverityT com_audit_logging_level;
    MwSpiSeverityT log_severity_level;
    comte_log_priorities_t *comte_log_priorities;
};

void comte_cond_free(void *ptr);

void log_comte_config(comte_config_t *config);
void comte_parse_config(const char *configString, comte_config_t *config);
void comte_destroy_config(comte_config_t *config);
int is_forced_facility(uint16_t fac, uint16_t sev, comte_log_priorities_t* lf);


// Handle erlang client connections
comte_client_con_t* find_client_con(comte_trans_t* con, MafOamSpiTransactionHandleT txHandle);
comte_client_con_t* remove_client_con(comte_client_con_t* con, MafOamSpiTransactionHandleT txHandle);

// Encode and decode bert rpc calls
MafReturnT comte_add_nbi_bert_messages(bert_term_t* msgs);
MafReturnT decode_rpc(comte_buff_t *buff, bert_term_t **result);
MafReturnT decode_ok_response(comte_buff_t *buff);
MafReturnT encode_rpc(char* function, bert_term_t* args, comte_buff_t* buff);

// bert utility functions
uint32_t bert_list_length(bert_list_t* list);
char* copy_bert_binary(bert_binary_t* term);
char* copy_bert_atom(bert_term_t * bert_atom);

char * str_concat(char *str, ...);
uint32_t str_to_uint32(char *str, uint32_t default_val);


// log function
void comte_set_logger(MafMwSpiLog_1T* logger);
MafReturnT comte_log_printf(char *file, const char *func, const uint32_t line, MwSpiSeverityT sev, MwSpiFacilityT fac, const char *format, ...);
void comte_set_tracer(MafMwSpiTrace_1T* tracer);
MafReturnT comte_trace_wrapper_printf(const char *file, const char *func, const uint32_t line, uint32_t group, int type, ...);


// Thread Context NBI error message interface
void comte_set_thread_context(MafMgmtSpiThreadContext_2T* interface);
MafReturnT comte_add_nbi_message(const char *msg);

// memory list types and functions
typedef enum comte_mem_list_type {
	SPI, BERT, VOID
} comte_mem_list_type_t;

MafReturnT comte_mem_list_create(MafOamSpiTransactionHandleT id);
MafReturnT comte_mem_list_push(MafOamSpiTransactionHandleT id,
		comte_mem_list_type_t type, void* data);
MafReturnT comte_mem_list_free(MafOamSpiTransactionHandleT id);


#endif

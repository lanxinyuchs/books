#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <pthread.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include "ComtEUtils_1.h"


static MafMwSpiLog_1T* comte_logger = NULL;
static MafMwSpiTrace_1T* comte_tracer = NULL;
static MafMgmtSpiThreadContext_2T *comte_thread_context = NULL;


void comte_cond_free(void *ptr){
    if(ptr != NULL)
        comte_free(ptr);
    return;
}

static char * bool_str(short cfg){
    return ((cfg == 1) ? "true" : "false" );
}


void log_comte_config(comte_config_t *config){


    /* IP and ports */
    INFO("Socket(erl): %s:%s",config->comte_ip,config->comte_port);
    INFO("Socket(lib): %s:%s",config->com_ip,config->com_port);

    /* Lib setup */
    INFO("start_mw: %s", bool_str(config->start_mw));

    /* Producer config */
    INFO("Enable alarm_producer: %s",
         bool_str(config->comte_enabled_oam_comps->alarm_producer) );
    INFO("Enable cm_event_producer: %s",
         bool_str(config->comte_enabled_oam_comps->cm_event_producer) );
    INFO("Enable pm_event_producer: %s",
         bool_str(config->comte_enabled_oam_comps->pm_event_producer) );

    /* Logging */
    INFO("COM_LOGGING_LEVEL: %i", config->log_severity_level);
    INFO("com_log_dir: %s", config->com_log_dir);
    INFO("com_log_size: %i", config->com_log_size);
    INFO("com_log_rotation: %i", config->com_log_rotation);

    INFO("comte_logging: %s", bool_str(config->comte_logging));
    INFO("comte_logging_level: %i", config->comte_logging_level);
    INFO("com_audit_logging: %s", bool_str(config->com_audit_logging));
    INFO("com_audit_logging_level: %i", config->com_audit_logging_level);

    comte_log_priorities_t* lf =
        config->comte_log_priorities;
    int i;

    INFO("Log facility count: %zu", lf->prio_count);
    for(i = 0; i < lf->prio_count; i++){
        INFO("Log priority[f:s] (%d/%zu): [%d:%d]",
              i+1, lf->prio_count,
              lf->prio_arr[i].facility,
              lf->prio_arr[i].severity);
    }

}


static char * trim(char *c) {
    if(c){
        char * e = c + strlen(c) - 1;
        while(*c && isspace(*c)) c++;
        while(e > c && isspace(*e)) *e-- = '\0';
    }
    return c;
}

uint32_t str_to_uint32(char *str, uint32_t default_val){
    if(!str)
        return default_val;

    uint32_t val = strtoul(trim(str), (char**)NULL, 10);
    if(errno == ERANGE){
        val = default_val;
    }

    return val;
}

static uint32_t
get_config_int(const char *str, const char *needle, uint32_t default_val) {

    uint32_t val = default_val;
    int len = 0;
    char *tag = strstr(str, needle);

    if (!tag)
        return val;

    while(tag && tag[-1] != '>')
        tag++;
    while(tag && tag[len] != '<')
        len++;

    if(len > 0){
        char *str_int = strndup(tag, len);
        val = str_to_uint32(str_int, default_val);
        comte_free(str_int);
    }
    else
        val = default_val;


    return val;
}

static char*
get_config_str(const char *str, const char *needle, const char * default_val) {
	char *tag = strstr(str, needle);
	size_t len = 0;
	while(tag && tag[-1] != '>')
	    tag++;
	while (tag && tag[len] != '<')
	    len++;
	if (!tag)
	    return strdup(default_val);
	return strndup(tag, len);
}

static uint8_t
get_config_bool(const char *str, const char *needle, const uint8_t default_val) {
    char *tag = strstr(str, needle);
    while(tag && tag[-1] != '>')
    	    tag++;
    if (tag && tag[0] == 't')
	return (uint8_t)1;
    else if (tag && tag[0] == 'f')
    	return (uint8_t)0;
    else
	return default_val;
}

static int comp_fac(const void *a, const void *b) {
    log_prio_t *ap = (log_prio_t*)a;
    log_prio_t *bp = (log_prio_t*)b;
    return (ap->facility - bp->facility);
}

int is_forced_facility(uint16_t facility,
                       uint16_t severity,
                       comte_log_priorities_t* lp) {

    if(lp->prio_count == 0)
        return false;

    log_prio_t *res = bsearch(&facility, lp->prio_arr,
                              lp->prio_count,
                              sizeof(log_prio_t), comp_fac);
    if(res)
        return (severity <= res->severity);
    else
        return false;
}


static log_prio_t decode_log_prio(char *prio) {
    log_prio_t log_prio;
    char *tok = strsep(&prio, ":");

    if(prio){
        // Delimiter found
        log_prio.facility = (uint16_t)strtol(tok, (char**)NULL, 10);
        log_prio.severity = (uint16_t)strtol(prio, (char**)NULL, 10);
    } else{
        log_prio.facility = (uint16_t)strtol(tok, (char**)NULL, 10);
        log_prio.severity = MW_SA_LOG_SEV_INFO;
    }
    return log_prio;
}

static void * decode_log_priorities(char *str_list, size_t list_sz) {
    int i;


    comte_log_priorities_t *lp =
        comte_malloc(sizeof(comte_log_priorities_t));

    lp->prio_count = list_sz;
    lp->prio_arr = NULL;

    if(list_sz > 0){
        char *str_prios = strdup(str_list);
        char *strp = str_prios;

        lp->prio_arr = comte_malloc(sizeof(log_prio_t) * (list_sz));

        /* Convert to integers */
        const char *delim = ",";
        char *tok = strsep(&str_prios, delim);
        for(i = 0; i < list_sz; i++){
            lp->prio_arr[i] = decode_log_prio(tok);
            tok = strsep(&str_prios, delim);
        }
        comte_free(strp);
        /* Sort array */
        qsort(lp->prio_arr, lp->prio_count, sizeof(log_prio_t), comp_fac);
    }

    return lp;
}

static void * decode_enable_oam_comps(char *str_list, size_t list_sz) {
    comte_enabled_oam_comps_t *eoamc =
        comte_malloc(sizeof(comte_enabled_oam_comps_t));

    eoamc->alarm_producer = false;
    eoamc->cm_event_producer = false;
    eoamc->pm_event_producer = false;

    if(list_sz > 0){
        eoamc->alarm_producer =
            (strstr(str_list, "alarm_producer") != NULL) ? 1 : 0;

        eoamc->cm_event_producer =
            (strstr(str_list, "cm_event_producer") != NULL) ? 1 : 0;

	 eoamc->pm_event_producer =
            (strstr(str_list, "pm_event_producer") != NULL) ? 1 : 0;

    }
    return eoamc;
}


static size_t count_list_sz(char *str_list, const char *delim) {
    size_t list_sz = 0;
    char *str_count = strdup(str_list);
    char *strp = str_count;

    char *tok = strsep(&str_count, delim);
    while(tok && ((int)strlen(tok) > 0)){
        list_sz++;
        tok = strsep(&str_count, delim);
    }
    comte_free(strp);

    return list_sz;
}



static void *get_config_list
(const char *haystack, const char *needle, const char *default_val,
 void*(*decode)(char*, size_t) ) {

    char *tag = strstr(haystack, needle);
    char *str_list = NULL;
    size_t len = 0;

    /* Parse XML */
    if (!tag){
        str_list = strdup(default_val);
    } else {
        while(tag && tag[-1] != '>')
            tag++;
        while (tag && tag[len] != '<')
            len++;

        if(len > 0)
            str_list = trim(strndup(tag, len));

    }

    void *dec_result = NULL;

    /* Empty config */
    if(!str_list || (str_list && (int)strlen(str_list) == 0) ){
        dec_result = (*decode)(NULL, 0);
        return dec_result;
    }


    /* Count items */
    size_t list_sz = count_list_sz(str_list, ",");

    /* Decode items */
    dec_result = (*decode)(str_list, list_sz);
    comte_free(str_list);

    return dec_result;
}

MwSpiSeverityT max_sev_level(MwSpiSeverityT severity){
    return (severity > MW_SA_LOG_SEV_TRACE) ? MW_SA_LOG_SEV_TRACE : severity;
}

void comte_parse_config(const char *configString, comte_config_t *config) {
	config->start_mw = get_config_bool(configString, "com_start_mw", 0);
	config->comte_port = get_config_str(configString, "comte_port", "2002");
	config->comte_ip = get_config_str(configString, "comte_ip", "localhost");
	config->com_port = get_config_str(configString, "com_port", "2003");
	config->com_ip = get_config_str(configString, "com_ip", "localhost");

	config->com_log_dir =
		get_config_str(configString, "com_log_dir", ".");
	config->com_log_size =
		get_config_int(configString, "com_log_size", 1048576);
	config->com_log_rotation =
		get_config_int(configString, "com_log_rotation", 5);

	config->comte_logging =
		get_config_bool(configString, "comte_logging", 0);
	config->comte_logging_level =
		max_sev_level
		((MwSpiSeverityT) get_config_int
		 (configString, "comte_logging_level", MW_SA_LOG_SEV_ERROR));
	config->com_audit_logging =
		get_config_bool(configString, "com_audit_logging", 0);
	config->com_audit_logging_level =
		max_sev_level
		((MwSpiSeverityT) get_config_int
		 (configString, "com_audit_logging_level", MW_SA_LOG_SEV_INFO));

	config->comte_log_priorities =
		(comte_log_priorities_t*) get_config_list
		(configString,
		 "comte_log_force_facilities",
		 "100,101",
		 decode_log_priorities);

	config->comte_enabled_oam_comps =
		(comte_enabled_oam_comps_t*) get_config_list
		(configString,
		 "comte_enable_oam_components",
		 "alarm_producer,cm_event_producer",
		 decode_enable_oam_comps);

	char* severity_level = getenv("COM_LOGGING_LEVEL");
	if (!severity_level)
		config->log_severity_level = MW_SA_LOG_SEV_ERROR;
	else {
		MwSpiSeverityT sev_level =
			(MwSpiSeverityT) strtol(severity_level, (char**) NULL, 10);
		if (errno == ERANGE)
			config->log_severity_level = MW_SA_LOG_SEV_ERROR;
		else
			config->log_severity_level = max_sev_level(sev_level);
	}

	return;
}

void comte_destroy_config(comte_config_t *config){
    /* Free IPs */
    comte_free(config->com_ip);
    comte_free(config->comte_ip);

    /* Free enabled oam components */
    comte_free(config->comte_enabled_oam_comps);

    /* Free log priorities */
    if(config->comte_log_priorities->prio_count > 0)
        comte_free(config->comte_log_priorities->prio_arr);
    comte_free(config->comte_log_priorities);

}


MafReturnT encode_rpc(char* function, bert_term_t* args, comte_buff_t* buff) {

    if(!buff){
        return MafFailure;
    }

    bert_term_t* mfa_elems[] = { catom("call"), catom(COMTE_CALLBACK), catom(
            function), args };
    bert_term_t* mfa = ctuple(4, mfa_elems);
    bert_buff_t* bbuff = bert_encode(mfa);
	if (bbuff == NULL) {
		buff->buff = NULL;
		buff->size = 0;
		return MafFailure;
	}
    buff->buff = bbuff->buff;
    buff->size = bbuff->length;

    bert_free((bert_term_t*) mfa);
    comte_free(bbuff);
    return MafOk;
}

static int is_bad_response(bert_term_t* resp){

    /* Check if response was decoded properly */
    if (!resp) {
        return 1;
    }

    if (resp->type != BERT_SMALL_TUPLE) {
        ERROR("BERT rpc reply is not tuple!");
        return 1;
    }

    /* Check size of response tuple */
    /* {reply, <reply_term>} */
    if (resp->stuple.size != 2) {
        ERROR("BERT rpc reply tuple is not of size 2!");
        return 1;
    }

    return 0;
}

static MafReturnT get_error(bert_term_t* resp) {
	/* Check if the returned value is an error tuple */
	/* and if so dig out the error code and maybe the reason */

	bert_term_t* et = resp->stuple.values[1];
	/* Check if <reply_term> is _not_ a tuple or tuple_size =< 1 */
	if (et->type != BERT_SMALL_TUPLE
		|| et->stuple.size <= 1) return MafOk;

	/* Check if the tag is the atom 'error' */
	bert_term_t* tag = et->stuple.values[0];
	if (tag->type != BERT_ATOM
		|| strcmp(tag->atom, "error") != 0) return MafOk;

	/* {error, error_code()} */
	/* {error, error_code() ...} */

	MafReturnT ret_code = MafFailure;

	/* Set error code from element 2 */
	switch(et->stuple.values[1]->type) {
	case BERT_INTEGER: {
		ret_code = et->stuple.values[1]->lint;
		break;
	}
	case BERT_SMALL_INTEGER: {
		ret_code = et->stuple.values[1]->sint;
		break;
	}
	default:
		ERROR("Error code was not recognized");
	}
	ERROR("Set error code: %d", ret_code);

	/* If there is/are (an) error reason(s), element 3; */
	/* add message(s) to thread context. */
	if (et->stuple.size >= 3) {
		/* {error, error_code(),
		 * <<"error_reason">> | [<<"error_reasons">> ... ] ...} */
		MafReturnT ret_code_messages =
			comte_add_nbi_bert_messages(et->stuple.values[2]);
		if (ret_code == MafOk) ret_code = ret_code_messages;
	}

	return ret_code;
}


static MafReturnT add_nbi_bert_message(bert_term_t* term) {
	MafReturnT ret = MafFailure;

	if (term->type != BERT_BINARY) {
		ERROR("Not a binary");
	}
	else {
		char *str = comte_malloc(sizeof(char) * (term->binary.length + 1));
		memcpy(str, term->binary.value, term->binary.length);
		str[term->binary.length] = 0;
		//ERROR("BERT RPC failed: %s!", str);
		ret = comte_add_nbi_message(str);
		comte_free(str);
	}

	return ret;
}

MafReturnT comte_add_nbi_bert_messages(bert_term_t* msgs) {
	if (msgs->type == BERT_NIL) return MafOk;
	if (msgs->type == BERT_BINARY) return add_nbi_bert_message(msgs);
	if (msgs->type != BERT_LIST) {
		ERROR("Not a binary nor a list");
		return MafFailure;
	}

	bert_list_t* hd = msgs->list;
	MafReturnT ret_code = MafOk;
	while (hd != NULL) {
		MafReturnT ret_code_message = add_nbi_bert_message(hd->value);
		if (ret_code == MafOk) ret_code = ret_code_message;
		hd = hd->next;
	}

	return ret_code;
}


MafReturnT decode_rpc(comte_buff_t *buff, bert_term_t **result) {
    MafReturnT ret_code = MafFailure;
    bert_buff_t bbuff;

    bbuff.buff = buff->buff;
    bbuff.length = buff->size;

    bert_term_t* response = bert_decode(&bbuff);

    /* If we cant decode the response, set default error code
       and return NULL */
    if(is_bad_response(response)){
        ERROR("decode_rpc failed");
        bert_free(response);
        return ret_code;
    }

    /* Set the error code and publish potential
       errors to thread context */
    ret_code = get_error(response);

    /* Update outgoing result */
    *result = response->stuple.values[1];

    /* Make sure to free the wrapping tuple */
    /* {reply, <reply_term>} */
    response->stuple.values[1] = NULL;
    bert_free(response);

    return ret_code;
}

MafReturnT decode_ok_response(comte_buff_t *buff) {
    MafReturnT res = MafFailure;
    bert_term_t *response = NULL;

    res = decode_rpc(buff, &response);

    if(res == MafOk){
        /* Check for 'ok' returns */
        if(response->type == BERT_ATOM &&
           strcmp(response->atom, "ok") == 0){
            /* The atom ok, leave res as MafOk */
        }
        else {
            /* Not an atom neither the atom ok */
            ERROR("Expected 'ok' response, got type: %i",
                  response->type);
            res = MafFailure;
        }
    }

    bert_free(response);
    return res;
}


comte_client_con_t* find_client_con(comte_trans_t* trans,
		MafOamSpiTransactionHandleT txHandle) {
	pthread_rwlock_rdlock(&trans->cc_rwlock);
	comte_client_con_t* curr = trans->client;
	while (curr && curr->transId != txHandle)
		curr = curr->next_trans;
	pthread_rwlock_unlock(&trans->cc_rwlock);
	return curr;
}

comte_client_con_t* remove_client_con(comte_client_con_t* con,
		MafOamSpiTransactionHandleT txHandle) {

	if (con->transId == txHandle) {
		comte_client_con_t* next = con->next_trans;
		comte_free(con);
		return next;
	}

	comte_client_con_t* curr = con;
	while (curr->next_trans && curr->next_trans->transId != txHandle)
		curr = curr->next_trans;

	comte_client_con_t* tmp = curr->next_trans;

	curr->next_trans = tmp->next_trans;
	comte_free(tmp);

	return con;
}

uint32_t bert_list_length(bert_list_t* list) {
	uint32_t count = 0;

        if(!list->value){
            return count;
        }

	bert_list_t* curr = list;
        while(curr) {
		count++;
		curr = curr->next;
	}
        return count;
}

char* copy_bert_binary(bert_binary_t* bert_term){
    char* str_ptr =
        comte_malloc(sizeof(char) * (bert_term->length+1) );
    if(str_ptr){
        memcpy(str_ptr, bert_term->value, bert_term->length);
        str_ptr[bert_term->length] = 0;
        return str_ptr;
    }
    return NULL;
}

char* copy_bert_atom(bert_term_t * bert_atom) {
    int len = strlen(bert_atom->atom);
    char * str_ptr = comte_malloc(sizeof(char)*(len+1));
    if (str_ptr) {
        memcpy(str_ptr, bert_atom->atom, len);
        str_ptr[len] = 0;
        return str_ptr;
    }
    return NULL;
}

void comte_set_logger(MafMwSpiLog_1T* logger) {
	comte_logger = logger;
}


void comte_set_tracer(MafMwSpiTrace_1T* tracer) {
	comte_tracer = tracer;
}





char * str_concat(char *str, ...) {

    if(!str)
        return NULL;

    va_list arg_ptr;
    va_start(arg_ptr, str);

    char *result = strdup(str);

    int sz = strlen(str);
    char *next_str;
    while((next_str = va_arg(arg_ptr, char *)) != NULL){
        sz += strlen(next_str);
        result = (char *)(comte_realloc(result, sizeof(char) * (sz+1)) );
        strcat(result, next_str);
    }

    va_end(arg_ptr);

    return result;
}

MafReturnT comte_log_printf(char *file, const char *func, const uint32_t line,
                            MwSpiSeverityT sev, MwSpiFacilityT facility,
                            const char *format, ...) {
    MafReturnT res = MafFailure;

    if (!comte_logger)
        return MafOk;

    int tot_len = 1 + 58, len;// 58 is the length of the string before the actual log message, and the 1 is the null terminator
    va_list argptr;

    va_start(argptr,format);
    tot_len += vsnprintf(NULL, 0, format, argptr);
    va_end(argptr);

    char *str = comte_malloc(sizeof(char)*(tot_len));
    len = snprintf
        (str, 58+1, "%-24.24s:%-6lu- %-25.25s",
         file, (unsigned long) line, func);

    va_start(argptr,format);
    tot_len = vsnprintf(str+len, tot_len-len, format, argptr);
    va_end(argptr);

    res = comte_logger->logWrite(0, sev, facility, str);
    comte_free(str);

    return res;
}


static int comte_trace_snprintf
(char *str, size_t size,
 const char *file, const uint32_t line) {
	return snprintf
		(str, size, "%-24s:%-5u;", file, (unsigned)line);
}

static int comte_trace_snprintf_func
(char *str, size_t size,
 const char *file, const uint32_t line, const char *func) {
	return snprintf
		(str, size, "%-24s:%-5u - %-24s;", file, (unsigned)line, func);
}

MafReturnT comte_trace_wrapper_printf
(const char *file, const char *func, const uint32_t line,
 uint32_t group, int type, ...) {
	va_list argptr;
	uint32_t exitCode;
	const char *format;
	int hdr_len, msg_len;
	char *str, *msg;

    if (!comte_tracer) {
		fprintf
			(stderr, "Tracing with closed trace wrapper! (%s:%d - %s)\n",
			 file, (unsigned int)line, func);
		return MafOk;
	}


	switch (type) {

	case 0: /* TRACE(group) */
		hdr_len = comte_trace_snprintf_func(NULL, 0, file, line, func);
		str = comte_malloc(hdr_len + 1);
		comte_trace_snprintf_func(str, hdr_len+1, file, line, func);
		comte_tracer->traceWrite(group, str);
		comte_free(str);
		break;

	case 1: /* TRACE_ARGS(group, format, ...) */
		hdr_len = comte_trace_snprintf_func(NULL, 0, file, line, func);
		va_start(argptr, type);
		format = va_arg(argptr, const char *);
		msg_len = vsnprintf(NULL, 0, format, argptr);
		va_end(argptr);

		str = comte_malloc(hdr_len + msg_len + 1);
		comte_trace_snprintf_func(str, hdr_len+1, file, line, func);
		msg = str + hdr_len;

		va_start(argptr, type);
		va_arg(argptr, const char *);
		vsnprintf(msg, msg_len+1, format, argptr);
		va_end(argptr);

		comte_tracer->traceWrite(group, str);
		comte_free(str);
		break;

	case 2: /* ENTER() */
		hdr_len = comte_trace_snprintf(NULL, 0, file, line);
		str = comte_malloc(hdr_len + 4 + 1);
		comte_trace_snprintf(str, hdr_len+1, file, line);
		strcpy(str + hdr_len, " >> ");
		comte_tracer->traceWriteEnter(group, func, str);
		comte_free(str);
		break;

	case 3: /* ENTER_ARGS(format, ...) */
		hdr_len = comte_trace_snprintf(NULL, 0, file, line);
		va_start(argptr, type);
		format = va_arg(argptr, const char *);
		msg_len = vsnprintf(NULL, 0, format, argptr);
		va_end(argptr);

		str = comte_malloc(hdr_len + 4 + msg_len + 1);
		comte_trace_snprintf(str, hdr_len+1, file, line);
		strcpy(str + hdr_len, " >> ");
		msg = str + hdr_len + 4;

		va_start(argptr, type);
		va_arg(argptr, const char *);
		vsnprintf(msg, msg_len+1, format, argptr);
		va_end(argptr);

		comte_tracer->traceWriteEnter(group, func, str);
		comte_free(str);
		break;

	case 4: /* LEAVE() */
		hdr_len = comte_trace_snprintf(NULL, 0, file, line);
		str = comte_malloc(hdr_len + 4 + 1);
		comte_trace_snprintf(str, hdr_len+1, file, line);
		strcpy(str + hdr_len, " << ");
		comte_tracer->traceWriteExit(group, func, 0, str);
		comte_free(str);
		break;

	case 5: /* LEAVE_ARGS(exit_code, format, ...) */
		hdr_len = comte_trace_snprintf(NULL, 0, file, line);

		va_start(argptr, type);
		va_arg(argptr, uint32_t);
		format = va_arg(argptr, const char *);
		msg_len = vsnprintf(NULL, 0, format, argptr);
		va_end(argptr);

		str = comte_malloc(hdr_len + 4 + msg_len + 1);
		comte_trace_snprintf(str, hdr_len+1, file, line);
		strcpy(str + hdr_len, " << ");
		msg = str + hdr_len + 4;

		va_start(argptr, type);
		exitCode = va_arg(argptr, uint32_t);
		va_arg(argptr, const char *);
		vsnprintf(msg, msg_len+1, format, argptr);
		va_end(argptr);

		comte_tracer->traceWriteExit(group, func, exitCode, str);
		comte_free(str);
		break;
	}

	return MafOk;
}


void comte_set_thread_context(MafMgmtSpiThreadContext_2T* interface) {
    comte_thread_context = interface;
}
MafReturnT comte_add_nbi_message(const char *msg) {
    MafReturnT ret =
	comte_thread_context->addMessage(ThreadContextMsgNbi_2, msg);
    if (ret != MafOk) {
	ERROR("addMessage failed: %d [%s]", (int) ret, msg);
    }
    return ret;
}

/**
 * Memory list functions.
 * Memory lists are used to keep track of memory allocated during a transaction
 * which needs to me free'd when the transaction finishes.
 **/
typedef struct comte_mem_list_item comte_mem_list_item_t;

struct comte_mem_list_item {
	comte_mem_list_item_t *next;
	comte_mem_list_type_t type;
	void *data;
};

typedef struct comte_mem_list comte_mem_list_t;
struct comte_mem_list {
	MafOamSpiTransactionHandleT id;
	comte_mem_list_t *next;

	pthread_mutex_t mutex;
	comte_mem_list_item_t *head;
};

static pthread_mutex_t mem_list_mutex = PTHREAD_MUTEX_INITIALIZER;
comte_mem_list_t *mem_list = NULL;

static comte_mem_list_t *create_mem_list(MafOamSpiTransactionHandleT id) {
	comte_mem_list_t *new_mem_list = comte_malloc(sizeof(comte_mem_list_t));
	new_mem_list->id = id;
	new_mem_list->next = NULL;
	new_mem_list->head = NULL;
	pthread_mutex_init(&new_mem_list->mutex, NULL);
	return new_mem_list;
}

static comte_mem_list_t *find_mem_list(MafOamSpiTransactionHandleT id) {
    comte_mem_list_t *curr = NULL;
    pthread_mutex_lock(&mem_list_mutex);
    curr = mem_list;
    while (curr) {
        if (curr->id == id) {
            break;
        }
        curr = curr->next;
    }
    pthread_mutex_unlock(&mem_list_mutex);
    return curr;
}

static void delete_mem_list(MafOamSpiTransactionHandleT id) {
	pthread_mutex_lock(&mem_list_mutex);
	comte_mem_list_t *curr = mem_list, *prev = NULL;
	while (curr) {
		if (curr->id == id && prev == NULL) {
			mem_list = curr->next;
			break;
		} else if (curr->id == id) {
			prev->next = curr->next;
			break;
		} else {
			prev = curr;
			curr = curr->next;
		}
	}

	comte_free(curr);
	pthread_mutex_unlock(&mem_list_mutex);
	return;
}

MafReturnT comte_mem_list_create(MafOamSpiTransactionHandleT id) {
	pthread_mutex_lock(&mem_list_mutex);
	if (!mem_list) {
		mem_list = create_mem_list(id);
	} else {
		comte_mem_list_t *curr = mem_list;
		do {
			if (curr->id == id) {
				pthread_mutex_unlock(&mem_list_mutex);
				ERROR("Transaction id already exists: %ld", id);
				return MafFailure;
			}
			if (curr->next)
				curr = curr->next;
			else {

				curr->next = create_mem_list(id);
				break;
			}
		} while (true);
	}
	pthread_mutex_unlock(&mem_list_mutex);
	return MafOk;
}

MafReturnT comte_mem_list_push(MafOamSpiTransactionHandleT id,
                               comte_mem_list_type_t type, void* data) {
	comte_mem_list_t *list = find_mem_list(id);
	if (!list) {
		ERROR("Could not find memory list for: %ld", id);
		return MafFailure;
	}
	pthread_mutex_lock(&list->mutex);

	comte_mem_list_item_t *new_item = comte_malloc(
			sizeof(comte_mem_list_item_t));
	new_item->data = data;
	new_item->next = list->head;
	new_item->type = type;
	list->head = new_item;

	pthread_mutex_unlock(&list->mutex);
	return MafOk;
}

MafReturnT comte_mem_list_free(MafOamSpiTransactionHandleT id) {
	comte_mem_list_t *list = find_mem_list(id);
	if (!list) {
		ERROR("Could not find memory list for: %ld", id);
		return MafFailure;
	}
	pthread_mutex_lock(&list->mutex);
	comte_mem_list_item_t *curr = list->head;
	while (curr) {
		switch (curr->type) {
		case SPI:
			destroy_avc((MafMoAttributeValueContainer_3T*) curr->data);
			break;
		case BERT:
			bert_free((bert_term_t*) curr->data);
			break;
		case VOID:
			comte_free(curr->data);
			break;
		}
		comte_mem_list_item_t *next = curr->next;
		comte_free(curr);
		curr = next;
	}
	list->head = NULL;
	pthread_mutex_destroy(&list->mutex);
	delete_mem_list(id);
	return MafOk;
}

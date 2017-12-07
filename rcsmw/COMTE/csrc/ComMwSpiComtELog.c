/**
 * @author Lukas Larsson
 * @created 2011-04-13
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <MafMwSpiServiceIdentities_1.h>
#include <errno.h>
#include <stdarg.h>
#include <time.h>

#include "ComMwSpiComtEComponent_1.h"
#include "ComMwSpiComtEConverter_1.h"
#include "ComMwSpiComtELog_1.h"
#include "ComtEUtils_1.h"

typedef struct comte_log_file {
    FILE* file;
    uint8_t postfix;
    uint32_t curr_size;
    uint16_t logpath_length;
    char* filename;
    uint32_t log_size;
    uint32_t log_rotation;
    pthread_rwlock_t rwlock;
} comte_log_file_t;

/* Global */
comte_mw_component_t* log_component = NULL;
static comte_log_file_t* LOG_FILE = NULL;
static comte_log_file_t* ALARM_FILE = NULL;
static comte_log_file_t* TRACE_FILE = NULL;
static comte_log_file_t* AUDIT_FILE = NULL;


static MafReturnT do_wrap(comte_log_file_t* logfile) {
    pthread_rwlock_wrlock(&logfile->rwlock);

    // Print wrapping log msg
    int rv = 0;
    rv = fprintf(logfile->file, "*** WRAPPING to %d ***\r\n",
                 (logfile->postfix + 1) % logfile->log_rotation);
    /* Debug */
    if(rv < 0){
        if(fileno(logfile->file) < 0)
            printf("Invalid stream %s, %s", logfile->filename, strerror(errno));

        int ferr = ferror(logfile->file);
        printf("Error indicator: %d", ferr);
        return MafFailure;
    }

    // Close old logfile
    if (logfile->file != NULL)
	if (fclose(logfile->file) != 0) {
	    printf("Could not close %s, %s", logfile->filename,
		    strerror(errno));
	    return MafFailure;
	}

    logfile->postfix++;
    if (logfile->postfix >= logfile->log_rotation * 4) // Wrap before uint8_t wraps
	logfile->postfix /= logfile->log_rotation;

    // Remove next logfile
    sprintf(logfile->filename + logfile->logpath_length, ".%d",
	    (logfile->postfix + 1) % logfile->log_rotation);
    remove(logfile->filename);

    // Open new logfile
    sprintf(logfile->filename + logfile->logpath_length, ".%d",
	    logfile->postfix % logfile->log_rotation);
    if ((logfile->file = fopen(logfile->filename, "w+")) == NULL) {
	printf("Could not close %s, %s", logfile->filename, strerror(errno));
	return MafFailure;
    }
    logfile->curr_size = 0;

    pthread_rwlock_unlock(&logfile->rwlock);

    return MafOk;
}

static MafReturnT do_logWrite(comte_log_file_t* logfile, const char *format, ...) {

    int rv;

    /**
     * Take locks as these calls can be done in parallel. Therefore if a wrap is
     * happening as a log entry comes in vfprintf would acces logfile->file when
     * it has been closed in another thread.
     */
    pthread_rwlock_rdlock(&logfile->rwlock);

    /* Log file has either not been opened yet, or it has already been closed. */
    if (logfile == NULL || logfile->file == NULL) {
	return MafOk;
    }

    va_list argptr;
    va_start(argptr,format);

    do {
	rv = vfprintf(logfile->file, format, argptr);
    }while (rv < 0 && errno == EINTR);

    va_end(argptr);

    if (rv < 0) {
	pthread_rwlock_unlock(&logfile->rwlock);
	return MafFailure;
    }

    logfile->curr_size+=rv;

    do {
	rv = fflush(logfile->file);
    }while (rv < 0 && errno == EINTR);

    pthread_rwlock_unlock(&logfile->rwlock);

    /* Debug */
    if(rv < 0){
        if(fileno(logfile->file) < 0)
            printf("Invalid stream %s, %s", logfile->filename, strerror(errno));

        int ferr = ferror(logfile->file);
        printf("Error indicator: %d", ferr);
        return MafFailure;
    }

    if (logfile->curr_size > logfile->log_size) {
	if (do_wrap(logfile) == MafFailure)
            return MafFailure;
    }

    if (rv < 0) {
	return MafFailure;
    }
    return MafOk;
}

static MafReturnT logWriteTime
(comte_log_file_t* logfile, uint32_t eventId, MwSpiSeverityT severity,
 MwSpiFacilityT facility, const char* databuffer) {
    char tstr[20];
    struct timeval tv;
    struct tm lt;

    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &lt);
    strftime(tstr, 20, "%Y-%m-%d %H:%M:%S", &lt);

    return do_logWrite
		(logfile, "%d: %s.%06lu [%hd] %hd %s\n",
		 eventId, tstr, (unsigned long) tv.tv_usec,
		 severity, facility, databuffer);
}


static MafReturnT logWrite
(uint32_t eventId, MwSpiSeverityT severity,
 MwSpiFacilityT facility, const char* databuffer) {
	MafReturnT res_file, res_cb;
	comte_buff_t *buff;

	/* Log to file */
	switch (facility) {
	case 13: /* log audit */
		if (log_component->config->com_audit_logging &&
			log_component->config->com_audit_logging_level >= severity) {
			res_file = logWriteTime
				(AUDIT_FILE, eventId, severity, facility, databuffer);
		}
		goto logWriteTime;

	case 100: /* alarm log record */
	case 101: { /* alert log record */
		res_file = do_logWrite(ALARM_FILE, "%s\n", databuffer);
		/* Fall through */
	}

	default: {
		logWriteTime:
		// When terminating log_component has been freed here. Not much to do about.
		if (severity > log_component->config->log_severity_level) {
			res_file = MafOk;
		}
		else {
			res_file = logWriteTime
				(LOG_FILE, eventId, severity, facility, databuffer);
		}
	}
	}

	bool forced_facility =
		is_forced_facility(facility, severity,
						   log_component->config->comte_log_priorities);

	if (! log_component->config->comte_logging
		|| (!forced_facility &&
			severity > log_component->config->comte_logging_level)) {
		return res_file;
	}

	/* Log to Erlang callback */
	buff = comte_malloc(sizeof(comte_buff_t));
	res_cb = encode_logWrite(eventId, severity, facility, databuffer, buff);
	if (res_cb != MafOk) {
		comte_free(buff);
		return res_cb;
	}
	res_cb = comte_connect_send_recv
		(log_component->config->comte_ip,
		 log_component->config->comte_port,
		 buff, !0);
	if (res_cb != MafOk) {
		comte_free(buff);
		return res_cb;
	}
	res_cb = decode_logWrite(buff);
	comte_free(buff->buff);
	comte_free(buff);
	if (res_cb != MafOk) return res_cb;

	return res_file;
}

static MafReturnT close_log(comte_log_file_t **logfile) {
    comte_log_file_t* tmp;

    tmp = *logfile;
    *logfile = NULL;
    comte_free(tmp->filename);
    pthread_rwlock_destroy(&tmp->rwlock);
    fclose(tmp->file);
    comte_free(tmp);

    return MafOk;
}

static comte_log_file_t *open_log
(char *log_path, uint32_t log_size, uint32_t log_rotation) {

    comte_log_file_t* file = comte_malloc(sizeof(comte_log_file_t));
    file->logpath_length = strlen(log_path);
    file->log_size = log_size;
    file->log_rotation = log_rotation;

    int i, latest_id = 0;
    time_t latest_mtime = 0;
    struct stat *file_stat = comte_calloc(1, sizeof(struct stat) * file->log_rotation);
    char **filename = comte_malloc(sizeof(char*) * file->log_rotation);

    // Check which file has been modified last
    for (i = log_rotation; i < (log_rotation * 2); i++) {
	filename[i % log_rotation] = comte_malloc(
		sizeof(char) * (file->logpath_length + 3));
	sprintf(filename[i % log_rotation], "%s.%d", log_path, i % log_rotation);
	if (stat(filename[i % log_rotation], file_stat + (i % log_rotation)) == 0) {
	    if (file_stat[i % log_rotation].st_mtim.tv_sec > latest_mtime) {
		latest_mtime = file_stat[i % log_rotation].st_mtim.tv_sec;
		latest_id = i;
	    }
	}
    }

    // Check if the file found is larger than LOG_SIZE
    if (file_stat + latest_id % log_rotation != NULL) {
	if (file_stat[latest_id % log_rotation].st_size > log_size) {
	    // If latest accesses file is too big we remove the next
	    // file and start writing there.
	    latest_id++;
	    if (remove(filename[latest_id % log_rotation]) != 0
		    && errno != ENOENT)
		return NULL;
	    file->curr_size = 0;
	} else
	    file->curr_size = file_stat[latest_id % log_rotation].st_size;
    } else
	file->curr_size = 0;

    file->postfix = latest_id;

    // Remove next log file
    if (remove(filename[(latest_id + 1) % log_rotation]) != 0
	    && errno != ENOENT) {
	ERROR("Error: %s", strerror(errno));
	return NULL;
    }

    // Open the current logfile
    if ((file->file = fopen(filename[latest_id % log_rotation], "a+")) == NULL)
	return NULL;
    file->filename = filename[latest_id % log_rotation];
    comte_free(file_stat);

    for (i = 0; i < log_rotation; i++) {
	if (i != latest_id % log_rotation
	) // Skip if this is the choosen filename
	    comte_free(filename[i]);
    }

    /* Cleanup */
    comte_free(filename);
    comte_free(log_path);

    pthread_rwlock_init(&file->rwlock, NULL);

    return file;
}

static void traceWrite(uint32_t group, const char* traceMessageStr) {
    if (log_component->config->log_severity_level <= MW_SA_LOG_SEV_INFO
    )
    return;

    char tstr[20];
    struct timeval tv;
    struct tm lt;

    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &lt);
    strftime(tstr, 20, "%Y-%m-%d %H:%M:%S", &lt);

    do_logWrite
    (TRACE_FILE, "%s.%.06lu TRACE [%3u  %-24s       ] %s\n",
     tstr, (unsigned long) tv.tv_usec, group, "", traceMessageStr);
}

static void traceWriteEnter(uint32_t group, const char* funcName,
    const char* traceMessageStr) {
    if (log_component->config->log_severity_level <= MW_SA_LOG_SEV_INFO
    )
    return;

    char tstr[20];
    struct timeval tv;
    struct tm lt;

    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &lt);
    strftime(tstr, 20, "%Y-%m-%d %H:%M:%S", &lt);
    do_logWrite
    (TRACE_FILE, "%s.%.06lu ENTER [%3u: %-24s       ] %s\n",
     tstr, (unsigned long) tv.tv_usec, group, funcName, traceMessageStr);
}

static void traceWriteExit(uint32_t group, const char* funcName,
    uint32_t exitCode, const char* traceMessageStr) {
    if (log_component->config->log_severity_level <= MW_SA_LOG_SEV_INFO
    )
    return;

    char tstr[20];
    struct timeval tv;
    struct tm lt;

    gettimeofday(&tv, NULL);
    localtime_r(&tv.tv_sec, &lt);
    strftime(tstr, 20, "%Y-%m-%d %H:%M:%S", &lt);
    do_logWrite
    (TRACE_FILE, "%s.%.06lu EXIT  [%3u: %-24s => %-3u] %s\n",
     tstr, (unsigned long) tv.tv_usec, group, funcName, exitCode, traceMessageStr);
}

#undef FILE_LINE
#undef FILE_LINE_1
#undef FILE_LINE_2
#undef FILE_LINE_3
#define FILE_LINE_3(string) string
#define FILE_LINE_2(file, line, func, string) \
    FILE_LINE_3(file ":" #line " - " #func "(); " string)
#define FILE_LINE_1(file, line, func, string) \
    FILE_LINE_2(file, line, func, string)
#define FILE_LINE(func, string) \
    FILE_LINE_1(__FILE__, __LINE__, func, string)



static char * get_log_path(char *dir, char *log_basename){
    if(!dir)
        return str_concat("./", log_basename, NULL);
    else
        return str_concat(dir, "/", log_basename, NULL);
}

MafReturnT comte_log_create(comte_mw_component_t* comp) {
    MafReturnT res = MafOk;

    log_component = comp;
    log_component->log = comte_malloc(sizeof(comte_log_t));
    comte_log_t* log = log_component->log;

    log->base.base.componentName = MW_COMPONENT_NAME;
    log->base.base.interfaceName = MafMwSpiLog_1Id.interfaceName;
    log->base.base.interfaceVersion = MafMwSpiLog_1Id.interfaceVersion;

    log->base.logWrite = logWrite;

    log_component->trace = comte_malloc(sizeof(comte_trace_t));
    comte_trace_t* trace = log_component->trace;

    trace->base.base.componentName = MW_COMPONENT_NAME;
    trace->base.base.interfaceName = MafMwSpiTrace_1Id.interfaceName;
    trace->base.base.interfaceVersion = MafMwSpiTrace_1Id.interfaceVersion;

    trace->base.traceWrite = traceWrite;
    trace->base.traceWriteEnter = traceWriteEnter;
    trace->base.traceWriteExit = traceWriteExit;


    char *log_dir = log_component->config->com_log_dir;

    /* Default COM log */
    LOG_FILE = open_log
		(get_log_path(log_dir, "com.log"),
		 log_component->config->com_log_size,
		 log_component->config->com_log_rotation);
	logWriteTime
		(LOG_FILE, 0, MW_SA_LOG_SEV_TRACE, 5,
		 FILE_LINE(comte_log_create, "Opened LOG_FILE; First Entry."));

    /* Alarm log */
    ALARM_FILE = open_log
		(get_log_path(log_dir, "com_alarm.log"),
		 log_component->config->com_log_size,
		 log_component->config->com_log_rotation);
    do_logWrite
		(ALARM_FILE, "%s\n",
		 FILE_LINE(comte_log_create, "Opened ALARM_FILE; First Entry."));

    /* Trace log */
    TRACE_FILE = open_log
		(get_log_path(log_dir, "com_trace.log"),
		 log_component->config->com_log_size,
		 log_component->config->com_log_rotation);
    traceWrite
		(0,
		 FILE_LINE(comte_log_create, "Opened TRACE_FILE; First Entry."));

	if (log_component->config->com_audit_logging) {
		AUDIT_FILE = open_log
			(get_log_path(log_dir, "com_audit.log"),
			 log_component->config->com_log_size,
			 log_component->config->com_log_rotation);
		logWriteTime
			(AUDIT_FILE, 0, MW_SA_LOG_SEV_TRACE, 5,
			 FILE_LINE(comte_log_create, "Opened AUDIT_FILE; First Entry."));
	}

    if (LOG_FILE == NULL || ALARM_FILE == NULL || TRACE_FILE == NULL ||
		(log_component->config->com_audit_logging && AUDIT_FILE == NULL))
	return MafFailure;

    return res;
}

MafReturnT comte_log_destroy(comte_mw_component_t* comp) {
    MafReturnT res = MafOk;

    logWriteTime
		(LOG_FILE, 0, MW_SA_LOG_SEV_TRACE, 5,
		 FILE_LINE(comte_log_destroy, "closing LOG_FILE; Final Entry."));
    close_log(&LOG_FILE);

    do_logWrite
		(ALARM_FILE, "%s\n",
		 FILE_LINE(comte_log_destroy, "closing ALARM_FILE; Final Entry."));
    close_log(&ALARM_FILE);

    traceWrite
		(0,
		 FILE_LINE(comte_log_destory, "closing TRACE_FILE; Final Entry."));
    close_log(&TRACE_FILE);

	if (log_component->config->com_audit_logging) {
		logWriteTime
			(AUDIT_FILE, 0, MW_SA_LOG_SEV_TRACE, 5,
			 FILE_LINE(comte_log_create, "closing AUDIT_FILE; Final Entry."));
		close_log(&AUDIT_FILE);
	}

    comte_free(comp->log);
    comte_free(comp->trace);

    return res;
}

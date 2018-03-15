#ifndef _LMC_SERVER_H_
#define _LMC_SERVER_H_

#include <conn-establish-helper.h>
#include "xlfi.h"

#define LMC_SERVER_VERSIONS 2
#define LMC_SERVER_NAME     "lmc_server"

#define LMC_RESULT_SUCCESS            0
#define LMC_RESULT_INVALID_PARAM      1
#define LMC_RESULT_WRONG_STATE        2
#define LMC_RESULT_WRONG_CONFIG_DATA  3
#define LMC_RESULT_RESOURCE_SHORTAGE  4
#define LMC_RESULT_NOT_FOUND          5
#define LMC_RESULT_ACCESS_DENIED      6
#define LMC_RESULT_OTHER_ERROR        7
#define LMC_RESULT_WRONG_SEQ_NR       8

/* LMC state */
#define LMC_STATE_VALID        1
#define LMC_STATE_ERASED       2
#define LMC_STATE_ERPROG       3
#define LMC_STATE_WRPROG       4
#define LMC_STATE_ERROR        5
#define LMC_STATE_NON_EXISTENT 6

/* FIXME: use linked list and remove max subfiles */
#define LMC_TOO_MANY_SUBFILES     -20

#define LMC_MSG_BASE            0
#define LMC_CONN_ESTABLISH_REQ  (LMC_MSG_BASE + 0x1)
#define LMC_CONN_ESTABLISH_CFM  (LMC_MSG_BASE + 0x2)
#define LMC_CONN_ESTABLISH_REJ  (LMC_MSG_BASE + 0x3)
#define LMC_CONN_DISCONNECT_REQ (LMC_MSG_BASE + 0x4)
#define LMC_CONN_DISCONNECT_CFM (LMC_MSG_BASE + 0x5)
#define LMC_CONN_DISCONNECT_REJ (LMC_MSG_BASE + 0x6)
#define LMC_CONN_MONITOR_FWD    (LMC_MSG_BASE + 0x7)

#define LMC_GET_LOAD_FILE_INFO_REQ (LMC_MSG_BASE + 0x08)
#define LMC_GET_LOAD_FILE_INFO_CFM (LMC_MSG_BASE + 0x09)
#define LMC_GET_SUBFILE_INFO_REQ   (LMC_MSG_BASE + 0x0A)
#define LMC_GET_SUBFILE_INFO_CFM   (LMC_MSG_BASE + 0x0B)
#define LMC_GET_SUBFILE_INFO_REJ   (LMC_MSG_BASE + 0x0C)
#define LMC_LOAD_SUBFILE_OPEN_REQ  (LMC_MSG_BASE + 0x0D)
#define LMC_LOAD_SUBFILE_OPEN_CFM  (LMC_MSG_BASE + 0x0E)
#define LMC_LOAD_SUBFILE_OPEN_REJ  (LMC_MSG_BASE + 0x0F)
#define LMC_LOAD_SUBFILE_READ_REQ  (LMC_MSG_BASE + 0x10)
#define LMC_LOAD_SUBFILE_READ_CFM  (LMC_MSG_BASE + 0x11)
#define LMC_LOAD_SUBFILE_READ_REJ  (LMC_MSG_BASE + 0x12)
#define LMC_LOAD_SUBFILE_CLOSE_REQ  (LMC_MSG_BASE + 0x13)
#define LMC_LOAD_SUBFILE_CLOSE_CFM  (LMC_MSG_BASE + 0x14)
#define LMC_LOAD_SUBFILE_CLOSE_REJ  (LMC_MSG_BASE + 0x15)
#define LMC_LOAD_FILE_INIT_REQ      (LMC_MSG_BASE + 0x16)
#define LMC_LOAD_FILE_INIT_CFM      (LMC_MSG_BASE + 0x17)
#define LMC_LOAD_FILE_INIT_REJ      (LMC_MSG_BASE + 0x18)
#define LMC_LOAD_FILE_DATA_REQ      (LMC_MSG_BASE + 0x19)
#define LMC_LOAD_FILE_DATA_CFM      (LMC_MSG_BASE + 0x1A)
#define LMC_LOAD_FILE_DATA_REJ      (LMC_MSG_BASE + 0x1B)
#define LMC_LOAD_FILE_END_REQ       (LMC_MSG_BASE + 0x1C)
#define LMC_LOAD_FILE_END_CFM       (LMC_MSG_BASE + 0x1D)
#define LMC_LOAD_FILE_END_REJ       (LMC_MSG_BASE + 0x1E)
#define LMC_LOAD_FILE_DELETE_REQ    (LMC_MSG_BASE + 0x1F)
#define LMC_LOAD_FILE_DELETE_CFM    (LMC_MSG_BASE + 0x20)
#define LMC_LOAD_FILE_DELETE_REJ    (LMC_MSG_BASE + 0x21)
#define LMC_LOAD_FILE_DATA_IND      (LMC_MSG_BASE + 0x22)
#define LMC_LOAD_FILE_DELETE_IND    (LMC_MSG_BASE + 0x23)
#define LMC_READ_LOAD_FILE_DATA_REQ	    (LMC_MSG_BASE + 0x24)
#define LMC_READ_LOAD_FILE_DATA_CFM	    (LMC_MSG_BASE + 0x25)
#define LMC_READ_LOAD_FILE_DATA_REJ	    (LMC_MSG_BASE + 0x26)
#define LMC_LOAD_FILE_DATA_GET_SEQ_REQ      (LMC_MSG_BASE + 0x27)
#define LMC_LOAD_FILE_DATA_GET_SEQ_CFM      (LMC_MSG_BASE + 0x28)
#define LMC_LOAD_FILE_DATA_GET_SEQ_REJ      (LMC_MSG_BASE + 0x29)



#define LMC_CONN_ESTABLISH_MESSAGES_STRUCT(name)            \
	struct conn_establish_msg_numbers  name =       \
	{                                               \
		LMC_CONN_ESTABLISH_REQ,                 \
		LMC_CONN_ESTABLISH_CFM,                 \
		LMC_CONN_ESTABLISH_REJ,                 \
		LMC_CONN_DISCONNECT_REQ,                \
		LMC_CONN_DISCONNECT_CFM,                \
		LMC_CONN_DISCONNECT_REJ,                \
		LMC_CONN_MONITOR_FWD                    \
	}


#define LMC_SERVER_MESSAGES                                             \
	struct lmc_get_load_file_info_req 	get_load_file_info_req;  \
	struct lmc_get_load_file_info_cfm 	get_load_file_info_cfm;  \
	struct lmc_get_subfile_info_req   	get_subfile_info_req;    \
	struct lmc_get_subfile_info_cfm   	get_subfile_info_cfm;    \
	struct lmc_get_subfile_info_rej   	get_subfile_info_rej;    \
	struct lmc_load_subfile_open_req  	load_subfile_open_req;   \
	struct lmc_load_subfile_open_cfm  	load_subfile_open_cfm;   \
	struct lmc_load_subfile_open_rej  	load_subfile_open_rej;   \
	struct lmc_load_subfile_read_req  	load_subfile_read_req;   \
	struct lmc_load_subfile_read_cfm  	load_subfile_read_cfm;   \
	struct lmc_load_subfile_read_rej  	load_subfile_read_rej;   \
	struct lmc_load_subfile_close_req 	load_subfile_close_req;  \
	struct lmc_load_subfile_close_cfm 	load_subfile_close_cfm;  \
	struct lmc_load_subfile_close_rej 	load_subfile_close_rej;  \
	struct lmc_load_file_init_req     	load_file_init_req;      \
	struct lmc_load_file_init_cfm     	load_file_init_cfm;      \
	struct lmc_load_file_init_rej     	load_file_init_rej;      \
	struct lmc_load_file_data_req     	load_file_data_req;      \
	struct lmc_load_file_data_cfm     	load_file_data_cfm;      \
	struct lmc_load_file_data_rej     	load_file_data_rej;      \
	struct lmc_load_file_end_req      	load_file_end_req;       \
	struct lmc_load_file_end_cfm      	load_file_end_cfm;       \
	struct lmc_load_file_end_rej      	load_file_end_rej;       \
	struct lmc_load_file_delete_req   	load_file_delete_req;    \
	struct lmc_load_file_delete_cfm   	load_file_delete_cfm;    \
	struct lmc_load_file_delete_rej   	load_file_delete_rej;    \
	struct lmc_load_file_data_ind     	load_file_data_ind;      \
	struct lmc_load_file_delete_ind   	load_file_delete_ind;     \
	struct lmc_read_load_file_data_req     	read_load_file_data_req;      \
	struct lmc_read_load_file_data_cfm     	read_load_file_data_cfm;      \
	struct lmc_read_load_file_data_rej     	read_load_file_data_rej;      \
	struct lmc_load_file_data_get_seq_req   load_file_data_get_seq_req;   \
	struct lmc_load_file_data_get_seq_cfm   load_file_data_get_seq_cfm;   \
	struct lmc_load_file_data_get_seq_rej   load_file_data_get_seq_rej   \


/**
 * lmc_load_file_entry:permission bit mask
 */
#define LMC_LOAD_FILE_PERMISSION_UNLOCKED   0x1
#define LMC_LOAD_FILE_PERMISSION_LOCKED     0x2
#define LMC_LOAD_FILE_PERMISSION_RO         0x4

struct lmc_load_file_entry {
	uint32_t state;          /* LMC_STATE_VALID,  LMC_STATE_ERASED           */
	/* LMC_STATE_ERPROG, LMC_STATE_WRPROG           */
	/* LMC_STATE_ERROR,  LMC_STATE_NON_EXISTENT     */
	uint32_t is_current;     /* 1 if this is current loadfile 0 if not       */
	uint32_t slot_size;      /* Size of slot                                 */
	uint32_t lmc_size;       /* Size of the LMC                              */
	uint32_t seq_number;     /* Sequence number of loadfile                  */
	uint32_t file_type;      /* LMC_FILETYPE_AUBOOT                          */
	/* LMC_FILETYPE_AUAPPLIC                        */
	uint32_t time;           /* The time and tadet whe the LMC was built     */
	uint32_t subfile_counter; /* Number of sub-files in lmc                   */
	uint32_t working;        /* 1 if marked as working, 0 if not             */

	/** permission bit mask:
	 * @ref LMC_LOAD_FILE_PERMISSION_UNLOCKED
	 * @ref LMC_LOAD_FILE_PERMISSION_LOCKED
	 * @ref LMC_LOAD_FILE_PERMISSION_RO
	 */
	uint32_t permissions;         /* bit 0-1=1:unlocked slot, bit 0-1=2:locked slot, */
	                              /* bit 2=0:readwrite, bit2=1:readonly */
	char partition_name[32];
	char lmid[XLF_SUID_LEN + 2];
};

struct lmc_subfile_blob {
	uint32_t crc32;
	uint32_t header_size;
	uint32_t type;
	uint32_t major_version;
	uint32_t minor_version;
	char     lmid[XLF_SUID_LEN + 2];
	uint32_t time;
	char     name[XLF_BLOB_NAME_SIZE];
};

union lmc_subfile_info {
	uint32_t magic;
	struct {
		uint32_t magic;
		struct lmc_subfile_blob info;
	} blob;
	struct {
		uint32_t magic;
	} user;
};
/* Signal define */
#define BOARD_MSG_HEADER        \
	uint32_t msgno;         \
	uint32_t procedure_ref; \
	uint32_t connection_ref;

/**
 * lmc_get_load_file_info_req:flags bit mask for get load
 * file info of all partitions,
 * i.e. including pboot, boot and bootenv and board parameters.
 */
#define LMC_GET_LOAD_FILE_INFO_REQ_FLAGS_ALL  0x1


struct lmc_get_load_file_info_req {
	BOARD_MSG_HEADER
	/** Request flags:
	 * @ref LMC_GET_LOAD_FILE_INFO_REQ_FLAGS_ALL,
	 */
	uint32_t flags;
};

struct lmc_get_load_file_info_cfm {
	BOARD_MSG_HEADER
	uint32_t lmc_count;
	struct lmc_load_file_entry info[];
};

struct lmc_get_subfile_info_req {
	BOARD_MSG_HEADER
	uint32_t use_current;
	uint32_t index;
};

struct lmc_get_subfile_info_cfm {
	BOARD_MSG_HEADER
	int32_t result;
	uint32_t counter;
	union lmc_subfile_info list[];
};

struct lmc_get_subfile_info_rej {
	BOARD_MSG_HEADER
	int32_t error_code;
};

struct lmc_load_subfile_open_req {
	BOARD_MSG_HEADER
	uint32_t lmc_index;
	uint32_t lm_index;
};

struct lmc_load_subfile_open_cfm {
	BOARD_MSG_HEADER
	int32_t handle;
};

struct lmc_load_subfile_open_rej {
	BOARD_MSG_HEADER
	int32_t error_code;
};

struct lmc_load_subfile_read_req {
	BOARD_MSG_HEADER
	int32_t  handle;
	uint32_t pos;
	uint32_t size;
};

struct lmc_load_subfile_read_cfm {
	BOARD_MSG_HEADER
	int32_t nof_read_bytes;
	uint8_t  buf[];
};

struct lmc_load_subfile_read_rej {
	BOARD_MSG_HEADER
	int32_t error_code;
};

struct lmc_load_subfile_close_req {
	BOARD_MSG_HEADER
	int32_t handle;
};

struct lmc_load_subfile_close_cfm {
	BOARD_MSG_HEADER
};

struct lmc_load_subfile_close_rej {
	BOARD_MSG_HEADER
	int32_t error_code;
};

struct lmc_load_file_init_req {
	BOARD_MSG_HEADER
	char loadmodule[];
};

struct lmc_load_file_init_cfm {
	BOARD_MSG_HEADER
	uint32_t max_block_size;
};

struct lmc_load_file_init_rej {
	BOARD_MSG_HEADER
	uint32_t error_code;
};

struct lmc_load_file_data_req {
	BOARD_MSG_HEADER
	uint32_t lm_block_size;
	uint16_t lm_seq_nr;
	uint8_t  lm_block[];
};

struct lmc_load_file_data_cfm {
	BOARD_MSG_HEADER
};

struct lmc_load_file_data_rej {
	BOARD_MSG_HEADER
	uint32_t error_code;
};

struct lmc_load_file_end_req {
	BOARD_MSG_HEADER
};

struct lmc_load_file_end_cfm {
	BOARD_MSG_HEADER
	uint16_t result;
};

struct lmc_load_file_end_rej {
	BOARD_MSG_HEADER
	uint32_t error_code;
};

struct lmc_load_file_data_get_seq_req {
	BOARD_MSG_HEADER
};

struct lmc_load_file_data_get_seq_cfm {
	BOARD_MSG_HEADER
	uint32_t result;
};

struct lmc_load_file_data_get_seq_rej {
	BOARD_MSG_HEADER
	uint32_t error_code;
};

struct lmc_load_file_delete_req {
	BOARD_MSG_HEADER
	char loadmodule[];
};

struct lmc_load_file_delete_cfm {
	BOARD_MSG_HEADER
};

struct lmc_load_file_delete_rej {
	BOARD_MSG_HEADER
	uint32_t error_code;
};
struct lmc_read_load_file_data_req {
	BOARD_MSG_HEADER
	uint32_t lmc_index;
	uint32_t addroffset;
	uint32_t  length;
};

struct lmc_read_load_file_data_cfm {
	BOARD_MSG_HEADER
        int32_t nof_read_bytes;
        uint8_t  lmc_data[];
};

struct lmc_read_load_file_data_rej {
	BOARD_MSG_HEADER
	uint32_t error_code;
};

/* Indication signals are always sent to xpai
   and then forwarded to the client */
struct lmc_load_file_data_ind {
	BOARD_MSG_HEADER
	uint16_t lm_seq_nr;
};

struct lmc_load_file_delete_ind {
	BOARD_MSG_HEADER
	uint32_t result;
};

#endif

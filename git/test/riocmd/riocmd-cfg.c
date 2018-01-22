#include <regex.h>

#include "riocmd-internal.h"

#define MAX_MATCH                  1

#define DEFAULT_CFG_FILE_NAME    "/etc/riocmd/riocmd.cfg"
#define DEFAULT_LNH_MBOX         "first_lnh"
#define DEFAULT_BOARD_TYPE        DUW
#define DEFAULT_MASTER_DESTID     0xa00


static char *match_strings[] = {
	"LNH_MBOX_NAME=",
	"BOARD_TYPE=",
	"MASTER_DESTID=",
};

enum patterns {
	LNH_MBOX_NAME=0,
	BOARD_TYPE,
	MASTER_DESTID,
	PATTERNS_NUMBER
};

static int process_line(char *line, regex_t *expr_conf, struct cfg_data *cfg)
{
	regmatch_t pmatch[MAX_MATCH + 1];
	int status = 0;
	int offset = 0;
	int i = 0;

	for (i = LNH_MBOX_NAME; i < PATTERNS_NUMBER; i++) {
		status = regexec(&expr_conf[i], line, MAX_MATCH, pmatch, 0);

		if (status != 0) {
			continue;
		}
		offset = pmatch[0].rm_eo;
		switch (i) {
		case LNH_MBOX_NAME:
			strcpy(cfg->lnh_mbox_name, line + offset);
			/* Clear the new line character */
			cfg->lnh_mbox_name[strlen(cfg->lnh_mbox_name) - 1] = '\0';
			return status;
		case BOARD_TYPE:
			cfg->board_type = atoi(line + offset);
			return status;
		case MASTER_DESTID:
			cfg->master_destid = atoi(line + offset);
			return status;
		default:
			continue;
		}
	}
	return status;
}

static int cfg_parser(struct cfg_data *cfg)
{
	regex_t expr_conf[PATTERNS_NUMBER];
	int i;
	int status = 0;
	char conf_line[LINE_SIZE];
	FILE *conf_file;

	/* Compile the regex array for parsing */
	for (i = 0; i < PATTERNS_NUMBER; i++) {
		status = regcomp(expr_conf + i, match_strings[i], REG_EXTENDED);
		if (status != 0) {
			syslog(LOG_ERR, "Compile regex error code %d\n", status);
			return -1;
		}
	}
	conf_file = fopen(cfg->cfg_file_name, "r");
	if (conf_file == NULL) {
		syslog(LOG_ERR, "Opening configuration file failed: %s!", 
		       cfg->cfg_file_name);
		return -1;
	}
	while (!feof(conf_file)) {
		if (!fgets(conf_line, LINE_SIZE, conf_file))
			break;
		process_line(conf_line, expr_conf, cfg);
	}
	fclose(conf_file);
	return 0;
}

void read_config_file(char *file_name, struct cfg_data *cfg)
{
	if (file_name)
		strcpy(cfg->cfg_file_name, file_name);
	else
		strcpy(cfg->cfg_file_name, DEFAULT_CFG_FILE_NAME);

	strcpy(cfg->lnh_mbox_name, DEFAULT_LNH_MBOX);
	cfg->board_type = DEFAULT_BOARD_TYPE;
	cfg->master_destid = DEFAULT_MASTER_DESTID;

	cfg_parser(cfg);
}


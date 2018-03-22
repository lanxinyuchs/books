#ifndef _WHITELIST_H
#define _WHITELIST_H

extern int whitelist_init(void);
extern char ** whitelist_get_cmd(char *program);
extern char * whitelist_get_next(const char *expr, const char *prev);
extern char * whitelist_get_help(char *program);
extern char * whitelist_get_usage(char *program);
extern char * whitelist_get_descr(char *program);

#endif /* _WHITELIST_H */

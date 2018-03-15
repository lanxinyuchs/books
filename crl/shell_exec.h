/**
 * Temporary file till tools team gets gtest frame work integrated and
 * provides support for shell output claiming.
 */


#ifndef __SHELL_EXEC_H
#define __SHELL_EXEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */
#include <stdint.h>

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */
typedef int (*ShellCmd)(int argc, char *argv[]);

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

int
ShellExecvo(char *out, uint32_t *outLen, ShellCmd cmd, char *argv[]);

#ifdef __cplusplus
}
#endif

#endif   /* ifndef __SHELL_EXEC_H */

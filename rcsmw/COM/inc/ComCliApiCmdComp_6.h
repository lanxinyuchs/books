#ifndef COM_CLI_API_COMMAND_COMPONENT_6_H
#define COM_CLI_API_COMMAND_COMPONENT_6_H

#include "ComCliApiCmdComp_5.h"
#include "ComMgmtSpiCommon.h"
#include "ComMgmtSpiInterface_1.h"

#include <stdint.h>
#include <stdbool.h>

/**
 *
 * @file ComCliApiCmdComp_6.h
 * CLI Command module interface.
 * COM uses this interface to retrieve the loaded cli command modules,
 * each of which supply a unique set of text cli commands
 */

/**
 *
 * Cli API: The interface that needs to be implemented by a Command Component in order to be used by the CLI Agent
 *
 */

/**
 * Template Identity for all of the CLI command modules
 */
#ifdef __cplusplus
const ComMgmtSpiInterface_1T ComCliApiCmdCompIf_6Id = {"Put your component name here", "ComCliApiCommandComponent", "6"};
#endif
#ifndef __cplusplus
#define ComCliApiCmdCompIf_6Id (ComMgmtSpiInterface_1T){"Put your component name here", "ComCliApiCommandComponent", "6"};
#endif


/**
 * Exclusive mode lock types.
 */
typedef enum {
    /**
     * No lock.
     */
    ComCliLockEnumNone = 1,

    /**
     * Transaction lock. No other transactional module than the one which has
     * set the lock is allowed to execute. Non-transactional modules are still
     * allowed to execute.
     */
    ComCliLockEnumTransaction = 2,

    /**
     * Exclusive lock. No other module than the one which has set the lock is
     * allowed to execute.
     */
    ComCliLockEnumAll = 3
} ComCliLockEnum_1T;


/**
 * Context structure
 *
 * passed back and forth between the CliAgent and the various command
 * modules.
 */
struct ComCliApiContext_5T {
    /** session identifier */
    uint32_t sessionId;

    /** transaction handle, if available */
    ComOamSpiTransactionHandleT txHandle;

    /** the user's login name */
    const char * loginName;

    /** config/exec/etc.. mode of execution */
    ComCliModeEnum_1T mode;

    /** the DN is a const coming in; to update it one must use the fact pointer below, provided for callback;
     *
     * Note that the distinguished name is the network/external name, if the
     * networkManagedElementId attribute of the ManagedElement MO has been set;
     * for instance "ManagedElement=Stockholm" rather than "ManagedElement=1".
     * This means that a CCM using the current position in other APIs or SPIs
     * may need to translate the distinguished name when applicable.
     */
    char *cursor;

    /** the current prompt */
    char *prompt;

    /** session connection timeout */
    uint32_t timeout;

    /** security manager handle, opaque to cross C calling boundary. */
    uint64_t authHandle;

    /**
     * Due to the fact that the context is owned by CliAgent (from a memory management perspective)
     * but the command modules have the knowledge to update it when needed, the agent offers the
     * command modules a few callback function pointers. The commmand modules will use these
     * function pointers to ask the agent to update various parts of the context according to the
     * command module's request.
     */

    /** function pointer used by a command module to ask the CliAgent to update the cursor */
    ComReturnT (*setCursor)(struct ComCliApiContext_5T *context,
                            const char *newCursor);

    /** function pointer used by a command module to ask CliAgent to update the prompt */
    ComReturnT (*setPrompt)(struct ComCliApiContext_5T *context,
                            const char *prompt);

    /** function pointer used by a command module to deposit the output into the agent. */

    ComReturnT (*setOutput)(struct ComCliApiContext_5T *context,
                            uint32_t lineNumber,
                            uint32_t totalLineCount,
                            uint32_t actionId,
                            const char *text);

    /** function pointer used by a command module to deposit the autocomplete result into the agent. */

    ComReturnT (*setAutocompleteResult)(struct ComCliApiContext_5T *context,
                                        uint32_t lineNumber,
                                        uint32_t totalLineCount,
                                        uint32_t actionId,
                                        ComCliCompletionToken_1T type,
                                        const char *text,
                                        const char *helpText);

    /** function pointer used by a command module to request CliAgent to terminate the session; the command module
    * cannot do this by itself, it needs to ask CliAgent to do it. This is needed so the agent can ask communicate
    * other command modules the imminent termination so they have a chance to do cleanup work.
    * */

    ComReturnT (*sessionShutdown)(struct ComCliApiContext_5T *context,
                                  uint32_t code);

    /** used to lock/grant access to only one module.
     * Deprecated: Use lock / setLock() instead.*/
    bool exclusiveUse;

    /** function pointer used by a command module to deposit the autocomplete result, including information about how
     * the result shall be sorted, into the agent.
     */

    ComReturnT (*setSortableAutocompleteResult)(struct ComCliApiContext_5T *context,
            uint32_t lineNumber,
            uint32_t totalLineCount,
            uint32_t actionId,
            ComCliCompletionToken_1T type,
            const char *text,
            const char *helpText,
            ComCliSortOrderEnum_3T sortOrder,
            const char* sortKey);

    /** the return status of the last command that was executed before this call to useModule().
     * it is set by CliAgent and may be read by the command modules.
     */

    ComReturnT lastStatus;

    /**
     * The current CLI module lock state.
     *
     * When a transactional lock is set, non-transactional command modules
     * will not be given access to the current transaction id or cursor.
     */
    ComCliLockEnum_1T lock;

    /**
     * Set a new CLI module lock state.
     *
     * Transactional command modules may set both transactional and exclusive
     * locks. Non-transactional command modules may only set exclusive locks.
     */
    ComReturnT (*setLock)(struct ComCliApiContext_5T *context,
                          ComCliLockEnum_1T newLock);
};


/**
 * Interface structure
 *
 * Each command module needs to use this structure to register its
 * interface call, and the CliAgent casts the ComMgmtSpiInterface_1T
 * structure to this in order to get the interface calls for each
 * command module
 */
typedef struct {

    /** base structure */
    ComMgmtSpiInterface_1T base;

    /** @brief interface function that the command module must implement and the CLI agent invokes
      * @param context: structure containing session state and callbacks
      * @param action: action to perform
      * @param actionId: unique number that is passed back in setOutput call
      *            and maps input to output
      * @param commandMatchCount - number of commands that accepted responsibility
      *            for an execute request.  If it is zero, then the command
      *            is not supported by this command module.
      * @param text: text containing a command with its parameters (offered for execution)or
      *             a part of it (offered for completion)
      * @return: the success state of the request service
      */
    ComReturnT (*useModule) ( struct ComCliApiContext_5T *context,
                              ComCliActionEnum_1T action,
                              unsigned int  actionId,
                              unsigned int  *commandMatchCount,
                              const char    *text);

    /** @brief returns a NULL-terminated string with the name
     *         of the command group the command module belongs to.
     *  @return the name of the command group
     */
    const char *(*getGroupName)(void);

    /**
     * @brief interface function implemented by the command module and used by
     * the CLI Agent to determine if the command module is allowed to execute
     * when a transactional lock is set. Non-transactional command modules are
     * allowed to execute when a transactional lock is set, but are not given
     * access to the current transaction id or cursor.
     *
     * This function is optional to implement; if not implemented, the CLI
     * Agent will assume that the CCM is transactional.

     * @param context structure containing session state and callbacks
     * @return true if the command module is transactional, false if not
     */
    const bool (*isTransactional)(struct ComCliApiContext_5T *context);
} ComCliApiCmdComp_6T;


#endif

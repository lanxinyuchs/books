#ifndef COM_CLI_API_COMMAND_COMPONENT_5_H
#define COM_CLI_API_COMMAND_COMPONENT_5_H

#include "ComCliApiCmdComp_4.h"
#include "ComMgmtSpiCommon.h"
#include "ComMgmtSpiInterface_1.h"

#include <stdint.h>
#include <stdbool.h>

/**
 *
 * @file ComCliApiCmdComp_5.h
 * CLI Command module interface.
 * COM uses this interface to retrieve the loaded cli command modules,
 * each of which supply a unique set of text cli commands
 * @deprecated Use the latest version of the CLI API. Support for this version will be removed in COM 4.1.
 */

/**
 * Template Identity for all of the CLI command modules
 * @deprecated Use the latest version of the CLI API. Support for this version will be removed in COM 4.1.
 */
#ifdef __cplusplus
const ComMgmtSpiInterface_1T ComCliApiCmdCompIf_5Id = {"Put your component name here", "ComCliApiCommandComponent", "5"};
#endif
#ifndef __cplusplus
#define ComCliApiCmdCompIf_5Id (ComMgmtSpiInterface_1T){"Put your component name here", "ComCliApiCommandComponent", "5"};
#endif


/**
 * Context structure
 *
 * passed back and forth between the CliAgent and the various command
 * modules.
 * @deprecated Use the latest version of the CLI API. Support for this version will be removed in COM 4.1.
 */
struct ComCliApiContext_4T {
    /** session identifier */
    uint32_t sessionId;

    /** transaction handle, if available */
    ComOamSpiTransactionHandleT txHandle;

    /** the user's login name */
    const char * loginName;

    /** config/exec/etc.. mode of execution */
    ComCliModeEnum_1T mode;

    /** the DN is a const coming in; to update it one must use the fact pointer below, provided for callback; */
    char *cursor;

    /** the current prompt */
    char *prompt;

    /** session connection timeout */
    uint32_t timeout;

    /** security manager handle, opaque to cross C calling boundary. */
    uint64_t authHandle;

    /**
     * Due to the fact that the context is owned by CliAgent (from a memory management perspective)
     * but the command components have the knowledge to update it when needed, the agent offers the
     * command components a few callback function pointers. The commmand components will use these
     * function pointers to ask the agent to update various parts of the context according to the
     * command component's request.
     */

    /** function pointer used by a command component to ask the CliAgent to update the cursor */
    ComReturnT (*setCursor)(struct ComCliApiContext_4T *context,
                            const char *newCursor);

    /** function pointer used by a command component to ask CliAgent to update the prompt */
    ComReturnT (*setPrompt)(struct ComCliApiContext_4T *context,
                            const char *prompt);

    /** function pointer used by a command component to deposit the output into the agent. */

    ComReturnT (*setOutput)(struct ComCliApiContext_4T *context,
                            uint32_t lineNumber,
                            uint32_t totalLineCount,
                            uint32_t actionId,
                            const char *text);

    /** function pointer used by a command component to deposit the autocomplete result into the agent. */

    ComReturnT (*setAutocompleteResult)(struct ComCliApiContext_4T *context,
                                        uint32_t lineNumber,
                                        uint32_t totalLineCount,
                                        uint32_t actionId,
                                        ComCliCompletionToken_1T type,
                                        const char *text,
                                        const char *helpText);

    /** function pointer used by a command component to request CliAgent to terminate the session; the command component
    * cannot  do this by itself, it needs to ask CliAgent to do it. This is needed so the agent can ask communicate
    * other command components the imminent termination so they have a chance to do cleanup work.
    * */

    ComReturnT (*sessionShutdown)(struct ComCliApiContext_4T *context,
                                  uint32_t code);

    /** used to lock/grant access to only one CmdComp */

    bool exclusiveUse;

    /** function pointer used by a command component to deposit the autocomplete result, including information about how
     * the result shall be sorted, into the agent.
     */

    ComReturnT (*setSortableAutocompleteResult)(struct ComCliApiContext_4T *context,
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
};


/**
 * Interface structure
 *
 * Each command module needs to use this structure to register its
 * interface call, and the CliAgent casts the ComMgmtSpiInterface_1T
 * structure to this in order to get the interface calls for each
 * command module
 * @deprecated Use the latest version of the CLI API. Support for this version will be removed in COM 4.1.
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
    ComReturnT (*useModule) ( struct ComCliApiContext_4T *context,
                              ComCliActionEnum_1T action,
                              unsigned int  actionId,
                              unsigned int  *commandMatchCount,
                              const char    *text);



    /**
     *
     * The code snippet below captures the invocation of the useModule() function inside an iteration
     * over all the registered command components.
     *
     * @code
     *      _lockedModule = 0;
     *      ...
     *      for(int x = _lockedModule; coCoArray && coCoArray[x]; ++x) {
     *          if (strcmp(coCoArray[x]->getGroupName(), _currentGroup) == 0) {
     *              rv = coCoArray[x]->useModule(context, action, actionId, commandMatchCount, text);
     *              if(rv == ComOk) {
     *                  processResults();
     *                  _lockedModule = (context->exclusiveUse) ? x : 0;
     *              }
     *
     *              if (_lockedModule != 0) {
     *                 break;
     *              }
     *          }
     *      }
     * @endcode
     *
     * Note: the locking observable above refers to the case where a command component requests exclusive access to the user input for a certain period.
     * For example, if the user is in the process of setting a struct's content, providing each member of the struct's value might entail a few rounds
     * of input separated by <CR>. For as long as the structure is incomplete, the command module may request exclusive access to the user's input so the
     * user is not allowed to address other command componenets or attempt other operations until the structure is fully populated with values.
     *
     */

    /** @brief returns a NULL-terminated string with the name
     *         of the command group the command module belongs to.
     *  @return the name of the command group
     */
    const char *(*getGroupName)(void);

} ComCliApiCmdComp_5T;


#endif

#ifndef COM_CLI_API_COMMAND_COMPONENT_H
#define COM_CLI_API_COMMAND_COMPONENT_H

#include "ComMgmtSpiCommon.h"
#include "ComMgmtSpiInterface_1.h"

#include <stdint.h>
#include <stdbool.h>

/**
 *
 * @file ComCliApiCmdComp_1.h
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
const ComMgmtSpiInterface_1T ComCliApiCmdCompIf_1Id = {"Put your component name here", "ComCliApiCommandComponent", "1"};
#endif
#ifndef __cplusplus
#define ComCliApiCmdCompIf_1Id (ComMgmtSpiInterface_1T) {"Put your component name here", "ComCliApiCommandComponent", "1"}
#endif

/**
 * Action to be performed by the cli command module.
 */
typedef enum {
    /**
     * Com asks the command module to terminate and clean up.
     */
    ComCliActionEnumTerminate = 1,
    /**
     * Com asks the command module to execute a command
     */
    ComCliActionEnumExecute,
    /**
     * Com asks the command module to retrieve completion options
     * for a partial command.
     */
    ComCliActionEnumAutocomplete,

    /**
     * Com asks the command module to retrieve help for a command.
     * This enumerator is new for version 2 of this SPI.
     */
    ComCliActionEnumHelp
} ComCliActionEnum_1T;

/**
 * Session mode. There are two modes of operation, the last value being only a terminator for the enumeration
 */
typedef enum {
    /**
     * Exec Mode - read operations permitted
     */
    ComCliModeEnumExec = 1,
    /**
     * Config Mode - read/write operations permitted
     */
    ComCliModeEnumConfig,
    /**
     * marker for end of enumeration
     */
    ComCliModeEnumLast,
    /**
     * special marker to represent all modes
     */
    ComCliModeEnumAll = 0xFFFF

} ComCliModeEnum_1T;

/**
 * Ghost root dn
 * This type is used to specify the top of the dynamic object tree.
 */
#define ComCliGhostRootDN ""

/**
 * Exec mode prompt
 * This type specifies the prompt information to supply when the
 * session is in exec mode.
 *
 * Note that the cli appends ">" to the end of the set prompt,
 * so the actual exec mode prompt is ">", not "".
 */
#define ComCliExecModePrompt ""

/**
 * Config mode prompt
 * These types specify the prompt information to supply when the
 * session is in exec mode.
 *
 * When the cursor is at the ghost root, CONFIG_MODE_PROMPT is used.
 * When the cursor is below the ghost root, the format is
 * prefixRDNsuffix.  For example, if the cursor is "A=1,B=2,C=3",
 * the prompt should be (config-C=3)
 *
 * Note that the cli appends ">" to the end of the set prompt, so
 * in the example above the actual prompt would look like
 * (config-C=3)>
 *
 * The struct prompt separator is used to separate the normal config
 * prompt from the struct name, when building the prompt that appears
 * while editing a structure, for example, (config-C=3-myStructAttr)
 */

#define ComCliConfigModePrompt "(config)"
#define ComCliConfigModePromptPrefix "(config-"
#define ComCliConfigModePromptSeperator "-"
#define ComCliConfigModePromptSuffix ")"

/**
 * Output level indent
 *
 * In multi-line command output, the output level indent is the extra prefix
 * added to each successive level of output.  For example, with output level
 * indent set to three spaces, output might look like this:
 *      A=1
 *         aa 11
 *         bb 22
 *         B=7700
 *            cccccc  444
 *      C=2
 *         dd 33
 */
#define COM_CLI_OUTPUT_LEVEL_INDENT "   "

/**
 * The maximum lines that are printed from autocomplete result
 */
#define COM_CLI_MAX_AUTO_COMPLETE_LINES 100

/**
 * Each autocomplete result includes handling instructions for the line
 */
typedef enum {

    /**
     * Invalid value
     */
    ComCliCompletionEnumInvalid = 0,

    /**
     * Don't try to autocomplete on this line, force listing.
     */
    ComCliCompletionEnumForceList,

    /**
     * Don't try to autocomplete on this line but put a space on the command line
     * if all the autocomplete results say to do so, then force listing item.
     */
    ComCliCompletionEnumSpaceForceList,


    /**
     * Append difference between request and result
     */
    ComCliCompletionEnumComplete,

    /**
     * Continue to next token with no space
     */
    ComCliCompletionEnumNext,

    /**
     * Put a space, then continue to next token
     */
    ComCliCompletionEnumSpaceNext,

    /**
     * End value for array allocation
     */
    ComCliCompletionEnumEnd
} ComCliCompletionToken_1T;


/**
 * Context structure
 *
 * passed back and forth between the CliAgent and the various command
 * modules.
 * @deprecated Use the latest version of the CLI API. Support for this version will be removed in COM 4.1.
 */
struct ComCliApiContext_1T {
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
     * command components a few callback function pointers. The command components will use these
     * function pointers to ask the agent to update various parts of the context according to the
     * command component's request.
     */

    /** Function pointer used by a command component to ask the CliAgent to update the cursor */
    ComReturnT (*setCursor)(struct ComCliApiContext_1T *context,
                            const char *newCursor);

    /** function pointer used by a command component to ask CliAgent to update the prompt */
    ComReturnT (*setPrompt)(struct ComCliApiContext_1T *context,
                            const char *prompt);

    /** function pointer used by a command component to deposit the output into the agent. */

    ComReturnT (*setOutput)(struct ComCliApiContext_1T *context,
                            uint32_t lineNumber,
                            uint32_t totalLineCount,
                            uint32_t actionId,
                            const char *text);

    /** function pointer used by a command component to deposit the autocomplete result into the agent. */

    ComReturnT (*setAutocompleteResult)(struct ComCliApiContext_1T *context,
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

    ComReturnT (*sessionShutdown)(struct ComCliApiContext_1T *context,
                                  uint32_t code);

    /** used to lock/grant access to only one CmdComp */

    bool exclusiveUse;
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
      * @param commandMatchCount: number of commands that accepted responsibility
      *            for an execute request.  If it is zero, then the command
      *            is not supported by this command module.
      * @param text: text containing a command with its parameters (offered for execution)or
      *             a part of it (offered for completion)
      * @return: the success state of the request service
      */
    ComReturnT (*useModule) ( struct ComCliApiContext_1T *context,
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
     *
     *          rv = coCoArray[x]->useModule(context, action, actionId, commandMatchCount, text);
     *          if(rv == ComOk) {
     *              processResults();
     *              _lockedModule = (context->exclusiveUse) ? x : 0;
     *          }
     *
     *          if (_lockedModule != 0) {
     *             break;
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
} ComCliApiCmdComp_1T;


#endif




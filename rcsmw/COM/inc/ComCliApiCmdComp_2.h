#ifndef COM_CLI_API_COMMAND_COMPONENT_2_H
#define COM_CLI_API_COMMAND_COMPONENT_2_H

#include "ComCliApiCmdComp_1.h"
#include "ComMgmtSpiCommon.h"
#include "ComMgmtSpiInterface_1.h"

#include <stdint.h>
#include <stdbool.h>

/**
 *
 * @file ComCliApiCmdComp_2.h
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
const ComMgmtSpiInterface_1T ComCliApiCmdCompIf_2Id = {"Put your component name here", "ComCliApiCommandComponent", "2"};
#endif
#ifndef __cplusplus
#define ComCliApiCmdCompIf_2Id (ComMgmtSpiInterface_1T){"Put your component name here", "ComCliApiCommandComponent", "2"}
#endif


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
} ComCliApiCmdComp_2T;


#endif




/* ----------------------------------------------------------------------
 * %CCaseFile:	test_cmd_module.c %
 * %CCaseRev:	/main/R2A/5 %
 * %CCaseDate:	2013-10-21 %
 * %CCaseDocNo: %
 * Author:	erarafo
 * Author: <name>, <e-mail address>
 *
 * Short description: Demonstration of a CLI Extension shared library.
 * ----------------------------------------------------------------------
 * %CCaseTemplateFile:	template.c %
 * %CCaseTemplateId: CCver: /main/1 %
 *
 * %CCaseCopyrightBegin%
 * Copyright (c) Ericsson AB 2013 All rights reserved.
 * 
 * The information in this document is the property of Ericsson.
 * 
 * Except as specifically authorized in writing by Ericsson, the 
 * receiver of this document shall keep the information contained 
 * herein confidential and shall protect the same in whole or in 
 * part from disclosure and dissemination to third parties.
 * 
 * Disclosure and disseminations to the receivers employees shall 
 * only be made on a strict need to know basis.
 * %CCaseCopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *
 * Revision history:
 *
 * Rev        Date       Name        What
 * -----      -------    --------    --------------------------
 * R2A/1      2013-09-06 erarafo     First (incomplete) version in ClearCase.
 * R2A/2      2013-09-30 erarafo     Simple demo version, brings 3 CLI commands
 * R2A/3      2013-09-30 erarafo     Dropped unnecessary #include
 * R2A/4      2013-10-01 erarafo     Refactoring
 * R2A/5      2013-10-21 erarafo     Enabled only when cursor in subtree
 * ----------------------------------------------------------------------
 */


// This CLI Extension is active only if the cursor is the one below or longer.
// The string is case sensitive.
#define SUBTREE "ManagedElement=1,TestRoot=1"


#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "MafMgmtSpiInterfacePortal_1.h"
#include "MafMgmtSpiInterfacePortalAccessor.h"
#include "ComCliApiCmdComp_6.h"


MafMgmtSpiInterfacePortal_1T *portal;

MafMgmtSpiComponent_1T component;

ComCliApiCmdComp_6T cliComponent;

MafMgmtSpiInterface_1T *emptyDependencyArray[] = {NULL};

MafMgmtSpiInterface_1T *oneInterfaceArray[] = {
    (MafMgmtSpiInterface_1T *)&cliComponent,
    NULL
};


MafReturnT
start(MafStateChangeReasonT reason) {
  return MafOk;
}


MafReturnT
stop(MafStateChangeReasonT reason) {
  return MafOk;
}


const char *getGroupName() {
  const char *result = "ComBasicCommands";
  return result;
}


const bool isTransactional(struct ComCliApiContext_5T *context) {
  return true;
}


/**
 * Handles all command execution.
 */
static ComReturnT
action(struct ComCliApiContext_5T *context, uint32_t actionId, const char *arg) {
  if (strcmp(arg, "xbye") == 0) {
    (context->sessionShutdown)(context, 0);
  }
  else {
    char *message;
    asprintf(&message, "doing: %s", arg);
    (context->setOutput)(context, 1, 1, actionId, message);
    free(message);
  }
  return MafOk;
}


/**
 * Represents one menu entry.
 */
typedef struct {
  char *cmdName;                             // command name
  char *sortKey;                             // sort key; controls the 'help' action
  char *helpText;                            // one-liner help text
  ComReturnT (*action)(                      // pointer to function to execute
      struct ComCliApiContext_5T *context,
      uint32_t actionId,
      const char *arg);
  char *arg;                                 // single argument passed to function
} MenuEntry;


/**
 * A simple commands menu.
 */
MenuEntry *menu[] = {
    &(MenuEntry){"xthis", "1", "This CLI Extension command", &action, "this!"},
    &(MenuEntry){"xthat", "2", "That other CLI extension command", &action, "that!"},
    &(MenuEntry){"xbye", "3", "Bye by CLI Extension", &action, "xbye"},
    NULL};


/*
 * Returns true if the given prefix is a prefix of
 * the given command name.
 */
static bool
isPrefix(const char *prefix, const char *command) {
  unsigned int prefixLength = strlen(prefix);
  return prefixLength <= strlen(command)
      && strncmp(prefix, command, prefixLength) == 0;
}


/*
 * Returns true if the cursor is in the specified
 * MO subtree.
 */
static bool isInSubtree(const char *cursor) {
  if (cursor == NULL) {
    return false;
  }
  else {
    const unsigned int subtreeLength = strlen(SUBTREE);
    return strlen(cursor) >= subtreeLength \
        && strncmp(cursor, SUBTREE, subtreeLength) == 0;
  }
}


/**
 * Returns the match count for the given prefix. If
 * the given offset is 0 then the entire menu is
 * considered. If a positive value is given then
 * matching starts at that index. The index must
 * not exceed the number of menu entries.
 */
static unsigned int
getMatchCount(MenuEntry *rep[], const char *prefix, unsigned int offset) {
  unsigned int result = 0;
  for (unsigned int k = offset; rep[k] != NULL; k++) {
    if (isPrefix(prefix, rep[k]->cmdName)) {
      result++;
    }
  }
  return result;
}


/*
 * Gets one candidate character for extending the
 * given prefix. If no character can be found then
 * '\0' is returned.
 */
static char
getNextChar(MenuEntry *rep[], const char *prefix) {
  for (unsigned int k = 0; rep[k] != NULL; k++) {
    const char *cmdName = rep[k]->cmdName;
    if (isPrefix(prefix, cmdName)) {
      return cmdName[strlen(prefix)];
    }
  }
  return '\0';
}


/*
 * Returns the longest possible prefix that offers the
 * same number of completions as the given prefix. The
 * returned string is an equal-length or longer string.
 * The returned string is always a newly allocated
 * string which the caller must always free.
 */
char *extend(MenuEntry *rep[], const char *prefix) {
  unsigned int matchCount = getMatchCount(rep, prefix, 0);
  char n = getNextChar(rep, prefix);
  if (n == '\0') {
    char *result;
    asprintf(&result, "%s", prefix);
    return result;
  }
  else {
    char *prefix1;
    asprintf(&prefix1, "%s%c", prefix, n);
    unsigned int m1 = getMatchCount(rep, prefix1, 0);
    if (m1 < matchCount) {
      char *result;
      asprintf(&result, "%s", prefix);
      free(prefix1);
      return result;
    }
    else {
      char *result = extend(rep, prefix1);
      free(prefix1);
      return result;
    }
  }
}


ComReturnT
useModule(
    struct ComCliApiContext_5T *context,
    ComCliActionEnum_1T action,
    unsigned int actionId,
    unsigned int *commandMatchCount,
    const char *text) {

  if (action == ComCliActionEnumAutocomplete) {
    if (! isInSubtree(context->cursor)) {
      *commandMatchCount = 0;
      return MafOk;
    }
    else {
      unsigned int mCount = getMatchCount(menu, text, 0);
      if (mCount >= 1) {
        char *temp = extend(menu, text);
        char extendedText[strlen(temp)+1];
        strcpy(extendedText, temp);
        free(temp);
        (context->setAutocompleteResult)(
            context,
            0,
            0,
            actionId,
            ComCliCompletionEnumComplete,
            extendedText,
            ""
        );
      }
      *commandMatchCount = mCount;
      return MafOk;
    }
  }
  else if (action == ComCliActionEnumHelp) {
    if (isInSubtree(context->cursor)) {
      for (int k = 0; menu[k] != NULL; k++) {
        ComCliCompletionToken_1T token =
            getMatchCount(menu, text, k+1) > 0 ?
                ComCliCompletionEnumForceList :
                ComCliCompletionEnumEnd;
        if (isPrefix(text, menu[k]->cmdName)) {
          (context->setSortableAutocompleteResult)(
              context,
              0,
              0,
              actionId,
              token,
              menu[k]->cmdName,
              menu[k]->helpText,
              ComCliSortOrderFirst,
              menu[k]->sortKey);
        }
      }
    }
    *commandMatchCount = 0;
    return MafOk;
  }
  else if (action == ComCliActionEnumExecute) {
    if (! isInSubtree(context->cursor)) {
      *commandMatchCount = 0;
      return MafOk;
    }
    else {
      for (int k = 0; menu[k] != NULL; k++) {
        if (strcmp(text, menu[k]->cmdName) == 0) {
          ComReturnT result = (menu[k]->action)(context, actionId, menu[k]->arg);
          *commandMatchCount = 1;
          return result;
        }
      }
      *commandMatchCount = 0;
      return MafOk;
    }
  }
  else if (action == ComCliActionEnumTerminate) {
    *commandMatchCount = 0;
    return MafOk;
  }
  else {
    *commandMatchCount = 0;
    return MafOk;
  }
}


MafReturnT
mafLCMinit(
    struct MafMgmtSpiInterfacePortalAccessor *accessor,
    const char *config) {

  // Get hold of the portal; the argument "1" is from the Programmer's Guide.
  portal = (MafMgmtSpiInterfacePortal_1T *)(accessor->getPortal("1"));

  cliComponent.base.componentName = "Xyz";
  cliComponent.base.interfaceName = "ComCliApiCommandComponent";
  cliComponent.base.interfaceVersion = "6";

  cliComponent.getGroupName = &getGroupName;
  cliComponent.isTransactional = &isTransactional;
  cliComponent.useModule = &useModule;

  component.base.componentName = "Xyz";
  component.base.interfaceName = "ComMgmtSpiComponent";
  component.base.interfaceVersion = "1";

  component.dependencyArray = emptyDependencyArray;
  component.interfaceArray = oneInterfaceArray;

  component.start = &start;
  component.stop = &stop;

  return portal->registerComponent(&component);
}


void
mafLCMterminate() {
  portal->unregisterComponent(&component);
  return;
}

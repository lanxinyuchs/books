%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
%%%
%%% The information in this document is the property of Ericsson.
%%%
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2014-01-17 etxpeno     Created
%%% ----------------------------------------------------------
-module(eqs_vii).

-vsn('/main/R2A/3').
-date('2014-02-11').
-author('etxpeno').

-export(['CelloVii_visualIndRequest'/1]).

-include("eqs_vii.hrl").

-type 'CelloViiCommand_e'() :: ?CELLO_VII_FAULT                     |
			       ?CELLO_VII_NO_FAULT                  |
			       ?CELLO_VII_POWER                     |
			       ?CELLO_VII_NO_POWER                  |
			       ?CELLO_VII_BACKUP_START              |
			       ?CELLO_VII_BACKUP_END                |
			       ?CELLO_VII_BOOTTEST_START            |
			       ?CELLO_VII_BOOTTEST_END              |
			       ?CELLO_VII_LOADTEST_START            |
			       ?CELLO_VII_LOADTEST_END              |
			       ?CELLO_VII_MISSING_RESOURCE_START    |
			       ?CELLO_VII_MISSING_RESOURCE_END      |
			       ?CELLO_VII_REMOTE_UNIT_FAULT_START   |
			       ?CELLO_VII_REMOTE_UNIT_FAULT_END     |
			       ?CELLO_VII_MEDIUM_BUTTON_PRESS_START |
			       ?CELLO_VII_MEDIUM_BUTTON_PRESS_END   |
			       ?CELLO_VII_SHORT_BUTTON_PRESS_START  |
			       ?CELLO_VII_SHORT_BUTTON_PRESS_END    |
			       ?CELLO_VII_BOARD_LOCKED              |
			       ?CELLO_VII_BOARD_UNLOCKED            |
			       ?CELLO_VII_SHUTDOWN_START            |
			       ?CELLO_VII_SHUTDOWN_END              |
			       ?CELLO_VII_ALARM_SUPPRESS_START      |
			       ?CELLO_VII_ALARM_SUPPRESS_END.

-type  'CelloViiResult_e'() :: ?CELLO_VII_FAILED |
			       ?CELLO_VII_SUCCESS.

-spec 'CelloVii_visualIndRequest'(Indication::'CelloViiCommand_e'()) ->
					 'CelloViiResult_e'().
'CelloVii_visualIndRequest'(Indication) ->
    eqs_vii_service:'CelloVii_visualIndRequest'(Indication).

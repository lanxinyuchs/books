%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	aicDb.erl %
%% @author etxpeno
%% @copyright Ericsson AB 2017
%% @version /main/R9A/1

%% @doc
%% Handles access of the AIC mnesia tables.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(aicDb).
-vsn('/main/R9A/1').
-date('2017-02-21').
-author(etxpeno).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:  module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R9A/1   2017-02-21 etxpeno  Created.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###

-export([init_tables/1,
	 init_data/0]).

-export([read/1, read/2, write/2, delete/1]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###

%%% ----------------------------------------------------------
%%% #           aicVariables
%%% Description: Record for internal disc_copy table
%%% ----------------------------------------------------------
-record(aicVariables, {key, value}).

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% #############################################################################

init_tables(DbNodes) ->
    {atomic, ok} =
        clhI:mnesia_create_table(aicVariables,
                                 [{type, set},
                                  {disc_copies, DbNodes},
                                  {attributes, record_info(fields,
                                                           aicVariables)} |
                                  add_clh_option(aicVariables)]),
     ok.

init_data() ->
    IsUpgradeOngoing = swmI:is_upgrade_ongoing(),
    ok = init_data(IsUpgradeOngoing).

init_data(false) ->
    ok;
init_data(true) ->
    swmI:copy_old_table(aicVariables),
    ok.

read(Key) ->
    read(Key, undefined).
read(Key, Default) ->
     case mnesia:dirty_read(aicVariables, Key) of
	 [] ->
	     Default;
	 [#aicVariables{value = V}] ->
	     V
     end.

write(Key, Value) ->
    Rec = #aicVariables{key   = Key,
			value = Value},
    ok = mnesia:dirty_write(Rec).

delete(Key) ->
    ok = mnesia:dirty_delete(aicVariables, Key).

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###

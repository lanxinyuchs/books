%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiServer.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/2

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains the COI Server process which supports the coi.erl
%%% interface functionality.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiServer).
-vsn('/main/R3A/R4A/2').
-date('2015-10-21').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% R3A/    ---------- -------  ------------------------------------------------
%% In the COMSA block:
%% ===================
%%% 1      2014-10-07 etxberb  Created.
%%% 2      2014-10-10 etxberb  Added new_transIdNo/0 & is_max_limit_reached/1.
%%% R3A/3  2014-12-12 etxberb  Added coiEvent:init_ets_tables() & test functions
%% ===================
%% R3A/1   2015-01-13 etxberb  Moved to the COI block.
%% R3A/2   2015-01-15 etxpeno  Support for regular role
%% R3A/3   2015-02-04 etxberb  Added call to sysTestServer in coi_notify/1.
%% R4A/1   2015-06-25 etxberb  Added call to coiMim:init().
%% R4A/2   2015-10-21 etxberb  Added call to coiMib:init().
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([child_specs/0,
	 start_link/0]).

-export([new_transIdNo/0]).
-export([is_max_limit_reached/1]).

-export([get_state/0, info/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.2 Callbacks
%%% ###---------------------------------------------------------------------###
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% Test functions
-export([test_subscribe/0,
	 test_unsubscribe/0,
	 coi_notify/1]).

%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("coi.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
%% General
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).
-define(PROC_INFO(Pid), sysUtil:pid_info(Pid, {all, [error_handler]})).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

%% 
-define(CHILD_coiServer, {?MODULE,
			  {?MODULE, start_link, []},
			  permanent,
			  1000,
			  worker,
			  [?MODULE]}).

-define(TblName, ?MODULE).
-define(Max_Value_TransactionIdNumber, 8192).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(state, {}).

-record(?TblName, {key,
		   value}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Return specifications of children processes in the supervisor tree.
%%%
%%% @end
%%% ###=====================================================================###
child_specs() ->
    [?CHILD_coiServer].

%%% ###########################################################################
%%% @doc Start the server process.
%%%
%%% @end
%%% ###=====================================================================###
start_link() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    InitArg = #state{},
    Options = [],
    gen_server:start_link(ServerName, Module, InitArg, Options).

%%% ###########################################################################
%%% @doc Generate a new transaction identity number.
%%%
%%% @end
%%% ###=====================================================================###
new_transIdNo() ->
    ets:update_counter(?TblName,
		       transIdNo,
		       %% {Pos, Incr, Threshold, SetValue}
		       {3, 1, ?Max_Value_TransactionIdNumber, 1}).

%%% ###########################################################################
%%% @doc Check if number of active transactions have reached
%%%   ?Max_Value_TransactionIdNumber.
%%%
%%% @end
%%% ###=====================================================================###
is_max_limit_reached(TransIdPattern) ->
    case comsaTransactionServer:select_count(TransIdPattern) of
	NoOfBusy_TIds when NoOfBusy_TIds < ?Max_Value_TransactionIdNumber ->
	    false;
	_ ->
	    true
    end.

%%% ###########################################################################
%%% @doc Get the server process state.
%%%
%%% @end
%%% ###=====================================================================###
get_state() ->
    gen_server:call(?MODULE, {?MODULE, get_state}).

%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    gen_server:cast(?MODULE, {?MODULE, info}).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% Initiate the server process.
%%%
%%% ###=====================================================================###
init(State) ->
    case clhI:core_state() of
	active ->
	    ets:new(?TblName, [named_table, public, ordered_set, {keypos, 2}]),
	    ets:insert(?TblName, #?TblName{key = transIdNo, value = 0}),
	    coiMim:init(),
	    coiMib:init(),
	    coiEvent:init_ets_tables(),
	    error_logger:info_report([{?MODULE, ?FUNCTION} |
				      ?STATE_INFO(State)]),
	    {ok, State};
	_ ->
	    ignore
    end.

%%% ###########################################################################
%%% handle_call
%%%
%%% ###=====================================================================###
handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 {from, _From},
				 {unrecognized_msg, _Request} |
				 ?STATE_INFO(State)]),
    {reply, ok, State}.

%%% ###########################################################################
%%% handle_cast
%%%
%%% ###=====================================================================###
handle_cast({?MODULE, info}, State) ->
    error_logger:info_report(?PROC_INFO(self()) ++ ?STATE_INFO(State)),
    {noreply, State};
handle_cast(_Request, State) ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, _Request} |
				 ?STATE_INFO(State)]),
    {noreply, State}.

%%% ###########################################################################
%%% handle_info
%%%
%%% ###=====================================================================###
handle_info(_Msg, State) ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 {unrecognized_msg, _Msg} |
				 ?STATE_INFO(State)]),
    {noreply, State}.

%%% ###########################################################################
%%% code_change
%%%
%%% ###=====================================================================###
code_change(_OldVsn, State, _Extra) ->
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      {oldVsn, _OldVsn},
			      {extra, _Extra} | ?STATE_INFO(State)]),
    {ok, State}.

%%% ###########################################################################
%%% terminate
%%%
%%% ###=====================================================================###
terminate(_Reason, _State) ->
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      {reason, _Reason} | ?STATE_INFO(_State)]),
    ok.

%%% ###########################################################################
%%% @doc Test function for COI Subscribe.
%%%
%%% @end
%%% ###=====================================================================###
test_subscribe() ->
    coiEvent:subscribe(?MODULE).

%%% ###########################################################################
%%% @doc Test function for COI Unsubscribe.
%%%
%%% @end
%%% ###=====================================================================###
test_unsubscribe() ->
    coiEvent:unsubscribe(?MODULE).

%%% ###########################################################################
%%% @doc Test function for COI Notifications.
%%%
%%% @end
-spec coi_notify(Notification :: coi_notification_1()) ->
    ok.
%%% ###=====================================================================###
coi_notify(CoiNotification) ->
    STS_Res = sysTestServer:put_callback_result(CoiNotification),
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      test,
			      CoiNotification,
			      {sysTestServer, STS_Res}]).

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###

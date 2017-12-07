%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmOiI.erl %
%%% Author:
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfImmOiI).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/1').
-date('2016-10-06').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R3A/2      2015-03-09 erarafo     Startup timing
%%% R5A/1      2015-11-17 etxpeno     remove dead code
%%% R7A/1      2016-10-06 erarafo     wait for data conversion when upgrade
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%% IMM
-export([activate/0]).
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

%%----------------------------------------------------------------------
%% OI callback functions
%%----------------------------------------------------------------------
-export([
         ccb_object_create_2/5,
         ccb_object_delete/3,
         ccb_object_modify_2/4,
         ccb_completed/2,
         ccb_apply/2,
         ccb_abort/2,
         rt_attr_update/3
        ]).

-include("oi.hrl").
-include("gmf.hrl").


-record(state,
	{oi_handle,
	 slask = []}).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-spec activate() -> any().

activate() ->
    spawn(fun() -> doActivate() end).

%%% ----------------------------------------------------------
%%% @doc Become Object Implementer for Transport=1. In case
%%% upgrade is ongoing, wait for data conversion to finish.
%%% @end
%%% ----------------------------------------------------------
-spec doActivate() -> any().

doActivate() ->
    RequestId = ?REQUEST_ID_GMF_1,
    case swmI:is_upgrade_ongoing() of
	false ->
	    ok;
	true ->
	    case gmfImmUgIcti:waitForDataConversion(RequestId) of
		ok ->
		    sysInitI:info_msg(
		      "~w:doActivate/0, done waiting~n",
		      [?MODULE]);
		{error, Reason} ->
		    % not expected to happen ever
		    sysInitI:error_msg(
		      "~w:doActivate/0, failure in waiting: ~p~n",
		      [?MODULE, Reason])
	    end
    end,

    Vers = #safsVersion{releaseCode  = 65,
			majorVersion = 2,
			minorVersion = 11},
    gcall({initialize, [?MODULE, Vers]}),
    gcall({implementer_set, [list_to_binary(?MODULE_STRING)]}),
    %% TO DO: This needs to written in such a way that we know
    %% what classes we need to set the implementer for
    %% For the timebeing this is hardcoded.
    gcall({class_implementer_set, ["Transport"]}),
    gcall({object_implementer_set, [["transportId=1"], sa_imm_one]}).


start_link() ->
    s(start_link).
start() ->
    s(start).

s(F) ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:F(ServerName, Module, Args, Options).
stop() ->
    gcall(stop).

gcall(Msg) ->
    %% Def timeout is 5000
    gcall(Msg, 5000).
gcall(Msg, Timeout) ->
    gen_server:call(?MODULE, Msg, Timeout).

%%====================================================================
%% IMM Callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: ccb_object_create_2/5
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_2(_OiHandle, _CcbId, _ClassName, _ParentName, _Attr) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_delete/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_delete(_OiHandle, _CcbId, _ObjectName) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_2/4
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_2(_OiHandle, _CcbId, _ObjectName, _AttrMods) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_completed/2
%% Description:
%%--------------------------------------------------------------------
ccb_completed(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_apply/2
%% Description:
%%--------------------------------------------------------------------
ccb_apply(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_abort/2
%% Description:
%%--------------------------------------------------------------------
ccb_abort(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: rt_attr_update/3
%% Description:
%%--------------------------------------------------------------------
rt_attr_update(_OiHandle, _ObjectName, _AttributeNames) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_) ->
    sysInitI:log_startup_time(gmfImmOi_started),
    {ok, #state{}}.

handle_call({initialize, [Module, Vers]}, _From, State) ->
    {ok, Handle, _Vers} = safs_imm_oi:initialize(Module, Vers),
    {reply, ok, State#state{oi_handle = Handle}};
handle_call({F, Args},
	    _From,
	    #state{oi_handle = Handle} = State) ->
    Res = apply(safs_imm_oi, F, [Handle|Args]),
    {reply, Res, State};

handle_call({M, F, A}, _From, State) ->
    Res = apply(M, F, A),
    {reply, Res, State};
handle_call(stop, _From, State) ->
    {stop,normal,ok,State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.
handle_info(_, State) ->
    {noreply, State}.
code_change(_, State, _) ->
    {ok, State}.
terminate(_, _) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------




%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

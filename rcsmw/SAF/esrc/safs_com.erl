%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_com.erl
%%
%% Description:
%%    This file contains the commucation ....
%%
%%--------------------------------------------------------------------
-module(safs_com).
-behaviour(supervisor).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_com.hrl").
-include("safs_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/1,
	 trace_groups/0,
	 trace_points_list/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1,
	 terminate/2,
	 handle_call/3
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEBUG_LEVEL, 7).


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_sup/1
%% Description:
%%--------------------------------------------------------------------
start(Opts) ->
    supervisor:start_link({local, safs_com_sup}, ?MODULE,
			  {safs_com, Opts}).



%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error].

trace_points_list() ->
    [
     {error, 
      [{safs_error, 2}]}     
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description: 
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%----------------------------------------------------------------------
%% Server functions
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func: init/1
%%----------------------------------------------------------------------
init({safs_com, Opts}) ->

    SupFlags = {one_for_one, 5, 1000},	%Max 5 restarts in 1 second

    ImmOmPort = safs:get_env(imm_om_port, 10001),
    ImmOiPort = safs:get_env(imm_oi_port, 10002),
    NtfPort   = safs:get_env(ntf_port,    10003),
    LogPort   = safs:get_env(log_port,    10004),
    ImmCtPort = safs:get_env(imm_ct_port, 10005),

    StdListenPorts = [{port, {safs_imm_om_com, undefined}, ImmOmPort},
		      {port, {safs_imm_oi_com, undefined}, ImmOiPort},
		      {port, {safs_ntf_com, undefined},    NtfPort},
		      {port, {safs_log_com, undefined},    LogPort}],
    
    ListenPorts = 
	case safs:get_env(imm_ct_cb, undefined) of
	    undefined -> StdListenPorts;
	    ImmCtCb ->
		[{port, {safs_imm_ct_com, ImmCtCb}, ImmCtPort} |StdListenPorts]
	end,

    ChildSpec =
	[
	 {safs_com_insup, {safs_com_insup, start,
			   [sup, Opts]},
	  permanent, 10000, supervisor, [safs_com_insup]},
	 {safs_com_socketsup, {safs_com_socketsup, start,
			       [sup, Opts]},
	  permanent, 10000, supervisor, [safs_com_socketsup]},
	 {safs_com_net, {safs_com_net, start,
			 [ListenPorts]},
	  permanent, 10000, worker, [safs_com_net]}
	],
    {ok, {SupFlags, ChildSpec}}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------

%% Func: handle_call/3
%%----------------------------------------------------------------------
handle_call(Req, From, State) ->
    safs_error(handle_call, {"got call:", {Req,From}}),
    {reply, ok, State}.


%%======================================================================
%% Internal functions
%%======================================================================


%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------



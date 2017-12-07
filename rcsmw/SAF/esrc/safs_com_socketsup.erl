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
%%-----------------------------------------------------------------
%% File: safs_com_socketsup.erl
%% Description:
%%    This file contains the supervisor for the socket accept processes.
%%
%%-----------------------------------------------------------------
-module(safs_com_socketsup).
-behaviour(supervisor).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_internal.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start/2, 
	 start_accept/2 
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 terminate/2
	]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: start/2
%% Description: 
%%----------------------------------------------------------------------
start(sup, Opts) ->
    supervisor:start_link({local, safs_com_socketsup}, 
			  safs_com_socketsup,
			  {sup, Opts}).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: 
%%-----------------------------------------------------------------
init({sup, _Opts}) ->
%%    ?INFO("safs_com_socketsup starting\n", []),
    SupFlags = {simple_one_for_one, 500, 100},
    ChildSpec = [
		 {name3, {safs_com_net_accept, start, []}, temporary, 
		 10000, worker, [safs_com_net_accept]}
		],
    {ok, {SupFlags, ChildSpec}}.


%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: start_accept/1
%%-----------------------------------------------------------------
start_accept(Listen, Type) ->
%%    ?INFO("safs_com_socketsup starting accept child\n", []),
    supervisor:start_child(safs_com_socketsup, 
			   [Listen, Type]).


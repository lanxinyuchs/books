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
%% File: safs_com_net_accept.erl
%%
%% Description:
%%    This file contains the process which are waiting in accept for new
%%    connections.
%%
%%
%%-----------------------------------------------------------------
-module(safs_com_net_accept).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_internal.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start/2
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 net_accept/3
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
%% Function: start/1
%% Description:
%%----------------------------------------------------------------------
start(Listen, Type) ->
    Pid = proc_lib:spawn_link(?MODULE, net_accept,
			      [Listen, Type, self()]),
    {ok, Pid}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: net_accept/3
%%-----------------------------------------------------------------
net_accept(ListenFd, Type, Parent) ->
    % ?INFO("safs_com_net_accept waiting for connections\n", []),
    case safs_socket:accept(ListenFd) of
	{error, _E} ->
	    % ?INFO("safs_com_net_accept accept failed: ~p\n", [_E]),
	    ok;
	{ok, S} ->
	    case safs_com_net:connect(Type, S, self()) of
		{ok, Pid, ReadyToGo} ->
		    case safs_socket:controlling_process(S, Pid) of
			ok ->
			    % ?INFO("safs_com_net_accept controlling_process ok: ~p\n", [Pid]),
			    Pid ! set_active,
			    ok;
			_Reason ->
			    % ?INFO("safs_com_net_accept controlling_process: ~p failed: ~p\n", [Pid, _Reason]),
			    safs_socket:close(S),
			    gen_server:cast(Pid, stop)
			    % safs_socket:clear(S)
		    end,
		    ready_to_go(ReadyToGo);
		denied ->
		    % ?INFO("safs_com_net_accept connect got denied\n", []),
		    safs_socket:close(S);
		    % safs_socket:clear(S);
		_X ->
		    % ?INFO("safs_com_net_accept connect got : ~p\n", [_X]),
		    safs_socket:close(S)
		    % safs_socket:clear(S)
	    end,
	    net_accept(ListenFd, Type, Parent)
    end.

ready_to_go(true) ->
    ok;
ready_to_go(Ref) ->
    receive
	{Ref, ok} ->
	    ok
    end.


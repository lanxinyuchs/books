%%
%%%CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ftpesd_sup.erl %
%%% @author eivmiha
%%% @copyright Ericsson AB 2016
%%% @version /main/R8A/3
%%%
%%% ----------------------------------------------------------
-module(ftpesd_sup).
-behaviour(supervisor).
-vsn('/main/R8A/3').
-date('2016-11-28').
-author('eivmiha').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R8A/1    2016-11-18   ekurnik    Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0, 
         start_link/1, 
         start_child/2, 
         stop_child/1, 
         stop/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1]).

-define(SUP_SPEC, {one_for_one, 1, 60}).
-include("ftpesd.hrl").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

start_child(Name, Config) ->
    ChildSpec = get_listener_child_spec(self(), Name, Config),
    supervisor:start_child(?MODULE, ChildSpec).

stop(Pid) ->
    exit(Pid,shutdown), 
    ok.

%%% ----------------------------------------------------------
%%% #   stop_child/1
%%% ----------------------------------------------------------
%% @doc Function which takes Pid from child process. If there is a process, it is 
%%% terminated and the child specification is kept by the supervisor. To remove the
%%% child specification use delete_child/2. 
%%% 
-spec stop_child(Child :: pid() | atom()) -> ok | error_reason() .
%%% ----------------------------------------------------------
stop_child(Child) when is_pid(Child)->
    case get_child_id(Child) of
        {error, R} ->
            {error, R};
        Id ->
            case supervisor:terminate_child(?MODULE, Id) of
                ok ->
                       supervisor:delete_child(?MODULE, Id);
                Error ->
                    Error
            end
    end;

stop_child(Child) when is_atom(Child)->
    Id = get_child_name(Child),
    case supervisor:terminate_child(?MODULE, Id) of
        ok ->
               supervisor:delete_child(?MODULE, Id);
        Error ->
            Error
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init([]) ->
    {ok, {?SUP_SPEC, []}}.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% %%% ----------------------------------------------------------
%% %%%  #   set_child_id/1
%% %%% ----------------------------------------------------------
%% %%% @doc Function which sets bind address port. If fd is used, no need for 
%% %%% bind_address and port.
%% %%%  
%% -spec set_child_id(Config :: list()) -> tuple().
%% %%% ----------------------------------------------------------
%% set_child_id(Config) ->
%%     Address = proplists:get_value(ip, Config, no_address),
%%     Port = proplists:get_value(port, Config, ?DEFAULT_PORT),
%%     Fd = proplists:get_value(fd, Config, no_fd),
%%     {listener_srv, {Address, Port, Fd}}.

%%% ----------------------------------------------------------
%%% #    get_child_id/1
%%% ----------------------------------------------------------
%% @doc Function which gets the Id from a child process. 
-spec get_child_id(Child :: pid()) -> string() | {error, error_reason()}.
%%% ----------------------------------------------------------
get_child_id(Child)->
    case lists:keyfind(Child, 2, supervisor:which_children(?MODULE)) of
        {Id, Child, _, _} -> Id;
        _ -> {error, child_not_found}
    end.

get_listener_child_spec(SupPid, Name, Config) ->
    ChildName = get_child_name(Name),
    NewArgs = [ {sup_pid, SupPid}, {name, ChildName} | Config],
    {ChildName, {ftpesd_listener, start_link, [NewArgs]},
                    permanent, 100000, worker, [ftpesd_listener]}.

get_child_name(Name) ->
   ChildName = atom_to_list(ftpesd_listener) ++ "_" ++ atom_to_list(Name),
   ChildNameAtom = list_to_atom(ChildName),
   ChildNameAtom.

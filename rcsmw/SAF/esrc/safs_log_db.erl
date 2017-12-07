%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_log_db.erl
%%
%% Description:
%%    LOG Service Database
%%
%%--------------------------------------------------------------------
-module(safs_log_db).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%% -include("safs_ais.hrl").
%% -include("safs_log.hrl").

-include("safs_log_db.hrl").
-include("safs_log.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([init_ets/0]).
-export([init_mnesia/0]).

-export([stream_config_delete/1]).
-export([stream_config_get/1]).
-export([stream_config_write/1]).
-export([stream_config_update/2]).
-export([stream_config_noof/0]).

-export([log_user_insert/2]).
-export([log_user_delete/2]).
-export([log_user_delete_all_obj/1]).
-export([log_user_get/2]).

-export([sh2sn_insert/3]).
-export([sh2sn_delete/2]).
-export([sh2sn_get/2]).

-export([sn2h_add/3]).
-export([sn2h_delete/3]).
-export([sn2h_get/2]).

-export([h2sn_insert/4]).
-export([h2sn_delete/3]).
-export([h2sn_get/2]).

-export([handle_create/1]).
-export([stream_handle_create/1]).

-export([print_tables/2]).
%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
%% Tid for the different ETS tables
-record(tids, {id,      %% {Key, Value}
	       user,    %% #log_user
	       h2sn,    %% {Handle, StreamName}
	       sh2sn,   %% {StreamHandle, StreamName}
	       sn2h}).  %% {StreamName, [{StreamHandle, Handle}]}
 
%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(TABLES, [id, user, sh2sn, h2sn, sn2h]).
-define(LOG_USER,  safs_log_user).
-define(LOG_H2SN,  safs_log_handle_2_stream_name).
-define(LOG_SH2SN, safs_log_stream_handle_2_stream_name).
-define(LOG_SN2H,  safs_log_stream_name_2_handles).
-define(LOG_ID,    safs_log_id).

-define(KP_RECORD, 2).

%%====================================================================
%% External functions
%%====================================================================

%%====================================================================
%% 
%%====================================================================
init_ets() ->
    Id    = ets:new(?LOG_ID,    []),
    User  = ets:new(?LOG_USER,  [{keypos, ?KP_RECORD}]),
    H2SN  = ets:new(?LOG_H2SN,  []),
    SH2SN = ets:new(?LOG_SH2SN, []),
    SN2H  = ets:new(?LOG_SN2H,  []),

    ets:insert(Id, {handle, 1}),
    ets:insert(Id, {stream_handle, 1}),
    #tids{id    = Id,
	  user  = User,
	  h2sn  = H2SN,
	  sh2sn = SH2SN,
	  sn2h  = SN2H}.


%%====================================================================
%% 
%%====================================================================
init_mnesia() ->
    Nodes  = [node()],
    Tables = tables(),
    create_tables(Nodes, Tables),
    clear_tables(Tables),
    ok.


%%====================================================================
%% safs_log_stream_config (Mnesia table)
%%====================================================================
stream_config_delete(StreamName) ->
    trans(fun() -> mnesia:delete({safs_log_stream_config, StreamName}) end).

stream_config_get(StreamName) ->
    trans(fun() -> mnesia:read(safs_log_stream_config, StreamName) end).
    

stream_config_write(Obj) when is_record(Obj, safs_log_stream_config) ->
    trans(fun() -> mnesia:write(Obj) end).


stream_config_update(Obj, {create_attributes, CA}) 
  when is_record(Obj, safs_log_stream_config) ->
    trans(fun() -> 
		  mnesia:write(Obj#safs_log_stream_config{create_attributes =
							  CA})
	  end).

stream_config_noof() ->
    ets:info(safs_log_stream_config, size).

%%====================================================================
%% safs_log_id
%%====================================================================
handle_create(Tids) ->
    ets:update_counter(tid_get(id, Tids), handle, 1).

stream_handle_create(Tids) ->
    ets:update_counter(tid_get(id, Tids), stream_handle, 1).

%%====================================================================
%% safs_log_user
%%====================================================================
log_user_insert(Tids, Obj) ->
    ets:insert(tid_get(user, Tids), Obj).
    
log_user_delete(Tids, Handle) ->
    ets:delete(tid_get(user, Tids), Handle).

log_user_delete_all_obj(Tids) ->
    ets:delete_all_objects(tid_get(user, Tids)).

log_user_get(Tids, Handle) ->
    ets:lookup(tid_get(user, Tids), Handle).


%%====================================================================
%% safs_log_stream_handle_2_stream_name
%%====================================================================
sh2sn_insert(Tids, StreamHandle, StreamName) ->
    Tid = tid_get(sh2sn, Tids),
    ets:delete(Tid, StreamHandle),
    ets:insert(Tid, {StreamHandle, StreamName}).

sh2sn_get(Tids, StreamHandle) ->
    case ets:lookup(tid_get(sh2sn, Tids), StreamHandle) of
        [{_, StreamName}] ->
	    {ok, StreamName};
	_ ->
	    {error, ?SA_ERR_BAD_HANDLE}
    end.
	    

sh2sn_delete(Tids, Handle) ->
    ets:delete(tid_get(sh2sn, Tids), Handle).



%%====================================================================
%% safs_log_stream_name_2_handles
%%====================================================================
sn2h_add(Tids, StreamName, {_StreamHandle, _Handle} = Handles) ->
    Tid = tid_get(sn2h, Tids),
    OldHandles = case ets:lookup(Tid, StreamName) of
		     []       -> [];
		     [{_, H}] -> H
		 end,
    ets:insert(Tid, {StreamName, [Handles | OldHandles]}).

sn2h_get(Tids, StreamName) ->
    case ets:lookup(tid_get(sn2h, Tids), StreamName) of
	[{_, Handles}] -> Handles;
	[]             -> []
    end.


sn2h_delete(Tids, StreamName, Handles) ->
    Tid = tid_get(sn2h, Tids),
    sn2h_d(ets:lookup(Tid, StreamName),
	   Tid, 
	   Handles).

sn2h_d([{StreamName, [Handles]}], Tid, Handles) ->
    ets:delete(Tid, StreamName),
    [];
sn2h_d([{StreamName, Values}], Tid, Handles) ->
    NewValues = Values -- [Handles],
    ets:insert(Tid, {StreamName, NewValues}),
    NewValues.
    

%%====================================================================
%% safs_log_handle_2_stream_name
%%====================================================================
h2sn_insert(Tids, Handle, StreamName, StreamPid) ->
    Tid = tid_get(h2sn, Tids),
    h2sn_i(ets:lookup(Tid, Handle),
	   Tid, 
	   Handle, 
	   StreamName, 
	   StreamPid).
    
h2sn_i([], Tid, Handle, StreamName, StreamPid) ->
    ets:insert(Tid, {Handle, [{StreamName, StreamPid}]});
h2sn_i([{_, Values}], Tid, Handle, StreamName, StreamPid) ->
    ets:insert(Tid, {Handle, [{StreamName, StreamPid} | Values]}).
    
    
h2sn_delete(Tids, Handle, StreamName) ->
    Tid = tid_get(h2sn, Tids),
    h2sn_d(ets:lookup(Tid, Handle),
	   Tid, 
	   Handle, 
	   StreamName).

h2sn_d([{_, [{StreamName, _}]}], Tid, Handle, StreamName) ->
    ets:delete(Tid, Handle);
h2sn_d([{_, Values}], Tid, Handle, StreamName) ->
    NewValues = proplists:delete(StreamName, Values),
    ets:insert(Tid, {Handle, NewValues}).
    

h2sn_get(Tids, Handle) ->
    case ets:lookup(tid_get(h2sn, Tids), Handle) of
	[{_, StreamNames}] -> StreamNames;
	[]                 -> []
    end.



%%====================================================================
%% Test functions
%%====================================================================
print_tables(Table, Tids) when is_atom(Table) ->
    print_tables([Table], Tids);
print_tables([], _Tids) ->
    ok;
print_tables([H | T], Tids) when H == user;
				 H == id;
				 H == sh2sn;
				 H == h2sn;
				 H == sn2h ->
    Tid = tid_get(H, Tids),
    p(user, "=== ~p ===~n~n~p~n~n", [H, ets:tab2list(Tid)]),
    print_tables(T, Tids);
print_tables([config = H| T], Tids) ->
    p(user, "=== ~p ===~n~n~p~n~n", [H, ets:tab2list(safs_log_stream_config)]),
    print_tables(T, Tids);
print_tables([H|T], Tids) ->
    p(user, "ERROR: unknown table ~p~n~n", [H]),
    print_tables(T, Tids).
    
    



%%====================================================================
%% Internal functions
%%====================================================================

%% ets_lookup(Table, Handle) ->
%%     case ets:lookup(Table, Handle) of
%%         [] ->
%%             throw({error, sa_ais_err_bad_handle});
%%         [Obj] ->
%%             Obj
%%     end.



create_tables(Nodes, Tables) ->
    [ct_ok(create_table(Table, Nodes)) || Table <- Tables].

clear_tables(Tables) ->
    case mnesia:wait_for_tables(Tables, 10000) of
	ok ->     
	    [ct_ok(clear_table(Table)) || Table <- Tables];
	_ ->
	    error_logger:format("~p:~p Tables not created!!!!\n",
				[?MODULE, self()]),
	    clear_tables(Tables)
    end.

ct_ok({atomic,  ok})                  -> ok;
ct_ok({aborted, {already_exists, _}}) -> ok.

tables() ->
    [safs_log_stream_config].

create_table(Table, Nodes) ->
    mnesia:create_table(Table,
			[{type, set},
			 {attributes, ct_ri(Table)},
			 {disc_copies, Nodes}]).

clear_table(Table) ->
    mnesia:clear_table(Table).


ct_ri(safs_log_stream_config) ->
    record_info(fields, safs_log_stream_config).

tid_get(id,    #tids{id    = Val}) -> Val;
tid_get(user,  #tids{user  = Val}) -> Val;
tid_get(h2sn,  #tids{h2sn  = Val}) -> Val;
tid_get(sh2sn, #tids{sh2sn = Val}) -> Val;
tid_get(sn2h,  #tids{sn2h  = Val}) -> Val.


trans(Fun) ->
    case mnesia:transaction(Fun) of
	{atomic, ok}  -> 
	    case safs:get_env(imm_sync_cb, undefined) of
		{M, F} -> 
		    M:F(),
		    ok;
		undefined ->
		    ok
	    end;
	{atomic, Res} -> 
	    case safs:get_env(imm_sync_cb, undefined) of
		{M, F} -> 
		    M:F();
		undefined ->
		    ok
	    end,
	    {ok, Res};
	Error -> 
	    {error, Error}
    end.
   
	    

p(U,S,A) ->
    io:format(U,S,A).

%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_ntf_db.erl
%%
%% Description:
%%    NTF Database
%%
%%--------------------------------------------------------------------
-module(safs_ntf_db).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_ais.hrl").
-include("safs_ntf.hrl").

-include("safs_ntf_db.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([init/0,
	 insert_sent_notification/1, search_sent_notification/1,
	 create_notification_id/0]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(MAX_ENTRIES, 10).

%%====================================================================
%% External functions
%%====================================================================
init() ->
    internal_init().

insert_sent_notification(_SentNotification) ->
    ok.
%%     Size = mnesia:table_info(ntf_sent_notification, size),
%%     {atomic, ok} = mnesia:transaction(
%% 		    fun() ->
%% 			    N = 1+Size-?MAX_ENTRIES,
%% 			    ok = delete_sent_notifications(N),
%% 			    ok = mnesia:write(SentNotification)
%% 		    end),
%%     case safs:get_env(imm_sync_cb, undefined) of
%% 	{M, F} ->
%% 	    M:F(),
%% 	    ok;
%% 	undefined ->
%% 	    ok
%%     end.

%% delete_sent_notifications(N) when N =< 0 ->
%%     ok;
%% delete_sent_notifications(N) ->
%%     FirstKey = mnesia:first(ntf_sent_notification),
%%     ok = mnesia:delete({ntf_sent_notification, FirstKey}),
%%     delete_sent_notifications(N-1).

search_sent_notification(MatchSpec) ->
    {atomic, Objects} = mnesia:transaction(
			  fun() ->
				  mnesia:select(ntf_sent_notification,
						MatchSpec)
			  end),
    Objects.

create_notification_id() ->
    mnesia:dirty_update_counter(ntf_notification_id, notificationId, 1).

%%====================================================================
%% Internal functions
%%====================================================================
internal_init() ->
    Nodes = [node()],
    Tables = tables(),
    create_tables(Nodes, Tables),
    wait_for_tables(Tables),
    ok.

wait_for_tables(Tables) ->
    case mnesia:wait_for_tables(Tables, 10000) of
 	ok ->     
	    ok;
	_ ->
	    error_logger:format("~p:~p Tables not created!!!!\n",
				[?MODULE, self()]),
	    wait_for_tables(Tables)
    end.

create_tables(Nodes, Tables) ->
    lists:foreach(
      fun(Table) ->
	      case create_table(Table, Nodes) of
		  {atomic, ok} ->
		      ok;
		  {aborted, {already_exists, _}} ->
		      ok
	      end
      end, Tables),

    ok.

tables() ->
    [ntf_sent_notification, ntf_notification_id].    

create_table(ntf_sent_notification, Nodes) ->
    mnesia:create_table(ntf_sent_notification,
			[{type, ordered_set},
			 {attributes,
			  record_info(fields, ntf_sent_notification)},
			 {disc_copies, Nodes}]);
create_table(ntf_notification_id, Nodes) ->
    mnesia:create_table(ntf_notification_id,
			[{attributes,
			  record_info(fields, ntf_notification_id)},
			 {disc_copies, Nodes}]).

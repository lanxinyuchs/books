%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @doc A default replicated list module, this module is used as the
%%% default replicated list callback.
%%%
%%% <p>If you want to change it you have
%%% either set the correct app variable before comte starts or call
%%% `comte:register_replicated_list_callback/2'.  Through the app
%%% variable you can set options to {@link start/1}.</p>
%%%
%%% <p>Hopefully the callback specification and sample implementation is enough
%%% documentation. For more information about what the different functions
%%% should return see the COM documention and the header file MafMwSpiReplicatedList_1.h
%%  </p>
%%% @end
%%% Created : 21 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_default_replicated_list).

-behaviour(comte_replicated_list_api).

%% API
-export([install/1]).
-export([start/1]).

-export([listCreate/2]).
-export([listDelete/1]).
-export([listClear/1]).
-export([listGetSize/1]).
-export([listIsEmpty/1]).
-export([listGetFrontRef/1]).
-export([listPushBack/2]).
-export([listPopBack/1]).
-export([listEraseItem/2]).
-export([listFindItem/2]).
-export([listReplaceItem/3]).
-export([listNumberOfListInstances/0]).
-export([listMemoryUsage/0]).


-record(?MODULE, { instanceName, buffSize, list }).
%%%===================================================================
%%% API
%%%===================================================================
%%

%% @doc Create Mnesia tables for the replicated list.
%% This function assumes that the Mnesia tables have not been
%% created so it may only be called once on one node in a
%% cluster after Mnesia is started and before ComtE is started.
%%
%% `TabDef' is passed to `mnesia:create_table/2' so table
%% persistence and replication properties can be set.
%% @end
-spec install(TabDef :: [term()]) -> ok.
install(TabDef) ->
    Attributes = record_info(fields, ?MODULE),
    {atomic, ok} =
	mnesia:create_table(
	  ?MODULE, 
	  [{attributes, Attributes} | TabDef]),
    ok.

-type install_opt() :: {install, boolean()}.
%% If {@link install/1} has been called set this to `true' or the Mnesia
%% tables will be created again by {@link start/1}, which will fail.
%% <br />Default is `false' meaning Mnesia tables are created by
%% {@link start/1}.
 
-type timeout_opt() :: {timeout, infinity | non_neg_integer()}.
%% How long to wait for the Mnesia tables if {@link install_opt()} is `true'.
%% <br />Default is `infinity'.

-type start_opt() :: install_opt() | timeout_opt().

%% @doc The callback is invoked by ComtE during startup.
%% @end
-spec start(StartOpts :: [start_opt()]) -> ok.
start(StartOpts) ->
    Attributes = record_info(fields, ?MODULE),
    Timeout = proplists:get_value(timeout, StartOpts, infinity),
    case proplists:get_bool(install, StartOpts) of
	true ->
	    ok = mnesia:wait_for_tables([?MODULE], Timeout),
	    Attributes = mnesia:table_info(?MODULE, attributes);
	false ->
	    case mnesia:create_table(
		   ?MODULE,
		   [{attributes, Attributes}, {local_content, true}]) of
		{atomic, ok} ->
		    ok;
		{aborted, _} ->
		    ok = mnesia:wait_for_tables([?MODULE], Timeout),
		    Attributes = mnesia:table_info(?MODULE, attributes),
		    true = mnesia:table_info(?MODULE, local_content)
	    end
    end,
    ok.

listTransaction(Fun) ->
    case mnesia:transaction(Fun) of
	{atomic, Res} ->
	    Res;
	{aborted, {error, Reason}} ->
	    {error, Reason};
	Else ->
	    {error, Else}
    end.


-spec listCreate(comte_types:listinstancename(), integer()) ->
			ok | eexist |
			comte_types:com_error().
listCreate(ListInstanceName, DataBufferSize ) ->
    F = fun() ->
		case read(ListInstanceName) of
		    [] ->
                        mnesia:write(
			  #?MODULE{
			      instanceName = ListInstanceName,
			      buffSize = DataBufferSize,
			      list = []});
		    [#?MODULE{}] ->
			eexist
                end
        end,
    listTransaction(F).

-spec listDelete(comte_types:listinstancename()) ->
			ok | enoent |
			comte_types:com_error().
listDelete(ListInstanceName) ->
    F = fun() ->
		case read(ListInstanceName) of
		    [] ->
			enoent;
		    [#?MODULE{}] ->
                        mnesia:delete({?MODULE, ListInstanceName})
                end
        end,
    listTransaction(F).

-spec listClear(comte_types:listinstancename()) ->
		       ok | enoent |
		       comte_types:com_error().
listClear(ListInstanceName) ->
    F = fun() ->
		case read(ListInstanceName) of
		    [] ->
			enoent;
		    [#?MODULE{} = Instance] ->
			mnesia:write(Instance#?MODULE{ list = [] })
		end
        end,
    listTransaction(F).


-spec listGetSize(comte_types:listinstancename()) ->
			 non_neg_integer() | enoent |
			 comte_types:com_error().
listGetSize(ListInstanceName) ->
   F = fun() ->
	       case read(ListInstanceName) of
		   [] ->
		       enoent;
		   [#?MODULE{ list = List }] ->
		       length(List)
               end
       end,
    listTransaction(F).


-spec listIsEmpty(comte_types:listinstancename()) ->
			 boolean() | enoent |
			 comte_types:com_error().
listIsEmpty(ListInstanceName) ->
  F = fun() ->
	       case read(ListInstanceName) of
		   [] ->
		       enoent;
		   [#?MODULE{ list = List }] ->
		       List =:= []
               end
      end,
    listTransaction(F).


-spec listGetFrontRef(comte_types:listinstancename()) ->
			     [{comte_types:listitemname(),
			       comte_types:listdata()}] |
			     enoent |
			     comte_types:com_error().
listGetFrontRef(ListInstanceName) ->
    F = fun() ->
                case read(ListInstanceName) of
                    [] ->
                        enoent;
                    [#?MODULE{ list = List }] ->
                        List
                end
        end,
    listTransaction(F).


-spec listPushBack(comte_types:listinstancename(), comte_types:listdata()) ->
			  comte_types:listitemname() | enoent |
			  comte_types:com_error().
listPushBack(ListInstanceName,NewItemDataBuffer) ->
    F = fun() ->
                case read(ListInstanceName) of
                    [] ->
                        enoent;
                    [#?MODULE{ list = List } = Instance] ->
                        Key = get_next_key(List),
                        mnesia:write(
			  Instance
			  #?MODULE{
			      list = List ++ [{Key,NewItemDataBuffer}]}),
                        Key
                end
        end,
    listTransaction(F).


-spec listPopBack(comte_types:listinstancename()) ->
			 ok | enoent |
			 comte_types:com_error().
listPopBack(ListInstanceName) ->
    F = fun() ->
                case read(ListInstanceName) of
                    [] ->
                        enoent;
                    [#?MODULE{ list = List } = Instance] ->
                        NewList = delete_last(List),
                        mnesia:write(Instance#?MODULE{ list = NewList})
                end
        end,
    listTransaction(F).


-spec listEraseItem(
	comte_types:listinstancename(), comte_types:listitemname()) ->
			   ok | enoent |
			   comte_types:com_error().
listEraseItem(ListInstanceName,ListItemName ) ->
    F = fun() ->
                case read(ListInstanceName) of
                    [] ->
                        enoent;
                    [#?MODULE{ list = List } = Instance] ->
			case lists:keymember(ListItemName, 1, List) of
			    true ->
				NewList =
				    lists:keydelete(
				      ListItemName, 1, List),
				mnesia:write(
				  Instance#?MODULE{ list = NewList });
			    false ->
				enoent
			end
                end
        end,
    listTransaction(F).

-spec listFindItem(
	comte_types:listinstancename(), comte_types:listitemname()) ->
			  comte_types:listdata() | enoent |
			  comte_types:com_error().

listFindItem(ListInstanceName,ListItemName) ->
    F =
	fun() ->
                case read(ListInstanceName) of
                    [] ->
                        enoent;
                    [#?MODULE{ list = List }] ->
			case lists:keyfind(ListItemName, 1, List) of
			    false ->
				enoent;
			    {_, Value} ->
				Value
			end
		end
	end,
    listTransaction(F).

-spec listReplaceItem(
	comte_types:listinstancename(),
	comte_types:listitemname(),
	comte_types:listdata()) ->
			     ok | enoent |
			     comte_types:com_error().
listReplaceItem(ListInstanceName,ListItemName, ReplaceItemData) ->
    F = fun() ->
                case read(ListInstanceName) of
                    [] ->
                        enoent;
                    [#?MODULE{ list = List } = Instance] ->
			case lists:keymember(ListItemName, 1, List) of
			    true ->
				NewList =
				    lists:keyreplace(
				      ListItemName, 1, List,
				      {ListItemName, ReplaceItemData}),
				mnesia:write(
				  Instance#?MODULE{ list = NewList });
			    false ->
				enoent
			end
                end
        end,
    listTransaction(F).

-spec listNumberOfListInstances() -> non_neg_integer() |
				     comte_types:com_error().
listNumberOfListInstances() ->
    F = fun() ->
                length(mnesia:all_keys(?MODULE))
        end,
    listTransaction(F).

-spec listMemoryUsage() -> {non_neg_integer(),non_neg_integer()} |
			   comte_types:com_error().
listMemoryUsage() ->
    F = fun() ->
                {50,32768} % All fake numbers - 50% used - 32768 bytes left
        end,
    listTransaction(F).

%%%===================================================================
%%% Internal functions
%%%===================================================================

read(ListInstanceName) ->
    mnesia:read(?MODULE, ListInstanceName).

get_next_key(List) ->
    get_next_key(List, 0).
get_next_key(List, Cnt) ->
    Key = list_to_binary(integer_to_list(Cnt)),
    case lists:keymember(Key, 1, List) of
	false ->
	    Key;
	true ->
	    get_next_key(List, Cnt + 1)
    end.

delete_last([_Item]) ->
    [];
delete_last([Item|T]) ->
    [Item|delete_last(T)];
delete_last([]) ->
    [].

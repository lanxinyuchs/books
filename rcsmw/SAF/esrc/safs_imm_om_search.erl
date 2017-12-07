%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File    : safs_imm_om_search.erl
%%
%% Description : 
%%        Process to keep track of an IMM OM search.
%%
%%%-------------------------------------------------------------------
-module(safs_imm_om_search).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_imm_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 search/2,
	 get_next/2,
	 get_next_n/3,
	 stop/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2,
	 code_change/3
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {result, continuation, remove_param}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: set_result/1
%% Description: Set search result
%%--------------------------------------------------------------------
search(Pid, SearchHandle) ->
    call(Pid, {search, SearchHandle}).

%%--------------------------------------------------------------------
%% Function: get_next/2
%% Description: Get next object from search result
%%--------------------------------------------------------------------
get_next(Pid, Params) ->
    call(Pid, {get_next, Params}).

%%--------------------------------------------------------------------
%% Function: get_next_n/3
%% Description: Get next n objects from search result
%%--------------------------------------------------------------------
get_next_n(Pid, N, Params) ->
    call(Pid, {get_next_n, N, Params}).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stop the server
%%--------------------------------------------------------------------
stop(Pid) ->
    call(Pid, stop).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({search, Handle}, _From, State) ->
    SD = safs_imm_om:lookup_tbl(safs_imm_om_search_data, Handle),
    SearchAttrs = 
	case SD#safs_imm_om_search_data.options of
	    no -> no;
	    all -> all;
	    config -> config;
	    persistent -> persistent;
	    some -> {some, SD#safs_imm_om_search_data.attributes}
	end,
    SearchRoot = SD#safs_imm_om_search_data.rootname,

    {RemoveParam, SearchAttrs2} = 
	case SD#safs_imm_om_search_data.params of
	    {Name, _Val} ->
		add_to_search_attributes(Name, SearchAttrs);
	    undefined ->
		{false, SearchAttrs}
	end,
    try	    
        case SearchRoot of
	    <<>> ->
		ok;
	    _ ->
		RootCheckOp = safs_imm_db:search(SearchRoot, one, no, false),
		[] =:= mnesia:activity(sync_dirty, RootCheckOp) andalso throw(no_exists)
	end,
    
    SearchOp = safs_imm_db:search_incr(SearchRoot, 
				       SD#safs_imm_om_search_data.scope, 
				       SD#safs_imm_om_search_data.classes, 
				       SearchAttrs2, 
				       false),  
    case mnesia:activity(sync_dirty, SearchOp) of
	{Data, Cont} ->
	    {reply, ok, State#state{result=Data, continuation=Cont, remove_param=RemoveParam}};
	'$end_of_table' ->
	    {reply, ok, State#state{result=[], remove_param=RemoveParam}}
    end
    catch
	throw:no_exists -> {reply, {error, sa_ais_err_not_exist}, State};
	exit:_Reason ->
	    {reply, {error, sa_ais_err_failed_operation}, State} %should perhaps use try_again instead
    end;
handle_call({get_next, Params}, _From, State) ->
    case get_next_search_result(1, State#state.result, State#state.continuation, State#state.remove_param, 
				Params) of
	{[], Rest, Cont} ->
	    {reply, {error, sa_ais_err_not_exist}, State#state{result=Rest, continuation=Cont}};
	{[Result], Rest, Cont} ->
	    {reply, Result, State#state{result=Rest, continuation=Cont}}
    end;
handle_call({get_next_n, N, Params}, _From, State) ->
    case get_next_search_result(N, State#state.result, State#state.continuation, State#state.remove_param, 
				Params) of
	{[], Rest, Cont} ->
	    {reply, {error, sa_ais_err_not_exist}, State#state{result=Rest, continuation=Cont}};	
	{Result, Rest, Cont} ->
	    {reply, {ok, Result}, State#state{result=Rest, continuation=Cont}}
    end;
handle_call(stop, _From, State) ->
        {stop, normal, ok, State};
handle_call(Msg, From, #state{} = State) ->
    error_logger:format("~p~p got unexpected call from ~p:\n\t~p\n",
                        [?MODULE, self(), From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected cast:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected message:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Description: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
call(Pid, Request) ->
    gen_server:call(Pid, Request, infinity).

add_to_search_attributes(Attr, no) ->
    {true, {some, [Attr]}};
add_to_search_attributes(_Attr, all) ->
    {false, all};
add_to_search_attributes(_Attr, config) ->
    {false, config};
add_to_search_attributes(_Attr, persistent) ->
    {false, persistent};
add_to_search_attributes(Attr, {some, Attrs}) ->
    case lists:member(Attr, Attrs) of
	false ->
	    {true, {some, [Attr |Attrs]}};
	_ ->
	    {false, {some, Attrs}}
    end.

get_next_search_result(0, _Result, _Cont, _RemoveParam, _Params) ->
    [];
get_next_search_result(N, Result, Cont, RemoveParam, Params) ->
    get_next_search_result(N, Result, Cont, RemoveParam, Params, []).

get_next_search_result(0, [], Cont, _, _, Acc) ->
    {Acc, [], Cont}; %%Is reversed later (see safs_imm_om:search_next_common_1/3)
get_next_search_result(_N, [], undefined, _, _, Acc) ->
    {Acc, [], undefined};
get_next_search_result(N, [], Cont, RemoveParam, Params, Acc) ->
    SearchOp = safs_imm_db:search_cont(Cont),
    case mnesia:activity(sync_dirty, SearchOp) of
	'$end_of_table' ->
	    {Acc, [], Cont}; %%Is reversed later (see safs_imm_om:search_next_common_1/3)
	{Data, Cont1} -> 
	    get_next_search_result(N, Data, Cont1, RemoveParam, Params, Acc)
    end;
get_next_search_result(0, Rest, Cont, _, _, Acc) ->
    {Acc, Rest, Cont};
get_next_search_result(N, [{DN, Class, AttrList} |Rest], Cont, 
		       RemoveParam, undefined, Acc) -> 
    get_next_search_result(N-1, Rest, Cont, RemoveParam, undefined,
			   [{DN, Class, AttrList} |Acc]); 
get_next_search_result(N, [NextObj |Rest], Cont, RemoveParam, undefined, Acc) -> 
    get_next_search_result(N-1, Rest, Cont, RemoveParam, undefined,
			   [{NextObj, []} |Acc]);
get_next_search_result(N, [{DN, Class, AttrList} |Rest], Cont, 
		       RemoveParam, {Name, undefined} = Params, Acc) ->    
    case lists:keyfind(Name, 1, AttrList) of
	false ->
	    get_next_search_result(N, Rest, Cont, RemoveParam, Params, Acc);
	{Name, []} ->
	    get_next_search_result(N, Rest, Cont, RemoveParam, Params, Acc);
	{Name, _ValueList} ->
	    case RemoveParam of
		true ->
		    get_next_search_result(N-1, Rest, Cont, RemoveParam, Params,
					   [{DN, Class, lists:keydelete(Name, 1, AttrList)} |Acc]);
		false ->
		    get_next_search_result(N-1, Rest, Cont, RemoveParam, Params, 
					   [{DN, Class, AttrList} |Acc])
	    end
    end;
get_next_search_result(N, [{DN, Class, AttrList} |Rest], Cont, RemoveParam,
		       {'SaImmAttrClassName', ClassNames} = Params, Acc) when is_list(ClassNames) ->
    case lists:member(Class, ClassNames) of
	false ->
	    get_next_search_result(N, Rest, Cont, RemoveParam, Params, Acc);
	true ->
	    case RemoveParam of
		true ->
		    get_next_search_result(N-1, Rest, Cont, RemoveParam, Params,
					   [{DN, Class, 
					     lists:keydelete('SaImmAttrClassName', 1, AttrList)} |Acc]);
		false ->
		    get_next_search_result(N-1, Rest, Cont, RemoveParam, Params, 
					   [{DN, Class, AttrList} |Acc])
	    end
    end;
get_next_search_result(N, [{DN, Class, AttrList} |Rest], Cont, RemoveParam,
		       {Name, Value} = Params, Acc) ->
    case lists:keyfind(Name, 1,AttrList) of
	false -> 
	    get_next_search_result(N, Rest, Cont, RemoveParam, Params, Acc);
	{Name, ValueList} ->
	    case lists:member(Value, ValueList) of
		true ->
		    case RemoveParam of
			true ->
			    get_next_search_result(N-1, Rest, Cont, RemoveParam, Params,
						   [{DN, Class, lists:keydelete(Name, 1, AttrList)} |Acc]);
			false ->
			    get_next_search_result(N-1, Rest, Cont, RemoveParam, Params, 
						   [{DN, Class, AttrList} |Acc])
		    end;
		false ->
		    get_next_search_result(N, Rest, Cont, RemoveParam, Params, Acc)
	    end;
	 _ -> 
	     get_next_search_result(N, Rest, Cont, RemoveParam, Params, Acc)
    end.    


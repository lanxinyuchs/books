%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxlg
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R11A/1
%%%
%%% @doc ==New radio model==
%%% Basic implementation of the New radio model based on the S&T prototype
%%% @end

-module(nr_emp).
-vsn('/main/R9A/R11A/1').
-date('2017-10-16').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R9A/1       20170405  etxlg         Copied into FAKE
%%% R9A/2       20170518  etxlg         Got rid of annoying printout
%%% R11A/1      20171016  etxlg         Got rid of export_all
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

% TODO 
% done o Remove orphans from ECIM tables
% done o Use the IP:Port data from ECIM
% o Sync, check sync
% o Strategy for resync
% o Support for "strange" objects in "radio_config" (new ReST schema required)
% done o Filter out table update events where only reservedBy changed
% o Garbage collect postponed and deleted_orphans

-behavior(gen_server).

-export([start/0, start/1, stop/0]).
%%-compile([export_all]).

%debug
-export([list_top_objects/0, list_top_objects/1]).
-export([type_name_id/0]).
-export([erase_rest_db/0]).
-export([erase_ecim_db/0]).
-export([erase_all/0]).
-export([lookup/1]).

-export([debug_info/1, debug_rest/1, debug_general/1, debug_all/1]).

%gen_server callbacks
-export([init/1, terminate/2, code_change/3,
	 handle_call/3, handle_cast/2,
	 handle_info/2]).

-include("NrFunction.hrl").
-define(SERVER, nr_emp).
-define(PORT, 8080).
-define(PROFILE, db_service). %httpc-client profile

-define(RESTBASE, "/oam/v2/").
-define(NR_MODEL, "NrFunction").
-define(HEAD_TABLE, nrFunction).

-record(st, {db_ip, db_port, all_nr_tables, tree,
	     postponed = [],
	     deleted_orphans = []}).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, undefined, []).

start(Ip_tuple_or_string) ->
    Ip =
	if
	    is_tuple(Ip_tuple_or_string) ->
		Ip_tuple_or_string;
	    is_list(Ip_tuple_or_string) ->
		{ok, Ip_tuple} = inet:parse_address(Ip_tuple_or_string),
		Ip_tuple
	end,
    gen_server:start({local, ?SERVER}, ?MODULE, Ip, []).

stop() ->
    gen_server:stop(?SERVER).

list_top_objects() ->
    list_top_objects([]).

list_top_objects(Types) when is_list(Types) ->
    gen_server:call(?SERVER, {list_top_objects, Types});
list_top_objects(Types) ->
    list_top_objects([Types]).

type_name_id() ->
    gen_server:call(?SERVER, type_name_id).

erase_all() ->
    erase_rest_db(),
    erase_ecim_db().

erase_rest_db() ->
    gen_server:call(?SERVER, erase_rest_db).

erase_ecim_db() ->
    Almost = get_all_nr_tables() -- [?HEAD_TABLE],
    io:format("Erasing almost all tables: ~p~n", [Almost]),
    [mnesia:clear_table(Table) || Table <- Almost],
    ok.

lookup(Path) ->
    gen_server:call(?SERVER, {lookup, Path}).

debug_all(Bool) ->
    [change_debug(What, Bool) || What <- [info, rest, general]].
debug_info(Bool) -> change_debug(info, Bool).
debug_rest(Bool) -> change_debug(rest, Bool).
debug_general(Bool) -> change_debug(general, Bool).
change_debug(What, Bool) when Bool; not Bool ->
    gen_server:cast(?SERVER, {change_debug, What, Bool}).

init(Db_ip) ->
    All_tables = get_all_nr_tables(),
    [{ok, _} = mnesia:subscribe({table, Table, detailed}) || Table <-
	All_tables],
    case inets:start(httpc, [{profile, ?PROFILE }]) of
	{ok, _Pid} -> ok;
	{error, {already_started, _Pid}} -> ok
    end,
    Options = [], 
    ok = httpc:set_options(Options, ?PROFILE),
    {Rest_ip, Rest_port} =
	case Db_ip of
	    undefined ->
		get_rest_service_info();
	    Ip ->
		{Ip, ?PORT}
	end,
    case Rest_ip of
	undefined -> uinfo_msg("The agent address (ReST db) is not set", []);
	_ -> ok
    end,
    Tree = get_object_tree(),
    {ok, #st{db_ip = Rest_ip, db_port = Rest_port,
	     all_nr_tables = All_tables, tree = Tree}}.

handle_call(_, _Frm, #st{db_ip = undefined} = S) ->
    {reply, {error, "agent address (ReST db) is not set"}, S};
handle_call(type_name_id, _Frm, S) ->
    {reply, return_tnid(S), S};
handle_call({list_top_objects, Types}, _Frm, S) ->
    Actual_types =
	case Types of
	    [] -> get_top_objects(S);
	    _ -> check_top_objects(Types, S)
	end,
    Objects = [list_object(T, S) || T <- Actual_types],
    {reply, Objects, S};
handle_call(erase_rest_db, _Frm, S) ->
    To_erase = get_all_top_objects(S),
    info_msg("There are ~b top objects to erase", [length(To_erase)]),
    erase_objects(To_erase, S),
    {reply, ok, S};
handle_call({lookup, Path}, _Frm, S) ->
    Url = mk_url(path_join([?RESTBASE, Path]), S),
    case rest_get(Url, S) of
	{ok, Props} ->
	    {reply, Props, S};
	Woops ->
	    error_msg("Something went wrong: ~p ->~n~p", [Url, Woops]),
	    {reply, [], S}
    end;
handle_call(Req, Frm, S) ->
    error_msg("Unrecognized call[from: ~p]:~n~p", [Frm, Req]),
    {reply, unrecognized, S}.

%%% What: info | rest | general
handle_cast({change_debug, What, Bool}, S) ->
    Prev = put(What, Bool), %this is debug cheat!
    io:format("~w: Debug status of ~p, ~p -> ~p~n",
	      [?MODULE, What, Prev, Bool]),
    {noreply, S};
handle_cast(Req, S) ->
    error_msg("Unrecognized cast:~n~p", [Req]),
    {noreply, S}.

handle_info({mnesia_table_event, Event}, S)
	when element(2, Event) =:= ?HEAD_TABLE ->
    dbg("Table event for: ~p table:~n~p", [?HEAD_TABLE, Event]),
    New_s = look_for_agent_address(Event, S),
    {noreply, New_s};
handle_info(Whatever, #st{db_ip = undefined} = S) ->
    mismatch("Agent address (ReST db) is not set, unable to process:~n~p",
	     [Whatever]),
    {noreply, S};
handle_info({mnesia_table_event, Event}, S) ->
    case lists:member(element(2, Event), S#st.all_nr_tables) of
	true ->
	    New_s = handle_table_event(Event, S),
	    {noreply, New_s};
	false ->
	    case element(2, Event) of
		schema -> % happens at clear, stay silent
		    {noreply, S};
		_ ->
		    error_msg("Event for unrecognized table:~n~p", [Event]),
		    {noreply, S}
	    end
    end;
handle_info(What, S) ->
    error_msg("Unrecognized info:~n~p", [What]),
    {noreply, S}.

code_change(_Old_v, S, _Extra) ->
    {ok, S}.

terminate(_Why, _S) -> ok.


%internals

%means ECIM/process-state/MongoDb is probably out of sync
mismatch(Fmt, Args) ->
    io:format("~p: MISMATCH: " ++ Fmt ++ "~n", [?MODULE | Args]).

error_msg(Fmt, Args) ->
    io:format("~p: ERROR: " ++ Fmt ++ "~n", [?MODULE | Args]).

info_msg(Fmt, Args) ->
    case get(info) of
	true -> uinfo_msg(Fmt, Args);
	_ -> ok
    end.

uinfo_msg(Fmt, Args) ->
	io:format("~p: info: " ++ Fmt ++ "~n", [?MODULE | Args]).

dbg(Fmt, Args) ->
    case get(general) of
	true ->
	    io:format("~p: dbg: " ++ Fmt ++ "~n", [?MODULE | Args]);
	_ -> ok
    end.
    
%return new (or old) State
handle_table_event({write, _Table, New, [], _Id}, S) ->
    handle_create(New, S);
handle_table_event({write, Table, New, Old_recs, _Id}, S) ->
    dbg("table: ~p update, Old_recs: ~p", [Table, Old_recs]),
    handle_update( New, Old_recs, S);
handle_table_event({delete, Table, What, _Old_recs, _Id}, S) ->
    handle_delete(Table, What, S);
handle_table_event(Event, S) ->
    error_msg("Unrecognized table event:~n~p", [Event]),
    S.

handle_create(Record, S) ->
    dbg("create: ~p", [Record]),
    %perform ReST POST for object creation
    case make_ecim_data(Record, S) of
	{_Type, Url, Json} ->
	     New_s = add_object(Url, Json, Record, S),
	    %{_Created_id , New_s} = add_object(Url, Json, Record, S),
	    New_s;
	{error, Err} ->
	    mismatch("Unable to try creating new ReST object: ~p: ~p",
		     [Record, Err]),
	    S
    end.

add_object(Url, Json, Record, S) ->
    case rest_create(Url, Json, S) of
	{ok, _Obj_id} ->
	    check_for_postponed(Record, S);
	{error, Err} ->
	    dbg("Failed to create new ReST object: ~p: ~p -> postpone",
		[Url, Err]),
	    Postponed = {erlang:monotonic_time(millisecond), Record},
	    S#st{postponed = [Postponed | S#st.postponed]}
    end.

%return S
check_for_postponed(_, #st{postponed = []} = S) ->
    S;
check_for_postponed(Just_created, #st{postponed = Postponed} = S) ->
    Created_id = get_ecim_id_from_record(Just_created),
    find_matching(Created_id, Postponed, [], S).

find_matching(_, [], Checked, S) ->
    S#st{postponed = Checked};
find_matching(Created_id, [{_, Postp} = H | T], Checked, S) ->
    Postponed_id = get_ecim_id_from_record(Postp),
    case is_child(Created_id, Postponed_id) of
	true ->
	    dbg("Found match in postponed list: ~p - trying to add",
		[Postponed_id]),
	    Next_s = create_postponed(Postp, S),
	    find_matching(Created_id, T, Checked, Next_s);
	false ->
	    find_matching(Created_id, T, [H | Checked], S)
    end.

create_postponed(Postp, S) ->
    Next_s = handle_create(Postp, S#st{postponed = []}),
    case Next_s#st.postponed of
	[] -> %went well didn't "postpone" again
	     Next_s;
	_ ->
	    mismatch("Failed to create postponed object: ~p~n"
		      "No more attempts will be made", [Postp]),
	    Next_s
    end.

is_child(Created_id, Postponed_id)
	when size(Created_id) =:= (size(Postponed_id) - 1) ->
    C = tuple_to_list(Created_id),
    P = tuple_to_list(Postponed_id),
    case lists:prefix(C, P) of
	true ->
	    dbg("is_child(~p,~p)_ -> true", [Created_id, Postponed_id]),
	    true;
	false ->
	    dbg("is_child(~p,~p) -> false", [Created_id, Postponed_id]),
	    false
    end;
is_child(C ,P) ->
    dbg("is_child(~p,~p)_ -> false", [C, P]),
    false.

handle_update(Record, [Old_rec], S) ->
    %This is doing quite a bit of work for little gain?
	case {create_intermediate_object_attributes(Record ,S),
	      create_intermediate_object_attributes(Old_rec ,S)} of
	{Same, Same} ->
	    dbg("Update: ~p~n, no change (reservedBy:moRef?) - skipped",
		[Record]),
	    S;
	_ ->
	    do_update(Record, S)
    end.
handle_delete(Table, {Table, Key}, S) ->
    do_delete(Table, Key, S);
handle_delete(Table, Record, S) ->
    Key = get_ecim_id_from_record(Record),
    do_delete(Table, Key, S).

do_update(Record, S) ->
    case make_ecim_data(Record, S) of
	{_Type, _Url, Json} ->
	    Path_list = ecim_record_to_path(Record, S),
	    Url = mk_url(path_join([?RESTBASE | Path_list]), S),
	    update_object(Url, Json, S);
	{error, Err} ->
	    mismatch("Update: bad result from make_ecim_data/2: ~p: ~p",
		     [Record, Err])
    end,
    S.

update_object(Url, Json, S) ->
    case rest_update(Url, Json, S) of
	{ok, _Rest_id} ->
	    ok;
	{error, Err} ->
	    mismatch("Failed to update, Url: ~p~nErr: ~p",
		      [Url, Err])
    end.

do_delete(Table, Key, #st{deleted_orphans = Dels} = S) ->
   case lists:keytake({Table, Key}, 2, Dels) of
	{value, _Gone, New_dels} ->
	    dbg("Got delete for orphan: ~p", [_Gone]),
	    S#st{deleted_orphans = New_dels};
	false ->
	    really_do_delete(Table, Key, S)
    end.

really_do_delete(Table, Key, S) ->
    dbg("delete: ~p: ~p", [Table, Key]),
    Path_list = type_to_path_list(Table, Key, S),
    Url = mk_url(path_join([?RESTBASE | Path_list]), S),
    case rest_delete(Url, S) of
	ok ->
	    remove_ecim_orphans(Table, Key, S);
	_ ->
	    S
    end.

make_ecim_data(Record, S) ->
    {Type, Json_enc} = ecim_to_intermediate(Record, S),
    Path_list = lists:droplast(ecim_record_to_path(Record, S)),
    Url = mk_url(path_join([?RESTBASE | Path_list]), S),
    {Type, Url, Json_enc}.

rest_create(Url, Json, _S) ->
    rest_debug(post, Url, Json),
    Headers = [],
    Content_type = "application/json",
    Http_opts = [],
    Opts = [{body_format, binary}],
    R = httpc:request(post, {Url, Headers, Content_type, Json},
		      Http_opts, Opts, ?PROFILE),
    dbg("Post Result: ~p", [R]),
    process_post_result(R).

process_post_result({ok, {{_http11, 201, "Created"}, _, Json}}) ->
    try jsone:decode(Json, [{object_format, proplist}]) of
	Proplist ->
	    result_debug(post, 201, Json),
	    {ok, proplists:get_value(<<"id">>, Proplist)}
    catch
	_:_ = Err ->
	    {error, Err}
    end;
process_post_result(Bad) ->
    dbg("Got negative response from post: ~n~p", [Bad]),
    {error, error}.

rest_update(Url, Json, _S) ->
    rest_debug(put, Url, Json),
    Headers = [],
    Content_type = "application/json",
    Http_opts = [],
    Opts = [{body_format, binary}],
    R = httpc:request(put, {Url, Headers, Content_type, Json},
		      Http_opts, Opts, ?PROFILE),
    dbg("Put Result: ~p", [R]),
    process_put_result(R).

process_put_result({ok, {{_http11, 200, "OK"}, _, Json}}) ->
    try jsone:decode(Json, [{object_format, proplist}]) of
	Proplist ->
	    result_debug(put, 200, Json),
	    {ok, proplists:get_value(<<"id">>, Proplist)}
    catch
	_:_ = Err ->
	    {error, Err}
    end;
process_put_result(Bad) ->
    dbg("Got negative response from put: ~n~p", [Bad]),
    {error, error}.

rest_delete(Url, _S) ->
    rest_debug(delete, Url, undefined),
    Headers = [],
    Content_type = "application/json", %makes no sense?
    Http_opts = [],
    Opts = [{body_format, binary}],
    R = httpc:request(delete, {Url, Headers, Content_type, []},
		      Http_opts, Opts, ?PROFILE),
    dbg("Delete Result: ~p", [R]),
    process_delete_result(R).

process_delete_result({ok, {{_http11, 200, "OK"}, _, Json}}) ->
    result_debug(delete, 200, Json),
    ok;
process_delete_result(Bad) ->
    error_msg("Got negative response from delete: ~n~p", [Bad]),
    {error, error}.

rest_get(Url, _S) ->
    rest_debug(get, Url, undefined),
    Headers = [],
    %Content_type = "application/json", %?
    %Http_opts = [{url_encode, true}],
    Http_opts = [],
    Opts = [{body_format, binary}],
    R = httpc:request(get, {Url, Headers},
		      Http_opts, Opts, ?PROFILE),
    dbg("Get Result: ~p", [R]),
    process_get_result(R).

process_get_result({ok, {{_http11, 200, "OK"}, _, Json}}) ->
    try jsone:decode(Json, [{object_format, proplist}]) of
	Proplist ->
	    result_debug(get, 200, Json),
	    {ok, Proplist}
    catch
	_:_ = Err ->
	    {error, Err}
    end;
process_get_result(Bad) ->
    dbg("Got negative response from get: ~n~p", [Bad]),
    {error, error}.

mk_url(Path, #st{db_ip = Ip, db_port = Port}) ->
    %%"http://" ++ inet:ntoa(Ip) ++ ":" ++ integer_to_list(Port) ++ Path.
    %Encoded = http_uri:encode(Path), %doesn't work - just do the space
    Encoded = re:replace(Path, [$\s], "%20", [global, {return, list}]),
    "http://" ++ inet:ntoa(Ip) ++ ":" ++ integer_to_list(Port) ++ Encoded.

get_all_top_objects(S) ->
    All_ids =
	[begin
	 Path = ?RESTBASE ++ atom_to_list(Type),
	 Url = mk_url(Path, S),
	 case rest_get(Url, S) of
	    {ok, Proplist} ->
		extract_id(Type, Proplist);
	    {error, Err} ->
		error_msg("Failed to GET object listing for: ~p, ~p",
			  [Type, Err]),
		[]
	 end
	 end || Type <- get_top_objects(S)],
    lists:flatten(All_ids).

extract_id(Type, [{_Types, Id_name_list}]) ->
    [{Type, plg(<<"name">>, Object), plg(<<"id">>, Object)} ||
	Object <- Id_name_list];
extract_id(_, _) -> [].

erase_objects(To_erase, S) ->
    [begin
	String_id = binary_to_list(Id),
	String_name = binary_to_list(Name),
	Path = ?RESTBASE ++ atom_to_list(Type) ++ "/" ++
		   String_id,
	Url = mk_url(Path, S),
	case rest_delete(Url, S) of
	    ok ->
		dbg("Erased: ~p/~p: ~p", [Type, String_name, String_id]),
		ok;
	    {error, Err} ->
		error_msg("Failed to erase ~p/~p: ~p, ~p",
			  [Type, String_name, String_id, Err]),
		ok
	end
    end || {Type, Name, Id} <- To_erase],
    ok.

rest_debug(Http_action, Url, Json_or_undef) ->
    catch rest_debug(Http_action, Url, Json_or_undef, get(rest)).
rest_debug(Http_action, Url, Json_or_undef, true) ->
    case Http_action of
	post ->
	    io:format("~w: POST: ~s~n", [?MODULE, Url]),
	    io:format("~w: ~s~n", [?MODULE, pretty_print(Json_or_undef)]);
	put ->
	    io:format("~w: PUT: ~s~n", [?MODULE, Url]),
	    io:format("~w: ~s~n", [?MODULE, pretty_print(Json_or_undef)]);
	delete ->
	    io:format("~w: DELETE: ~s~n", [?MODULE, Url]);
	get ->
	    io:format("~w: GET: ~s~n", [?MODULE, Url])
    end;
rest_debug(_, _, _, _) -> ok.

result_debug(Http_action, N, Json) ->
    result_debug(Http_action, N, Json, get(rest)).
result_debug(Http_action, N, Json, true) ->
    catch io:format("~w: ~p -> ~w~n~s~n",
		    [?MODULE, Http_action, N, pretty_print(Json)]);
result_debug(_, _, _, _) -> ok.

pretty_print(Json) ->
    Decoded = jsone:decode(Json),
    jsone:encode(Decoded, [{indent, 1},{space,2}]).

list_object(Type, S) when is_atom(Type) ->
    list_object(atom_to_list(Type), S);
list_object(Type, S) when is_list(Type) ->
    Path = ?RESTBASE ++ Type,
    Url = mk_url(Path, S),
    case rest_get(Url, S) of
	{ok, Proplist} ->
	    {Type, Proplist};
	{error, Err} ->
	    error_msg("Failed to GET object listing for: ~p, ~p", [Type, Err]),
	    {Type, error}
    end.

return_tnid(S) ->
    Top_objects = get_top_objects(S),
    return_tnid([], Top_objects, S).

return_tnid(_Path, [], _S) -> [];
return_tnid(Path, [H | T], S) ->
    dbg("return_tnid/3, Path: ~p, Head: ~p, Tail: ~p", [Path, H, T]),
    Url = mk_url(path_join([?RESTBASE, Path, H]), S),
    case rest_get(Url, S) of
%	{ok, [{_, []}] = Gotten} ->
%	    dbg("return_tnid/3, result: ~p", [Gotten]),
%	    [{H, "no objects"} | return_tnid(Path, T, S)];
	{ok, [{_, Id_name_list}] = Gotten} ->
	    dbg("return_tnid/3, result: ~p", [Gotten]),
	    [{H, [begin
		 {Name, Id} = name_id_from_propl(Propl),
		 Next_path = path_join([Path, H, Name]),
		 Sub_objects = get_sub_objects(H, S),
		 dbg("return_tnid/3, Sub_objects: ~p", [Sub_objects]),
		 {Name, Id, return_tnid(Next_path, Sub_objects, S)}
	      end || Propl <- Id_name_list, Propl =/= {}]} |
		    return_tnid(Path, T, S)];
	{ok, [{}]} ->
	    return_tnid(Path, T, S);
	{error, Err} ->
	    error_msg("Failed to GET object listing for: ~p -> ~p", [Url, Err]),
	    return_tnid(Path, T, S)
    end.

name_id_from_propl(Propl) ->
    {binary_to_list(proplists:get_value(<<"name">>, Propl)),
     proplists:get_value(<<"id">>, Propl)}.


get_sub_objects(Path, _S) ->
    Type = lists:last(filename:split(Path)),
    Match_spec =
	[{{comsaEcimRelations, {?NR_MODEL, rest_to_ecim(Type)}, {'_', '$2'}},
	  [], ['$2']}],
    Ecim_objects = mnesia:dirty_select(comsaEcimRelations, Match_spec),
    [ecim_to_rest(E) || E <- Ecim_objects].

%Returns: [{Type, Json_encoded_attribs}...]
% no longer list will have more than one entry only for the slice with ppu/ueu
ecim_to_intermediate(Ecim_record, S) ->
    {Type ,Attributes} = Dbg =
	create_intermediate_object_attributes(Ecim_record, S),
    dbg("Object intermediate attributes: ~p", [Dbg]),
    {Type, mk_json(Attributes)}.

mk_json(Ntv_list) when is_list(Ntv_list) ->
    %%this will skip any undefined attribute, i.e. anything that wasn't
    %%configured (since it was defined as optional in the model) will be
    %%skipped, this may not work towards the ReST service?
    %simple solution: update the model by making the attributes non optional
    Intermediate =
	[{Name, etm(Type, Value)} || {Name, Type, Value} <- Ntv_list,
	 Value =/= undefined],
    jsone:encode(Intermediate).

etm(boolean, Value) ->
    Value;
etm('moRef', Value) when is_binary(Value) -> %%confusing, is it bin or list?
    Value;
%etm('moRef', Value) when is_list(Value) -> %%confusing, is it bin or list?
%    list_to_binary(Value);
etm('string', Value) ->
    list_to_binary(Value);
etm('uint16', Value) ->
    Value;
etm('int32', Value) ->
    Value;
etm('int64', Value) ->
    Value;
etm({struct, Struct_name}, Record) ->
    fake_struct(Struct_name, Record);
etm({sequence, Seq_name}, Sequence) ->
    fake_seq(Seq_name, Sequence);
etm(Unknown, Value) ->
    dbg("Looking up unknown type; ~p [~p]", [Unknown, Value]),
    case mnesia:dirty_read(comsaEcimTypes, Unknown) of
	[{_,_, Type}] ->
	    etm(Type, Value);
	Err ->
	    dbg("Failed looking up type: ~p -> ~p", [Unknown, Err]),
	    erlang:error({Unknown, Err})
    end.

ecim_id_to_ntv(Ecim_id) ->
    Last_elem = element(tuple_size(Ecim_id), Ecim_id),
    {name, string, Last_elem}.

plg(Property, Property_list) -> proplists:get_value(Property, Property_list).

%workaround for not adding absolut path
path_join([[]| Rest]) ->
    path_join(Rest);
path_join(List) ->
    filename:join(List).

lookup_rest_id(Type, Key, S) ->
    Path_list = type_to_path_list(Type, Key, S),
    Url = mk_url(path_join([?RESTBASE | Path_list]), S),
    case rest_get(Url, S) of
	{ok, Prop_list} ->
	    Attrib_list = plg(list_to_binary(atom_to_list(Type)), Prop_list),
	    Bin_id = plg(<<"id">>, Attrib_list),
	    {ok, Bin_id};
	Error ->
	    Error
    end.

%return a list of elements corresponding to a path in ReST
%{#sau{}, {"1", "1", "Vip", "Extra"}} -> ["slice", "Vip", "sau", "Extra"] 
ecim_record_to_path(Ecim_rec, S)  ->
    Name = get_ecim_type_from_record(Ecim_rec),
    Id = get_ecim_id_from_record(Ecim_rec),
    type_to_path_list(Name, Id, S).

type_to_path_list(Ecim_type, Ecim_id, S) ->
    Id_list = tl(tl(tuple_to_list(Ecim_id))),
    {_Ecim_type, Path_list} = lists:keyfind(Ecim_type, 1, S#st.tree),
    combine(Path_list, Id_list).

combine([], []) -> [];
combine([H | T], [H1 | T1]) ->
    [H, H1 | combine(T, T1)].

get_all_nr_tables() ->
    Objects = comsaEcimModelAdaptor:get_model_classes(?NR_MODEL),
    [list_to_atom(uncapitalize(O)) || O <- Objects].

uncapitalize([Cap | Rest]) when Cap >= 65, Cap =< 90 ->
    [Cap + $\s | Rest];
uncapitalize(Not_cap) ->
    Not_cap.

capitalize([Lower | Rest]) when Lower >= 97, Lower =< 122 ->
    [Lower - $\s | Rest];
capitalize(Capped) ->
    Capped.

%return: New_s
look_for_agent_address(
	{write, _, #nrFunction{oamAgentAddress = Oam}, _, _}, S)
	when Oam =/= undefined ->
    #'IpAddressAndPort'{ipAddress = Ip, port = Port} = Oam,
    case inet:parse_address(Ip) of
	{ok, Ip_tuple} when is_integer(Port) ->
	info_msg("Got agent address (ReST db): ~p:~p", [Ip, Port]),
	    S#st{db_ip =  Ip_tuple, db_port = Port};
	_ ->
	    S
    end;

look_for_agent_address(_, S) -> S.
get_rest_service_info() ->
%    {{10, 68, 32, 244}, 8080}.
    Fun = fun() -> mnesia:read(?HEAD_TABLE, {"1", "1"}) end,
    case mnesia:transaction(Fun) of
	{atomic, [#nrFunction{oamAgentAddress = undefined}]} ->
	    dbg("Read agent address (ReST db) is not set", []),
	    {undefined, undefined};
	{atomic, [#nrFunction{oamAgentAddress = Oam}]} ->
	    dbg("Read agent address (ReST db): ~p", [Oam]),
	    #'IpAddressAndPort'{ipAddress = Ip, port = Port} = Oam,
	    case inet:parse_address(Ip) of
		{ok, Ip_tuple} when is_integer(Port) -> {Ip_tuple, Port};
		_ -> {undefined, undefined}
	    end;
	Fail ->
	    error_msg("Failure reading agent address (ReST db): ~p", [Fail]),
	    {undefined, undefined}
    end.

fake_seq(Seq_name, Value) ->
    dbg("Looking up unknown sequence type; ~p [~p]", [Seq_name, Value]),
    case mnesia:dirty_read(comsaEcimTypes, Seq_name) of
	[{_,_, Type}] ->
	    [etm(Type, V) || V <- Value];
	Err ->
	    dbg("Failed looking up sequence type: ~p -> ~p", [Seq_name, Err]),
	    erlang:error({Seq_name, Err})
    end.

fake_struct(Name, Record) ->
    [_Rec_name | Value_list ] = tuple_to_list(Record), %CHEAT
    Dn_rev = [<<>>, <<?NR_MODEL>>], %strange
    Type_list = comsaEcimModelAdaptor:get_struct_fields(Dn_rev, Name),
    lists:zipwith(fun struct_to_intermediate/2, Type_list, Value_list).

struct_to_intermediate({Name, Type}, Value) ->
    Json_value = etm(Type, Value),
    Rest_name = list_to_atom(ecim_to_rest(Name)),
    {Rest_name, Json_value}.

create_intermediate_object_attributes(Record, S) ->
    [Rec_type, Id | Value_list ] = tuple_to_list(Record), %CHEAT
    Rest_type = list_to_atom(ecim_to_rest(Rec_type)),
    %Rst = record_name_to_type(Rec_type),
    Rst = capitalize(atom_to_list(Rec_type)),
    {[_Id_type | Type_info], _} =
	comsaEcimModelAdaptor:get_class_types(?NR_MODEL,Rst),
    case Type_info of
	[] -> %object with only Id (no ReST attributes)
	    {Rest_type, [ecim_id_to_ntv(Id)]};
	_ ->
	    Combined = lists:zip(Type_info, Value_list),
	    Fun = fun(E, Acc) -> object_to_intermediate(E, Acc, S) end,
	    {Rest_type, [ecim_id_to_ntv(Id) |
		lists:foldl(Fun, [], Combined)]}
		%lists:foldl(fun object_to_intermediate/2, [], Combined)]}
    end.

object_to_intermediate({{Name, 'moRef', readWrite, _}, Value}, Acc, S) ->
    dbg("Preparing to fudge moRef: ~p=~p", [Name, Value]),
    Rest_id = moref_to_restid(Value, S),
    [{list_to_atom(ecim_to_rest(Name)), 'moRef', Rest_id} | Acc];
object_to_intermediate({{Name, Type, readWrite, _}, Value}, Acc, _S) ->
    [{list_to_atom(ecim_to_rest(Name)), Type, Value} | Acc];
object_to_intermediate(_, Acc, _) -> Acc.

ecim_to_rest(Obj_name) when is_atom(Obj_name) ->
    ecim_to_rest(atom_to_list(Obj_name));
ecim_to_rest([H | T]) when H >= 65, H =< 90 ->
    [H + $\s | eotrn(T)];
ecim_to_rest([H | T]) ->
    [H | eotrn(T)].

eotrn([H | T]) when H >= 65, H =< 90 ->
    [$_, H + $\s | eotrn(T)];
eotrn([H | T]) ->
    [H | eotrn(T)];
eotrn([]) -> [].

%rest_to_ecim([Lower | Rest]) when Lower >= 97, Lower =< 122 ->
%Example: srb_logical_channel -> SrbLogicalChannel
rest_to_ecim([Lower | Rest]) ->
    [Lower - $\s | rte(Rest)].

rte([$_, Lower | Rest]) ->
    [Lower - $\s | rte(Rest)];
rte([Any | Rest]) ->
    [Any | rte(Rest)];
rte([]) -> [].

%Input is a binary, eg: <<"ManagedElement=1,NrFunction=1,Mme=mimmi">>
%Output, ReST id as binary, eg: <<"faba9995552">>
moref_to_restid(undefined, _S) -> undefined;
moref_to_restid(Moref, S) ->
    Type = moref_to_type(Moref),
    Ecim_id = comsaGeneric:mo_to_key(Moref),
    case lookup_rest_id(Type, Ecim_id, S) of
        {ok, Rest_id} ->
	    dbg("Reference looked up, ~p:~p -> ~p", [Type, Ecim_id, Rest_id]),
	    Rest_id;
        Err ->
            mismatch("Referenced ~p object-id: ~p not found in ReST db: ~p",
                     [Type, Ecim_id, Err]),
            <<"deadbeef">>
    end.

moref_to_type(Moref) ->
    %this is silly, but I'm tired
    Parts = binary:split(Moref, [<<",">>, <<"=">>], [global]),
    Rev_parts = lists:reverse(Parts),
    Last_type = binary_to_list(hd(tl(Rev_parts))),
    list_to_atom(uncapitalize(Last_type)).

get_ecim_id_from_record(Record) ->
    element(2, Record). %CHEAT
get_ecim_type_from_record(Record) ->
    element(1, Record). %CHEAT

%return: New_s
remove_ecim_orphans(Table, Key, S) ->
    dbg("Removing ECIM orphans for: ~p:~p", [Table, Key]),
    Rest_object = ecim_to_rest(Table),
    remove_orphans(Rest_object, Key, S).

remove_orphans(Rest_object, Key, S) ->
    Sub_objects = get_sub_objects(Rest_object, S),
    Wild = extend_wild_key(Key),
    remove_orp(Sub_objects, Wild, S).

remove_orp([], _, S) -> S;
remove_orp([H | T], Wild, S) ->
    Table = rest_to_table(H),
    Pattern = mnesia:table_info(Table, wild_pattern),
    Match = setelement(2, Pattern, Wild), %CHEAT
    dbg("remove_orphans([~p | ~p], ~p, S)~nmatch: ~p~n",
	[H, T, Wild, Match]),
    Fun =
	fun() ->
	    case mnesia:match_object(Match) of
		[Delete_me] ->
		    mnesia:delete_object(Delete_me),
		    {deleted, Delete_me};
		_ ->
		    ok
	    end
	end,
    case mnesia:transaction(Fun) of
	{atomic, {deleted, Deleted}} ->
	    dbg("deleted: ~p", [Deleted]),
	    Gone = {erlang:monotonic_time(millisecond),
		    {Table, get_ecim_id_from_record(Deleted)}},
	    Dels = [Gone | S#st.deleted_orphans],
	    New_s = remove_orphans(H, Wild, S#st{deleted_orphans = Dels}),
	    remove_orp(T, Wild, New_s);
	{atomic, ok} ->
	    remove_orp(T, Wild, S);
	Err ->
	    mismatch("Failure deleting match_object: ~p -> ~p", [Match, Err]),
	    remove_orp(T, Wild, S)
    end.

extend_wild_key(Key) ->
    erlang:append_element(Key, '_').

rest_to_table(Rest_object) ->
    list_to_atom(uncapitalize(rest_to_ecim(Rest_object))).

% Example (V2):
%[
% {mme,["mme"]},
% {radioConfig,["radio_config"]},
% {phys,["radio_config","phys"]},
% {mac,["radio_config","mac"]},
% {srbPdcp,["radio_config","srb_pdcp"]},
% {drbPdcp,["radio_config","drb_pdcp"]},
% {srbLogicalChannel,["radio_config","srb_logical_channel"]},
% {drbLogicalChannel,["radio_config","drb_logical_channel"]},
% {bpu,["bpu"]},
% {tp,["bpu","tp"]}
% {domain,["domain"]},
% {vsa,["domain","vsa"]},
% {vpp,["domain","vpp"]},
% {sa,["domain","sa"]},
% {vue,["domain","vue"]},
%]

%object tree creation
get_object_tree() ->
%    Match_spec =
%	[{{comsaEcimRelations, {?NR_MODEL, '$1'}, {'_', '$2'}},
%	  [{'=/=', '$1', ?NR_MODEL}], [{{'$1', '$2'}}]}],
    Match_spec =
	[{{comsaEcimRelations, {?NR_MODEL, '$1'}, {'_', '$2'}},
	  [], [{{'$1', '$2'}}]}],
    Rels = mnesia:dirty_select(comsaEcimRelations, Match_spec),
    %io:format("Tuples in comsaEcimRelations: ~p~n", [Rels]),
    relation_tuples_to_tree(Rels, Rels, []).

%object tree creation
relation_tuples_to_tree([], _, Result) ->
    Result;
relation_tuples_to_tree([H | T], R_tuples, Result) ->
    Branch = build_branch(H, R_tuples),
    relation_tuples_to_tree(T, R_tuples, [Branch | Result]).
    
build_branch({?NR_MODEL, Child}, _R_tuples) ->
    {list_to_atom(uncapitalize(Child)), [ecim_to_rest(Child)]};
build_branch({Parent, Child}, R_tuples) ->
    build_branch(Child, Parent, R_tuples,
		 [ecim_to_rest(Parent), ecim_to_rest(Child)]).

%object tree creation
build_branch(Child, Above, R_tuples, Branch) ->
    case lists:keyfind(Above, 2, R_tuples) of
        {?NR_MODEL, _Above} ->
	    {list_to_atom(uncapitalize(Child)), Branch};
	{PP, _Above} ->
	    build_branch(Child, PP, R_tuples, [ecim_to_rest(PP) | Branch]);
	false ->
	    {list_to_atom(uncapitalize(Child)), Branch}
    end.
	
get_top_objects(S) ->
    [P || {_, [P]} <- S#st.tree].

check_top_objects(Types, S) ->
    Tops = get_top_objects(S),
    lists:foldl(
	fun(E, Acc) ->
	    case lists:member(E, Tops) of
		true ->
		    [E | Acc];
		false ->
		    error_msg("~p: Not a top object - filtered out", [E]),
		    Acc
	    end
	end, [], Types).
		    

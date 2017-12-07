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
%% File: safs_imm_xml.erl
%% 
%% Description:
%%
%%--------------------------------------------------------------------
-module(safs_imm_xml).
%%-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("safs_imm_db.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 parse_file/2,
	 event/3,
	 to_type/2
	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
	 test/0
        ]).

%%======================================================================
%% Macros
%%======================================================================
%%----------------------------------------------------------------------
%% Error handling
%%----------------------------------------------------------------------
-define(error(Reason), 
	throw({safs_imm_xml, Reason})).

%%======================================================================
%% Records
%%======================================================================
-record(state, {stack=[], classes=gb_trees:empty(), objects=[]}).

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: parse_file(File, Options) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
parse_file(File, Options0) ->
    {Options2, State} = parse_options(Options0),
    Options = [skip_external_dtd, {event_fun, fun event/3}, {event_state, State}|Options2],
    try xmerl_sax_parser:file(File, Options) of
	{ok, #state{classes=Classes, objects=Objects}, _} ->
	   {gb_trees:values(Classes), lists:reverse(Objects)};
	{error, {_, Error}} ->
	    error_logger:error_msg("~p ~s:~p: ~p\n\n", [?MODULE, File, 0, Error]), 
	    %%io:format("~n~s:~p: ~p~n",[File, 0, Error]),
	    {error, Error, {File,0}}
    catch
	{event_receiver_error, _, {error, {_, _, Line}, Error}} ->
	    io:format("~n~s:~p: ~p~n",[File, Line, Error]),
	    {error, Error, {File,Line}}
    end.

%%----------------------------------------------------------------------
%% Function: event(Event, LineNo, State) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
event(Event, LineNo, State) ->
    try parse(Event, State)
    catch Error ->
	    throw({error, LineNo, Error});
	  error:Error ->
	    throw({error, LineNo, {Error, erlang:get_stacktrace()}})
    end.

%%----------------------------------------------------------------------
%% Function: to_type(Value, Type) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
to_type(Value, sa_imm_attr_saint32t)  -> list_to_integer(Value);
to_type(Value, sa_imm_attr_sauint32t) -> list_to_integer(Value);
to_type(Value, sa_imm_attr_saint64t)  -> list_to_integer(Value);
to_type(Value, sa_imm_attr_sauint64t) -> list_to_integer(Value);
to_type(Value, sa_imm_attr_sastringt) -> unicode:characters_to_binary(Value);
to_type(Value, sa_imm_attr_sanamet)   -> unicode:characters_to_binary(Value);
to_type(Value, sa_imm_attr_satimet)   -> list_to_integer(Value);
to_type(Value, sa_imm_attr_safloatt)  -> list_to_float(Value);
to_type(Value, sa_imm_attr_sadoublet) -> list_to_float(Value);
to_type(Value, sa_imm_attr_saanyt)    -> Value.


%%======================================================================
%% Internal functions
%%======================================================================
parse_options(Options) ->
    {Options, #state{}}.

parse({ignorableWhitespace, _}, S) -> S;
parse({characters, Chars}, S = #state{stack=Stack}) ->
    S#state{stack=[Chars|Stack]};

parse({startElement, _, "name", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[name|Stack]};
parse({endElement, _, "name", _}, S = #state{stack=[Val,name|Stack]}) ->
    S#state{stack=[{name, Val}|Stack]};

parse({startElement, _, "value", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[value|Stack]};
parse({endElement, _, "value", _}, S = #state{stack=[Val,value|Stack]}) ->
    S#state{stack=[{value, Val}|Stack]};
parse({endElement, _, "value", _}, S = #state{stack=[value|Stack]}) ->
    S#state{stack=[{value, ""}|Stack]};

parse({startElement, _, "dn", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[dn|Stack]};
parse({endElement, _, "dn", _}, S = #state{stack=[Val,dn|Stack]}) ->
    S#state{stack=[{dn, Val}|Stack]};

parse({startElement, _, "type", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[type|Stack]};
parse({endElement, _, "type", _}, S = #state{stack=[Val,type|Stack]}) ->
    S#state{stack=[{type, Val}|Stack]};

parse({startElement, _, "flag", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[flag|Stack]};
parse({endElement, _, "flag", _}, S = #state{stack=[Val,flag|Stack]}) ->
    S#state{stack=[{flag, Val}|Stack]};

parse({startElement, _, "category", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[category|Stack]};
parse({endElement, _, "category", _}, S = #state{stack=[Val,category|Stack]}) ->
    S#state{stack=[{category, Val}|Stack]};

parse({startElement, _, "default-value", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[default|Stack]};
parse({endElement, _, "default-value", _}, S = #state{stack=[Val,default|Stack]}) ->
    S#state{stack=[{default, Val}|Stack]};
parse({endElement, _, "default-value", _}, S = #state{stack=[default|Stack]}) ->
    S#state{stack=[{default, ""}|Stack]};

parse({startElement, _, "attr", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[attr|Stack]};
parse({endElement, _, "attr", _}, S = #state{stack=Stack0}) ->
    {Stack, Attr} = make_attr(Stack0, #imm_attr{}),
    S#state{stack=[Attr|Stack]};

parse({startElement, _, "rdn", _, _}, S = #state{stack=Stack}) ->
    S#state{stack=[rdn|Stack]};
parse({endElement, _, "rdn", _}, S = #state{stack=Stack0}) ->
    {Stack, Rdn} = make_rdn(Stack0, #imm_rdn{}),
    S#state{stack=[Rdn|Stack]};

parse({startElement, _, "class", _, [{_, _, "name", Name}]},
      S = #state{stack=Stack}) ->
    S#state{stack=[{name,Name}, class|Stack]};
parse({endElement, _, "class", _}, S = #state{stack=Stack0, classes=Cs}) ->
    {Stack, Class} = make_class(Stack0, #imm_class{}),
    S#state{stack=Stack, classes=gb_trees:insert(Class#imm_class.name, Class,Cs)};

parse({startElement, _, "object", _, [{_, _, "class", Class}]},
      S = #state{stack=Stack}) ->
    S#state{stack=[{classname,Class}, object|Stack]};
parse({endElement, _, "object", _}, S = #state{stack=Stack0, objects=Os}) ->
    {Stack, Obj} = make_object(Stack0, #imm_object{}),
    S#state{stack=Stack, objects=[Obj|Os]};

parse({endElement, _, _, _, _}, S) -> S;

parse({_, _, "IMM-contents", _, _}, S) -> S;
parse(startDocument, State) -> State;
parse(endDocument, State) ->   State;

parse({startElement, _, Tag, _, _}, #state{stack=Curr=[_|_]}) ->
    throw({unhandled, start, Tag, Curr});

parse({comment, _}, S) -> S;
parse(_Ignored, S=#state{stack=[]}) -> S;
parse(What, #state{stack=Curr}) ->
    throw({unhandled, What, Curr}).

make_class([{name, Value}|R], T) ->
    make_class(R, T#imm_class{name=list_to_atom(Value)});
make_class([Value=#imm_rdn{}|R], T)  ->
    make_class(R, T#imm_class{rdn=Value});
make_class([{category, Value}|R], T) ->
    make_class(R, T#imm_class{category=category(Value)});
make_class([Value=#imm_attr{}|R], T=#imm_class{attrs=Vs}) ->
    make_class(R, T#imm_class{attrs=[Value|Vs]});
make_class([class|Stack], T) ->
    {Stack, T}.

make_rdn([{name, Value}|R], T) ->
    make_rdn(R, T#imm_rdn{name=list_to_atom(Value)});
make_rdn([{type, Value}|R], T) ->
    make_rdn(R, T#imm_rdn{type=type(Value)});
make_rdn([{category, Value}|R], T) ->
    make_rdn(R, T#imm_rdn{category=category(Value)});
make_rdn([{flag, Value}|R], T=#imm_rdn{flags=Fs}) ->
    make_rdn(R, T#imm_rdn{flags=[flag(Value)|Fs]});
make_rdn([rdn|Stack], T) ->
    {Stack, T}.

make_attr([{name, Value}|R], T) ->
    make_attr(R, T#imm_attr{name=list_to_atom(Value)});
make_attr([{value, Value}|R], T=#imm_attr{values=Vs}) ->  %% For objects
    make_attr(R, T#imm_attr{values=[Value|Vs]});
make_attr([{type, Value}|R], T) ->
    make_attr(R, T#imm_attr{type=type(Value)});
make_attr([{category, Value}|R], T) ->
    make_attr(R, T#imm_attr{category=category(Value)});
make_attr([{flag, Value}|R], T=#imm_attr{flags=Fs}) ->
    make_attr(R, T#imm_attr{flags=[flag(Value)|Fs]});
make_attr([{default, Value}|R], T) ->
    make_attr(R, T#imm_attr{default=Value});
make_attr([attr|Stack], T) ->
    {Stack, convert_default(T)}.

make_object([{classname, Value}|R], T) ->
    make_object(R, T#imm_object{class=list_to_atom(Value)});
make_object([{dn, Value}|R], T) ->
    make_object(R, T#imm_object{dn=unicode:characters_to_binary(Value)});
make_object([#imm_attr{name=Name, values=Vs}|R], T=#imm_object{attrs=As}) ->
    make_object(R, T#imm_object{attrs=[{Name,undefined,Vs}|As]});
make_object([object|R], T) ->
    {R, T}.

category("SA_RUNTIME") -> runtime;
category("SA_CONFIG")  -> config;
category(UnknownCategory) ->
    ?error({"Unknown category", UnknownCategory}).

flag("SA_RDN")           -> rdn;
flag("SA_INITIALIZED")   -> initialized;
flag("SA_MULTI_VALUE")   -> multi_value;
flag("SA_WRITABLE")      -> writable;
flag("SA_CACHED")        -> cached;
flag("SA_PERSISTENT")    -> persistent;
flag("SA_NO_DANGLING")   -> no_dangling;
flag("SA_NOTIFY")        -> notify;
flag("SA_NO_DUPLICATES") -> no_duplicates;
flag(UnknownFlag)        -> 
    ?error({"Unknown attribute flag", UnknownFlag}).

type("SA_INT32_T")  -> sa_imm_attr_saint32t;
type("SA_UINT32_T") -> sa_imm_attr_sauint32t;
type("SA_INT64_T")  -> sa_imm_attr_saint64t;
type("SA_UINT64_T") -> sa_imm_attr_sauint64t;
type("SA_STRING_T")  -> sa_imm_attr_sastringt;
type("SA_NAME_T")    -> sa_imm_attr_sanamet;
type("SA_TIME_T")    -> sa_imm_attr_satimet;
type("SA_FLOAT_T")   -> sa_imm_attr_safloatt;
type("SA_DOUBLE_T")  -> sa_imm_attr_sadoublet;
type("SA_ANY_T")     -> sa_imm_attr_saanyt;
type(UnknownType) ->
    ?error({"Unknown attribute type", UnknownType}).

convert_default(A=#imm_attr{default=undefined}) -> A;
convert_default(A=#imm_attr{default=Default, type=Type}) ->
    A#imm_attr{default=to_type(Default, Type)}.

test() ->
    {Cs, Os} = parse_file("/home/dgud/Desktop/imm.xml", []),
    io:format("Classes ~p Objects ~p~n", [length(Cs), length(Os)]),
    ok.


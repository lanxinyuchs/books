%%
%% %CopyrightBegin%
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
%%% %CCaseFile:	rct_mo_handler_lib.erl %
%%% Author: ekurnik
%%% Description: MO handling library, provides easy way to manipulate 
%%%              MOs via netconf calls.
%%%
%%% Current known limitations:
%%%     - When getting sequence attribute with single member it will not returned in list
%%%     - When getting any MO, null attributes are not returned.
%%%     - When explicitly getting an attribute, if it's not returned it will default to undefined 
%%%       (even if it doesn't exist in MO)
%%%     - No support for schema (would fix all of the above problems)
%%%
%%% ----------------------------------------------------------

-module(rct_mo_handler_lib).
-vsn('/main/R10A/R11A/1').
-author('ekurnik').

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
%%% -----      ---------  --------    ------------------------
%%% R10A/1   2017-06-10   ekurnik      Created
%%% R10A/2   2017-06-12   ekurnik      Fixed decoding undefined attributes
%%% R10A/3   2017-06-14   ekurnik      Moved some code from cert lib
%%% R10A/4   2017-06-17   ekurnik      Minor fix in setting struct attrs
%%% R11A/1   2017-08-03   ekurnik      Improved checking action finished
%%%--------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

%% main interfaces functions
-export([exists_mo/2, get_mo/2, get_mo/3, get_children_mo/2, get_children_mo/3, 
         set_mo/3, create_mo/3, delete_mo/2, 
         action_mo/4, async_action_mo/4, async_action_mo/5,
         get_mo_id/1]).

%% use netconf directly
-export([create_netconf_config/3, netconf/3]).

%% directly call actions
-export([get_progress_struct/3, wait_for_progress/4, wait_for_progress/5]).

%% for test:
-export([suite/0, all/0, test/1]).


%% type spec
-type handler() :: {netconf, atom()}.
-type dn() :: string().
-type attrs() :: [{atom(), term()}] | #{atom() => term()}.

-define (OPER_DELETE, [{'xc:operation', "delete"}]).

suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
     {rct_netconf,{nc1, html}},
     {cth_conn_log,[]},
     {rct_core,[]},
     {rct_logging,
         {all,
             [{erlang,
                     {["ERROR REPORT", "CRASH REPORT"], 
                      []}}]}}
    ]}].

all() ->
    [test].

%% Used for testing the library
test(_Config) ->
    
    
    %% "Unit" test
    DN = "ManagedElement=1,A=2,B=3,TestMo=Bla",
    
    "TestMo=Bla" = to_ldn(DN),
    "ManagedElement=1,A=2,B=3" = parent_dn(DN),
    testMoId = mo_to_id("TestMo"),
    [{"TestMo", "Bla"}, {"B", "3"}, {"A", "2"}, {"ManagedElement", "1"}] = encode_dn(DN),
    DN = add_root_mo("A=2,B=3,TestMo=Bla"),
    
    Attrs = #{userLabel => "label", subjectName => "CN=commonName", 
              keyInfo => "RSA_128"},
    
    Attrs2 = [{a,"1"},{b,["3","2","1"]},{c,[{ab,"22"},{cd,"44"}]}],
    
    ct:pal("Original: ~p~nEncode + decode:~p~n", [Attrs2, decode_attr_config(create_attr_config(Attrs2))]),
    
    %% Test config creation
    {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],["1"]},
         {'SystemFunctions', [],
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{secMId,[],["1"]},
             {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{certMId,[],["1"]},
               {'NodeCredential', [],
                [{nodeCredentialId, [], ["1"]},
                    {keyInfo, [], ["RSA_128"]},
                    {subjectName, [], ["CN=commonName"]},
                    {userLabel, [], ["label"]}
                     ]}]}]}]}]}
        =
        create_netconf_config("ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1", Attrs),
    
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],["1"]},
         {'SystemFunctions', [],
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{secMId,[],["1"]},
             {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{certMId,[],["1"]},
               {'NodeCredential', [],
                [{nodeCredentialId, [], ["1"]}]}]}]}]}]}
        =
        create_netconf_config("ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1", []),
    
    
      {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],["1"]},
         {'SystemFunctions', [],
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{secMId,[],["1"]},
             {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{certMId,[],["1"]},
               {'NodeCredential', ?OPER_DELETE,
                [{nodeCredentialId, [], ["1"]}]}]}]}]}]}
       =
        create_netconf_config(delete, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1", []),
    
       {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"},
           {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
          [{managedElementId,[],["1"]},
           {'SystemFunctions', [],
        [{systemFunctionsId,[],["1"]},
         {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
          [{secMId,[],["1"]},
           {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
            [{certMId,[],["1"]},
             {'NodeCredential', [],
              [{nodeCredentialId, [], ["1"]},
               {startOfflineCsrEnrollment, 
            [{uri, [], ["sftp://bezze"]},
             {uriPassword, [], ["shifra"]}]}]}]}]}]}]} 
        =
        create_netconf_config(action, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1", {startOfflineCsrEnrollment, 
                                #{uri => "sftp://bezze", uriPassword => "shifra"}}),
    
    
     %% Test interface functions
      Res = create_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2", [{administrativeState, "UNLOCKED"}, {serverAddress, "10.67.31.11"}]),
      ct:pal("Create MO result: ~p~n", [Res]),
    
      ct:pal("MO exists: ~p~n", [exists_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2")]),
     
      Res1 = get_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2"),
      ct:pal("Get MO result: ~p~n", [Res1]),
    
      Res11 = get_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2", [serverAddress, unknown_attr]),
      ct:pal("Get MO attr result: ~p~n", [Res11]),
     
      Res2 = set_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2", [{administrativeState, "LOCKED"}]),
      ct:pal("Set MO result: ~p~n", [Res2]),
     
      timer:sleep(5000),
     
      Res3 = delete_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2"),
      ct:pal("Delete MO result: ~p~n", [Res3]),
    
      ct:pal("MO exists: ~p~n", [exists_mo({netconf, nc1}, "SystemFunctions=1,SysM=1,NtpServer=2")]),
    
    
     ct:pal("Action result: ~p~n", [async_action_mo({netconf, nc1}, 
                                                    "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1", 
                                                    installTrustedCertFromUri, 
                                                    [{uri, "sftp://labuser@10.68.101.131/home/labuser/cert.crt"},
                                                     {uriPassword, "wrong_pass"},
                                                     {fingerprint, "AB:CD:EF"}])]),
    
    {ok, [{reportProgress, Progress}]} = get_mo({netconf, nc1}, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1", reportProgress),
    
    ct:pal("ProgressReport:  ~p~n", [Progress]),
    ct:pal("ActionId: ~p~n", [get_action_id(get_progress_struct({netconf, nc1}, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1", reportProgress))]),
    ct:pal("ActionId from struct: ~p~n", [get_action_id(Progress)]),
    ct:pal("TimeActionCompleted from struct: ~p~n", [get_time_action_completed(Progress)]),
    ct:pal("State from struct: ~p~n", [get_progress_state(Progress)]),
    ct:pal("Result from struct: ~p~n", [get_progress_result(Progress)]),
    
    ct:pal("MO children: ~p~n", [get_children_mo({netconf, nc1}, "ManagedElement=1")]),
    ct:pal("MO children: ~p~n", [get_children_mo({netconf, nc1}, "Transport=1,Router=1")]),
    ct:pal("MO children: ~p~n", [get_children_mo({netconf, nc1}, "SystemFunctions=1,SysM=1", "NtpServer")]),
    
    "1" = get_mo_id("SystemFunctions=1,SysM=1"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns true if MO exists on the node, false otherwise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec exists_mo(Handler :: handler(), DN :: dn()) -> true | false.
exists_mo(Handler, DN) ->
    case get_mo(Handler, DN) of
        {ok, _} ->
            true;
        _Error ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Fetches MO configuration from node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_mo(Handler :: handler(), DN :: dn()) -> {ok, attrs()} | {error, term()}.
get_mo({netconf, NC}, DN) ->
    Config = create_netconf_config(DN, []),
    
    case netconf(NC, get, [Config]) of
        {ok, Result} ->
             {ok, decode_config(DN, Result)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Fetches MO's attribute/s configuration from node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_mo(Handler :: handler(), DN :: dn(), Atrrs :: list() | atom()) -> {ok, attrs()} | {error, term()}.
get_mo(Handler, DN, Attrs) when is_atom(Attrs) ->
        get_mo(Handler, DN, [Attrs]);
get_mo(Handler, DN, Attrs) when is_list(Attrs) ->
    case get_mo(Handler, DN) of
        {ok, Result} ->
            {ok, filter_attr_results(Result, Attrs)};
        Error ->
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Fetches MO's children MO DNs from the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_children_mo(Handler :: handler(), DN :: dn()) -> {ok, [dn()]} | {error, term()}.
get_children_mo({netconf, NC}, DN) ->
    Config = create_netconf_config(DN, []),
    
    case netconf(NC, get, [Config]) of
        {ok, Result} ->
             {ok, decode_config(children, DN, Result)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Fetches specific MO's children MO DNs from the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_children_mo(Handler :: handler(), DN :: dn(), MoPattern :: string) -> 
            {ok, [dn()]} | {error, term()}.
get_children_mo(Handler, DN, MoPattern) ->
    case get_children_mo(Handler, DN) of
        {ok, ChildrenMOs} ->
            %% Leave only elements which satisfy the pattern
            ResultDNs = 
            lists:filter(fun(ChildDN) -> case string:str(ChildDN, MoPattern) of
                                        0 ->
                                            false;
                                        _ ->
                                            true
                                    end end, ChildrenMOs),
            {ok, ResultDNs};
        Other ->
            Other
    end.
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets MO attribute/s configuration to the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_mo(Handler :: handler(), DN :: dn(), Attrs :: attrs()) -> ok | {error, term()}.
set_mo({netconf, NC}, DN, Attrs) ->
    Config = create_netconf_config(DN, Attrs),
    
    netconf(NC, edit_config, [running, Config]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Deletes MO configuration from the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_mo(Handler :: handler(), DN :: dn()) -> ok | {error, term()}.
delete_mo({netconf, NC}, DN) ->
    
    Config = create_netconf_config(delete, DN, []),
    
    netconf(NC, edit_config, [running, Config]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calls asynchronous action and waits until it's done
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec async_action_mo(Handler :: handler(), DN :: dn(), 
                      Action :: atom(), Params :: attrs()) ->
                    {ok, term()} | {error, term()}.
async_action_mo(Handler, DN, Action, Params) ->
    async_action_mo(Handler, DN, Action, Params, {DN, reportProgress}).

-spec async_action_mo(Handler :: handler(), DN :: dn(), 
                      Action :: atom(), Params :: attrs(), 
                      Progress :: undefined | atom() | {dn(), atom()}) ->
                    {ok, term()} | {error, term()}.
async_action_mo(Handler, DN, Action, Params, undefined) ->
    async_action_mo(Handler, DN, Action, Params, {DN, reportProgress});

async_action_mo(Handler, DN, Action, Params, ProgressAttr) when is_atom(ProgressAttr) ->
    async_action_mo(Handler, DN, Action, Params, {DN, ProgressAttr});
    
async_action_mo(Handler, DN, Action, Params, {ProgressDn, ProgressAttr}) ->
    
    ProgressStruct = get_progress_struct(Handler, ProgressDn, ProgressAttr),
    OldActionId = get_action_id(ProgressStruct),
    OldTimeComplete = get_time_action_completed(ProgressStruct),
    
     case action_mo(Handler, DN, Action, Params) of
         {ok, _} ->
             wait_for_progress(Handler, ProgressDn, ProgressAttr, {OldActionId, OldTimeComplete});
         Error ->
             Error
     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calls system action on target MO with action parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec action_mo(Handler :: handler(), DN :: dn(), 
                Action :: atom(), Params :: attrs()) -> 
          {ok, term()} | {error, term()}.
action_mo({netconf, NC}, DN, Action, Params) ->
     Config = create_netconf_config(action, DN, {Action, Params}),
     
     netconf(NC, action, [Config]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates MO configuration on the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_mo(Handler :: handler(), 
                DN :: dn(), 
                Attrs :: attrs()) -> 
          ok | {error, term()}.

create_mo({netconf, NC}, DN, Attrs) ->
    Config = create_netconf_config(DN, Attrs),
    
    netconf(NC, edit_config, [running, Config]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates netconf MO configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_netconf_config(DN :: dn(), Attrs :: attrs()) -> term().
create_netconf_config(DN, Attrs) ->
    create_netconf_config(create, DN, Attrs).

-spec create_netconf_config(Op :: create | delete, 
                            DN :: dn(), Attrs :: attrs()) -> term();
                            (Op :: action, DN :: dn(), 
                             {Action :: atom(), Params :: attrs()}) -> term.
create_netconf_config(Op, DN, Attrs) when is_binary(DN) ->
    create_netconf_config(Op, binary_to_list(DN), Attrs);

create_netconf_config(create, DN, Attrs) ->
    AttrConfig = create_attr_config(Attrs),
    Config = create_mo_config(DN, AttrConfig),
    create_DN_config(encode_dn(parent_dn(add_root_mo(DN))), Config);

create_netconf_config(delete, DN, _) ->
    DeleteConfig = delete_mo_config(DN),
    create_DN_config(encode_dn(parent_dn(add_root_mo(DN))), DeleteConfig);

create_netconf_config(action, DN, {Action, Params}) ->
    ParamConfig = create_attr_config(Params),
    create_DN_config(encode_dn(add_root_mo(DN)), {Action, ParamConfig}).

create_attr_config(Attrs) when is_map(Attrs) ->
    create_attr_config(maps:to_list(Attrs));
create_attr_config(Attrs) ->
    create_attr_config(Attrs, []).

%% return result
create_attr_config([], Conf) ->
    Conf;
%% delete attribute
create_attr_config([{K, undefined} | Attrs], Conf) ->
    create_attr_config(Attrs, Conf ++ [{K, ?OPER_DELETE, []}]);
%% struct attr
create_attr_config([{K, [{structName, StructName} | V]} | Attrs], Conf) ->
    StructConf = create_struct_attr_config(K, StructName, V),
    create_attr_config(Attrs, Conf ++ [StructConf]);
%% struct attr, unknown struct name
create_attr_config([{K, [{_, _} | _] = V} | Attrs], Conf) ->
    StructConf = create_struct_attr_config(K, undefined, V),
    create_attr_config(Attrs, Conf ++ [StructConf]);
%% sequence attr
create_attr_config([{K, [First | _] = V} | Attrs], Conf) when is_list(First) ->
    SequenceConf = create_sequence_attr_config(K, V),
    create_attr_config(Attrs, Conf ++ SequenceConf);
%% simple key-value set
create_attr_config([{K, V} | Attrs], Conf) ->
    create_attr_config(Attrs, Conf ++ [{K, [], [V]}]).

create_struct_attr_config(K, undefined, V) ->
    {K, [], create_attr_config(V, [])};
create_struct_attr_config(K, StructName, V) ->
    {K, [{struct, StructName}], create_attr_config(V, [])}.

create_sequence_attr_config(K, V) ->
    Attrs = lists:map(fun(M) -> {K, M} end, V),
    create_attr_config(Attrs, []).

decode_config(DN, Result) ->
    decode_config(attrs, DN, Result).

decode_config(attrs, DN, Result) ->
    [Mo, _Id] = string:tokens(to_ldn(DN), "="),
    {ok, {_, _, Contents}} = extract_element(list_to_atom(Mo), Result),
    decode_attr_config(Contents);

decode_config(children, DN, Result) ->
    [Mo, _Id] = string:tokens(to_ldn(DN), "="),
    {ok, {_, _, Contents}} = extract_element(list_to_atom(Mo), Result),
    ChildMos = decode_children_config(Contents),
    lists:map(fun(ChildMo) -> DN ++ "," ++ ChildMo end, ChildMos).

decode_attr_config(Contents) ->
    decode_attr_config(Contents, []).

decode_attr_config([], Attrs) ->
    %% Checking for sequence attributes
    lists:map(fun(K) -> case proplists:get_all_values(K, Attrs) of
                            [V] ->
                                {K, V};
                            VList->
                                {K, VList}
                        end 
              end, proplists:get_keys(Attrs));
%% struct attribute
decode_attr_config([{Attr, [{struct, _}], Value} | Contents], Attrs) ->
    StructValue = decode_attr_config(Value),
    decode_attr_config(Contents, [{Attr, StructValue} | Attrs]);
%% simple key/value
decode_attr_config([{Attr, [], [Value]} | Contents], Attrs) ->
    decode_attr_config(Contents, [{Attr, Value} | Attrs]);
decode_attr_config([{Attr, [Value]} | Contents], Attrs) ->
    decode_attr_config(Contents, [{Attr, Value} | Attrs]);
%% value is not set
decode_attr_config([{Attr, [], []} | Contents], Attrs) ->
    decode_attr_config(Contents, [{Attr, undefined} | Attrs]);
decode_attr_config([{Attr, []} | Contents], Attrs) ->
    decode_attr_config(Contents, [{Attr, undefined} | Attrs]);
%% end of attribute listing
decode_attr_config([{_Attr, [], _MoConfig} | _], Attrs) ->
    decode_attr_config([], Attrs).

decode_children_config(Contents) ->
    decode_children_config(Contents, []).

decode_children_config([], ChildMOs) ->
    ChildMOs;

decode_children_config([{MO, _, [{MoId, _, [MoIdValue]} | _]} | Contents], ChildMOs) ->
    case mo_to_id(atom_to_list(MO)) =:= MoId of
        true ->
            ChildMO = atom_to_list(MO) ++ "=" ++ MoIdValue,
            decode_children_config(Contents, ChildMOs ++ [ChildMO]);
        false ->
            decode_children_config(Contents, ChildMOs)
    end;

decode_children_config([_ | Contents], ChildMOs) ->
    decode_children_config(Contents, ChildMOs).
  
filter_attr_results(Result, Attrs) ->
    filter_attr_results(Result, Attrs, []).

filter_attr_results([], Attrs, Result) ->
    %% add undefined attributes
    UndefList = [{K, undefined} || K <- Attrs, not proplists:is_defined(K, Result)],
    Result ++ UndefList;
filter_attr_results([{K, V} | Rest], Attrs, Result) ->
    case lists:member(K, Attrs) of
        true ->
            filter_attr_results(Rest, Attrs, [{K, V} | Result]);
        false ->
            filter_attr_results(Rest, Attrs, Result)
    end.

create_mo_config(DN, AttrConfig) ->
     [{Mo, Id} | _RestDN] = encode_dn(add_root_mo(DN)),
     {list_to_atom(Mo), get_mo_NS(Mo), 
       [{mo_to_id(Mo), [], [Id]} | AttrConfig]}.

delete_mo_config(DN) ->
    [{Mo, Id} | _RestDN] = encode_dn(DN),
    {list_to_atom(Mo), ?OPER_DELETE, 
         [{mo_to_id(Mo), [], [Id]}]}.

%% Example: 
%%	Input:  "ManagedElement=1,SystemFunctions=1,SysM=1"
%%	Output: [{"SysM", "1"}, {"SystemFunctions, "1"}, {"ManagedElement, "1"}]
encode_dn(DN) when is_list(DN) ->
    SplitDN = string:tokens(DN, ","), %% ["ManagedElement=1", "SystemFunctions=1" , "SysM=1"]
    lists:reverse(lists:map(fun(X) -> [Mo, Id] = string:tokens(X, "="), {Mo, Id} end, SplitDN)).

create_DN_config([], Config) ->
    Config;

create_DN_config([{Mo, Id} | DecodedDN], []) ->
    NewConfig =
        {list_to_atom(Mo), get_mo_NS(Mo), 
         [{mo_to_id(Mo), [], [Id]}]},
    
    create_DN_config(DecodedDN, NewConfig);

create_DN_config([{Mo, Id} | DecodedDN], Config) ->
    NewConfig =
        {list_to_atom(Mo), get_mo_NS(Mo), 
         [{mo_to_id(Mo), [], [Id]}, Config]},
    
    create_DN_config(DecodedDN, NewConfig).

-spec get_mo_NS(Mo :: string()) -> 	list().
get_mo_NS("ManagedElement") ->
    [{xmlns,"urn:com:ericsson:ecim:ComTop"},
           {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}];
get_mo_NS("SecM") ->
    [{xmlns,"urn:com:ericsson:ecim:ComSecM"}];
get_mo_NS("CertM") ->
    [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}];
get_mo_NS("Fm") ->
    [{xmlns, "urn:com:ericsson:ecim:ComFm"}];
get_mo_NS(_) ->
     [].
    
%% Example: 
%%	Input:  "ManagedElement"
%% 	Output: 'managedElementId'
-spec mo_to_id(Mo :: string()) -> atom().
mo_to_id([FirstLetter | Rest]) ->
    list_to_atom([string:to_lower(FirstLetter) | Rest] ++ "Id").

%% Example: 
%%  Input:  "ManagedElement=1,SystemFunctions=1,SysM=1"
%%  Output: "ManagedElement=1,SystemFunctions=1"
-spec parent_dn(DN :: dn()) -> dn().
parent_dn(DN) ->
    TokenizedDN = string:tokens(DN, ","),
    string:join(lists:droplast(TokenizedDN), ",").

%% Example: 
%%  Input:  "ManagedElement=1,SystemFunctions=1,SysM=1"
%%  Output: "SysM=1"
-spec to_ldn(DN :: dn()) -> dn().
to_ldn(DN) ->
    [LDN | _] = lists:reverse(string:tokens(DN, ",")),
    LDN.

%% Example: 
%%  Input:  "SystemFunctions=1,SysM=1"
%%  Output: "ManagedElement=1,SystemFunctions=1,SysM=1"
-spec add_root_mo(DN :: dn()) -> dn().
add_root_mo(DN) ->
    case string:tokens(DN, "=,") of
        ["ManagedElement" | _] ->
            DN;
        _ ->
            "ManagedElement=1," ++ DN
    end.

%% Example: 
%%  Input:  "SystemFunctions=1,SysM=1"
%%  Output: "1"
-spec get_mo_id(DN :: dn()) -> string().
get_mo_id(DN) ->
    [Id | _] = lists:reverse(string:tokens(DN, ",=")),
    Id.

get_progress_struct(Handler, ProgressDn, ProgressAttr) ->
    case get_mo(Handler, ProgressDn, ProgressAttr) of
        {ok, [{ProgressAttr, ProgressStruct}]} when is_list(ProgressStruct) ->
            ProgressStruct;
        _ ->
            undefined
    end.

get_action_id(undefined) ->
    0;
get_action_id(ProgressStruct) when is_list(ProgressStruct) ->
    list_to_integer(proplists:get_value(actionId, ProgressStruct, "0")).

get_time_action_completed(undefined) ->
    undefined;
get_time_action_completed(ProgressStruct) when is_list(ProgressStruct) ->
    proplists:get_value(timeActionCompleted, ProgressStruct, undefined).

get_progress_state(undefined) ->
    undefined;
get_progress_state(ProgressStruct) ->
    proplists:get_value(state, ProgressStruct, undefined).

get_progress_result(undefined) ->
    undefined;
get_progress_result(ProgressStruct) ->
    proplists:get_value(result, ProgressStruct, undefined).

is_action_done(undefined, _) ->
    false;
is_action_done(ProgressStruct, {OldActionId, OldTimeComplete}) when is_list(ProgressStruct) ->
    ActionId = get_action_id(ProgressStruct),
    TimeComplete = get_time_action_completed(ProgressStruct),
    
    case get_progress_state(ProgressStruct) of
        Done when (Done =:= "CANCELLED" orelse Done =:= "FINISHED")
                    andalso (OldActionId < ActionId orelse 
                             OldTimeComplete =/= TimeComplete) ->
            true;
        _ ->
            false
    end.
            

wait_for_progress(Handler, ProgressDn, ProgressAttr, OldState) ->
    wait_for_progress(Handler, ProgressDn, ProgressAttr, OldState, 120000).

wait_for_progress(_Handler, _ProgressDn, _ProgressAttr, _OldState, Timeout) when Timeout =< 0 ->
    {error, timeout};

wait_for_progress(Handler, ProgressDn, ProgressAttr, OldState, Timeout) ->
    timer:sleep(5000),
    ProgressStruct = get_progress_struct(Handler, ProgressDn, ProgressAttr),
    
    case is_action_done(ProgressStruct, OldState) of
        true ->
            {ok, get_progress_result(ProgressStruct)};
        false ->
            wait_for_progress(Handler, ProgressDn, ProgressAttr, OldState, Timeout-5000)
    end.

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(NC, F, A) ->
    {ok, _} = ct_netconfc:open(NC, []),
    Res = apply(ct_netconfc, F, [NC | A]),
    
    Status = 
        case Res of
             {error, _} ->
                 ct_netconfc:close_session(NC),
                 Res;
             _ ->
                 ct_netconfc:close_session(NC)
         end,
    
    %% Result of negative edit requests are seen first when closing session.
    case Status of
        ok ->
            Res;
        _ ->
            Status
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

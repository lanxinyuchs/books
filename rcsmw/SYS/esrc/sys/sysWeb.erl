%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysWeb.erl %
%%% Author:	etxjotj
%%% Description: Receiver of appdata registrations for the web service
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(sysWeb).
-vsn('/main/R2A/R3A/R4A/R9A/R10A/R11A/1').
-date('2017-10-17').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/1      2013-04-12 etxjotj     Created
%%% R2A/7      2013-05-06 etxtory     timeout problem fixed in 
%%%				      httpd:reload_config
%%% R3A/1      2014-10-03 etxberb     Added alias, erl_script_alias,
%%%                                   erl_script_nocache & mime.
%%% R3A/2      2014-10-23 etxberb     Removed is_insertable check on alias.
%%% R3A/3      2015-01-18 etxtory     Handle several http servers
%%% R3A/5      2015-04-10 etxtory     
%%% R4A/1      2015-07-07 etxberb     Changed mnesia:create_table to
%%%                                   clhI:mnesia_create_table.
%%% R4A/3      2015-09-03 etxasta     Added web sec
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-01-26 etxberb  Added registerOnNodes.
%% R9A/2   2017-03-06 etxberb  Added call to swmI:is_node_type_valid
%% ----    ---------- -------  ------------------------------------------------
%% R10A/1  2017-06-16 etxberb  Enhanced erl_script_alias to allow multiple
%%                             modules in same Url.
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-10-17 etxberb  Adaptions to OTP20.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, init_data/0, appdata/3, appdata/4]).
-export([update_servers/2]).
-export([get_inets_props/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("SysWeb.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).
-define(STACKTRACE_E,   % Stacktrace at Exception
	erlang:get_stacktrace()).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.1.1 API
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
init(DbNodes) ->
    %% This table holds appdata registrations for the web service
    {atomic, ok} =
	clhI:mnesia_create_table(sysWeb, 
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields, sysWeb)} |
				  sysDataInit:add_clh_option(sysWeb)]),
    %% This table holds appdata registrations for the web sec service
    {atomic, ok} =
	clhI:mnesia_create_table(sysWebSec, 
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
                                      record_info(fields, sysWebSec)} |
				  sysDataInit:add_clh_option(sysWebSec)]),

    ok.

%%% ----------------------------------------------------------
init_data() ->
    swmI:register_appdata_receiver("web", sysWeb),
    swmI:register_appdata_receiver("web_sec", sysWebSec).

%%% ----------------------------------------------------------
appdata(ProdId, Version, AppdataE) ->
    appdata(web, ProdId, Version, AppdataE).

appdata(Type, ProdId, Version, #xmlElement{content = Content} = AppdataE) ->
    Fun =
	fun() ->
		try
		    Props = parse_all(Content),
		    appdata_write(Props, Type, ProdId, Version)
		catch
		    throw : {registerOnNode, false} ->
			ok
		end
	end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, R} ->
	    erlang:error(R, [ProdId, Version, AppdataE])
    end.

%%% ----------------------------------------------------------
appdata_write([Prop | Tail], web = Type, ProdId, Version) ->
    mnesia:write(#sysWeb{key = get_next_key(sysWeb),
			 prodId = ProdId,
			 prodVersion = Version,
			 prop = Prop}),
    appdata_write(Tail, Type, ProdId, Version);
appdata_write([Prop | Tail], web_sec = Type, ProdId, Version) ->
    mnesia:write(#sysWebSec{key = get_next_key(sysWebSec),
			    prodId = ProdId,
			    prodVersion = Version,
			    prop = Prop}),
    appdata_write(Tail, Type, ProdId, Version);
appdata_write([], _, _, _) ->
    ok.

%%% ----------------------------------------------------------
%update_servers(Type, Pid) ->
%    update_servers(Type, inets:services()).

%update_servers(_,[]) ->
%    ok;
%update_servers(web, [{httpd, ServerPid} | T]) ->
%    Props = httpd:info(ServerPid),
%    update_server(ets:tab2list(sysWeb), Props),
%    update_servers(web, T);
%update_servers(web_sec, [{httpd, ServerPid} | T]) ->
%    Props = httpd:info(ServerPid),
%    update_server(ets:tab2list(sysWeb), Props),
%    update_servers(web_sec, T);
%update_servers(Type, [_ | T]) ->
%    update_servers(Type, T).

update_servers(Table, ServerPid) ->
    Props = httpd:info(ServerPid),
    update_server(ets:tab2list(Table), Props).


%%% --------------------------------------
update_server([#sysWeb{prop = #dir{source = RealName,
                published = Alias}} = Prop | T], Props) ->
    NewProps = 
    upd_alias(Prop#sysWeb{prop = #alias{realName = RealName,
                alias = Alias}}, Props),
    update_server(T, NewProps);
update_server([#sysWebSec{prop = #dir{source = RealName,
                published = Alias}} = Prop | T], Props) ->
    NewProps = 
    upd_alias(Prop#sysWebSec{prop = #alias{realName = RealName,
                alias = Alias}}, Props),
    update_server(T, NewProps);
update_server([#sysWeb{prop = #alias{}} = Prop | T], Props) ->
    update_server(T, upd_alias(Prop, Props));
update_server([#sysWebSec{prop = #alias{}} = Prop | T], Props) ->
    update_server(T, upd_alias(Prop, Props));
update_server([#sysWeb{prop = #erl_script_alias{}} = Prop | T], Props) ->
    update_server(T, upd_erl_script_alias(Prop, Props));
update_server([#sysWebSec{prop = #erl_script_alias{}} = Prop | T], Props) ->
    update_server(T, upd_erl_script_alias(Prop, Props));
update_server([#sysWeb{prop = #erl_script_nocache{}} = Prop |T], Props) ->
    update_server(T, upd_erl_script_nocache(Prop, Props));
update_server([#sysWebSec{prop = #erl_script_nocache{}} = Prop |T], Props) ->
    update_server(T, upd_erl_script_nocache(Prop, Props));
update_server([#sysWeb{prop = #mime{}} = Prop | T], Props) ->
    update_server(T, upd_mime(Prop, Props));
update_server([#sysWebSec{prop = #mime{}} = Prop | T], Props) ->
    update_server(T, upd_mime(Prop, Props));

update_server([Prop | T], Props) when is_record(Prop, sysWeb) ->
    ?LOG_WARN([unrecognized_property, {sysWeb, Prop}]),
    update_server(T, Props);
update_server([Prop | T], Props) when is_record(Prop, sysWebSec) ->
    ?LOG_WARN([unrecognized_property, {sysWebSec, Prop}]),
    update_server(T, Props);
update_server([], NewProps) ->
    %% In R16B03 and older, the function httpd:reload_config
    %% timeouted due to 5s default timeout. This has now been
    %% fixed in R16B03-1 and the timer defaults to infinity.
    case httpd:reload_config(NewProps, disturbing) of
        ok ->
            ok;
        Error ->
            ?LOG_WARN([{httpd, reload_config}, {res, Error}]),
            Error
    end.

%%% ----------------------------------------------------------
upd_alias(#sysWeb{prop = #alias{realName = RealName,
		                alias = Alias}} = Prop,
	  Props) ->
    {ok, CxpPath} = swmI:get_cxp_path(Prop#sysWeb.prodId,
				      Prop#sysWeb.prodVersion),
    Pattern = filename:join(CxpPath, RealName),
    case filelib:wildcard(Pattern) of
	[NewRealName] ->
	    NewAlias = case Alias of
			   undefined ->
			       [$/ | filename:basename(NewRealName)];
			   _ -> 
			       Alias
		       end,
	    Value = {NewAlias, find_file(NewRealName)},
	    NewProp = {alias, Value},
	    case is_insertable(Value,
			       get_all_values(Props, alias),
			       Prop,
			       multiple)
		of
		true ->
		    [NewProp | Props];
		false ->
		    Props
	    end;
	WildcardRes ->
	    ?LOG_ERR([{'filelib:wildcard ->', WildcardRes},
		      {incorrect_value, {realName, RealName}},
		      {property, Prop#sysWeb.prop},
		      {load_module, {Prop#sysWeb.prodId,
				     Prop#sysWeb.prodVersion}}]),
	    Props
    end;
upd_alias(#sysWebSec{prop = #alias{realName = RealName,
				   alias = Alias}} = Prop,
	  Props) ->
    {ok, CxpPath} = swmI:get_cxp_path(Prop#sysWebSec.prodId,
				      Prop#sysWebSec.prodVersion),
    Pattern = filename:join(CxpPath, RealName),
    case filelib:wildcard(Pattern) of
	[NewRealName] ->
	    NewAlias = case Alias of
			   undefined ->
			       [$/ | filename:basename(NewRealName)];
			   _ -> 
			       Alias
		       end,
	    Value = {NewAlias, find_file(NewRealName)},
	    NewProp = {alias, Value},
	    case is_insertable(Value,
			       get_all_values(Props, alias),
			       Prop,
			       multiple)
		of
		true ->
		    [NewProp | Props];
		false ->
		    Props
	    end;
	WildcardRes ->
	    ?LOG_ERR([{'filelib:wildcard ->', WildcardRes},
		      {incorrect_value, {realName, RealName}},
		      {property, Prop#sysWebSec.prop},
		      {load_module, {Prop#sysWebSec.prodId,
				     Prop#sysWebSec.prodVersion}}]),
	    Props
    end.



%%% ----------------------------------------------------------
upd_erl_script_alias(#sysWeb{prop = #erl_script_alias{url = Url,
						      module = Mod}},
		     Props) ->
    case split(Props, erl_script_alias, Url) of
	{{_, {_, SplitUrlValue}}, SplitProps} ->
	    ok;
	{_, SplitProps} ->
	    SplitUrlValue = []
    end,
    Value = {Url, [make_atom(Mod) | SplitUrlValue]},
    [{erl_script_alias, Value} | SplitProps];
upd_erl_script_alias(#sysWebSec{prop = #erl_script_alias{url = Url,
							 module = Mod}},
		     Props) ->
    case split(Props, erl_script_alias, Url) of
	{{_, {_, SplitUrlValue}}, SplitProps} ->
	    ok;
	{_, SplitProps} ->
	    SplitUrlValue = []
    end,
    Value = {Url, [make_atom(Mod) | SplitUrlValue]},
    [{erl_script_alias, Value} | SplitProps].

%%% ----------------------------------------------------------
split(Props, PropName, PropName2) ->
    split(Props, PropName, PropName2, []).

split([{PN, {PN2, _PN2Value}} = MatchProp | Tail], PN, PN2, AccProps) ->
    {MatchProp, AccProps ++ Tail};
split([Prop | Tail], PN, PN2, AccProps) ->
    split(Tail, PN, PN2, AccProps ++ [Prop]);
split([], _, _, AccProps) ->
    {no_match, AccProps}.

%%% ----------------------------------------------------------
upd_erl_script_nocache(#sysWeb{prop =
        #erl_script_nocache{bool = Bool}} = Prop, Props) ->
    insert_element({erl_script_nocache, make_atom(Bool)}, Props, Prop, single);
upd_erl_script_nocache(#sysWebSec{prop =
        #erl_script_nocache{bool = Bool}} = Prop, Props) ->
    insert_element({erl_script_nocache, make_atom(Bool)}, Props, Prop, single).


%%% ----------------------------------------------------------
upd_mime(#sysWeb{prop = #mime{extension = Extension, type = Type}} = Prop,
    Props) ->
    case lists:keyfind(mime_types, 1, Props) of
	{_, MimeTypes} ->
	    MimeTypes;
	false ->
	    MimeTypes = []
    end,
    NewMimeType = {Extension, Type},
    NewProp = {mime_types, insert_element(NewMimeType,
					  MimeTypes,
					  Prop,
					  single)},
    [NewProp | lists:keydelete(mime_types, 1, Props)];
upd_mime(#sysWebSec{prop = #mime{extension = Extension, type = Type}} = Prop,
    Props) ->
    case lists:keyfind(mime_types, 1, Props) of
	{_, MimeTypes} ->
	    MimeTypes;
	false ->
	    MimeTypes = []
    end,
    NewMimeType = {Extension, Type},
    NewProp = {mime_types, insert_element(NewMimeType,
					  MimeTypes,
					  Prop,
					  single)},
    [NewProp | lists:keydelete(mime_types, 1, Props)].

%%% #---------------------------------------------------------
%%% #3.1.2 Miscellaneous exported functions
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Get properties of all inets servers
%%% ----------------------------------------------------------
get_inets_props() ->
    get_inets_props(inets:services()).

get_inets_props([{ServerType, ServerPid} | Tail]) ->
    [{ServerType, ServerType:info(ServerPid)} | get_inets_props(Tail)];
get_inets_props([UnknownFormat | Tail]) ->
    ?LOG_WARN([{unknown_format, UnknownFormat}]),
    get_inets_props(Tail);
get_inets_props([]) ->
    [].

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
get_all_values([{Tag, Value} | Tail], Tag) ->
    [Value | get_all_values(Tail, Tag)];
get_all_values([_ | Tail], Tag) ->
    get_all_values(Tail, Tag);
get_all_values([], _) ->
    [].

%%% ----------------------------------------------------------
insert_element(Elem, Elems, SysWeb, Type) ->
    case is_insertable(Elem, Elems, SysWeb, Type) of
	true ->
	    [Elem | Elems];
	false ->
	    Elems
    end.

%%% ----------------------------------------------------------
is_insertable(Elem, Elems, SysWeb, Type) ->
    case lists:member(Elem, Elems) of
	false ->
	    case {lists:keymember(element(1, Elem), 1, Elems), Type} of
		{false, _} ->
		    true;
		{_, single} ->
		    ?LOG_ERR([{already_exists, trying_to_change_value},
			      {property, SysWeb#sysWeb.prop},
			      {load_module, {SysWeb#sysWeb.prodId,
					     SysWeb#sysWeb.prodVersion}}]),
		    false;
		{_, multiple} ->
		    true
	    end;
	true ->
	    %% Same value; ignore
	    false
    end.

%%% ----------------------------------------------------------
make_atom(Term) when is_atom(Term) ->
    Term;
make_atom(Term) when is_list(Term) ->
    list_to_atom(Term).

%%% ----------------------------------------------------------
%%% Parsing directory
%%% ----------------------------------------------------------
parse_all([#xmlElement{name = dir} = Elem | Tail]) ->
    [parse_dir(Elem) | parse_all(Tail)];
parse_all([#xmlElement{name = alias} = Elem | Tail]) ->
    [parse_alias(Elem) | parse_all(Tail)];
parse_all([#xmlElement{name = erl_script_alias} = Elem | Tail]) ->
    [parse_erl_script_alias(Elem) | parse_all(Tail)];
parse_all([#xmlElement{name = erl_script_nocache} = Elem | Tail]) ->
    [parse_erl_script_nocache(Elem) | parse_all(Tail)];
parse_all([#xmlElement{name = mime} = Elem | Tail]) ->
    [parse_mime(Elem) | parse_all(Tail)];
parse_all([#xmlElement{name = registerOnNodes,
		       content = Content} | Tail]) ->
    case is_registerOnNode(Content) of
	true ->
	    parse_all(Tail);
	false ->
	    throw({registerOnNode, false})
    end;
parse_all([_ | Tail]) ->
    parse_all(Tail);
parse_all([]) ->
    [].

%%% ----------------------------------------------------------
parse_dir(Elem) ->
    #dir{source = find_value(Elem, source, mandatory),
	 published = find_value(Elem, published, optional)}.

%%% ----------------------------------------------------------
parse_alias(Elem) ->
    #alias{realName = find_value(Elem, realName, mandatory),
	   alias = find_value(Elem, alias, optional)}.

%%% ----------------------------------------------------------
parse_erl_script_alias(Elem) ->
    #erl_script_alias{url = find_value(Elem, url, mandatory),
		      module = find_value(Elem, module, mandatory)}.

%%% ----------------------------------------------------------
parse_erl_script_nocache(Elem) ->
    #erl_script_nocache{bool = find_value(Elem, undefined, mandatory)}.

%%% ----------------------------------------------------------
parse_mime(Elem) ->
    #mime{extension = find_value(Elem, extension, mandatory),
	  type = find_value(Elem, type, mandatory)}.

%%% ----------------------------------------------------------
parse_registerOnNodes([#xmlElement{name = node} = Elem | Tail]) ->
    [find_value(Elem, type, mandatory) | parse_registerOnNodes(Tail)];
parse_registerOnNodes([_ | Tail]) ->
    parse_registerOnNodes(Tail);
parse_registerOnNodes([]) ->
    [].

%%% ----------------------------------------------------------
get_next_key(Table) -> % sysWeb | sysWebSec
    case mnesia:all_keys(Table) of
	[] ->
	    1;
	Keys ->
	    lists:max(Keys) + 1
    end.

%%% ----------------------------------------------------------
add_proc_dict(Tag, Values) ->
    case get(Tag) of
	undefined ->
	    put(Tag, Values);
	OldValues ->
	    put(Tag, OldValues ++ Values)
    end.

%%% ----------------------------------------------------------
find_value(#xmlElement{attributes = [],
		       content = [#xmlText{value = Value}]},
	   _,
	   _) ->
    string:strip(Value);
find_value(#xmlElement{attributes = Attrs}, AttrName, Required) ->
    case lists:keyfind(AttrName, #xmlAttribute.name, Attrs) of
        #xmlAttribute{value = Value} ->
            Value;
        false ->
	    case Required of
		optional ->
		    undefined;
		mandatory ->
		    Stacktrace = ?STACKTRACE_C,
		    ?LOG_ERR([{mandatory_attribute, missing},
			      {AttrName, Attrs} |
			      Stacktrace]),
		    undefined
	    end
    end.

%%% ----------------------------------------------------------
%%%  this version of find_file also approves directories
%%%  which swmI:find_file does not.
find_file(File) ->
    Base = filename:basename(File),
    Patch = filename:join(sysEnv:dev_patches_dir(), Base),
    case filelib:is_file(Patch) of
	true ->
	    Patch;
	false ->
	    File
    end.

%%% ----------------------------------------------------------
is_registerOnNode(Elem) ->
    RegisterOnNodes = parse_registerOnNodes(Elem),
    NodeType = swmI:node_type(),
    try
	ok = validate_registerOnNodes(RegisterOnNodes),
	lists:member(NodeType, RegisterOnNodes)
    catch
	throw : registerOnAllNodes ->
	    true;
	throw : {warnings, Warnings} ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_WARN(["--- Anomalies in xml file ---"
		       | Warnings] ++
		      ["--- Stacktrace ---"
		       | Stacktrace]),
	    lists:member(NodeType, RegisterOnNodes);
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{"Error in xml file", "Not able to register webAttrs"},
		      {node_type, NodeType},
		      {ErrClass, ErrReason},
		      "--- Stacktrace ---"
		      | Stacktrace]),
	    error
    end.

%%% ----------------------------------------------------------
validate_registerOnNodes([]) ->   % Default, legacy, = "ALL"
    throw(registerOnAllNodes);
validate_registerOnNodes(["ALL"]) ->
    throw(registerOnAllNodes);
validate_registerOnNodes(NodeTypes) ->
    case lists:member("ALL", NodeTypes) of
	false ->
	    validate_registerOnNodes_list(NodeTypes);
	true ->
	    erlang:error({illegal_combination, registerOnNodes, NodeTypes})
    end.

validate_registerOnNodes_list([NodeType | Tail]) ->
    case swmI:is_node_type_valid(NodeType) of
	true ->
	    validate_registerOnNodes_list(Tail);
	deprecated ->
	    add_proc_dict(registerOnNodes,
			  [{"Deprecated Node Type", NodeType}]),
	    validate_registerOnNodes_list(Tail);
	false ->
	    erlang:error({illegal_node_type, registerOnNodes, NodeType})
    end;
validate_registerOnNodes_list([]) ->
    case erase(registerOnNodes) of
	undefined ->
	    ok;
	Values ->
	    throw({warnings, Values})
    end.

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_register.erl %
%%% Author:     etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_register).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/3').
-date('2016-02-11').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-07-12   etxlg     Created
%%% R2A/2      2013-08-27   etxlg     Editing
%%% R2A/3      2013-09-05   etxlg     /coli -> ?INTERNAL_DIR
%%% R2A/4      2013-09-11   etxlg     xml format changed
%%% R2A/5      2013-10-10   etxlg     exports for ecoli_debug
%%% R3A/1      2014-12-09   etxlg     handle multipart description
%%% R4A/5      2015-07-14   etxlg     Support both new  and old roles
%%% R4A/7      2015-09-15   uabesvi   error_logger -> sysInitI
%%% R5A/1      2016-02-10   uabesvi   updated error print outs
%%% R5A/3      2016-02-11   uabesvi   removed args
%%% ----------------------------------------------------------
%%%
-include("ecoli.hrl"). %need definition of ?INTERNAL_DIR
-include_lib("xmerl/include/xmerl.hrl").

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([appdata/3]).

%exported for use by ecoli_debug
-export([validate_and_add_cmd/1, all_props/0]).


-define(ALWAYS_REQUESTED, 
	[cli_name, cli_path, type, cxp_path, usage, description]).


%called by SWM when appdata with target=coli_reg|coli_auth is found
appdata(Prod_id, Version, #xmlElement{name = appdata} = App_data) ->
    {ok, Cxp_path} = swmI:get_cxp_path(Prod_id, Version),
    process_registrations(App_data#xmlElement.content, Cxp_path),
    ok. %the returnvalue is not used in swmAppData

process_registrations([Reg | T], Cxp_path) ->
    process_reg(Reg, Cxp_path),
    process_registrations(T, Cxp_path);
process_registrations([], _) ->
    ok.

process_reg(#xmlElement{name = coli_external_reg} = Reg, Cxp_path) ->
    process_cont([{type, external}, {cxp_path, Cxp_path}], Reg);
process_reg(#xmlElement{name = coli_cri_reg} = Reg, Cxp_path) ->
    process_cont([{type, legacy}, {cxp_path, Cxp_path}], Reg);
process_reg(#xmlElement{name = coli_erlang_reg} = Reg, Cxp_path) ->
   process_cont( [{type, erts}, {cxp_path, Cxp_path}], Reg);
process_reg(#xmlElement{name = coli_rootfs_reg} = Reg, Cxp_path) ->
    process_cont([{type, rootfs}, {cxp_path, Cxp_path}], Reg);

process_reg(#xmlElement{name = coli_fruacc_reg} = Reg, Cxp_path) ->
    process_cont([{type, fruacc}, {cxp_path, Cxp_path}], Reg);

process_reg(#xmlElement{name = coli_authorization} = Auth, _) ->
    Coli_auth = parse_attributes(Auth#xmlElement.attributes),
    validate_and_add_auth(Coli_auth),
    ok;
process_reg(_, _) -> %skip unknown element
    ok.

process_cont(Sofar, #xmlElement{} = Reg) ->
    Desc  = get_description(parse_content(Reg#xmlElement.content)),
    Attrs = parse_attributes(Reg#xmlElement.attributes),
    validate_and_add_cmd(Sofar ++ Desc ++ Attrs),
    ok.

get_description({ok, Desc}) ->
    [{description, Desc}];
get_description(_) ->
    [].



parse_attributes([#xmlAttribute{name = cli_name, value = Value} | T]) ->
    New_value = lists:filter( %clean away any occurrence of "/"
	fun($/) -> false;
	    (_) -> true end, Value),
    [{cli_name, New_value} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = cli_path, value = Value} | T]) ->
    %% we require a "/" at the beginning, but none at the end
    New_value = 
	case {Value, lists:reverse(Value)} of
	    {[$/], _ } -> %% a single "/" is also ok
		Value;
	    {[$/ | _], [$/ | Rev] } -> %% begins and ends with "/"
		lists:reverse(Rev);
	    {[$/ | _], _ } -> %% begins with "/"
		Value;
	    {_, [$/ | Rev] } -> %% ends with "/"
		[$/ | lists:reverse(Rev)];
	    _ -> %% no "/" either in front nor in back
		[$/ | Value]
	end,
    [{cli_path, New_value} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = authorization, value = Value} | T]) ->
    [{authorization, Value} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = usage, value = Value} | T]) ->
    [{usage, Value} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = relpath, value = Value} | T]) ->
    [{relpath, Value} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = subcommand, value = Value} | T]) ->
    [{subcommand, subcommand_to_list(Value)} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = filepath, value = Value} | T]) ->
    [{filepath, Value} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = module, value = Value} | T]) ->
    [{module, list_to_atom(Value)} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = function, value = Value} | T]) ->
    [{function, list_to_atom(Value)} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = cli_scope, value = Value} | T]) ->
    [{cli_scope, list_to_atom(Value)} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = fru_type, value = Value} | T]) ->
    [{fru_type, list_to_atom(Value)} | parse_attributes(T)];
parse_attributes([#xmlAttribute{name = cli_type, value = Value} | T]) ->
    %% cli_type is always handled as lower case whithin ecoli
    [{cli_type, list_to_atom(string:to_lower(Value))} | parse_attributes(T)];
parse_attributes([_ | T]) ->
    parse_attributes(T);
parse_attributes([]) ->
    [].

parse_content([#xmlElement{name = description, content = Cont} | _]) ->
    {ok, concat_descriptions(Cont, <<>>)};
parse_content([_ | T]) ->
    parse_content(T);
parse_content([]) ->
    {nok, <<>>}.

concat_descriptions([#xmlText{value = Text} | T], Acc) ->
    concat_descriptions(T, <<Acc/binary, (list_to_binary(Text))/binary>>);
concat_descriptions([], Acc) ->
    Acc.

subcommand_to_list(Value) ->
    string:tokens(Value, "\s\t").

% in the IWD there is only "legacy_auth_strings", but incase someone is clever,
% lets support also the "official_auth_strings"
validate_and_add_auth(Coli_auth) ->
    [Name, Path, Auth] = All = [proplists:get_value(Prop, Coli_auth) ||
				Prop <- [cli_name, cli_path, authorization]],
    case {lists:any(fun(undefined) -> true;
			(_) -> false end, All),
	    lists:member(Auth,
			 ecoli_lib:get_official_auth_strings() ++
			     ecoli_lib:get_legacy_auth_strings())} of
	{false, true} ->
	    ecoli_datainit:add_auth(Name,
				    Path,
				    ecoli_lib:auth_to_integer(Auth));
	Why ->
	    warn_msg("Malformed XML in ecoli authorization: [~p, ~p]  - "
			"skipping entry", [Why, All])
    end.

validate_and_add_cmd(Coli_reg) ->
    Type = proplists:get_value(type, Coli_reg),
    Cmd_name = proplists:get_value(cli_name, Coli_reg),
    Cmd_path = proplists:get_value(cli_path, Coli_reg),
    {ok, Required_props} = get_required_props(Type),
    Required = ?ALWAYS_REQUESTED ++ Required_props,
    validate_and_add_cmd(validate_requested(Coli_reg, Required),
			 [Opt || {K, _} = Opt <- Coli_reg, not lists:member(K, Required)],
			 is_legal_name(Cmd_name), 
			 is_legal_dir(Cmd_path),
			 Cmd_name,
			 Cmd_path,
			 Coli_reg).

    
validate_and_add_cmd({ok, Valid_props}, Optional, true, true, _, _, _) ->
    ecoli_datainit:add_cmd(Valid_props ++  Optional);
validate_and_add_cmd({ok, _}, _, false, _, Cmd_name, _, _) ->
    warn_msg("Illegal cmd-name  in ecoli registration - discarding: ~p",
	     [Cmd_name]);
validate_and_add_cmd({ok, _}, _, _, false, _, Cmd_path, _) ->
    warn_msg("Illegal cmd-path  in ecoli registration - discarding: ~p",
	     [Cmd_path]);
validate_and_add_cmd(already_printed, _, _, _, _, _, _) ->
    ok;
validate_and_add_cmd(Why, _, _, _, _, _, Coli_reg) ->
    warn_msg("Malformed XML in ecoli cmd: [~p, ~p] - skipping entry",
	     [Why, Coli_reg]).


validate_requested(PropList, Required) ->
    RequiredProps = [proplists:get_value(P, PropList) || P <- Required],
    case lists:any(fun(undefined) -> true;
		      (_)         -> false end, RequiredProps) of
	true ->
	    vr_warning(PropList, Required);
	_ ->
	    {ok, lists:zip(Required, RequiredProps)}
    end.


vr_warning(PropList, Required) ->
    Fun = fun({type, Type},     {_, Path, Attrs}) -> {Type, Path, Attrs};
	     ({cxp_path, Path}, {Type, _, Attrs}) -> {Type, Path, Attrs};
	     (Attr, {Type, Path, Attrs})          -> {Type, Path, [Attr | Attrs]}
	  end,
    {Type, CxpPath, Attrs} = 
	lists:foldl(Fun, {undefined, undefined, []}, PropList),

    ReqAttrs = (Required -- [type, cxp_path]) -- [Attr || {Attr, _} <- Attrs],

    warn_msg("Malformed XML for ecoli cmd: missing required attribute(s). "
	     "Skipping entry.~n"
	     "Type    = ~p~n"
	     "CxpPath = ~p~n"
	     "Received attributes = ~p~n"
	     "Missing  attributes = ~p~n",
	     [Type, CxpPath, Attrs, ReqAttrs]),
    already_printed.


get_required_props(external) ->
    {ok, [relpath]};
get_required_props(legacy) ->
    {ok, [subcommand]};
get_required_props(erts) ->
    {ok, [module, function]};
get_required_props(rootfs) ->
    {ok, [filepath]};
get_required_props(fruacc) ->
    {ok, [cli_type]}.

%% certain command names are defined internally in ECOLI and they are
%% illegal in normal registration usage (independently of the commmands path)
is_legal_name("cd") -> false;
is_legal_name("..") -> false;
is_legal_name("exit") -> false;
is_legal_name("help") -> false;
is_legal_name(_) -> true.

% the command directory ?INTERNAL_DIR is reserved for use by internal coli
% commands
is_legal_dir(?INTERNAL_DIR) -> false;
is_legal_dir(?INTERNAL_DIR "/" ++ _) -> false;
is_legal_dir(_) -> true.


%% warn_msg(Fmt) ->
%%     warn_msg(Fmt, []).
warn_msg(Fmt, Data) ->
    Format = "~p: " ++  Fmt ++ "~n",
    sysInitI:warning_msg(Format, [?MODULE | Data]).

all_props() ->
    [type, cli_name, cli_path, args, cxp_path, subcommand, filepath,
     module, function, usage, description, cli_scope].

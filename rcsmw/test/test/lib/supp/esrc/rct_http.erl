%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_http.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R4A/R9A/2
%%% @doc ==Common Test hook for sending http(s) requests==
%%%
%%%
%%% Builds http requests and starts OTP application inets.<br/>
%%%
%%% Hook formats:
%%% ```{rct_http, [{N, Name, IPType}]}'''
%%%
%%% There is a short format for testing towards one node:
%%% ```{rct_http, Name}  expands to {rct_http, [{1, Name, ssh_lmt_ipv4}]}'''
%%% 
%%% There is a short format for testing towards clustered node:
%%% ```{rct_http, [Name1,Name2]} expands to {rct_http, [{1, Name1, ssh_lmt_ipv4},{2, Name2, ssh_lmt_ipv4}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simuleted environment.
%%%    Name     = atom()                         Used as identifier
%%%    IPType   = ssh_lmt_ipv4 | ssh_lmt_ipv6    Used in target env to specify which IP address http uses.
%%%                                              Requires config variables below to be specified in stp.cfg file: 
%%%                                              {ssh_lmt_ipv4, [{ssh, string()}]},
%%%                                              {ssh_lmt_ipv6, [{ssh, string()}]},
%%%                                              Not used in simulated environment'''
%%% Example single node:<br/>
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_http,www}]}].'''
%%% Example clustered nodes
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_http,[www1, www2]}]}].'''
%%%
%%% Testcase example.
%%% ```mytest(_) ->
%%%        {ok, Url} = rct_http:http_url(www,"/models/ComFm.xml"),
%%%        {ok, Reply} = httpc:request(Url).'''
%%% @end

-module(rct_http).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/R9A/2').
-date('2017-02-02').
-author('etxkols').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% R2A/1      2013-04-04 etxkols     Created
%%% R2A/2      2013-04-10 etxkols     Added support to reuse Aliases
%%% R2A/3      2014-03-26 etxkols     Faulty init/2 return value
%%% R4A/1      2015-05-28 etxkols     cluster and always use du1
%%% R4A/2      2015-05-28 etxkols     Fix du1 when single suite on clustered nodes
%%% R4A/3      2015-06-02 etxkols     Fix terminate/1
%%% R4A/4      2015-06-02 etxkols     Fix https_url/2
%%% R4A/5      2015-06-02 etxkols     No http in R4
%%% R4A/7      2016-03-08 etxkols     5G
%%% R9A/1      2017-02-01 etxkols     Refactoring and support for multi instances in 5g cloud
%%% R9A/2      2017-02-02 etxkols     Removed comments
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([init/2,
	 pre_init_per_suite/3,
	 terminate/1,
	 http_url/2,
	 https_url/2,
	 update_config/2]).

-include_lib("common_test/include/ct.hrl").

-define(TARGET_WWW_PORT, 8080).

%%===========================================================================
%% @spec http_url(Name, Request) ->
%%    {ok, Url} | {error, Reason}
%% Name = atom()
%% Request = string()
%% Url = url()
%% Reason = term()
%%
%% @doc Makes http Url for sim or target to be used in conjuction with <a href="http://www.erlang.org/doc/man/httpc.html">httpc</a>.<br/>
%% Example: 
%% ```httpc:request(http_url(node1, "/models/ComFm.xml")). ->
%%       "http://10.86.148.187:8080/models/ComFm.xml"'''
http_url(Name, Request) ->
    http_urls(Name, Request, http).

%%===========================================================================
%% @spec https_url(Name, Request) ->
%%    {ok, Url} | {error, Reason}
%% Name = atom()
%% Request = string()
%% Url = url()
%% Reason = term()
%%
%% @doc Makes https Url for sim or target to be used in conjuction with <a href="http://www.erlang.org/doc/man/httpc.html">httpc</a>.<br/>
%% Example: 
%% ```httpc:request(https_url(node1, "/models/ComFm.xml")). ->
%%       "https://10.86.148.187:8080/models/ComFm.xml"'''
https_url(Name, Request) ->
    crypto:start(),
    ssl:start(),
    http_urls(Name, Request, https).

http_urls(Name, Request, Type) ->
    case ct:get_config(make_name_module(Name)) of
	[{ssh,IP},{port,Port}|_] ->
	    case Type of
		http ->
		    {ok, "http://" ++ IP ++ ":"++integer_to_list(Port) ++ Request};
		https ->
		    case os:getenv("SIM_OR_TARGET") of
			"sim" -> 
			    {ok, "https://" ++ IP ++ ":"++integer_to_list(Port) ++ Request};
			"target" ->
			    {ok, "https://" ++ IP ++ Request};
			"cloudish" ->
			    {ok, "https://" ++ IP ++ Request}
		    end;
		Other ->
		    {error, Other}
	    end
    end.
    
%%% @hidden
%%% init function for ct_hook
init(_Id, Opts) ->
    {ok,Opts}.

%%===========================================================================
%% @spec pre_init_per_suite(Suite, Config, States) -> 
%%    {Config, States} | {{fail,Reason}, States}
%%
%% @doc Prepares IP and Ports for http requests and starts OTP application inets.<br/>
pre_init_per_suite(_Suite,Config = {fail,_},States) -> {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States) -> {Config,States};
pre_init_per_suite(Suite,Config,Name) when is_atom(Name) ->
    pre_init_per_suite(Suite,Config,[Name]);  
pre_init_per_suite(_Suite,Config,CthState) ->
    case inets:start() of
        R when R =:= ok;
               R =:= {error,{already_started,inets}} ->
	    case do_pre_init_per_suite(CthState,[],1) of
		{ok,CthState2} ->                       
		    AliasToHooks = [{Name, {N, ?MODULE}}||{N,Name,_}<-CthState2],		
		    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
				    undefined ->
					Config ++ [{alias_to_hooks,AliasToHooks}];
				    OldAliasToHooks ->
					lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
				end,
		    {NewConfig, CthState2};
		Other ->
                     Other
	    end;
        _ ->
            {{fail, "inets could not be started!"},CthState}
    end.

do_pre_init_per_suite([],R,_Num) ->
    {ok,R};
do_pre_init_per_suite([Name|T],R,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name,ssh_lmt_ipv4}|T],R,Num);
do_pre_init_per_suite([CthState = {N, Name, IPType}|T],R,Num) ->
    case get_port(N, Name) of
	{ok,Port} ->
	    case get_ip(N, Name, IPType) of
		{ok, IP} ->
		    ok = rct_multi_node_cfg:require(make_name_module(Name),[{ssh,IP},{port,Port},{iptype,IPType}]),
		    do_pre_init_per_suite(T,R ++ [CthState],Num + 1);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

%% @hidden
%% Clean away items in ct_attributes
terminate(CthState) ->
    do_terminate(CthState),
    ok.

do_terminate([]) ->
    ok;
do_terminate([{_,Name,_}|T]) ->
    rct_multi_node_cfg:remove_config(make_name_module(Name)),
    do_terminate(T).

get_ip(N, Name, IPType) ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> 
	    {ok, "localhost"};
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    case rct_multi_node_cfg:get_config(N,IPType) of
		undefined ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[IPType,Name,N]});
		[{ssh,IP}|_] ->
		    {ok,IP};			    
		LoginData ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter ssh in ~p for ~p in node ~p",[LoginData,Name,N]})
            end
    end.

get_port(N, Name) ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    {ok, ?TARGET_WWW_PORT};
	"cloudish" ->
	    {ok, ?TARGET_WWW_PORT};
	"sim" -> 
            User = os:getenv("USER"),
            {ok, HostName} = inet:gethostname(),          
            ErlNode = list_to_atom(User ++ "@" ++ HostName),
	    case rpc:call(ErlNode, sysEnv, get_port_conf, [https]) of
	    	Port when is_integer(Port) ->
	    	    {ok,Port};
	    	Other ->
                    fail_generic(?FUNCTION_NAME,?LINE,{"Could not get http port for ~p (~p) in node ~p, Reason ~p",[Name,ErlNode,N,Other]})
	    end
    end.

make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%% @doc callback function for updating configuration data.
%% ```Name = atom()                      Alias for cli or coli towards node.
%%    Update = tuple()                   {ssh_lmt_ipv4, [{ssh, IP}, {port, Port}, {user, User}, {password, Password}]} '''
update_config(Name, Update) ->
    Name2 = make_name_module(Name),
    IPType = proplists:get_value(iptype, ct:get_config(Name2)),
    update_config(Name2,IPType,Update).

update_config(Name, IPType, {IPType, Data}) ->
    Config = ct:get_config(Name),
    IP = proplists:get_value(ssh, Data),
    NewConfig=lists:keyreplace(ssh,1,Config,{ssh, IP}),
    rct_multi_node_cfg:remove_config(Name),
    ok = rct_multi_node_cfg:require(Name, NewConfig);
update_config(_,_,_) ->
    not_affected_by_update.

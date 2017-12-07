%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_netconf.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R9A/R10A/2
%%% @doc ==Common Test hook for sending and receiving netconf messages==
%%%
%%% This module contains a common test hook implements at netconf client
%%% using erlang/OTP ct_netconfc. 
%%% Please not that additional cthook {cth_conn_log,[]} is necessary for
%%% netconf logging to work.
%%%
%%% Hook format:
%%% ```{rct_netconf, [{N, Name, IPType, Nopts, Log}]},
%%%    {cth_conn_log,[]}'''
%%% Old hook format is still supported for legacy reasons, but new testsuites should use hook format above.
%%% ```{rct_netconf, {[{N, Name, IPType}], Log | Auth | Opts}]},
%%%    {cth_conn_log,[]}'''
%%%
%%% There are two short formats when running towards one card:
%%% ```{rct_netconf, Name}         expands to {rct_logging, [{1, Name, oam_auto,[{user,"expert"},{password,"expert"}], html}]}
%%%    {rct_netconf, {Name, Nopts} expands to {rct_logging, [{1, Name, oam_auto, Nopts, html}]}'''
%%% Old format is still supported, but new testsuites should use hook format above.
%%% ```{rct_netconf, {Name, Opts}  expands to {rct_logging, {[{1, Name, oam_auto, [{user,"expert"},{password,"expert"}] | [], Log}]}}'''
%%%
%%% There are two short format when running towards multi nodes:
%%% ```{rct_netconf, [Name1, Name2]}                      expands to {rct_logging,  [{1, Name1, oam_auto, [{user,"expert"},{password,"expert"}], html},
%%%                                                                                  {2, Name2, oam_auto, [{user,"expert"},{password,"expert"}], html}]}
%%%    {rct_netconf, [{Name1,Nopts1}, {Name2,Nopts2}]}    expands to {rct_logging,  [{1, Name1, oam_auto, Nopts1, html},
%%%                                                                                  {2, Name2, oam_auto, Nopts2, html}]}'''
%%% Old format is still supported, but new testsuites should use hook format above.
%%% ```{rct_netconf, {[Name1, Name2], Log | Auth | Opts}} expands to {rct_logging, {[{1, Name1, oam_auto, [{user,"expert"},{password,"expert"}] | [], Log},
%%%                                                                                  {2, Name2, oam_auto, [{user,"expert"},{password,"expert"}] | [], Log}]}}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                        Used to match card in stp.cfg file when running on target.
%%%                                                Not used in simuleted environment.
%%%    Name     = atom()                           Used as identifier
%%%    IPType   =                                  Used in target env to specify which IP address netconf uses,
%%%               oam_auto |                       Management IP address will be automatically selected depending on precence of -oamap_ipv4 or -oamap_ipv6 flags to rct_run.sh or
%%%                                                `{jenkins_config,[{oamap_ipv4, []}]}', `{jenkins_config,[{oamap_ipv6, []}]}' config parameters.
%%%               ssh_lmt_ipv4 |                   LMT IPv4 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv4 |                  TNA IPv4 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv4_alt|               Alternateive TNA IPv4 is used, read from stp.cfg file
%%%               ssh_lmt_ipv6 |                   LMT IPv6 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv6 |                  TNA IPv6 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv6_alt|               Alternateive TNA IPv6 is used, read from stp.cfg file
%%%                                                Not used in simulated environment
%%%    NOpts   = [{user, User}|{password,Password}|{port,Port}] 
%%%                                                Options per node
%%%    Log     = html | pretty | raw | silent      Specifies logging options. See documentation for ct_netconfc.
%%%    Opts    = [ Log | Auth ]                    Options
%%%    Auth    = man_auth | expert_auth            man_auth expands to [] for each node
%%%                                                expert_auth expands to [{user,"expert"},{password,"expert"}] for each node
%%%                                                man_auth is used to set user/passwd when ct_netconfc:open/2
%%%                                                default is expert_auth where user/passwd = expert/expert'''
%%% 
%%% If `rcstprep.sh' script (wrapped by `rcs_install.sh' script) has been run with `-oamap_ipv4' or `-oamap_ipv6' flag, OaM Accesspoint has been configured with either ipv4 or ipv6 address during installation.
%%%
%%% If `IPType=oam_auto' (default) is set in the testsuite, the management IP address will be automatically selected depending on precense of `-oamap_ipv4' or `-oamap_ipv6' as arguments to `rct_run.sh' or `{jenkins_config,[{oamap_ipv4, []}]}' or `{jenkins_config,[{oamap_ipv6, []}]}' as config parameters, Examples:
%%%
%%% ```rct_run.sh -stp dus5000                  will run netconf commands over ssh_lmt_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv4      will run netconf commands over ssh_TN_A_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv6      will run netconf commands over ssh_TN_A_ipv6'''
%%%
%%% `-oamap_ipv4' and `-oamap_ipv6' has precedence over `{jenkins_config,[{oamap_ipv4, []}]}' and `{jenkins_config,[{oamap_ipv6, []}]}', i.e. the config parameters will only be checked if the arguments are not given to `rct_run.sh'.
%%% 
%%% Examples single node:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_netconf, nc},
%%%                     {cth_conn_log,[]}]}].'''
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_netconf, {nc,man_auth}},
%%%                     {cth_conn_log,[]}]}].'''
%%% Examples clustered nodes:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_netconf,[nc1,{nc2,[{port,2022},{user,"expert"},{password,"expert"}]}]},
%%%                     {cth_conn_log,[]}]}].'''
%%%
%%% Testcase example.
%%% ```get_config(_) ->
%%%        {ok,_} = ct_netconfc:open(nc1,[]),
%%%        {ok,_} = ct_netconfc:get_config(nc1,running,subtree,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],[{managedElementId,[],["1"]}]}),
%%%        ok = ct_netconfc:close_session(nc1).'''
%%% @end

-module(rct_netconf).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-03-19 etxkols     Created
%%% R1A/2      2012-03-21 etxkols     Comment out ssh_connection:close in client_loop
%%% R1A/3      2012-05-25 etxkols     Removed ssh option ip_v6_disabled
%%% R1A/4      2012-06-11 etxkols     Rewritten for ct_netconfc
%%% R1A/5      2012-06-28 etxkols     Fixed when ssh & crypto already started
%%% R1A/6      2012-06-28 etxkols     Fixed tabs
%%% R1A/7      2012-07-05 etxkols     Updated for new ct_netconfc
%%% R1A/8      2012-10-16 etxkols     Fixed edoc
%%% R2A/1      2013-03-21 etxkols     Added update_port/1 for port change when rcssim is restarted
%%% R2A/2      2013-11-06 etxkols     Added new option man_auth to test arbitrary user and passwd
%%% R2A/3      2013-11-06 etxkols     Reverted change
%%% R2A/5      2013-11-07 etxkols     Another try at new option man_auth
%%% R2A/6      2013-11-12 etxkols     Deleting data in ct_attributes in post_end_per_suite.
%%%                                   etxkols will write ticket on CommonTest when using specs.
%%% R2A/7      2014-04-23 etxkols     Port fix for TLM
%%% R3A/1      2015-02-13 etxkols     Added -netconf_port argument to make port 830 migration easier
%%%                                   replaced post_end_per_suite with terminate
%%% R3A/2      2015-03-24 eolaand     Add fcn get_port
%%% R3A/3      2015-03-31 eolaand     Add clause for SkipOrFail
%%% R3A/4      2015-05-29 etxkols     Cluster fixes
%%% R3A/5      2015-05-29 etxkols     Bug in terminate
%%% R3A/6      2015-05-29 etxkols     Bug no 2 in terminate
%%% R3A/7      2015-06-01 etxkols     Bug no 3 in terminate
%%% R4A/5      2015-11-30 etxkols     oam_auto default IPType, will select OAMAP or LMT
%%% R5A/1      2016-02-02 etxkols     ipv6
%%% R5A/2      2016-02-04 etxkols     Temporarily backing out OAMAP for sec board until Jenkins is synced.
%%% R5A/3      2016-02-09 etxkols     New flags
%%% R5A/4      2016-02-10 etxkols     Documentation
%%% R5A/5      2016-02-10 etxkols     Documentation
%%% R5A/6      2016-02-12 etxkols     using rct_oamap_ipv:mgmnt_iptype/1
%%% R5A/8      2016-03-08 etxkols     5G
%%% R5A/9      2016-10-10 etxkols     Changed ssh behavior in OTP 19.1.2
%%% R9A/1      2017-02-01 etxkols     Refactoring and support for multi instances in 5g cloud
%%% R9A/2      2017-02-02 etxkols     Removed comments
%%% R9A/3      2017-02-09 etxkols     ssh_lmt_ipv6 exists in cloud
%%% R9A/4      2017-04-06 etxkols     Added possiblility to specify port number
%%% R10A/1     2017-05-09 etxkols     Type = sd (service discovery) added
%%% R10A/2     2017-09-13 etxkols     Delete _iptype in terminate/1
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([init/2,
	 pre_init_per_suite/3,
	 terminate/1,
	 update_port/1,
	 get_port/1,
	 update_config/2]).

-include_lib("common_test/include/ct.hrl").
-define(DEFAULT_TARGET_NETCONF_PORT,2022).
-define(DEFAULT_CLOUD_NETCONF_PORT,830).
%%===========================================================================
%% @spec update_port(Name) ->
%%    ok | {error, Reason}
%% Name = atom()
%%
%% @doc Special function for verifying simulated upgrade where port changes<br/>
update_port(Name) ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    ok;
	"cloudish" ->
	    ok;
	"sim" -> 
	    case get_port(Name) of 
		{ok,Port} ->
		    OldVals = rct_multi_node_cfg:remove_config(Name),
		    Vals = lists:keyreplace(port, 1, OldVals, {port, Port}),
		    rct_multi_node_cfg:require(Name, Vals);
		{fail, Reason} ->
		    {error, Reason}
	    end
    end.

%%% @hidden
%%% init function for ct_hook
%%% Clumsy because of backwards compability reasons.
init(_Id, Name) when is_atom(Name)->
    CthState = [{1,Name,oam_auto,[{user,"expert"},{password,"expert"}],html}],
    [rct_multi_node_cfg:append_config(ct_conn_log,[{ct_netconfc,[{log_type,Log},{hosts,[N]}]}])||{_,N,_,_,Log}<-CthState],
    {ok, CthState};
init(_Id, Name) when is_list(Name)->
    CthState = expand_names(Name,1,[]),
    [rct_multi_node_cfg:append_config(ct_conn_log,[{ct_netconfc,[{log_type,Log},{hosts,[N]}]}])||{_,N,_,_,Log}<-CthState],
    {ok, CthState};
init(_Id, {Name, Opts}) when is_atom(Name) ->
    CthState = [fix_opt({1,Name,oam_auto},Opts)],
    [rct_multi_node_cfg:append_config(ct_conn_log,[{ct_netconfc,[{log_type,Log},{hosts,[N]}]}])||{_,N,_,_,Log}<-CthState],
    {ok, CthState};
init(_Id, {Name, Opts}) when is_list(Name) -> % old format
    OldNames = expand_old_names(Name,1,[]),
    CthState = [fix_opt(OldName,Opts)||OldName<-OldNames], % {N,Name,IPType}
    [rct_multi_node_cfg:append_config(ct_conn_log,[{ct_netconfc,[{log_type,Log},{hosts,[N]}]}])||{_,N,_,_,Log}<-CthState],
    {ok, CthState}.

expand_old_names([],_Num,R) -> % old format
    R;
expand_old_names([{N,Name,IPType}|T],Num,R) ->
    expand_old_names(T,Num+1,R++[{N,Name,IPType}]);
expand_old_names([Name|T],Num,R) ->
    expand_old_names(T,Num+1,R++[{Num,Name,oam_auto}]).

expand_names([],_Num,R) ->
    R;
expand_names([Name|T],Num,R) when is_atom(Name) ->
    expand_names(T,Num+1,R++[{Num,Name,oam_auto,[{user,"expert"},{password,"expert"}], html}]);
expand_names([{Name,Opts}|T],Num,R) when is_atom(Name) ->
    expand_names(T,Num+1,R++[{Num,Name,oam_auto,Opts,html}]);
expand_names([{N,Name,IPType,Nopts,Log}|T],Num,R) when is_atom(Name) ->
    expand_names(T,Num+1,R++[{N,Name,IPType,Nopts,Log}]);
expand_names([{N,Name,IPType}|T],Num,R) when is_atom(Name) -> % People have used old undocumented format, fixing it anyway
    expand_names(T,Num+1,R++[{N,Name,IPType,[{user,"expert"},{password,"expert"}],html}]).

fix_opt({N,Name,IPType},Opt) when is_atom(Opt) -> % old format
    case Opt of 
	_Opt when _Opt == html; _Opt == pretty; _Opt == raw; _Opt == silent ->
	    {N,Name,IPType,[{user,"expert"},{password,"expert"}],Opt};
	man_auth ->
	    {N,Name,IPType,[],html};
	_ -> % expert_auth
	    {N,Name,IPType,[{user,"expert"},{password,"expert"}],html}
    end;
fix_opt({N,Name,IPType},Opt) when is_list(Opt) ->
    Log = case [X||X<-[html,pretty,raw,silent], lists:member(X,Opt)] of
	      [L] -> L;
	      []  -> html
	  end,
    case lists:member(man_auth,Opt) of
	true  -> {N,Name,IPType,[],Log};
	false -> {N,Name,IPType,[{user,"expert"},{password,"expert"}],Log}
    end.

%%% @hidden
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CthState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CthState};
pre_init_per_suite(_Suite,Config,CthState) ->
    case catch {crypto:start(), ssh:start()} of
	R when R =:= {ok, ok};
               R =:= {ok, {error,{already_started,ssh}}};
               R =:= {{error,{already_started,crypto}}, ok};
	       R =:= {{error,{already_started,crypto}}, {error,{already_started,ssh}}} ->
	    case do_pre_init_per_suite(CthState) of
		ok ->
		    AliasToHooks = [{Name, {N, ?MODULE}}||{N,Name,_,_,_}<-CthState],		
		    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
				    undefined ->
					Config ++ [{alias_to_hooks,AliasToHooks}];
				    OldAliasToHooks ->
					lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
				end,
		    {NewConfig, CthState};
		Other ->
		    Other
	    end;
	_ ->
	    {{skip, "Crypto and/or SSH could not be started!"},CthState}
    end.

do_pre_init_per_suite([]) ->
    ok;
do_pre_init_per_suite([{N, Name, IPType, Nopts, _Log}|T]) ->
    case get_port(N, Name, Nopts) of
	{ok,Port} ->
	    IPType2 = rct_oamap_ipv:mgmnt_iptype(IPType),
	    Inet = case IPType2 of
		       ssh_lmt_ipv6 -> inet6;
		       ssh_TN_A_ipv6 -> inet6;
		       ssh_TN_A_ipv6_alt -> inet6;
		       _ -> inet						
		   end,
	    case get_ip(N, Name, IPType2) of
		{ok, IP} ->
		    ConnData = [{ssh,IP},{port,Port},{inet,Inet}] ++ proplists:delete(port,Nopts),
		    timer:sleep(1000),

		    %% ConnData = case Auth of
		    %% 		   expert_auth -> [{ssh,IP},{port,Port},{user,"expert"},{password,"expert"},{inet,Inet}];
		    %% 		   man_auth    -> [{ssh,IP},{port,Port},{inet,Inet}]
		    %% 	       end,
		    ok = rct_multi_node_cfg:require(Name,ConnData),
		    ok = rct_multi_node_cfg:require(list_to_atom(atom_to_list(Name)++"_iptype"),[IPType2]), % needed to store IP type
		    do_pre_init_per_suite(T);
 		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

%%% If `rcstprep.sh' script (wrapped by `rcs_install.sh' script) has been run with `-oamap_ipv4' or `-oamap_ipv6' flag, OaM Accesspoint has been configured with either ipv4 or ipv6 address during installation.
%%%
%%% If `IPType=oam_auto' (default) is set in the testsuite, the management IP address will be automatically selected depending on precense of `-oamap_ipv4' or `-oamap_ipv6' as arguments to `rct_run.sh' or `{jenkins_config,[{oamap_ipv4, []}]}' or `{jenkins_config,[{oamap_ipv6, []}]}' as config parameters, Examples:
%%%
%%% ```rct_run.sh -stp dus5000                  will run netconf commands over ssh_lmt_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv4      will run netconf commands over ssh_TN_A_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv6      will run netconf commands over ssh_TN_A_ipv6'''
%%%
%%% `-oamap_ipv4' and `-oamap_ipv4' has precedence over `{jenkins_config,[{oamap_ipv4, []}]}' and `{jenkins_config,[{oamap_ipv6, []}]}', i.e. the config parameters will only be checked if the arguments are not given to `rct_run.sh'.
%% check_iptype(_N,oam_auto) ->
%%     case init:get_argument(oamap_ipv4) of
%% 	{ok,[[]]} ->
%% 	    ssh_TN_A_ipv4;
%% 	_ ->
%% 	    case init:get_argument(oamap_ipv6) of
%% 		{ok,[[]]} ->
%% 		    ssh_TN_A_ipv6;
%% 		_ ->
%% 		    case ct:get_config({jenkins_config,oamap_ipv4}) of
%% 			[] ->
%% 			    ssh_TN_A_ipv4;
%% 			_ ->
%% 			    case ct:get_config({jenkins_config,oamap_ipv6}) of
%% 				[] ->
%% 				    ssh_TN_A_ipv6;
%% 				_ ->
%% 				    ssh_lmt_ipv4
%% 			    end
%% 		    end
%% 	    end
%%     end;
%% check_iptype(_N,IPType) ->
%%     IPType.

%%% @hidden
terminate(CthState) ->
    rct_multi_node_cfg:remove_config(ct_conn_log,ct_netconfc),
    [rct_multi_node_cfg:remove_config(Name)||{_, Name, _, _, _}<-CthState],
    [rct_multi_node_cfg:remove_config(list_to_atom(atom_to_list(Name) ++ "_update_port"))||{_, Name, _, _, _}<-CthState],
    [rct_multi_node_cfg:remove_config(list_to_atom(atom_to_list(Name) ++ "_iptype"))||{_, Name, _, _, _}<-CthState],
    ssh:stop(),
    crypto:stop(),
    ok.

%%% @hidden
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


%%===========================================================================
%% @spec get_port(Name) ->
%%    {ok,Port} | {error, Reason}
%% Name = atom()
%% Port = integer()
%% @doc Get netconf port number for Name after upgrade<br/>
get_port(Name) ->    
    case lists:keysearch(port,1,ct:get_config(Name)) of
	{value,{port,Port}} -> {ok, Port};
	Reason -> {error, Reason}
    end.

%%% @hidden
get_port(N, Name, Nopts) ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" -> 
            User = os:getenv("USER"),
            {ok, HostName} = inet:gethostname(),          
            ErlNode = list_to_atom(User ++ "@" ++ HostName),
	    case rpc:call(ErlNode, sysEnv, get_port_conf, [netconf]) of
		Port when is_integer(Port) ->
		    {ok,Port};
		Other ->
                    fail_generic(?FUNCTION_NAME,?LINE,{"Could not get netconf port for ~p (~p) in node ~p, Reason ~p",[Name,ErlNode,N,Other]})
	    end;
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    case {get_ct_arg(netconf_port),rct_multi_node_cfg:get_config(N,netconf_port)} of
		{undefined,undefined} ->
		    case proplists:get_value(port,Nopts) of
			undefined ->
			    case {rct_node_state:get_type(N),TargetOrCloud} of
				{vnf,_}        -> {ok, ?DEFAULT_CLOUD_NETCONF_PORT};
				{vnfm,_}       -> {ok, ?DEFAULT_CLOUD_NETCONF_PORT};
				{sd,_}         -> {ok, ?DEFAULT_CLOUD_NETCONF_PORT};
				{du,_}         -> {ok, ?DEFAULT_TARGET_NETCONF_PORT};
				{_,"target"}   -> {ok, ?DEFAULT_TARGET_NETCONF_PORT};
				{_,"cloudish"} -> {ok, ?DEFAULT_CLOUD_NETCONF_PORT};
				Other          -> fail_generic(?FUNCTION_NAME,?LINE,{"Could not determine node type for ~p in node ~p, Reason: ~p",[Name,N,Other]})
			    end;
			Port ->
			    {ok, Port}
		    end;
		{Port,undefined} -> {ok, Port};
		{undefined,Port} -> {ok, Port};
		Other            -> fail_generic(?FUNCTION_NAME,?LINE,{"Could not determine node type for ~p in node ~p, Reason: ~p",[Name,N,Other]})
	    end
    end.

get_ct_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok,[[Reply]]} -> list_to_integer(Reply);
	error -> undefined
    end.

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.


%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%% @doc callback function for updating configuration data.
%% ```Name = atom()                      Alias for cli or coli towards node.
%%    Update = tuple()                   {ssh_lmt_ipv4, [{ssh, IP}, {port, Port}, {user, User}, {password, Password}]} '''
update_config(Name, Update) ->
    [IPType]=ct:get_config(list_to_atom(atom_to_list(Name)++"_iptype")), 
    update_config(Name,IPType,Update).

update_config(Name, IPType, {IPType, Data}) ->
    Config = ct:get_config(Name),
    IP = proplists:get_value(ssh, Data),
    NewConfig=lists:keyreplace(ssh,1,Config,{ssh, IP}),
    rct_multi_node_cfg:remove_config(Name),
    ok = rct_multi_node_cfg:require(Name, NewConfig);
update_config(_,_,_) ->
    not_affected_by_update.

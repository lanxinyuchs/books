%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_node_state.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/1
%%%
%%% @doc ==Used for multi node testing in a cloud environmet==
%%% 
%%% This hook is intended to be used for multi node and cloud testing and must be put first in the hook list in suite/0.
%%% 
%%% When testing towards a cloud environment with several nodes a number of problems arises:
%%% <ul>
%%% <li>In the cloud, properties like IP andresses may change during the test (Ex: IP addresses are not known until after the board is created)</li>
%%% <li>Erlang node names are the same for all cloud instances, i.e. root@vrcs</li>
%%% </ul>
%%% Other hooks that should be used for multinode testing in cloud environment, must adapt to following reqirements
%%% <ul>
%%% <li>Be able to handle that nodes are down when testsuites starts. Information on nodes state can be found by calling rct_node_state:get_state/1 and is initially set by this hook in the hook list in suite/0 function (see below)</li>
%%% <li>Must append certain data to an additional tuple with key alias_to_hooks added to Config variable, see examle below</li>
%%% <li>Implement update_config/2 function that should be able to handle that properties for the hook has changed.</li>
%%% </ul>
%%% Testsuite
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_node_state,[{N = integer(),Type = vnfm | vnf | du | sd, State = up | down }]}]}.
%%% 
%%%    suite() -> 
%%%        [{ct_hooks,[{rct_node_state, [{1,vnfm,up},{2,vnf,down},{3,du,up}]},
%%%		       {rct_coli,[{coli1,[manual_connect]},{coli2,[manual_connect]},{coli3,[manual_connect]}]},...]
%%% 
%%%    test(Config) ->
%%%        Config = [H|{alias_to_hooks,[{coli1,{1,rct_cli_coli_common}},
%%%                                     {coli2,{2,rct_cli_coli_common}},
%%%                                     {coli3,{3,rct_cli_coli_common}}]}] 
%%%        do something that will change the IP address of node 2 and inform coli hook about it
%%%        rct_node_state:update_config(2, [{[coli2], {ssh_lmt_ipv4, [{ssh, "10.67.239.239"}, {port, 22}, {user, "root"}, {password, "root"}]}}], Config).
%%%        rct_node_state:set_state_up(2),'''
%%% 
%%% This hook MUST be first in the hook list, suite/0, since other hooks will adapt to the initial properies given by this hook, ex State = down.
%%% 
%%% Looking at the rct_cli_coli_common.erl hook it adds below to the testcase Config parameter. This is done for this module to be able to identify which hooks are affected by a property change.
%%% ```{alias_to_hooks,[{Name = atom(),{N = integer(), CallbackModule = atom()}}]}
%%%    Name           = the name (alias) defined for the instance of the hook
%%%    N              = the node number, see .config file used by CommonTest
%%%    CallbackModule = Callback module which implements update_config/2 function'''
%%% 
%%% 
%%% 
%%% 
%%% 
%%% 
%%% @end

-module(rct_node_state). 
-id('Updated by CCase').
-vsn('/main/R9A/R10A/1').
-date('2017-05-09').
-author('etxkols').

%% Except as specifically authorized in writing by Ericsson, the receiver of
%% this document shall keep the information contained herein confidential and
%% shall protect the same in whole or in part from disclosure and dissemination
%% to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall only be made
%% on a strict need to know basis.
%% ----------------------------------------------------------
%% #1.    REVISION LOG
%% ----------------------------------------------------------
%% Rev        Date        Name        What
%% -----      ---------   --------    ------------------------
%% R9A/1      2017-01-23  etxkols    Created
%% R9A/2      2017-01-27  etxkols    Updates
%% R9A/3      2017-01-31  etxkols    New Config expected
%% R9A/5      2017-02-13  etxkols    Dirty fix in /etc/hosts
%% R9A/6      2017-02-14  etxkols    Added update_ip_addresses/3
%% R9A/7      2017-03-07  etxkols    fix_vnfm_resolv_conf/1
%% R10A/1     2017-05-09  etxkols    Type = sd (service discovery) added
%% -----------------------------------------------------------------------------

-export([id/1]).
-export([init/2]).
-export([terminate/1]).
-export([loop/1]).
-export([get_state/1]).
-export([get_type/1]).
-export([get_data/0]).
-export([update_lmt_ip/3]).
-export([set_state_up/1]).
-export([set_state_down/1]).
-export([update_config/3]).
-export([update_ip_addresses/3]).

%%% @hidden
id(_States) ->
    erlang:make_ref().

%%% @hidden
init(_Id, States) ->
    register(?MODULE, spawn(?MODULE, loop, [States])),
    fix_vnfm_etc_hosts(States),
    fix_vnfm_resolv_conf(States),
    {ok,States}.

%%% @hidden
terminate(_States) ->
    ?MODULE ! {self(),stop},    
    ok.

%%% @hidden
%%% Dirty fix for vnfm in cloud to resolve vim_url while waiting for cloud DNS 
fix_vnfm_etc_hosts([]) ->
    ok;
fix_vnfm_etc_hosts([{N,vnfm,up}|T]) ->
    crypto:start(),    
    ssh:start(),
    [{ssh, IP}, {port, Port}, {user, User}, {password, Pwd}] = rct_multi_node_cfg:get_config(N,ssh_lmt_ipv4),
    ct:pal("~p",[[{ssh, IP}, {port, Port}, {user, User}, {password, Pwd}]]),
    {ok,SSH} = ssh:connect(IP,Port,[{user_interaction,false},{silently_accept_hosts,true},{password,User},{user,Pwd},{quiet_mode, true},{connect_timeout, 10000}], 10000),
    {ok,Chn} = ssh_connection:session_channel(SSH, 5000),
    fix_etc_hosts(N,SSH, Chn),    
    ssh_connection:close(SSH, Chn),    
    ssh:close(SSH),
    fix_vnfm_etc_hosts(T);
fix_vnfm_etc_hosts([_|T]) ->
    fix_vnfm_etc_hosts(T).

fix_etc_hosts(N,SSH,Chn) ->
    No_proxy = rct_multi_node_cfg:get_config(N,{cloud_info,no_proxy}),
    Os_auth_url = rct_multi_node_cfg:get_config(N,{cloud_info,os_auth_url}),
    Line = No_proxy++" "++Os_auth_url,
    ct:pal("grep '"++Line++"' /etc/hosts || echo '"++Line++"'>> /etc/hosts"),
    success=ssh_connection:exec(SSH, Chn,"grep '"++Line++"' /etc/hosts || echo '"++Line++"'>> /etc/hosts" , 2000),
    receive
	{ssh_cm,SSH,{eof,_}} ->
	    ct:pal("Adding '10.68.97.4 ctrl-vran-redhat007.rnd.ki.sw.ericsson.se' to /etc/hosts in vnfm in node ~p",[N]);
	{ssh_cm,SSH,{exit_status,_,_}} ->
	    ct:pal("Adding '10.68.97.4 ctrl-vran-redhat007.rnd.ki.sw.ericsson.se' to /etc/hosts in vnfm in node ~p",[N]);
	{ssh_cm,SSH,{data,_,_,_Data}} -> % Data = <<"10.68.97.4 ctrl-vran-redhat007.rnd.ki.sw.ericsson.se\n">>}}
	    ok;
	Reply ->
	    fail_generic(?FUNCTION_NAME,?LINE,{"Unexpected reply when writing to /etc/hosts for vnfm node ~p, Reply: ~p",[N,Reply]})
    after 2000 ->
	    ct:pal("timeout")
    end,
    timer:sleep(1000),
    flush().

%%% @hidden
%%% Dirty fix for vnfm in cloud to be able to upload URL in zip package 
fix_vnfm_resolv_conf([]) ->
    ok;
fix_vnfm_resolv_conf([{N,vnfm,up}|T]) ->
    crypto:start(),    
    ssh:start(),
    [{ssh, IP}, {port, Port}, {user, User}, {password, Pwd}] = rct_multi_node_cfg:get_config(N,ssh_lmt_ipv4),
    ct:pal("~p",[[{ssh, IP}, {port, Port}, {user, User}, {password, Pwd}]]),
    {ok,SSH} = ssh:connect(IP,Port,[{user_interaction,false},{silently_accept_hosts,true},{password,User},{user,Pwd},{quiet_mode, true},{connect_timeout, 10000}], 10000),
    {ok,Chn} = ssh_connection:session_channel(SSH, 5000),
    fix_resolv_conf(N,SSH, Chn),    
    ssh_connection:close(SSH, Chn),    
    ssh:close(SSH),
    fix_vnfm_resolv_conf(T);
fix_vnfm_resolv_conf([_|T]) ->
    fix_vnfm_resolv_conf(T).

fix_resolv_conf(N,SSH,Chn) ->
    Line = "nameserver 147.214.6.234",
    ct:pal("grep '"++Line++"' /etc/resolv.conf || echo '"++Line++"'>> /etc/resolv.conf"),
    success=ssh_connection:exec(SSH, Chn,"grep '"++Line++"' /etc/resolv.conf || echo '"++Line++"'>> /etc/resolv.conf" , 2000),
    receive
	{ssh_cm,SSH,{eof,_}} ->
	    ct:pal("Adding '"++Line++"' to /etc/resolv.conf in vnfm in node ~p",[N]);
	{ssh_cm,SSH,{exit_status,_,_}} ->
	    ct:pal("Adding '"++Line++"' to /etc/resolv.conf in vnfm in node ~p",[N]);
	{ssh_cm,SSH,{data,_,_,_Data}} -> % Data = <<"10.68.97.4 ctrl-vran-redhat007.rnd.ki.sw.ericsson.se\n">>}}
	    ok;
	Reply ->
	    fail_generic(?FUNCTION_NAME,?LINE,{"Unexpected reply when writing to /etc/resolv.conf for vnfm node ~p, Reply: ~p",[N,Reply]})
    after 2000 ->
	    ct:pal("timeout")
    end,
    timer:sleep(1000),
    flush().

flush() ->
    receive 
	_ -> flush()
    after 1 ->
	    ok
    end.
    

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

%%% @spec get_state(N) -> up | down | undefined
%%% N = integer()
%%% @doc get registered node state for a node.
get_state(N) ->
    get_node_data(N, state).

%%% @spec get_type(N) -> atom() | undefined
%%% N = integer()
%%% @doc get registered node state for a node.
get_type(N) ->
    get_node_data(N, type).

get_node_data(N, Query) ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ?MODULE ! {self(), get_node_data, N, Query},
	    receive 
		{Pid, State} ->
		    State
	    end;
	_ ->
	    undefined
    end.

%%% @spec get_data() -> [{N, Type, up | down | undefined}]
%%% N = integer()
%%% Type = atom()
%%% @doc get all states for all registered nodes
get_data() ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ?MODULE ! {self(), get_data},
	    receive 
		{Pid, States} ->
		    States
	    end;
	_ ->
	    undefined
    end.

%%% @spec set_state_up(N) -> ok | undefined
%%% N = integer()
%%% @doc set state to up for node
set_state_up(N) ->
    set_state({N,up}).

%%% @spec set_state_down(N) -> ok | undefined
%%% N = integer()
%%% @doc set state to down for node
set_state_down(N) ->
    set_state({N,down}).

%%% @spec set_state({N,State}) -> ok | undefined
%%% N = integer()
%%% State = up | down | undefined
%%% @doc change state for a node
set_state({N,State}) ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ?MODULE ! {self(), set_node_state, N, State},
	    receive 
		{Pid, Result} ->
		    Result
	    end;
	_ ->
	    undefined
    end.

%% @hidden
loop(Data) ->
    receive 
	{_From, stop} ->
	    ok;
	{From, get_node_data, N, Query} ->
	    From ! {self(), get_loop_data(N, Query, Data)},
	    loop(Data);
	{From, get_data} ->
	    From ! {self(), Data},
	    loop(Data);
	{From, set_node_state, N, State} ->
	    {Result, Data2}=set_loop_state(N, State, Data),
	    From ! {self(), Result},
	    loop(Data2)
    end.
	
get_loop_data(N, Query, Data) ->
    case lists:keysearch(N,1,Data) of
	{value,Tuple} ->
	    case Query of
		type  -> element(2,Tuple);
		state -> element(3,Tuple)
	    end;
	_ ->
	    undefined
    end.

set_loop_state(N, NewState, Data) ->	
    case lists:keysearch(N,1,Data) of
	{value,Tuple}  ->
	    NewTuple = setelement(3,Tuple,NewState),
	    Data2=lists:keyreplace(N, 1, Data,NewTuple),
	    {ok, Data2};
	_ ->
	    {undefined, Data}
    end.

%% update_ip_addresses(2,[{pran_backhaul_ipv4,"10.224.177.26"},
%% 		          {om_ran_ipv4,"10.68.98.96"},
%% 		          {om_ran_ipv6,"2001:1b70:6282:b100::18d"},
%% 		          {ssh_lmt_ipv4,"10.68.98.94"},
%% 		          {ssh_lmt_ipv6,"2001:1b70:6282:b100::18c"},
%% 		          {pran_fronthaul_ipv4,"10.68.100.179"},
%% 		          {pran_fronthaul_ipv6,"2001:1b70:6282:b200::f7"}],
%% 		       [{watchdog,<0.169.0>},
%% 		        {tc_logfile,"/proj/rcs-tmp/stps/rcf_vnfm_rcf_etxkols/logs/ct_run.rcf_vnfm_rcf_etxkols@esekilxv8959.2017-02-15_09.57.10/ct2.jsone.vnmf_SUITE.logs/run.2017-02-15_09.57.10/vnmf_suite.create_vnf.html"},
%% 		        {priv_dir,"/proj/rcs-tmp/stps/rcf_vnfm_rcf_etxkols/logs/ct_run.rcf_vnfm_rcf_etxkols@esekilxv8959.2017-02-15_09.57.10/ct2.jsone.vnmf_SUITE.logs/run.2017-02-15_09.57.10/log_private.1/"},
%% 		        {tc_group_properties,[]},
%% 		        {tc_group_path,[]},
%% 		        {data_dir,"/home/etxkols/tmp/ct2/jsone/vnmf_SUITE_data/"},
%% 		        {alias_to_hooks,[{log1,{1,rct_logging}},
%% 			   	         {log2,{2,rct_logging}},
%% 				         {cli1,{1,rct_cli_coli_common}},
%% 				         {cli2,{2,rct_cli_coli_common}},
%% 				         {coli1,{1,rct_cli_coli_common}},
%% 				         {coli2,{2,rct_cli_coli_common}},
%% 				         {rpc1,{1,rct_rpc}},
%% 				         {rpc2,{2,rct_rpc}},
%% 				         {nc1,{1,rct_netconf}},
%% 				         {nc2,{2,rct_netconf}},
%% 				         {http1,{1,rct_http}},
%% 				         {http2,{2,rct_http}},
%% 				         {vnfm1,{1,rct_vnfm}},
%% 				         {rcs_core_unique_name1,{1,rct_core}},
%% 				         {rcs_core_unique_name2,{2,rct_core}}]}]) -> ok
%%% @spec update_ip_addresses(N,IPs,Config) -> ok 
%%% N = integer()
%%% IPs = [{Key, Value}]
%%% Config = list()
%%% Key = atom()
%%% Value = list()
%%% @doc Informs hooks that data has changed and updates config files on disk.
%%% ```N             The node that has changed the config, 0 will change toplevel config parameter.
%%%    IPs           List of Keys in .config file where content should be replaced with Value
%%%    Config        Config variable used in testcases now includes mapping of Name to hook callback file where update_config/2 is implemented'''
%%% Finds all aliases for node N and changes the IP addresses
%%% ```Change the .cfg file(s) on disk
%%%    Reload the CommonTest start configuration from the changed .cfg file from disk
%%%    Use callback function update_config/2 in hooks to update data for the aliases.
%%%
%%%    rct_node_state:update_ip_addresses(2, [{pran_backhaul_ipv4,"10.224.177.26"},
%%% 		                              {om_ran_ipv4,"10.68.98.96"},
%%% 		                              {om_ran_ipv6,"2001:1b70:6282:b100::18d"},
%%% 		                              {ssh_lmt_ipv4,"10.68.98.94"},
%%% 		                              {ssh_lmt_ipv6,"2001:1b70:6282:b100::18c"},
%%% 		                              {pran_fronthaul_ipv4,"10.68.100.179"},
%%% 		                              {pran_fronthaul_ipv6,"2001:1b70:6282:b200::f7"}], Config).'''
%%% @end
update_ip_addresses(N,IPs,Config) ->
    IPList=expand_ip_types(N,IPs,[]),
    update_config_files(N,IPList),
    case proplists:get_value(alias_to_hooks,Config) of
    	undefined ->
    	    ct:pal("No aliases registered for hooks");	
    	AliasToHooks ->
	    Aliases = [Alias||{Alias,{N2,_Function}}<-AliasToHooks, N2=:=N], % Find all aliases belonging to Node N
	    List=[{Aliases,IPdata}||IPdata<-IPList],
	    R=do_update_config(AliasToHooks,List,[]),
    	    ct:pal("Aliases have been updated according to below~n~s",[lists:flatten(R)])	    
    end.  	

%% expand_ip_types(2,[{pran_backhaul_ipv4,"10.224.177.26"},
%% 		   {om_ran_ipv4,"10.68.98.96"},
%% 		   {om_ran_ipv6,"2001:1b70:6282:b100::18d"},
%% 		   {ssh_lmt_ipv4,"10.68.98.94"},
%% 		   {ssh_lmt_ipv6,"2001:1b70:6282:b100::18c"},
%% 		   {pran_fronthaul_ipv4,"10.68.100.179"},
%% 		   {pran_fronthaul_ipv6,"2001:1b70:6282:b200::f7"}],[]) ->
%%     [{pran_backhaul_ipv4,[{ssh,"10.224.177.26"},{vlan,"0"},{netmask,"26"},{gateway,"10.224.177.1"}]},
%%      {om_ran_ipv4,[{ssh,"10.68.98.96"},{vlan,"0"},{netmask,"24"},{gateway,"10.68.98.1"}]},
%%      {om_ran_ipv6,[{ssh,"2001:1b70:6282:b100::18d"},{vlan,"0"},{netmask,"64"},{gateway,"2001:1b70:6282:b100::1"}]},
%%      {ssh_lmt_ipv4,[{ssh,"10.68.98.94"},{port,22},{user,"root"},{password,"root"}]},
%%      {erl_dist_ip,"10.68.98.94"},
%%      {ssh_lmt_ipv6,[{ssh,"2001:1b70:6282:b100::18c"},{port,22},{user,"root"},{password,"root"}]},
%%      {pran_fronthaul_ipv4,[{ssh,"10.68.100.179"},{vlan,"0"},{netmask,"24"},{gateway,"10.68.100.1"}]},
%%      {pran_fronthaul_ipv6,[{ssh,"2001:1b70:6282:b200::f7"},{vlan,"0"},{netmask,"64"},{gateway,"2001:1b70:6282:b200::1"}]}]
expand_ip_types(_N,[],R) ->
    R;
expand_ip_types(N,[{ssh_lmt_ipv4,IPAddress}|T],R) ->
    Data=rct_multi_node_cfg:get_config(N,ssh_lmt_ipv4),
    Data2=lists:keyreplace(ssh,1,Data,{ssh,IPAddress}),
    expand_ip_types(N,T,R ++ [{ssh_lmt_ipv4,Data2}] ++ [{erl_dist_ip,IPAddress}]);
expand_ip_types(N,[{IPType,IPAddress}|T],R) ->
    Data=rct_multi_node_cfg:get_config(N,IPType),
    Data2=lists:keyreplace(ssh,1,Data,{ssh,IPAddress}),
    expand_ip_types(N,T,R ++ [{IPType,Data2}]).

%%% @spec update_lmt_ip(N, IP, Config) -> ok 
%%% N = integer()
%%% IP = list()
%%% Config = list()
%%% @doc Informs hooks that data has changed and updates config files on disk.
%%% ```N             The node that has changed the config, 0 will change toplevel config parameter.
%%%    IP            New LMT IP address.
%%%    Config        Config variable used in testcases now includes mapping of Name to hook callback file where update_config/2 is implemented'''
%%% Finds all aliases for node N and changes the IP addresses
%%% ```Change the .cfg file(s) on disk
%%%    Reload the CommonTest start configuration from the changed .cfg file from disk
%%%    Use callback function update_config/2 in hooks to update data for the aliases.
%%%
%%%    rct_node_state:update_lmt_ip(2, "10.67.239.246", Config).'''
%%% @end
update_lmt_ip(N,IP,Config) ->
    LMT={ssh_lmt_ipv4, [{ssh, IP}, 
			{port, 22}, 
			{user, "root"}, 
			{password, "root"}]},
    ErlDist={erl_dist_ip,IP},
    update_config_files(N,[LMT,ErlDist]),
    case proplists:get_value(alias_to_hooks,Config) of
    	undefined ->
    	    ct:pal("No aliases registered for hooks");	
    	AliasToHooks ->
	    Aliases = [Alias||{Alias,{N2,_Function}}<-AliasToHooks, N2=:=N], % Find all aliases belonging to Node N
    	    R=do_update_config(AliasToHooks,[{Aliases,LMT}],[]),
    	    ct:pal("Aliases have been updated according to below~n~s",[lists:flatten(R)]),
    	    R2=do_update_config(AliasToHooks,[{Aliases,ErlDist}],[]),
    	    ct:pal("Aliases have been updated according to below~n~s",[lists:flatten(R2)])	    
    end.  

%%% @spec update_config(N, [{Names, Update}], Config) -> ok 
%%% N = integer()
%%% Names = list()
%%% Update = tuple()
%%% Config = list()
%%% @doc Informs hooks that data has changed and updates config files on disk.
%%% ```N             The node that has changed the config, 0 will change toplevel config parameter.
%%%    Name          Hook alias affected by configuration change.
%%%    Update        Line in stp.cfg file that is to be replaced.
%%%    Config        Config variable used in testcases now includes mapping of Name to hook callback file where update_config/2 is implemented'''
%%% Changing of configuration parameters, ex IP addresses, in for example a cloud node requires configuration to be updated. The update requires a number of steps
%%% ```Change the .cfg file(s) on disk
%%%    Reload the CommonTest start configuration from the changed .cfg file from disk
%%%    Use callback function update_config/2 in hooks to update data for the aliases.
%%%
%%%    rct_node_state:update_config(2, [{[cli2, coli2], {ssh_lmt_ipv4, [{ssh, "10.67.239.239"}, {port, 22}, {user, "root"}, {password, "root"}]}}], Config).
%%%    rct_node_state:update_config(0, [{[cli2, coli2], {ldap_server,[{host, "10.65.33.77"}]}}], Config).'''
%%% @end
update_config(N, NamesUpdates = [{_Names, _Update}|_T], Config) ->
    Updates = [Update||{_, Update}<-NamesUpdates],
    update_config_files(N,Updates),
    case proplists:get_value(alias_to_hooks,Config) of
    	undefined ->
    	    ct:pal("No aliases registered for hooks");	
    	AliasToHooks ->
    	    R=do_update_config(AliasToHooks,NamesUpdates,[]),
    	    ct:pal("Aliases have been updated according to below~n~s",[lists:flatten(R)])
    end.  

do_update_config(_,[],R) ->
    R;
do_update_config(AliasToHooks,[{Aliases,Update}|T],R) ->
    R2=do_update_config2(AliasToHooks,Aliases,Update,R ++ [lists:flatten(io_lib:format("~-20s ~p~n",["New config data",Update]))]),
    do_update_config(AliasToHooks,T,R2).

%%% Calls callback function update_config/2 for every "alias". "alias" to callback module is stored in Config variable in testcase.
%%% update_config/2 should return ok | no_configuration_found | not_affected_by_update
do_update_config2(_,[],_,R) ->
    R;
do_update_config2(AliasToHooks,[Alias|T],Update,R) ->
    Reply = case proplists:get_value(Alias,AliasToHooks) of
		undefined -> 
		    R ++ [lists:flatten(io_lib:format("~-30w ~s~n",[Alias,"not found in CommonTest Config list"]))];
		Value ->
		    CallbackModule = element(2, Value),
		    case apply(CallbackModule,update_config,[Alias,Update]) of
			ok ->
			    R ++ [lists:flatten(io_lib:format("~-30w ~s~n",[Alias,"updated"]))];
			no_configuration_found ->
			    R ++ [lists:flatten(io_lib:format("~-30w ~s~n",[Alias,"no configuration found"]))];
			not_affected_by_update ->
			    R ++ [lists:flatten(io_lib:format("~-30w ~s~n",[Alias,"not affected by update"]))]
		    end
	    end,
    do_update_config2(AliasToHooks,T,Update,Reply).  

%% @hidden
%% Update CommonTest .cfg files for node with new data
%% update_config_files(2,[{om_ran_ipv4,[{ssh,"10.68.98.99"},{vlan,"0"},{netmask,"24"},{gateway,"10.68.98.1"}]},
%% 		          {om_ran_ipv6,[{ssh,"2001:1b70:6282:b100::190"},{vlan,"0"},{netmask,"64"},{gateway,"2001:1b70:6282:b100::1"}]},
%% 		          {pran_fronthaul_ipv4,[{ssh,"10.68.100.181"},{vlan,"0"},{netmask,"24"},{gateway,"10.68.100.1"}]},
%% 		          {pran_fronthaul_ipv6,[{ssh,"2001:1b70:6282:b200::f9"},{vlan,"0"},{netmask,"64"},{gateway,"2001:1b70:6282:b200::1"}]},
%% 		          {ssh_lmt_ipv4,[{ssh,"10.68.98.98"},{port,22},{user,"root"},{password,"root"}]},
%% 		          {erl_dist_ip,"10.68.98.98"},
%% 		          {ssh_lmt_ipv6,[{ssh,"2001:1b70:6282:b100::18f"},{port,22},{user,"root"},{password,"root"}]},
%% 		          {pran_backhaul_ipv4,[{ssh,"10.224.177.27"},{vlan,"0"},{netmask,"26"},{gateway,"10.224.177.1"}]}]) -> ok
%% Top level update of .cfg file
update_config_files(0,Updates) ->
    [Nodes,_] = string:tokens(atom_to_list(node()),"@"),
    NodesPath = "/proj/rcs-tmp/stps/" ++ Nodes ++ "/config/stp.cfg",
    {ok, NodesConfig} = file:consult(NodesPath),
    NewNodesConfig = update_config_file(NodesConfig, Updates),
    file:write_file(NodesPath, [io_lib:format("~p.~n", [Term])||Term<-NewNodesConfig]),
    reload_config(NewNodesConfig);
%%% Node level update of .cfg file
update_config_files(N,Updates) ->
    Node = ct:get_config({test_nodes,N}),
    NodePath = "/proj/rcs-tmp/stps/" ++ atom_to_list(Node) ++ "/config/stp.cfg",
    {ok, NodeConfig} = file:consult(NodePath),
    NewNodeConfig = update_config_file(Node, NodeConfig, Updates),
    file:write_file(NodePath, [io_lib:format("~p.~n", [Term])||Term<-NewNodeConfig]),
    [Nodes,_] = string:tokens(atom_to_list(node()),"@"),
    case Node of
	Nodes -> % Single node tests
	    reload_config(NewNodeConfig);
	_ -> % Multi node tests needs to update the combined .cfg file
	    NodesPath = "/proj/rcs-tmp/stps/" ++ Nodes ++ "/config/stp.cfg",
	    {ok, NodesConfig} = file:consult(NodesPath),
	    NewNodesConfig = update_config_file(Node, NodesConfig, Updates),
	    file:write_file(NodesPath, [io_lib:format("~p.~n", [Term])||Term<-NewNodesConfig]),
	    reload_config(NewNodesConfig)
    end.

%%% Top level update of .cfg file
update_config_file(Config, []) ->
    Config;
update_config_file(Config, [Update|T]) ->
    Config2 = replace_tuple(Update, Config, []),
    update_config_file(Config2, T).

%%% Node level update of .cfg file
update_config_file(_Node, Config, []) ->
    Config;
update_config_file(Node, Config, [Update|T]) ->
    Config2 = find_node(Node, Update, Config, []),
    update_config_file(Node, Config2, T).

find_node(_Node, _Update, [], R) ->
    R;
find_node(Node, Update, [{Node,Rest}|T], R) ->
    R2 = replace_tuple(Update, Rest, []),
    find_node(Node, Update, T, R ++ [{Node,R2}]);
find_node(Node, Update, [H|T], R) ->
    find_node(Node, Update, T, R ++ [H]).

replace_tuple(_Update, [], R) ->
    R;
replace_tuple({Key,Tail}, [{Key,_}|T], R) ->
    R ++ [{Key,Tail}] ++ T;
replace_tuple(Update, [Line|T], R) ->
    replace_tuple(Update, T, R ++ [Line]).

%%% @hidden
%%% Reload the updated .cfg file
reload_config([]) ->
    ok;
reload_config([{Key,_Value}|T]) ->
    ct:reload_config(Key),
    reload_config(T).

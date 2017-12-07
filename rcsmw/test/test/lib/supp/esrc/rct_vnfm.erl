%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_vnfm.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/R12A/4
%%% @doc ==Common Test hook for sending vnfm REST API requests== 
%%%
%%%
%%% Stores IP address to vnfm instances and gives support for REST API.<br/>
%%%
%%% Hook formats:
%%% ```{rct_vnfm, [{N, Name, IPType}]}'''
%%%
%%% There is a short format for testing towards one node:
%%% ```{rct_vnfm, Name}  expands to {rct_vnfm, [{1, Name, ssh_lmt_ipv4}]}'''
%%% 
%%% There is a short format for testing towards clustered node:
%%% ```{rct_vnfm, [Name1,Name2]} expands to {rct_vnfm, [{1, Name1, ssh_lmt_ipv4},{2, Name2, ssh_lmt_ipv4}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match node in stp.cfg file when running on target.
%%%                                              Not used in simuleted environment.
%%%    Name     = atom()                         Used as identifier
%%%    IPType   = ssh_lmt_ipv4 | ssh_lmt_ipv6    Used in target env to specify which IP address vnfm uses.
%%%                                              Requires config variables below to be specified in stp.cfg file: 
%%%                                              {ssh_lmt_ipv4, [{ssh, string()}]},
%%%                                              {ssh_lmt_ipv6, [{ssh, string()}]},
%%%                                              Not used in simulated environment'''
%%% Example single node:<br/>
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_vnfm,vnfm1}]}].'''
%%% Example multi  nodes
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_vnfm,[vnfm1, vnfm2]}]}].'''
%%%
%%% Testcase example.
%%% ```mytest(_) ->
%%%        {ok, Map} = 	rct_vnfm:get_tenants(vnfm1).'''
%%% @end

-module(rct_vnfm).
-id('Updated by CCase').
-vsn('/main/R9A/R10A/R11A/R12A/4').
-date('2017-11-29').
-author('etxivri').
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
%%% -----      ---------  --------    ------------------------
%%% R9A/1      2017-02-02 etxkols     Created
%%% R9A/2      2017-02-03 etxkols     Updates
%%% R9A/3      2017-02-14 etxkols     Documentation
%%% R9A/4      2017-02-14 etxkols     Fix
%%% R9A/5      2017-02-15 etxkols     Bug fix
%%% R9A/6      2017-02-20 etxivri     update get_vnf_ips
%%% R9A/7      2017-02-25 etxivri     Add upgrade lcm api.
%%% R9A/8      2017-02-28 etxivri     Set network when use insert_vnfd.
%%% R9A/9      2017-02-28 etxivri     Update due to new vnfm behaviour.
%%%                                   Add fetch_vnfm_esi_rest_api
%%% R9A/10     2017-03-07 etxivri     Update to to create vnf using zip package.
%%% R9A/11     2017-03-09 etxivri     Add restart vnf_heal.
%%% R9A/12     2017-04-04 etxivri     changed to ct:log in parse_reply.
%%% R10A/1     2017-05-11 etxivri     Add confirm with fallback timeout as input.
%%% R10A/1     2017-06-02 etxivri     Update additionalParam to additionalParams due to
%%%                                   additionalParam is depricated.
%%% R10A/2     2017-07-06 etxivri     Update to correct value in additionalParams
%%% R11A/3     2017-10-11 etxivri     Update httpc req with httpoption needed for OTP20.
%%% R12A/1     2017-11-07 etxivri     Update due to new behaiour.
%%%                                   Some actions will now get info from Headers.
%%%                                   request_will_reply_header_and_body
%%%                                   -terminate_vnf
%%%                                   -vnf_heal
%%%                                   -instantiate_vnf_zip,
%%% R12A/2     2017-11-14 etxivri     Add get_vnfs_443 to test port 443 works. 
%%%                                   This is temporary until port 443 is the only port that shall be used. 
%%% R12A/3     2017-11-15 etxivri     Add instantiate with sitebasic and lkf.
%%% R12A/4     2017-11-27 etxivri     Update due to new behaviour.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/2,
         pre_init_per_suite/3,
         terminate/1,
         update_config/2,
	 get_tenants/1,
	 insert_datacenter/3,
	 associate_datacenter/2,
	 get_datacenters/2,
	 delete_datacenter/2,
	 insert_vnfd/3,
	 get_vnfds/2,
	 delete_vnfd/3,
	 create_vnf/2,
	 instantiate_vnf/2,
	 terminate_vnf/2,
	 get_vnfs/1,
	 get_vnf_ips/2,
	 delete_vnf/2,

	 %% create vnf using zip pkg.
	 onboarding_vnf_package/3,
	 create_vnf_zip/3,
	 instantiate_vnf_zip/2,
	 vnf_show_lcop/2,

	 %% AI
	 ai_instantiate_vnf_zip/2,

	 %% Upgrade
	 ug_create_job_id_operation/1,
	 ug_create_job_id_operation/4,
	 ug_create_job_id_operation/5,
	 ug_get_all_jobs/1,
	 ug_get_job_status/2,
	 ug_delete_job_id/2,
	 ug_prepare/2,
	 ug_verify/2,
	 ug_activate/2,
	 ug_confirm/2,
	 ug_confirm/3, %% Uses fallbacktimeout
	 ug_cancel/2,

	 %% Restart VNF
	 vnf_heal/2, %% Order an manual restart

	 %% Misc
	 get_all_vnf_packages/1,
	 get_single_vnf_packages/2,
	 get_vnfp_job_status/2,

	 fetch_vnfm_esi_rest_api/3,

	 get_vnfs_443/1,

	 %% good to have
	 get_vnf_lcm_op_occs_binary/1,
	 wait_for_lcop_success/3,
	 wait_for_cli/4
	]).

-include_lib("common_test/include/ct.hrl").

%%% @hidden
%%% init function for ct_hook
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
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

%% @hidden
%% Clean away items in ct_attributes
terminate(CthState) ->
    do_terminate(CthState),
    ok.

%%% @spec get_tenants(VName) -> {ok, map()} | {error, term()}
%%% @doc Get Tenants.
%%% ```VName = atom()    Identifier for VNFM instance
%%%
%%%    Example: rct_vnfm:get_tenants(vnfm1).'''
get_tenants(VName) ->
    Url="http://"++ip(VName)++":9090/openmano/tenants/ericsson",
    request(get,{Url}).

%%% @spec insert_datacenter(VName,Name,Type) -> {ok, map()} | {error, term()}
%%% @doc Insert datacenter.
%%% ```VName = atom()    Identifier for VNFM instance
%%%    Name  = string()  Name of datacenter
%%%    Type  = string()  Type of datacenter
%%%
%%%    Example: rct_vnfm:insert_datacenter(vnfm1,"rcs","openstack").'''
insert_datacenter(VName,Name,Type) ->
    Vim_url = ct:get_config({make_name_module(VName),vim_url}),
    Url="http://"++ip(VName)++":9090/openmano/datacenters",
    Json = jsone:encode(#{<<"datacenter">> => #{<<"name">>    => list_to_binary(Name),
						<<"type">>    => list_to_binary(Type),
						<<"vim_url">> => list_to_binary(Vim_url)}}),
    request(post,{Url,Json}).

%%% @spec associate_datacenter(VName,Datacenter_uuid) -> {ok, map()} | {error, term()}
%%% @doc Associate datacenter.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    Datacenter_uuid  = binary()  Datacenter uuid
%%%
%%%    Example: rct_vnfm:associate_datacenter(vnfm1,<<"72ff95d8-f28a-11e6-9415-fa163e465978">>).'''
associate_datacenter(VName,Datacenter_uuid) ->
    Vim_tenant_name = ct:get_config({make_name_module(VName),vim_tenant_name}),
    Vim_username= ct:get_config({make_name_module(VName),vim_username}),
    Vim_password= ct:get_config({make_name_module(VName),vim_password}),
    %% Vim_cacert= ct:get_config({make_name_module(VName),vim_cacert}),
    %% Vim_insecure=1,
    Url="http://"++ip(VName)++":9090/openmano/ericsson/datacenters/"++binary_to_list(Datacenter_uuid),
    case ct:get_config({make_name_module(VName),tenant}) of
	Tenant when Tenant =:= "redhat007-rcs";
		    Tenant =:= "redhat007-rcs-ci";
		    Tenant =:= "redhat-xft-ecn-openrc";
		    Tenant =:= "cee2-xft-ecn" ->
	    Json = jsone:encode(#{<<"datacenter">> => #{<<"vim_tenant_name">> => list_to_binary(Vim_tenant_name),
							<<"vim_username">>    => list_to_binary(Vim_username),
							<<"vim_password">>    => list_to_binary(Vim_password)
							%% <<"vim_cacert">>      => list_to_binary(Vim_cacert),
							%% <<"vim_insecure">>    => Vim_insecure
						       }});
	_ ->
	    Json = jsone:encode(#{<<"datacenter">> => #{<<"vim_tenant_name">> => list_to_binary(Vim_tenant_name),
							<<"vim_username">>    => list_to_binary(Vim_username),
							<<"vim_password">>    => list_to_binary(Vim_password)}})
    end,
    request(post,{Url,Json}).

%%% @spec get_datacenters(VName,Tenant_uuid) -> {ok, map()} | {error, term()}
%%% @doc Get datacenters.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Tenant_uuid = binary()  Tenant uuid
%%%
%%%    Example: rct_vnfm:get_datacenters(vnfm1,<<"01c02e88-f289-11e6-b50c-fa163e465978">>).'''
get_datacenters(VName,Tenant_uuid) ->
    Url="http://"++ip(VName)++":9090/openmano/"++binary_to_list(Tenant_uuid)++"/datacenters",
    request(get,{Url}).

%%% @spec delete_datacenter(VName,Datacenter_uuid) -> {ok, map()} | {error, term()}
%%% @doc Delete datacenter.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    Datacenter_uuid  = binary()  Datacenter uuid
%%%
%%%    Example: rct_vnfm:delete_datacenter(vnfm1,<<"72ff95d8-f28a-11e6-9415-fa163e465978">>).'''
delete_datacenter(VName,Datacenter_uuid) ->
    Url="http://"++ip(VName)++":9090/openmano/datacenters/"++binary_to_list(Datacenter_uuid),
    request(delete,{Url}).

%% insert_vnfd(VName,_N,Tenant_uuid) ->
%%     Url="http://"++ip(VName)++":9090/openmano/"++binary_to_list(Tenant_uuid)++"/vnfds",
%%     UP="VRCS-UP_CXS101657_4-R1A122",
%%     Fronthaul_network="KI10_rcs_traffic",
%%     Oam_network="KI10_rcs_oam",
%%     VNF_name="rcf_etxkols2",
%%     ID="vRC-1-0",
%%     Map = #{<<"vnfd">> => #{
%%     		<<"description">> => <<"VirtualRadioControlFunction">>,
%%     		<<"metadata">> => #{
%%     		    <<"ID">> => list_to_binary(ID),
%%     		    <<"name">> => <<"VRC">>,
%%     		    <<"vendor">> => <<"Ericsson">>,
%%     		    <<"version">> => <<"1.0">>,
%%     		    <<"vnfm_info">> => <<"EricssonRanVnfm">>},
%%     		<<"topology_template">> => #{
%%     		    <<"inputs">> => #{
%%     			<<"fronthaul_network_name">> => #{
%%     			    <<"description">> => <<"NetworkName">>,
%%     			    <<"type">> => <<"string">>},
%%     			<<"oam_network_name">> => #{
%%     			    <<"description">> => <<"NetworkName">>,
%%     			    <<"type">> => <<"string">>}},
%%     		    <<"lcm_scripts">> => [#{<<"event">> => [<<"startinstantiation">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/config_nfs_server.py">>},
%%     					  #{<<"event">> => [<<"endinstantiation">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/start_vnf_heartbeat.py">>},
%%     					  #{<<"event">> => [<<"starttermination">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/terminate_vnf_heartbeat.py">>},
%%     					  #{<<"event">> => [<<"endtermination">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/remove_vnf_lcm_script.py">>},
%%     					  #{<<"event">> => [<<"startVNFoperationstatechange">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/lcm_script_start_operate.py">>},
%%     					  #{<<"event">> => [<<"upgradeprepare">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/prepare.py">>},
%%     					  #{<<"event">> => [<<"upgradeverify">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/verify.py">>},
%%     					  #{<<"event">> => [<<"upgradeactivate">>],
%%     					    <<"script">> => <<"nfvo_lcm_scripts/activate.py">>}],
%%     		    <<"node_templates">> => #{
%%     			<<"fronthaul_network">> => #{
%%     			    <<"network_name">> => list_to_binary(Fronthaul_network),
%%     			    <<"type">> => <<"tosca.nodes.network.Network">>},
%%     			<<"fronthaul_port">> => #{
%%     			    <<"properties">> => #{
%%     				<<"order">> => 1},
%%     			    <<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
%%     						     #{<<"link">> => <<"fronthaul_network">>}],
%%     						   <<"type">> => <<"tosca.nodes.network.Port">>},
%%     			    <<"oam_network">> => #{
%%     				<<"network_name">> => list_to_binary(Oam_network),
%%     				<<"type">> => <<"tosca.nodes.network.Network">>},
%%     			    <<"oam_port">> => #{
%%     				<<"properties">> => #{
%%     				    <<"order">> => 0},
%%     				<<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
%%     							 #{<<"link">> => <<"oam_network">>}],
%%     						       <<"type">> => <<"tosca.nodes.network.Port">>},
%%                      list_to_binary(VNF_name) => #{
%%     			       <<"artifacts">> => #{
%%     				   <<"vrcImage">> => #{
%%     				       <<"file">> => list_to_binary(UP),
%%     				       <<"type">> => <<"tosca.artifacts.Deployment.Image.VM.QCOW2">>}},
%%     			       <<"capabilities">> => #{
%%     				   <<"nfv_compute">> => #{
%%     				       <<"properties">> => #{
%%     					   <<"disk_size">> => 8,<<"mem_size">> => 4096,<<"num_cpus">> => 2}}},
%%     			       <<"type">> => <<"tosca.nodes.Compute">>}}},
%%     			<<"tosca_definitions_version">> => <<"tosca_simple_profile_for_nfv_1_0_0">>}},
%%     Json = jsone:encode(Map),
%%     request(post,{Url,Json}).
		
%%% @spec insert_vnfd(VName,N,Tenant_uuid) -> {ok, map()} | {error, term()}
%%% @doc Insert VNFD (yaml file).
%%% ```VName       = atom()     Identifier for VNFM instance
%%%    N           = integer()  Number of the node in testsuite that the VNFD should be applied to
%%%    Tenant_uuid = binary()   Tenant uuid
%%%
%%%    Example: rct_vnfm:insert_vnfd(vnfm1,2,<<"01c02e88-f289-11e6-b50c-fa163e465978">>).'''
insert_vnfd(VName,N,Tenant_uuid) ->
    Url="http://"++ip(VName)++":9090/openmano/"++binary_to_list(Tenant_uuid)++"/vnfds",
    UP=rct_multi_node_cfg:get_config(N,{cloud_info,image_name}),
    VNF_name=atom_to_list(ct:get_config({test_nodes,N})),
    %% %% ID="vRC-1-0",
    %% ID="ivri-77",
    ID=VNF_name,
    case ct:get_config({make_name_module(VName),tenant}) of
	Tenant when Tenant =:= "redhat007-rcs";
		    Tenant =:= "redhat007-rcs-ci";
		    Tenant =:= "redhat-xft-ecn-openrc";
		    Tenant =:= "cee2-xft-ecn" ->
	    Map = #{<<"description">> => <<"Virtual Radio Control Function">>,
		    <<"metadata">> => #{
			<<"ID">> => list_to_binary(ID),
			<<"name">> => <<"VRC">>,
			<<"vendor">> => <<"Ericsson">>,
			<<"version">> => <<"PREV">>,
			<<"vnfm_info">> => <<"EricssonRanVnfm">>},
		    <<"topology_template">> => #{
			<<"inputs">> => #{
			    <<"backhaul_network_name">> => #{
				<<"description">> => <<"Network Name">>,
				<<"type">> => <<"string">>},
			    <<"fronthaul_network_name">> => #{
				<<"description">> => <<"Network Name">>,
				<<"type">> => <<"string">>},
			    <<"oam_network_name">> => #{
				<<"description">> => <<"Network Name">>,
				<<"type">> => <<"string">>},
			    <<"tenant_network_name">> => #{
				<<"description">> => <<"Network Name">>,
				<<"type">> => <<"string">>}},
			%% <<"lcm_scripts">> => [#{<<"event">> => [<<"start instantiation">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/config_nfs_server.py">>},
			%% 		      #{<<"event">> => [<<"end instantiation">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/start_vnf_heartbeat.py">>},
			%% 		      #{<<"event">> => [<<"start termination">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/terminate_vnf_heartbeat.py">>},
			%% 		      #{<<"event">> => [<<"end termination">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/remove_vnf_lcm_script.py">>},
			%% 		      #{<<"event">> => [<<"start VNF operation state change">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/lcm_script_start_operate.py">>},
			%% 		      #{<<"event">> => [<<"upgrade prepare">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/prepare.py">>},
			%% 		      #{<<"event">> => [<<"upgrade verify">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/verify.py">>},
			%% 		      #{<<"event">> => [<<"upgrade activate">>],
			%% 			<<"script">> => <<"nfvo_lcm_scripts/activate.py">>}],
			<<"lcm_scripts">> => [#{<<"event">> => [<<"start instantiate">>],
						<<"script">> => <<"nfvo_lcm_scripts/config_nfs_server.py">>},
					      #{<<"event">> => [<<"end instantiate">>],
						<<"script">> => <<"nfvo_lcm_scripts/start_vnf_heartbeat.py">>},
					      #{<<"event">> => [<<"start terminate">>],
						<<"script">> => <<"nfvo_lcm_scripts/terminate_vnf_heartbeat.py">>},
					      #{<<"event">> => [<<"end terminate">>],
						<<"script">> => <<"nfvo_lcm_scripts/remove_vnf_lcm_script.py">>},
					      #{<<"event">> => [<<"start VNF operation state change">>],
						<<"script">> => <<"nfvo_lcm_scripts/lcm_script_start_operate.py">>},
					      #{<<"event">> => [<<"upgrade prepare">>],
						<<"script">> => <<"nfvo_lcm_scripts/prepare.py">>},
					      #{<<"event">> => [<<"upgrade verify">>],
						<<"script">> => <<"nfvo_lcm_scripts/verify.py">>},
					      #{<<"event">> => [<<"upgrade activate">>],
						<<"script">> => <<"nfvo_lcm_scripts/activate.py">>}],
			<<"node_templates">> => #{
			    list_to_binary(VNF_name) => #{
					    <<"artifacts">> => #{
						<<"vrcImage">> => #{<<"file">> => list_to_binary(UP),
								    <<"type">> => <<"tosca.artifacts.Deployment.Image.VM.QCOW2">>}},
					    <<"capabilities">> => #{
						<<"nfv_compute">> => #{
						    <<"properties">> => #{<<"disk_size">> => 0,<<"mem_size">> => 4096,<<"num_cpus">> => 2}}},
					    <<"type">> => <<"tosca.nodes.Compute">>},
			    <<"backhaul_network">> => #{
				%% %% <<"network_name">> => #{<<"get_input">> => <<"backhaul_network_name">>},
				%% <<"network_name">> => #{<<"backhaul_network_name">> => <<"pran_backhaul">>},
				<<"network_name">> => <<"pran_backhaul">>,
				<<"type">> => <<"tosca.nodes.network.Network">>},
			    <<"backhaul_port">> => #{
			<<"properties">> => #{<<"order">> => 2},
				<<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
						       #{<<"link">> => <<"backhaul_network">>}],
				<<"type">> => <<"tosca.nodes.network.Port">>},
			    <<"fronthaul_network">> => #{
				%% %% <<"network_name">> => #{<<"get_input">> => <<"fronthaul_network_name">>},
				%% <<"network_name">> => #{<<"fronthaul_network_name">> => <<"pran_fronthaul">>},
				<<"network_name">> => <<"pran_fronthaul">>,
				<<"type">> => <<"tosca.nodes.network.Network">>},
			    <<"fronthaul_port">> => #{
				<<"properties">> => #{<<"order">> => 3},
				<<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
						       #{<<"link">> => <<"fronthaul_network">>}],
				<<"type">> => <<"tosca.nodes.network.Port">>},
			    <<"oam_network">> => #{
				%% %% <<"network_name">> => #{<<"get_input">> => <<"oam_network_name">>},
				%% <<"network_name">> => #{<<"oam_network_name">> => <<"om_ran">>},
				<<"network_name">> => <<"om_ran">>,
				<<"type">> => <<"tosca.nodes.network.Network">>},
			    <<"oam_port">> => #{
				<<"properties">> => #{<<"order">> => 1},
				<<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
						       #{<<"link">> => <<"oam_network">>}],
				<<"type">> => <<"tosca.nodes.network.Port">>},
			    <<"tenant_network">> => #{
				%% %% <<"network_name">> => #{<<"get_input">> => <<"tenant_network_name">>},
				%% <<"network_name">> => #{<<"tenant_network_name">> => <<"om_ran">>},
				<<"network_name">> => <<"om_ran">>,
				<<"type">> => <<"tosca.nodes.network.Network">>},
			    <<"tenant_port">> => #{
				<<"properties">> => #{<<"order">> => 0},
				<<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
						       #{<<"link">> => <<"tenant_network">>}],
				<<"type">> => <<"tosca.nodes.network.Port">>}}},
		    <<"tosca_definitions_version">> => <<"tosca_simple_profile_for_nfv_1_0_0">>},
	    Json = jsone:encode(Map),
	    request(post,{Url,Json});
	_ ->
	    Map = #{<<"vnfd">> => #{
			<<"description">> => <<"VirtualRadioControlFunction">>,
			<<"metadata">> => #{
			    <<"ID">> => list_to_binary(ID),
			    <<"name">> => <<"VRC">>,
			    <<"vendor">> => <<"Ericsson">>,
			    <<"version">> => <<"1.0">>,
			    <<"vnfm_info">> => <<"EricssonRanVnfm">>},
			<<"topology_template">> => #{
			    <<"inputs">> => #{
				<<"fronthaul_network_name">> => #{
				    <<"description">> => <<"NetworkName">>,
				    <<"type">> => <<"string">>},
				<<"oam_network_name">> => #{
				    <<"description">> => <<"NetworkName">>,
				    <<"type">> => <<"string">>}},
			    <<"lcm_scripts">> => [#{<<"event">> => [<<"startinstantiation">>],
						    <<"script">> => <<"nfvo_lcm_scripts/config_nfs_server.py">>},
						  #{<<"event">> => [<<"endinstantiation">>],
						    <<"script">> => <<"nfvo_lcm_scripts/start_vnf_heartbeat.py">>},
						  #{<<"event">> => [<<"starttermination">>],
						    <<"script">> => <<"nfvo_lcm_scripts/terminate_vnf_heartbeat.py">>},
						  #{<<"event">> => [<<"endtermination">>],
						    <<"script">> => <<"nfvo_lcm_scripts/remove_vnf_lcm_script.py">>},
						  #{<<"event">> => [<<"startVNFoperationstatechange">>],
						    <<"script">> => <<"nfvo_lcm_scripts/lcm_script_start_operate.py">>},
						  #{<<"event">> => [<<"upgradep repare">>],
						    <<"script">> => <<"nfvo_lcm_scripts/prepare.py">>},
						  #{<<"event">> => [<<"upgrade verify">>],
						    <<"script">> => <<"nfvo_lcm_scripts/verify.py">>},
						  #{<<"event">> => [<<"upgrade activate">>],
						    <<"script">> => <<"nfvo_lcm_scripts/activate.py">>}],
			    <<"node_templates">> => #{
				list_to_binary(VNF_name) => #{
						<<"artifacts">> => #{
						    <<"vrcImage">> => #{<<"file">> => list_to_binary(UP),
									<<"type">> => <<"tosca.artifacts.Deployment.Image.VM.QCOW2">>}},
						<<"capabilities">> => #{
						    <<"nfv_compute">> => #{
							<<"properties">> => #{
							    <<"disk_size">> => 8,<<"mem_size">> => 4096,<<"num_cpus">> => 2}}},
						<<"type">> => <<"tosca.nodes.Compute">>},				
%%     Fronthaul_network="KI10_rcs_traffic",
%%     Oam_network="KI10_rcs_oam",
				<<"fronthaul_network">> => #{
				    <<"network_name">> => #{<<"get_input">> => <<"fronthaul_network_name">>},
				    %% <<"network_name">> => list_to_binary(Fronthaul_network),
				    <<"type">> => <<"tosca.nodes.network.Network">>},
				<<"fronthaul_port">> => #{
				    <<"properties">> => #{
					<<"order">> => 1},
				    <<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
							   #{<<"link">> => <<"fronthaul_network">>}],
				    <<"type">> => <<"tosca.nodes.network.Port">>},
				<<"tenant_network">> => #{
				    <<"network_name">> => #{<<"get_input">> => <<"tenant_network_name">>},
				    <<"type">> => <<"tosca.nodes.network.Network">>},
				<<"tenant_port">> => #{
				    <<"properties">> => #{<<"order">> => 0},
				    <<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
							   #{<<"link">> => <<"tenant_network">>}],
				    <<"type">> => <<"tosca.nodes.network.Port">>}}},
			        %% <<"oam_network">> => #{
				%%     <<"network_name">> => list_to_binary(Oam_network),
				%%     <<"type">> => <<"tosca.nodes.network.Network">>},
				%% <<"oam_port">> => #{
				%%     <<"properties">> => #{
				%% 	<<"order">> => 0},
				%%     <<"requirements">> => [#{<<"binding">> => list_to_binary(VNF_name)},
				%% 			   #{<<"link">> => <<"oam_network">>}],
				%%     <<"type">> => <<"tosca.nodes.network.Port">>}}},
    			<<"tosca_definitions_version">> => <<"tosca_simple_profile_for_nfv_1_0_0">>}},
	    Json = jsone:encode(Map),
	    request(post,{Url,Json})
    end.

%%% @spec get_vnfds(VName,Tenant_uuid) -> {ok, map()} | {error, term()}
%%% @doc Get VNFDs.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Tenant_uuid = binary()  Tenant uuid
%%%
%%%    Example: rct_vnfm:get_vnfds(vnfm1,<<"01c02e88-f289-11e6-b50c-fa163e465978">>).'''
get_vnfds(VName,Tenant_uuid) ->
    Url="http://"++ip(VName)++":9090/openmano/"++binary_to_list(Tenant_uuid)++"/vnfds",
    request(get,{Url}).

%%% @spec delete_vnfd(VName,Tenant_uuid,Vnfd_id) -> {ok, map()} | {error, term()}
%%% @doc Delete VNFD.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Tenant_uuid = binary()  Tenant uuid
%%%    Vnfd_id     = binary()  VNFD ID
%%%
%%%    Example: rct_vnfm:delete_vnfd(vnfm1,<<"01c02e88-f289-11e6-b50c-fa163e465978">>,<<"vRC-1-0">>).'''
delete_vnfd(VName,Tenant_uuid,Vnfd_id) ->
    Url="http://"++ip(VName)++":9090/openmano/"++binary_to_list(Tenant_uuid)++"/vnfds/"++binary_to_list(Vnfd_id),
    request(delete,{Url}).

%%% @spec create_vnf(VName,Vnfd_id) -> {ok, map()} | {error, term()}
%%% @doc Create VNF.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Vnfd_id     = binary()  VNFD ID
%%%
%%%    Example: rct_vnfm:create_vnf(vnfm1,<<"vRC-1-0">>).'''
create_vnf(VName,Vnfd_id) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_instances",
    Json = jsone:encode(#{<<"vnfDescriptorId">> => Vnfd_id}),
    request(post,{Url,Json}).


%%% @spec get_vnfs(VName) -> {ok, map()} | {error, term()}
%%% @doc Get VNFs.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%
%%%    Example: rct_vnfm:get_vnfs(vnfm1).'''
get_vnfs(VName) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_instances",
    request(get,{Url}).

get_vnfs_443(VName) ->
    Url="https://"++ip(VName)++":443/lcm/v0/vnf_instances",
    ok = ssl:start(),
    Answ = request(get,{Url}),
    ssl:stop(),
    Answ.

%%% @spec get_vnf_ips(VName,VnfInstanceId) -> {ok, map()} | {error, term()}
%%% @doc Get VNF IP addresses.
%%% ```VName         = atom()    Identifier for VNFM instance
%%%    VnfInstanceId = binary()  VnfInstance ID
%%%
%%%    Example: rct_vnfm:get_vnf_ips(vnfm1,<<"73a4b590-f28a-11e6-a85c-fa163e465978">>).'''
get_vnf_ips(VName,VnfInstanceId) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_instances/"++binary_to_list(VnfInstanceId)++"?fields=extCps",
    case request(get,{Url}) of 
	{ok, Map} ->
	    {ok,fix_ips([{binary_to_list(maps:get(<<"cpdId">>,E)),
			  maps:get(<<"addresses">>,E)}||E<-maps:get(<<"extCps">>,Map)],[])};
	    %% {ok,fix_ips([{binary_to_list(maps:get(<<"port_name">>,E)),
	    %% 		  %% binary_to_list(maps:get(<<"network">>,E)),
	    %% 		  maps:get(<<"address">>,E)}||E<-maps:get(<<"extCps">>,Map)],[])};
	Other ->
	    Other
    end.

%%% @spec delete_vnf(VName,Vnfd_id) -> {ok, map()} | {error, term()}
%%% @doc Delete VNF.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Vnfd_id     = binary()  VNFD ID
%%%
%%%    Example: rct_vnfm:delete_vnf(vnfm1,<<"73a4b590-f28a-11e6-a85c-fa163e465978">>).'''
delete_vnf(VName,Vnfd_id) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_instances/"++binary_to_list(Vnfd_id),
    request(delete,{Url}).

%%% @spec instantiate_vnf(VName,Vnfd_id) -> {ok, map()} | {error, term()}
%%% @doc Instantiate VNF.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Vnfd_id     = binary()  VNFD ID
%%%
%%%    Example: rct_vnfm:instantiate_vnf(vnfm1,<<"73a4b590-f28a-11e6-a85c-fa163e465978">>).'''
instantiate_vnf(VName,Vnfd_id) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_instances/"++binary_to_list(Vnfd_id)++"/instantiate",
    Json = case ct:get_config({make_name_module(VName),tenant}) of
	       Tenant when Tenant =:= "redhat007-rcs";
			   Tenant =:= "redhat007-rcs-ci";
			   Tenant =:= "redhat-xft-ecn-openrc";
			   Tenant =:= "cee2-xft-ecn" ->
		   %% jsone:encode(#{<<"additionalParam">> => [[<<"tenant_network_name">>,<<"om_ran">>],
		   %% 					    [<<"oam_network_name">>,<<"om_ran">>],
		   %% 					    [<<"backhaul_network_name">>,<<"pran_backhaul">>],
		   %% 					    [<<"fronthaul_network_name">>,<<"pran_fronthaul">>]]});
		   ct:pal("This shall be done in insert_vnfd"),
		   jsone:encode(#{});
	       "rcs" ->
		   jsone_encode_additionalParams();
		   %% jsone:encode(#{<<"additionalParams">> => [[<<"tenant_network_name">>,<<"KI10_rcs_oam">>],
		   %% 					    [<<"fronthaul_network_name">>,<<"KI10_rcs_traffic">>]]});
	       "rcs-ci" ->
		   jsone_encode_additionalParams()
		   %% jsone:encode(#{<<"additionalParams">> => [[<<"tenant_network_name">>,<<"KI10_rcs_ci_oam">>],
		   %% 					    [<<"fronthaul_network_name">>,<<"KI10_rcs_ci_traffic">>]]})
	   end,
    request(post,{Url,Json}).


%%% @spec terminate_vnf(VName,Vnfd_id) -> {ok, map()} | {error, term()}
%%% @doc Terminate VNF.
%%% ```VName       = atom()    Identifier for VNFM instance
%%%    Vnfd_id     = binary()  VNFD ID
%%%
%%%    Example: rct_vnfm:terminate_vnf(vnfm1,<<"73a4b590-f28a-11e6-a85c-fa163e465978">>).'''
terminate_vnf(VName,Vnfd_id) ->
    Url="http://"++ip(VName)++":9100/vnflcm/v1/vnf_instances/"++binary_to_list(Vnfd_id)++"/terminate",
    Json = jsone:encode(#{<<"terminationType">> => 'FORCEFUL'}),
    request_will_reply_header_and_body(post,{Url,Json}).



do_pre_init_per_suite([],R,_Num) ->
    {ok,R};
do_pre_init_per_suite([Name|T],R,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name,ssh_lmt_ipv4}|T],R,Num);
do_pre_init_per_suite([CthState = {N, Name, IPType}|T],R,Num) ->
    case rct_multi_node_cfg:get_config(N,IPType) of
	undefined ->
	    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[IPType,Name,N]});
	[{ssh,IP}|_] ->
	    case rct_multi_node_cfg:get_config(N,cloud_info) of
		undefined ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[cloud_info,Name,N]});
		CloudInfo ->
                    ok = rct_multi_node_cfg:require(make_name_module(Name),CloudInfo ++ [{ssh,IP},{iptype,IPType}]),
                    do_pre_init_per_suite(T,R ++ [CthState],Num + 1)
	    end;
	LoginData ->
	    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter ssh in ~p for ~p in node ~p",[LoginData,Name,N]})
    end.

do_terminate([]) ->
    ok;
do_terminate([{_,Name,_}|T]) ->
    rct_multi_node_cfg:remove_config(make_name_module(Name)),
    do_terminate(T).

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

request(Method,{Url}) -> % get | head | delete | trace | options
    request(Method,{Url,[]},[{ssl,[{server_name_indication,"test_vc"}]}], []);
request(Method,{Url,Body}) -> % post | put
    request(Method,{Url,[],"application/json",Body},[{ssl,[{server_name_indication,"test_vc"}]}], []).

request(Method,{Url,Headers},HTTPOptions, Options) -> % get | head | delete | trace | options
    ct:pal("httpc:request(~p,{\"~s\",~p},~p,~p).",[Method,Url,Headers,HTTPOptions,Options]),
    Reply = httpc:request(Method,{Url,Headers},HTTPOptions,Options),
    parse_reply(Reply);
request(Method,{Url,Headers,Content_type,Body},HTTPOptions, Options) -> % post | put
    ct:pal("httpc:request(~p,{\"~s\",~p,~p,~p},~p,~p).",[Method,Url,Headers,Content_type,Body,HTTPOptions,Options]),
    Reply = httpc:request(Method,{Url,Headers,Content_type,Body},HTTPOptions,Options),
    parse_reply(Reply).

%parse_reply(Reply = {ok,{{_HTTP,200,"OK"},_Headers,Body}}) ->
parse_reply(Reply = {ok,{{_HTTP,Code,Reason},_Headers,Body}})  when {Code, Reason} =:= {200,"OK"};
								    {Code, Reason} =:= {201,"Created"};
								    {Code, Reason} =:= {202,"Accepted"} ->
    ct:log("~p~n~n~s",[Reply,Body]),
    {ok, jsone:decode(list_to_binary(Body))};

%% %% When Body is not used.
%% parse_reply(Reply = {ok,{{_HTTP,Code,Reason},Headers,Body}}) when {Code, Reason} =:= {202,"Accepted"} ->
%%     ct:log("parse_reply when 202 accepted is rcvd: ~nReply:~p~nHeaders:~p~nBody:~p",[Reply,Headers,Body]),
%%     ct:pal("Ivan1: ~p ", [Reply]),
%%     ct:pal("Ivan2: ~p ", [Headers]),
%%     ct:pal("Ivan3: ~p ", [Body]),
%%     %% DecodeHeader = jsone:decode(list_to_binary(Headers)),
%%     DecodeBody = jsone:decode(list_to_binary(Body)),
%%     {ok, Headers, DecodeBody};

parse_reply(Reply = {ok,{{_,RC,_},_Headers,Body}}) ->
    ct:log("~p~n~n~s",[Reply,Body]),
    {error, RC};
parse_reply(Reply = {error, _Reason}) ->
    ct:log("~p",[Reply]),
    Reply.

%%%%%%%%%%%%%%%%
%% This can only be used when Code 202 and Reason Accecpted is rcvd.
%%%%%%%%%%%%%%%%
request_will_reply_header_and_body(post,{Url,Body}) ->
    request_reply_header_and_body(post,{Url,[],"application/json",Body},[{ssl,[{server_name_indication,"test_vc"}]}], []).
request_reply_header_and_body(Method, {Url,Headers,Content_type,Body}, HTTPOptions, Options) ->
    ct:log("## httpc:request(~p,{\"~s\",~p,~p,~p},~p,~p).",[Method,Url,Headers,Content_type,Body,HTTPOptions,Options]),
    Reply = httpc:request(Method,{Url,Headers,Content_type,Body},HTTPOptions,Options),
    reply_header_and_decoded_body(Reply).

reply_header_and_decoded_body(Reply = {ok,{{_HTTP,Code,Reason},Headers,Body}}) when {Code, Reason} =:= {202,"Accepted"} ->
    ct:log("parse_reply when 202 accepted is rcvd: ~nReply:~p~nHeaders:~p~nBody:~p",[Reply,Headers,Body]),
    ct:log("Reply: ~p ", [Reply]),
    ct:log("Headers: ~p ", [Headers]),
    ct:log("Body: ~p ", [Body]),
    %% DecodeHeader = jsone:decode(list_to_binary(Headers)),
    DecodeBody = case Body of
		     [] ->
			 [];
		     _Else ->
			 jsone:decode(list_to_binary(Body))
		 end,
    {ok, Headers, DecodeBody}.

%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%

ip(VName) -> ct:get_config({make_name_module(VName),ssh}).
    
%% fix_ips([{"backhaul_port",[<<"10.224.177.26">>]},
%% 	 {"oam_port",[<<"10.68.98.96">>,<<"2001:1b70:6282:b100::18d">>]},
%% 	 {"tenant_port",[<<"10.68.98.94">>,<<"2001:1b70:6282:b100::18c">>]},
%% 	 {"fronthaul_port",[<<"10.68.100.179">>,<<"2001:1b70:6282:b200::f7">>]}],[]) ->
%%     [{pran_backhaul_ipv4,"10.224.177.26"},
%%      {om_ran_ipv4,"10.68.98.96"},
%%      {om_ran_ipv6,"2001:1b70:6282:b100::18d"},
%%      {ssh_lmt_ipv4,"10.68.98.94"},
%%      {ssh_lmt_ipv6,"2001:1b70:6282:b100::18c"},
%%      {pran_fronthaul_ipv4,"10.68.100.179"},
%%      {pran_fronthaul_ipv6,"2001:1b70:6282:b200::f7"}]
fix_ips([],R) ->
    R;
fix_ips([{"tenant_port",IPs}|T],R) ->
    Lists=[binary_to_list(IP)||IP<-IPs],
    IPv4=[{ssh_lmt_ipv4,IP}||IP<-Lists, lists:member($.,IP)],
    IPv6=[{ssh_lmt_ipv6,IP}||IP<-Lists, lists:member($:,IP)],
    fix_ips(T,R++IPv4++IPv6);
fix_ips([{"oam_port",IPs}|T],R) ->
    Lists=[binary_to_list(IP)||IP<-IPs],
    IPv4=[{om_ran_ipv4,IP}||IP<-Lists, lists:member($.,IP)],
    IPv6=[{om_ran_ipv6,IP}||IP<-Lists, lists:member($:,IP)],
    fix_ips(T,R++IPv4++IPv6);    
fix_ips([{"fronthaul_port",IPs}|T],R) ->
    Lists=[binary_to_list(IP)||IP<-IPs],
    IPv4=[{pran_fronthaul_ipv4,IP}||IP<-Lists, lists:member($.,IP)],
    IPv6=[{pran_fronthaul_ipv6,IP}||IP<-Lists, lists:member($:,IP)],
    fix_ips(T,R++IPv4++IPv6);    
fix_ips([{"backhaul_port",IPs}|T],R) ->
    Lists=[binary_to_list(IP)||IP<-IPs],
    IPv4=[{pran_backhaul_ipv4,IP}||IP<-Lists, lists:member($.,IP)],
    IPv6=[{pran_backhaul_ipv6,IP}||IP<-Lists, lists:member($:,IP)],
    fix_ips(T,R++IPv4++IPv6).    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ========================= create vnf using zip pkg ====================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @spec onboarding_vnf_package(Config, VName, Href) -> {ok, map()} | {error, term()}
%%% @doc   Onboarding VNF package asynchronously.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    Href             = string , The url.
%%%
%%%    Example: rct_vnfm:onboarding_vnf_package(VName, Href).'''
onboarding_vnf_package(Config, VName, Href) ->
    Url="http://"++ip(VName)++":9090/openmano/pkg/v0/vnf_packages", 
    Json = case string:str(Href, "http") of 
	       0 -> %%Not http, create
		   jsone:encode(#{<<"_links">> => #{<<"file">> => #{<<"href">> => list_to_binary(build_http_zip(Config, Href))}}});
	       _ ->
		   jsone:encode(#{<<"_links">> => #{<<"file">> => #{<<"href">> => list_to_binary(Href)}}})
	   end,
    request(post,{Url,Json}).		     
%%% @spec  create_vnf_zip(VName, Vnfd_id, VnfNmame)-> {ok, map()} | {error, term()}
%%% @doc   Create an VNF from zip package.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    Vnfd_id          = string , vnfdId from vnfp.
%%%    VnfNmame         = string , optional instance name.
%%%
%%%    Example: rct_vnfm:create_vnf_zip(VName, Vnfd_id, VnfNmame).'''
create_vnf_zip(VName, Vnfd_id, VnfNmame) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_instances",
    Json = jsone:encode(#{<<"vnfInstanceName">> => list_to_binary(VnfNmame),
			  <<"vnfdId">> => Vnfd_id}),
    request(post,{Url,Json}).

%%% @spec  instantiate_vnf_zip(VName, VnfInstanceId)-> {ok, map()} | {error, term()}
%%% @doc   Instantiate VNF when using zip. 
%%%        Note!. additionalParams is needed when yaml file contains  network_name :get_input .
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    VnfInstanceId    = binary , from create_vnf_zip
%%%
%%%    Example: rct_vnfm:instantiate_vnf_zip(VName,Vnfd_id).'''
instantiate_vnf_zip(VName,VnfInstanceId) ->
    Url="http://"++ip(VName)++":9100/vnflcm/v1/vnf_instances/"++binary_to_list(VnfInstanceId)++"/instantiate",
    Json = jsone_encode_additionalParams(),
    %% request(post,{Url,Json}).
    request_will_reply_header_and_body(post,{Url,Json}).

%% This shal be used when instantiate_vnf_zip use /vnflcm/v1/
jsone_encode_additionalParams() ->
    Json =jsone:encode(#{<<"additionalParams">> => #{<<"tenant_network_name">> => <<"om_ran">>,
			                             <<"oam_network_name">> => <<"om_ran">>,
						     <<"backhaul_network_name">> => <<"pran_backhaul">>,
						     <<"fronthaul_network_name">> => <<"pran_fronthaul">>}}),
    Json.

%% %% This shal be used when instantiate_vnf_zip use /lcm/v0/
%% jsone_encode_additionalParams() ->
%%     Json =jsone:encode(#{<<"additionalParams">> => [#{<<"name">> => <<"tenant_network_name">>,
%% 						      <<"type">> => <<"string">>,
%% 						      <<"value">> => <<"om_ran">>},
%% 						    #{<<"name">> => <<"oam_network_name">>,
%% 						      <<"type">> => <<"string">>,
%% 						      <<"value">> => <<"om_ran">>},
%% 						    #{<<"name">> => <<"backhaul_network_name">>,
%% 						      <<"type">> => <<"string">>,
%% 						      <<"value">> => <<"pran_backhaul">>},
%% 						    #{<<"name">> => <<"fronthaul_network_name">>,
%% 						      <<"type">> => <<"string">>,
%% 						      <<"value">> => <<"pran_fronthaul">>}]}),
%%     Json.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Instantiate with SiteBasic and lkf , base64
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ai_instantiate_vnf_zip(VName,VnfInstanceId) ->
    Url="http://"++ip(VName)++":9100/vnflcm/v1/vnf_instances/"++binary_to_list(VnfInstanceId)++"/instantiate",
    Json = jsone_encode_additionalParams_ai(),
    %% request(post,{Url,Json}).
    request_will_reply_header_and_body(post,{Url,Json}).

%% This shal be used when instantiate_vnf_zip use /vnflcm/v1/
jsone_encode_additionalParams_ai() ->
    {ok, SiteBasicBase64Bin} = file:read_file("/proj/rcs-tmp/etxivri/vnf_base64/p12_base64_siteBasicFile.txt"),
    %% ct:log("SiteBasicBase64: ~n~p",[SiteBasicBase64Bin]),
    {ok, LkfBase64Bin} = file:read_file("/proj/rcs-tmp/etxivri/vnf_base64/p12_base64_myVPP_171031_083532.txt"),
    %% ct:log("LkfBase64: ~n~p",[LkfBase64Bin]),

    Json =jsone:encode(#{<<"additionalParams">> => #{<<"tenant_network_name">> => <<"om_ran">>,
						     <<"oam_network_name">> => <<"om_ran">>,
						     <<"backhaul_network_name">> => <<"pran_backhaul">>,
						     <<"fronthaul_network_name">> => <<"pran_fronthaul">>,
						     <<"file:sitebasic.xml">> => SiteBasicBase64Bin,
						     <<"file:licensekeyfile.xml">> => LkfBase64Bin
						    }}),
    Json.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ========================= Upgrade eri_lcm => LCM => port 9999 ====================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OpenMano create new package
%% 


%%% @spec ug_create_job_id_operation(VName) -> {ok, map()} | {error, term()}
%%% @doc Create job id operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%
%%%    Example: rct_vnfm:ug_create_job_id_operation(VName).'''
ug_create_job_id_operation(VName) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/job", %%% eri_lcm => LCM => port 9999
%%    get_data(),
    VnfDescId = 1,
    VnfId = 1,
    VnfPkgId =1,
    Json = jsone:encode(#{<<"vnf_descriptor_id">> => VnfDescId,
			  <<"vnf_id">> => VnfId,
			  <<"vnf_package_id">> => VnfPkgId}),
    request(post,{Url,Json}).

%%% @spec ug_create_job_id_operation(VName, VnfId, VnfPkgId, VnfDescId) -> {ok, map()} | {error, term()}
%%% @doc Create job id operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    VnfId  = string  Datacenter instanceId
%%%    VnfPkgId = string Datacenter vnfPackageId
%%%    VnfId  = string  Datacenter vnfdId
%%%
%%%    Example: rct_vnfm:ug_create_job_id_operation(VName, VnfId, VnfPkgId, VnfDescId).'''
ug_create_job_id_operation(VName, VnfId, VnfPkgId, VnfDescId) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/job", %%% eri_lcm => LCM => port 9999
    Json = jsone:encode(#{<<"vnf_descriptor_id">> => VnfDescId,
			  <<"vnf_id">> => VnfId,
			  <<"vnf_package_id">> => VnfPkgId}),
    request(post,{Url,Json}).

%%% @spec ug_create_job_id_operation(VName, FbTimer, VnfId, VnfPkgId, VnfDescId) -> {ok, map()} | {error, term()}
%%% @doc Create job id operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    FbTimer  = integer
%%%    VnfId  = string  Datacenter instanceId , from instance.
%%%    VnfPkgId = string Datacenter vnfPackageId, to pkg.
%%%    VnfId  = string  Datacenter vnfdId, to pkg.
%%%
%%%    Example: rct_vnfm:ug_create_job_id_operation(VName, FbTimer, VnfId, VnfPkgId, VnfDescId).'''
ug_create_job_id_operation(VName, FbTimer, VnfId, VnfPkgId, VnfDescId) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/job", 
    Json = jsone:encode(#{<<"fallback_timeout">> => FbTimer,
			  <<"vnf_descriptor_id">> => VnfDescId,
			  <<"vnf_id">> => VnfId,
			  <<"vnf_package_id">> => VnfPkgId}),
    request(post,{Url,Json}).


%%% @spec ug_get_all_jobs(VName) -> {ok, map()} | {error, term()}
%%% @doc  Get ongoing upgrade job IDs .
%%% ```VName            = atom()    Identifier for VNFM instance
%%%
%%%    Example: rct_vnfm:ug_get_all_jobs(VName).'''
ug_get_all_jobs(VName) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/job", 
    request(get,{Url}).


%%% @spec ug_get_job_status(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc Get job status operation for an ongoing upgrade job.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_get_job_status(VName, Job_Id).'''
ug_get_job_status(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id), 
    request(get,{Url}).


%%% @spec ug_delete_job_id(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc  Request for deletion of job id.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_delete_job_id(VName, Job_Id).'''
ug_delete_job_id(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id), 
    request(delete,{Url}).


%%% @spec ug_prepare(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc  Prepare operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_prepare(VName, Job_Id).'''
ug_prepare(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id)++"/prepare", 
    Json = jsone:encode(#{<<"job_id">> => Job_Id}),
    request(post,{Url,Json}).


%%% @spec ug_verify(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc  Verify operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_verify(VName, Job_Id).'''
ug_verify(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id)++"/verify", 
    Json = jsone:encode(#{<<"job_id">> => Job_Id}),
    request(post,{Url,Json}).


%%% @spec ug_activate(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc  Activate operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_activate(VName, Job_Id).'''
ug_activate(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id)++"/activate", 
    Json = jsone:encode(#{<<"job_id">> => Job_Id}),
    request(post,{Url,Json}).


%%% @spec ug_confirm(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc  Confirm operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_confirm(VName, Job_Id).'''
ug_confirm(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id)++"/confirm", 
    Json = jsone:encode(#{<<"job_id">> => Job_Id}),
    request(post,{Url,Json}).


%%% @spec ug_confirm(VName, Job_Id, FbTimer) -> {ok, map()} | {error, term()}
%%% @doc  Confirm operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%    Job_Id  = string    From create job id operation for upgrade.
%%%    FbTimer  = integer
%%%
%%%    Example: rct_vnfm:ug_confirm(VName, Job_Id, FbTimer).'''
ug_confirm(VName, Job_Id, FbTimer) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id)++"/confirm", 
    Json = jsone:encode(#{<<"job_id">> => Job_Id,
			  <<"fallback_timeout">> => FbTimer }),
    request(post,{Url,Json}).


%%% @spec ug_cancel(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc  Cancel operation for upgrade.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id  = string    From create job id operation for upgrade.
%%%
%%%    Example: rct_vnfm:ug_cancel(VName, Job_Id).'''
ug_cancel(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9999/eri_lcm/v1/jobs/"++integer_to_list(Job_Id)++"/cancel", 
    Json = jsone:encode(#{<<"job_id">> => Job_Id}),
    request(post,{Url,Json}).


%%%%%%%%%%%%%%%%%%%%%% Restart VNF %%%%%%%%%%%%%%%%%%%%
%%% @spec  vnf_heal(VName, InstanceId) -> {ok, map()} | {error, term()}
%%% @doc  Order a restart on VNF.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```InstanceId       = binary , InstanceId.
%%% 
%%%    Example: rct_vnfm:vnf_heal(VName, InstanceId).'''
vnf_heal(VName, InstanceId) ->
    Url="http://"++ip(VName)++":9100/vnflcm/v1/vnf_instances/"++binary_to_list(InstanceId)++"/heal",
    Json = jsone:encode(#{<<"cause">> => <<"Manual restart">>}),
    %% request(post,{Url,Json}).
    request_will_reply_header_and_body(post,{Url,Json}).


%%%%%%%%%%%%%%%%%%%%%% Misc %%%%%%%%%%%%%%%%%%%%
%%% @spec get_all_vnf_packages(VName) -> {ok, map()} | {error, term()}
%%% @doc   List VNF packages.
%%% ```VName            = atom()    Identifier for VNFM instance
%%%
%%%    Example: rct_vnfm:get_all_vnf_packages(VName).'''
get_all_vnf_packages(VName) ->
    Url="http://"++ip(VName)++":9090/openmano/pkg/v0/vnf_packages", 
    request(get,{Url}).


%%% @spec get_single_vnf_packages(VName, VnfdId) -> {ok, map()} | {error, term()}
%%% @doc List one specific VNF packages .
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```VnfdId           = string
%%%
%%%    Example: rct_vnfm:get_single_vnf_packages(VName, VnfdId).'''
get_single_vnf_packages(VName, VnfdId) ->
    Url="http://"++ip(VName)++":9090/openmano/pkg/v0/vnf_packages/"++VnfdId, 
    request(get,{Url}).


%%% @spec get_vnfp_job_status(VName, Job_Id) -> {ok, map()} | {error, term()}
%%% @doc List one specific VNF packages .
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```Job_Id           = string , From create package operation
%%%
%%%    Example: rct_vnfm:get_vnfp_job_status(VName, Job_Id).'''
get_vnfp_job_status(VName, Job_Id) ->
    Url="http://"++ip(VName)++":9090/openmano/pkg/v0/jobs/"++binary_to_list(Job_Id), 
    request(get,{Url}).


%%% @spec vnf_show_lcop(VName, Lcop) -> {ok, map()} | {error, term()}
%%% @doc Query Single VNF LCM Operation Occurence .
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ``` Lcop         = string , From instantiate
%%%
%%%    Example: rct_vnfm:.vnf_lcop_show(VName, Lcop)'''
vnf_show_lcop(VName, Lcop) ->
    Url="http://"++ip(VName)++":9100/lcm/v0/vnf_lc_ops/"++binary_to_list(Lcop),
    request(get,{Url}).


%%% @spec  fetch_vnfm_esi_rest_api(VName, EsiName, EsiLogPath) -> {ok, map()} | {error, term()}
%%% @doc Fetch esi from vnfm using rest api. Note after SP419 is ready logM Mo shall be used.
%%% ```VName            = atom()    Identifier for VNFM instance
%%% ```EsiName          = string
%%% ```EsiLogPath       = path string to the dir to put esi.
%%%
%%%    Example: rct_vnfm:fetch_vnfm_esi_rest_api(VName, EsiName, EsiLogPath).'''
fetch_vnfm_esi_rest_api(VName, EsiName, EsiLogPath) ->
    %% wget --output-document MyEsi.tgz --no-proxy http://10.68.97.135/eri_esi/v1/export_esi
    ct:log("# # EsiName: ~p", [EsiName]),
    ct:log("# # EsiLogPath: ~p", [EsiLogPath]),
    FetchEsiCmd = "wget --output-document "++EsiName++" --no-proxy http://"++ip(VName)++"/eri_esi/v1/export_esi",
    Res = os:cmd("cd "++EsiLogPath++"; "++FetchEsiCmd),
    ct:log("Res : ~n~p", [Res]),
    Res.

build_http_zip(Config, InputZip) ->
    ct:log("Create httpZip from file"),
    LogPath =  ?config(priv_dir,Config),
    ct:log("Path to store zip file: ~n~p",[LogPath]),
    os:cmd("chmod 777 "++ LogPath), % else permission.

    WebServer = ct:get_config({image_webserver, url}),
    %%WebServer = "https://rbs-rde.rnd.ki.sw.ericsson.se", %% Should bee in stp.cfg
    ct:log("WebServer: ~p",[WebServer]),

    %%copy the zip to dir where webserver is accesible
    os:cmd("cp " ++ re:replace(InputZip,"\n", "" ,[global, {return, list}]) ++ " " ++ filename:join(LogPath, ".")),
    
    %%Filter out the zip from path
    ZipCxp = re:replace(lists:last(filename:split(InputZip)),"\n", "",[global, {return, list}]) ,
    ct:log("zipCxp ~n~p",[ZipCxp]),
    %%Make the file unique
    ZipCxpDate = integer_to_list(erlang:system_time()) ++ ZipCxp,
    
    os:cmd("mv " ++ filename:join(LogPath, ZipCxp) ++ " " ++ filename:join(LogPath, ZipCxpDate)),
    ProjZipCxpDat = filename:join(LogPath, ZipCxpDate),
    HttpZip = WebServer ++  ProjZipCxpDat,
    ct:log("HttpZip ~n~p",[HttpZip]),
    HttpZip.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% good to have
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_vnf_lcm_op_occs_binary(Header) ->
    ct:log("## HeaderPropList:~p", [Header]),			  
    Location = proplists:get_value("location", Header),
    ct:log("Location: ~p", [Location]),
    VnfLcOpId = lists:last(string:tokens(Location,"/")),
    ct:log("vnfLcOpId:~p", [VnfLcOpId]),
    BinaryVnfLcOpId = list_to_binary(VnfLcOpId),
    ct:log("BinaryVnfLcOpId:~p", [BinaryVnfLcOpId]),
    BinaryVnfLcOpId.



wait_for_lcop_success(_Vnfm, _VnfLcOpId, Timeout) when Timeout < 0 ->
    ct:pal("## lcop not successful within expected time."),
    nok;
wait_for_lcop_success(Vnfm, VnfLcOpId, Timeout) ->
    {ok, #{<<"status">> := LcopRes}} = 
	rct_vnfm:vnf_show_lcop(Vnfm, VnfLcOpId),
    ct:log("LcopRes ~p",[LcopRes]),
    case binary_to_list(LcopRes) of
	"success" ->
	    ct:pal("## lcop is success."),
	    ok;
	 Other ->
	    ct:log("## lcop is : ~p , not expected. sleep and try againg.", [Other]),
	    timer:sleep(10000),
	    wait_for_lcop_success(Vnfm, VnfLcOpId, Timeout-10000)
    end.


wait_for_cli(_Cli, Timeout, _CliCmd, _MatchStr) when Timeout < 0 ->
    nok;
wait_for_cli(Cli, Timeout, CliCmd, MatchStr) ->
    ct:log("CliCmd : ~p ~nMatchStr : ~p ",[CliCmd, MatchStr]),
    ok = rct_cli:connect(Cli),
    {ok,A} = rct_cli:send(Cli, CliCmd),
    ct:log("# A: ~p", [A]),
    rct_cli:disconnect(Cli),
    case re:run(A, MatchStr) of
   	{match, _} ->
   	    ct:pal("Result from cli cmd match expected str, and that is great!"),
   	    ok;
   	nomatch ->
   	    ct:pal("# Sleep 10 sec and check again #"),
   	    timer:sleep(10000),
   	    wait_for_cli(Cli, Timeout-10000, CliCmd, MatchStr)
    end.

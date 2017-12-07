%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_api.erl %
%%% Author:	etxbjca
%%% Description:
%%%  All calls in and out of this block are made through this module.
%%%  The only API functions in the normal use of the word are those
%%%  called from OOT and CERTM as a result of subscription for data
%%%  change.
%%%  The "outgoing" calls (OOT and CERTM) have been collected here to make
%%%  it easier to keep track of them while everything is in flux.
%%%  These are all i "exported INTERNAL functions"
%%%
%%%  TODO: update COMSA to call here (omc_server:authorize_user/1
%%%               SYS                (omc_server:autointegration_fun/1)
%%%		  ECOLI              (omc_server:authorize_user/1)
%%%
%%%  Wrap the LDAP functions here.
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(omc_api).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R10A/R11A/1').
-date('2017-10-10').
-author('ehsmbj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R          2014-01-29   etxlg     renamed from omc_stub
%%% R2A/5      2014-01-30   etxlg     dialyzer fixing
%%% R2A/6      2014-02-19   etxlg     Activity since restart
%%% R2A/8      2014-04-09   etxlg     prepare_to_shutdown/0
%%% R2A/9      2014-04-11   etxlg     restart_complete/0
%%% R2A/10     2014-06-16   etxlg     netconf_tls_notify/0 + cleaning
%%% R2A/11     2014-06-17   etxlg     logout_non_support_users()
%%%				      is_only_support_users()
%%% R2A/12     2014-06-24   etxlg     netconf_ssh_notify()
%%% R2A/14     2014-07-08   etxlg     TLS ports from sysEnv
%%% R2A/15     2014-09-02   etxlg     get_dscp_opt_list, subscribe-oot
%%% R3A/1      2014-12-05   etxlg     get_oam_ns_opt_list, get_lmt_ns_opt_list
%%%					is_net_namespace
%%% R3A/2      2015-01-08   etxlg     try stopping ssh when shutting down
%%% R3A/3      2015-01-12   etxlg     undid above
%%% R3A/4      2015-01-20   etxtory   Added fc to omc_https:restart_complete
%%% R3A/5      2015-01-20   etxtory   Added fc to omc_https:change_notify
%%% R3A/6      2015-01-21   etxlg     prepare_to_shutdown/0 - again
%%% R3A/7      2015-02-13   etxlg     ns_to_opt_list/1
%%% R3A/8      2015-03-08   etxlg     restart_complete -> omc_tls_server
%%% R3A/9      2015-03-18   etxlg     sysinit connected to get_lmt_ns_opt_list
%%% R3A/10     2015-04-08   etxtory   Added omc_https for tls_notify
%%% R4A/0      2015-05-20   etxasta   Added cli_tls_notify/0 and cli_ssh_notify/0
%%% R4A/2      2015-08-25   etxlg     Removed io:format, not used function
%%% R4A/4      2015-11-09   etxlg     IPv6
%%% R5A/1      2016-04-26   etxpeno   (TR HU78388)
%%% R6A/1      2016-08-30   emariad   CSUC feature, cipher configuration
%%% R7A/1      2016-10-18   ehsake    WP6081, Generate ESI at rollback
%%% R10A/1     2017-05-23   eolaand   SP419, add fcn oam_traffic_class_notify/0 
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([authorize_user/1, autointegration_fun/1]).
-export([ldap_lookup/1,ldap_lookup/2]). %called by COI for authentication of user
-export([is_activity_since_restart/0]).
-export([prepare_to_shutdown/0, restart_complete/0]).
-export([netconf_tls_notify/0]). %called by comSysM when ECIM:NetconfTls changes
-export([netconf_ssh_notify/0]). %called by comSysM when ECIM:NetconfSsh changes
-export([cli_tls_notify/0]). %called by comSysM when ECIM:CliTls changes
-export([cli_ssh_notify/0]). %called by comSysM when ECIM:CliSsh changes
-export([oam_traffic_class_notify/0]). %called by comSysM when ECIM:OamTrafficClass changes
-export([tls_cipher_notify/0]).%called when ECIM:Tls changes
-export([ssh_config_notify/0]). %called when ECIM:Ssh changes.

-export([logout_non_support_users/0]).
-export([is_only_support_users/0]).
-export([get_dscp_opt_list/0, get_dscp_opt_list/1]).
-export([get_oam_ns_opt_list/0, get_lmt_ns_opt_list/0]).
-export([ns_to_opt_list/1]).
-export([get_oot_ip_opt_list/0]).
-export([is_net_namespace/0]).
-export([generate_rollback_esi/0]).
%%-export([various callbacks for subscription results here]).

%%for test, remove when happy
-export([debug_new_config/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([get_local_ip/0, get_port/2]).

-export([subscribe/1, subscribe/2]).
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
authorize_user(User)->
    omc_server:authorize_user(User).

%%called from COI   
ldap_lookup(User)->
    omc_ldap_server:ldap_lookup(User).
    
%%called from COI   
ldap_lookup(User, Pw)->
    omc_ldap_server:ldap_lookup(User, Pw).    

autointegration_fun(Fun) when is_function(Fun); Fun =:= undefined ->
    omc_server:autointegration_fun(Fun).

-spec  is_activity_since_restart() -> boolean().
is_activity_since_restart() ->
    omc_server:is_activity_since_restart().

%%called by appm(?) to indicate that system is going to be shutting down
%%prepare for this by e.g. disconnecting sessions and disabling any new logins
prepare_to_shutdown() ->
    %%ssh:stop(), %not good, process crash in OTP
    omc_server:prepare_to_shutdown(),
    ok.

%%called from APPM to signal everything is up
%%and services for OaM login can be activated
restart_complete() ->
    omc_server:restart_complete(),
    omc_tls_server:restart_complete(),
    omc_https:restart_complete(),
    ok.

%%called from COMSA when TLS cipher configuration changed
tls_cipher_notify() ->	 
    omc_tls_server:close_all(),
    omc_https:tls_cipher_change_notify().

%%called from COMSA
netconf_tls_notify() ->
    omc_tls_server:change_notify().

%%called from COMSA
cli_tls_notify() ->
    omc_tls_server:change_notify(),
    omc_https:change_notify().

%%called from COMSA
%%the only thing that can change is the administrativeState = 'LOCKED'|'UNLOCKED'
netconf_ssh_notify() ->
    omc_server:change_notify().

%%called from COMSA
%%the only thing that can change is the administrativeState = 'LOCKED'|'UNLOCKED'
cli_ssh_notify() ->
    omc_server:change_notify(),
    omc_https:change_notify().

%%called from COMSA
%%the only thing that can change is dscp
oam_traffic_class_notify() ->
    %% FIXME: This function should have its own cast in omc_server.
    %% For now the OOT callback is used.
    omc_server:config_update([]).

%%called when the ssh configuration is changed
ssh_config_notify() ->
    omc_server:ssh_config_notify().

%% Callback function for ESI rollback collection. 
%% Triggered by LOG block.
generate_rollback_esi() ->
    omc_lib:generate_rollback_esi().

get_dscp_opt_list(Dscp) ->
    ootI:get_dscp_opt_list(Dscp).

get_dscp_opt_list() ->
    ootI:get_dscp_opt_list().

-spec get_oot_ip_opt_list() -> list().
get_oot_ip_opt_list() ->
    case ootI:get_oap_ip_addr() of
	[] -> [];
	IP_string ->
	    case inet:parse_address(IP_string) of
		{ok, IP_tuple} ->
		    [{ip, IP_tuple}];
		_ ->
		    sysInitI:warning_msg(
                            "~p: unable to decode OaM IP address ~p~n",
                                [?MODULE, IP_string]),
		    []
	    end
    end.
%%called from COMSA
-spec logout_non_support_users() -> ok.
logout_non_support_users() ->
    omc_server:logout_non_support_users().

%%called from COMSA
-spec is_only_support_users() -> boolean().
is_only_support_users() ->
    omc_server:is_only_support_users().


-spec get_oam_ns_opt_list() -> list().
get_oam_ns_opt_list() ->
    case ootI:get_oap_namespace() of
	{ok, <<>>} -> [];
	{ok, Ns} -> [{netns, <<"/var/run/netns/", Ns/binary>>}];
	_Err ->
	    sysInitI:warning_msg("~p: Failed to read OaM namespace: ~p~n",
		[?MODULE, _Err]),
	[]
    end.

-spec get_lmt_ns_opt_list() -> list().
get_lmt_ns_opt_list() ->
    case sysInitI:get_lmt_ns() of
	<<>> -> %the LMT port isn't namespaced
	    [];
	Bin_ns ->
	    ns_to_opt_list(Bin_ns)
    end.

%convert the short Ns-binary (e.g. <<"fib_3">> into an inets-option
-spec ns_to_opt_list(binary()) -> list().
ns_to_opt_list(<<>>) ->
    [];
ns_to_opt_list(Ns) when is_binary(Ns) ->
    [{netns, <<"/var/run/netns/", Ns/binary>>}].

-spec is_net_namespace() -> boolean().
is_net_namespace() ->
    get_oam_ns_opt_list() =/= []  orelse get_lmt_ns_opt_list() =/= [].

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-spec get_local_ip() -> tuple() | any | undefined. % i.e. der_encoded()
get_local_ip() ->
    any.
    %{147, 214, 7, 29}. %exekilvxen519

-spec get_port(atom(), atom()) -> integer().
get_port(tls, cli) ->
    sysEnv:get_port_conf(cli_tls);
get_port(tls, netconf) ->
    sysEnv:get_port_conf(netconf_tls);
get_port(tls, coli) ->
    try sysEnv:get_port_conf(coli_tls)
    catch
	_:_ -> 9831
    end;
get_port(ssh, T) when T =:= cli; T =:= netconf; T =:= coli; T =:= sftp ->
    sysEnv:get_port_conf(T).


subscribe(ssh_certs) ->
    %call CERTM for this
    %debug("subscribe ssh_certs"),
    ok;
subscribe(What_ever) ->
    debug("subscribe to unknown service"), %using debug will silence dialyzer
    erlang:error({"unknown subscription type", What_ever}),
    ok.

subscribe(oot, Fun) ->
    ootI:register_cfg_upd_cb(Fun).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

debug(Format) ->
    debug(Format, []).

debug(Format, Params) ->
    io:format("dbg ~p:" ++ Format ++ "~n", [?MODULE | Params]).

-define(BASEDIR, "/home/etxlg/Work/Tls_oam").  %FIXME for test

debug_new_config() ->
   Config = [{cert, dbg_get_node_cert()},
	{cacerts, dbg_get_ca_certs()},
	{key, dbg_get_node_key()}],
    omc_server:config_update(Config).

dbg_get_node_cert() ->
    Cert_file_pem = filename:join([?BASEDIR, "Certs/server.crt"]),
    {ok, Cert_pem} = file:read_file(Cert_file_pem),
    [{'Certificate', Cert_der, not_encrypted}] =
	public_key:pem_decode(Cert_pem),
    Cert_der.

dbg_get_ca_certs() ->
    Ca_file_pem = filename:join([?BASEDIR, "Certs/user-ca.crt"]),
    {ok, Ca_pem} = file:read_file(Ca_file_pem),
    [{'Certificate', Ca_der, not_encrypted}] =
	public_key:pem_decode(Ca_pem),
    [Ca_der].

dbg_get_node_key() ->
    Key_file_pem = filename:join([?BASEDIR, "Certs/server.key"]),
    {ok, Key_pem} = file:read_file(Key_file_pem),
    % DIALYZER warns for this - fault is in OTP:public_key
    % I've inforrmed them informally/lg
%%    [{'PrivateKeyInfo', Key_der, not_encrypted}] =
%%	public_key:pem_decode(Key_pem),
%%    {'PrivateKeyInfo', Key_der}.
    [{Workaround, Key_der, not_encrypted}] =
	public_key:pem_decode(Key_pem),
    {Workaround, Key_der}.

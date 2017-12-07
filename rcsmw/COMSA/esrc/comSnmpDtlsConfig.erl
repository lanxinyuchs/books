%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comSnmpDtlsConfig.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R6A/R8A/R10A/R11A/24

%%% @doc ==Agent implementation for SNMP==
%%% This is the agent implementation the SNMP model.
%%% The data is then forward to ComEA for configuration the net-snmp library

-module(comSnmpDtlsConfig).
-behaviour(gen_server).
-vsn('/main/R6A/R8A/R10A/R11A/24').
-date('2017-09-08').
-author(enenteo).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% ------------------------------------------------------
%%% Rev        Date       Name          What
%%% -----    -------      --------      ------------------------
%%% R6A/2    2016-08-17   enenteo       First version after refactoring    
%%% R10A/1   2017-06-20   etxarnu       Removed update_env/0
%%% R10A/2   2017-07-07   eivmiha       Added snmp config handler functions
%%% R10A/5   2017-07-14   estjako       Added snmp directory handler functions
%%% R10A/6   2017-07-17   estifil       Added snmp event handler
%%% R11A/1   2017-07-24   estifil       Code refactoring
%%% R11A/2   2017-08-02   ekurnik       "Empty" node credential issue fix,
%%%                                     added activate/0 and improved logging
%%% R11A/3   2017-08-03   ekurnik       Activate is now 'cast' to avoid timeout
%%% R11A/4   2017-08-04   ekurnik       Removed stop/start snmp in init
%%% R11A/5   2017-08-07   ekurnik       init is now done after NS is received to 
%%%                                     avoid errors in syslog
%%% R11A/6   2017-08-08   estjako       Changed NODE_CREDENTIAL macro
%%% R11A/7   2017-08-08   evadumb       Restructured handle_change, added new functions
%%%                                     for agentAddressDtls
%%% R11A/8   2017-08-09   evadumb       Added agentAddressDtls to initialize
%%% R11A/9   2017-08-11   estjako       Config handler dtls user
%%% R11A/11  2017-08-16   ekurnik       Changed comea_snmp calls directly through appmServer
%%%                                     Added is_snmp_dtls_enabled check
%%% R11A/12  2017-08-18   ekurnik       Fingerprint for NC is taken from DER encoded cert
%%% R11A/13  2017-08-18   ekurnik       Fixed issue with restart snmpd after DTLS becomes disabled
%%% R11A/14  2017-08-21   eivmiha       Fixed bool_to_rw
%%% R11A/15  2017-08-21   estjako       Small fix
%%% R11A/16  2017-08-22   ekurnik       Changed ca-certs extensions from .der to .crt
%%% R11A/17  2017-08-23   emirbos       Added snmpTargetV3Dtls event handler
%%% R11A/18  2017-08-24   ekurnik       Improved state machine for writing dtls params to config
%%% R11A/19  2017-08-23   emirbos       snmpTargetV3Dtls config update in init
%%%                                     snmpTargetV3Dtls administrative state added
%%% R11A/23  2017-09-06   enekdav       snmpTargetV3Dtls event handler fix
%%% R11A/23  2017-09-08   enekdav       Fixed issue with CipherFilter notify info message
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


%% callbacks from CERT and OOT
-export([cert_event/1,
         oot_event/1]).

%% called from comsaDataInit
-export([activate/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsSnmp.hrl").

-export([start/0,
         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

%% internal functions (exported for test)
-export([
     initialize/1,
     store_nc_cert_info/1,
     remove_nc_cert_info/0,
     get_cert/1,
     get_fingerprint/1,
     resubscribe_mo/2,
     subscribe_mo/2,
     unsubscribe_mo/1,
     store_tc_cert_info/1,
     remove_tc_cert_info/0,
     store_agent_address_dtls_info/1,
     get_tcat_and_crlcheck/1,
     generate_nc_fingerprint/1,
     generate_tc_fingerprints/1,
     agent_address_dtls_to_list/1
        ]).

%% tls directory functions (exported for test)
-export([
     clear_nc_dir/0,
     clear_tc_dir/0,
     create_dtls_dir_struct/0,
     delete_dtls_dir_struct/0,
     write_nc_tls_dir/2,
     write_tc_tls_dir/1
        ]).

%% comea-snmp functions (exported for test)
-export([
     config_set_nc/1,
     config_set_tc/1, 
     config_update_tc/1,
     config_delete_nc/0,
     config_delete_tc/0,
     config_set_agent_address_dtls/1,
     config_delete_agent_address_dtls/0,
     config_set_dtls_user/1,
     config_delete_dtls_user/0, 
     get_proto/1,
     config_handler/2,
     config_set_dtlsuser/1,
     config_set_cipher_filter/1
        ]).

%% called from rcsSecM.erl
-export([dtls_cipher_filter_notify/0]).

%% for testing
-export([is_snmp_dtls_enabled/0, is_snmp_dtls_enabled/1, restart_snmpd/1]).

-define(LOG(Msg), ?LOG(Msg, [])).
-define(LOG(Format, Args), io:format("comSnmpDtlsConfig trace: " ++ Format, Args)).
-define(CERT_EVENT_OFFSET, 100).
-define(LOCKED, 0).
-define(UNLOCKED, 1).
-define(CIPHER_FILTER,      "setCipherStringDtls").

-record(state, {oam_namespace,   %% at startup/restart wait until namespace is provided
               is_dtls_enabled = false}). %% current state of dtls, used in reconfiguration

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Triggers initialization
activate() ->
    gen_server:cast(?MODULE, activate).

init(_Arg) ->
    ?LOG("Process started [~p]~n", [self()]),
    create_dtls_dir_struct(),
    {ok, #state{oam_namespace = undefined}}.

cert_event(MoRef) ->
    %% wait before sending to avoid race conditions
    timer:apply_after(?CERT_EVENT_OFFSET, gen_server, cast, [?MODULE, {cert_event, MoRef}]).

oot_event(Config) ->
    gen_server:cast(?MODULE, {oot_event, Config}).

-spec dtls_cipher_filter_notify() -> ok.
dtls_cipher_filter_notify() ->
    gen_server:cast(?MODULE, cipher_filter_notify).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

handle_call(_Msg, _From, State)->
    {reply, ok, State}.

handle_cast(cipher_filter_notify, #state{is_dtls_enabled = true} = State) ->
    CipherFilter = rcsSecM:get_tls_cipher_filter(),
    sysInitI:info_msg("~p: CipherFilter notify received: ~p~n", [?MODULE, CipherFilter]),
    config_handler(?CIPHER_FILTER, CipherFilter),
    restart_snmpd(is_snmp_unlocked(get_snmp_info())),
    {noreply, State};

handle_cast({cert_event, MoRef}, State) ->
    sysInitI:info_msg("~p: cert_event received - MO changed: [~p]~n", [?MODULE, MoRef]),
    NewState = reconfigure_snmp(State),
    {noreply, NewState};

%% only case where we're interested in oot config updates is when namespace is undefined (on startup/restart)
handle_cast({oot_event, Config}, #state{oam_namespace = undefined} = State) ->
    OamNs = proplists:get_value(oap_namespace, Config, undefined),
    sysInitI:info_msg("~p: oot_event received - OAM namespace: [~p]~n", [?MODULE, OamNs]),
    NewState = initialize(State#state{oam_namespace = OamNs}),
    {noreply, NewState};

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(activate, State) ->
    ok = ootI:register_cfg_upd_cb(fun ?MODULE:oot_event/1),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, {write, snmp, #snmp{trustCategory = OldTC, nodeCredential = OldNC, agentAddressDtls = OldAAD},
                                              [#snmp{trustCategory = OldTC, nodeCredential = OldNC, agentAddressDtls = OldAAD}], _}}, State) ->
    %% no change to dtls
    {noreply, State};

handle_info({mnesia_table_event, {write, snmp, #snmp{}=NewSnmp, [#snmp{}=OldSnmp], _ActivityId}}, #state{is_dtls_enabled = WasEnabled} = State) ->
    IsEnabled = is_snmp_dtls_enabled(NewSnmp),
    resubscribe_mo(OldSnmp, NewSnmp),
    DtlsReconfigured = config_dtls_state_machine({WasEnabled, IsEnabled}, {OldSnmp, NewSnmp}),
    restart_snmpd(is_snmp_unlocked(NewSnmp) andalso DtlsReconfigured),
    sysInitI:info_msg("Handling SNMP MO change!~n"),
    handle_snmp_change(WasEnabled, IsEnabled),
    {noreply, State#state{is_dtls_enabled = IsEnabled}};

%% If dtls is disabled, don't write user/s to config
handle_info({mnesia_table_event, {_, snmpTargetV3Dtls, _, _, _}}, #state{is_dtls_enabled = false} = State) ->
    {noreply, State};

%% SnmpTargetV3Dtls created
handle_info({mnesia_table_event, {write, snmpTargetV3Dtls, #snmpTargetV3Dtls{}=NewSnmpTargetV3Dtls, [], _ActivityId}}, State) ->
    handle_change_snmptargetV3Dtls(NewSnmpTargetV3Dtls),
    sysInitI:info_msg("Handling SNMPTargetV3DTLS MO change!~n"),
    IsEnabled = is_snmp_dtls_enabled(),
    handle_target_change(undefined, NewSnmpTargetV3Dtls, IsEnabled),
    restart_snmpd(is_snmp_unlocked(get_snmp_info())),
    {noreply, State};

%% No change in V3Dtls target
handle_info({mnesia_table_event, {write, snmpTargetV3Dtls, OldSnmpTargetV3Dtls, [OldSnmpTargetV3Dtls], _ActivityId}}, State) ->
    {noreply, State};

%% SnmpTargetV3Dtls modified
handle_info({mnesia_table_event, {write, snmpTargetV3Dtls, #snmpTargetV3Dtls{operationalState = OPS}=NewSnmpTargetV3Dtls, [#snmpTargetV3Dtls{operationalState = OPS}=OldSnmpTargetV3Dtls], _ActivityId}}, State) ->
    handle_change_snmptargetV3Dtls(NewSnmpTargetV3Dtls),
    IsEnabled = is_snmp_dtls_enabled(),
    handle_target_change(OldSnmpTargetV3Dtls, NewSnmpTargetV3Dtls, IsEnabled),
    restart_snmpd(is_snmp_unlocked(get_snmp_info())),
    {noreply, State};

handle_info({mnesia_table_event, {write, snmpTargetV3Dtls, #snmpTargetV3Dtls{}, [#snmpTargetV3Dtls{}], _ActivityId}}, State) ->
    {noreply, State};

%% SnmpTargetV3Dtls deleted
handle_info({mnesia_table_event, {delete, snmpTargetV3Dtls, {_snmpTargetV3Dtls, _Key}, _OldSnmpTargetV3DtlsList, _ActivityId}}, State) ->
    handle_change_snmptargetV3Dtls(),
    restart_snmpd(is_snmp_unlocked(get_snmp_info())),
    {noreply, State};

handle_info(_Msg, State)->
    {noreply, State}.

terminate(_Reason, _State) ->
    delete_dtls_dir_struct(),
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.2.1 SNMPv3 over DTLS configuration functions
%%% #---------------------------------------------------------
-define (ROOT_DIR,      sysEnv:tmp_dir()).
-define (SNMP_DIR,      ?ROOT_DIR ++ "/snmp/").
-define (TLS_DIR,       ?SNMP_DIR ++ "tls/").
-define (CA_CERTS_DIR,  ?TLS_DIR ++ "ca-certs/").
-define (CERTS_DIR,     ?TLS_DIR ++ "certs/").
-define (NEWCERTS_DIR,  ?TLS_DIR ++ "newcerts/").
-define (PRIVATE_DIR,   ?TLS_DIR ++ "private/").
-define (CERT_EXT, ".pem").
-define (TC_EXT, ".crt").
-define (TRUST_CATEGORY,     "dtlsTrustCategory").
-define (NODE_CREDENTIAL,    "dtlsNodeCredentialRcs").
-define (AGENT_ADDRESS_DTLS, "dtlsAgentAddress").
-define (DTLS_USER,        "dtlsUser").
-define (COMEADIR,   filename:join([sysEnv:releases_vsn_dir(), "comte", "comea"])).
-define (COMEA,   "bin/comea").
-define (COMEASCRIPT,   filename:join([?COMEADIR, ?COMEA])).
-define (SNMP_CONFIG,   "etc/snmpd.conf").
-define (SNMPDCF,   filename:join([?COMEADIR, ?SNMP_CONFIG])).
-define (DIRS, [?SNMP_DIR,
                ?TLS_DIR, 
                ?CA_CERTS_DIR, 
                ?CERTS_DIR, 
                ?NEWCERTS_DIR, 
                ?PRIVATE_DIR]).
-define (TC_NAME(N), "tc" ++ lists:flatten(io_lib:format("~p", [N])) ++ ?TC_EXT).
-define (NC_NAME, "nc" ++ ?CERT_EXT).
-define (NC_KEY_NAME, "nc.key").
-define (U_RWX, 8#00700).  %USER can read, write and execute
-define (U_RWX_G_RX_O_X, 8#00751).

-spec config_set_nc(string()) -> ok.
config_set_nc(NcFingerprint) ->
    config_handler(?NODE_CREDENTIAL, NcFingerprint).

-spec config_delete_nc() -> ok.
config_delete_nc() ->
    config_handler(?NODE_CREDENTIAL, "").

-spec config_set_agent_address_dtls(list()) -> ok.
config_set_agent_address_dtls([]) ->
    ok;
config_set_agent_address_dtls([AgentAddressDtls|Rest]) ->
    config_handler(?AGENT_ADDRESS_DTLS, AgentAddressDtls),
    config_set_agent_address_dtls(Rest).

-spec config_delete_agent_address_dtls() -> ok.
config_delete_agent_address_dtls() ->
    config_handler(?AGENT_ADDRESS_DTLS, "").

-spec config_set_tc(list()) -> ok.
config_set_tc([TcFingerprintH|TcFingerprintT]) ->
    config_handler(?TRUST_CATEGORY, TcFingerprintH),
    config_set_tc(TcFingerprintT);

config_set_tc([]) ->
    ok.
-spec config_update_tc(list()) -> ok.
config_update_tc(TcFingerprints) ->
    config_delete_tc(),
    config_set_tc(TcFingerprints).

-spec config_delete_tc() -> ok.
config_delete_tc() ->
    config_handler(?TRUST_CATEGORY, "").

config_set_dtlsuser([]) ->
    ok;
config_set_dtlsuser([UserH|UserT]) ->
    config_handler(?DTLS_USER, UserH),
    config_set_dtlsuser(UserT).

-spec config_set_cipher_filter(string()) ->
    ok.
config_set_cipher_filter(CipherFilter) ->
    config_handler(?CIPHER_FILTER, CipherFilter).

config_delete_cipher_filter() ->
    config_handler(?CIPHER_FILTER, []).

-spec config_handler(string(), string()) -> ok.
config_handler(Parameter, Value) ->
    Args = "snmp configure " ++ Parameter ++ " " ++ Value,
    run_comea_script(Args).

-spec config_handler_restart() -> ok.
config_handler_restart() ->
    config_handler_restart(sysEnv:rcs_mode_2()).

config_handler_restart(simulated) ->
    ok;
config_handler_restart(_) ->
    Args = "snmp restart",
    run_comea_script(comea_snmp_cb_keep, Args).

-spec run_comea_script(string()) -> ok.
run_comea_script(Args) ->
    run_comea_script(comea_snmp_cb_stop, Args).
                               
run_comea_script(CB, Args) ->
    ?LOG("Running comea-snmp command: ~p~n", [Args]),
    TokenArgs = string:tokens(Args," \n"),
    ok = appmServer:do_run_comea_snmp(CB, TokenArgs).

-spec config_set_dtls_user(Snmpv3DtlsParams :: #snmpTargetV3Dtls{}) ->
                           ok.
config_set_dtls_user(#snmpTargetV3Dtls{administrativeState = ?LOCKED}) ->
	ok;
config_set_dtls_user(#snmpTargetV3Dtls{administrativeState = ?UNLOCKED} = SnmpV3DtlsParams) ->
	Retry = integer_to_list(SnmpV3DtlsParams#snmpTargetV3Dtls.informRetryCount),
    Ip = SnmpV3DtlsParams#snmpTargetV3Dtls.address,
    Port = integer_to_list(SnmpV3DtlsParams#snmpTargetV3Dtls.port),
    Timeout = integer_to_list(SnmpV3DtlsParams#snmpTargetV3Dtls.informTimeout),
    Transport = integer_to_transport(SnmpV3DtlsParams#snmpTargetV3Dtls.transportMethod),
    RW = bool_to_rw(SnmpV3DtlsParams#snmpTargetV3Dtls.isMibWritable),
    User = SnmpV3DtlsParams#snmpTargetV3Dtls.user,
    Protocol = get_proto(Ip),
    Space = " ",
    
    % dtlsUser <user> <r/w> <protocol> <host> <port> <transportType> <retries> <timeout>
    DtlsUser = User ++ Space ++ 
               RW ++ Space ++ 
               Protocol ++ Space ++
               Ip ++ Space ++ 
               Port ++ Space ++
               Transport ++ Space ++
               Retry ++ Space ++ Timeout,

    config_handler(?DTLS_USER, DtlsUser).

config_delete_dtls_user() ->
 config_handler(?DTLS_USER, "").
    
bool_to_rw(true) ->
     "rw";
bool_to_rw(false) ->
    "ro".

integer_to_transport(1) ->
    "TRAP";
integer_to_transport(2) ->
    "INFORM".

get_proto(Ip) ->
    case ootI:getaddr(Ip) of
        {ok, {_,_,_,_,_,_,_,_}} -> 
            "dtlsudp6";
        {ok, {_,_,_,_}} ->
            "dtlsudp";
        _->
            sysInitI:warning_msg("Cannot resolve hostname: ~p", [Ip]), 
             "dtlsudp"
    end.


%######## directory_handler interface functions ##############

create_dtls_dir_struct()->
    sysInitI:info_msg("~p: Creating DTLS dir structure.~n", [?MODULE]),
    create_dtls_dir_struct(?DIRS).

%%function used to create dir struct depending on input list of dirs
%%in case that some dir already exists will be cleared
create_dtls_dir_struct([HDir|Restdirs])->
    case file:list_dir_all(HDir) of
    {ok, Filenames} -> %%dir exist, delete files, dirs will remain
        lists:foreach(fun(Filename)-> ok = file:delete(HDir ++ Filename) end, Filenames),
        create_dtls_dir_struct(Restdirs);
    {error, enoent} ->  %%the dir doesn't exist, recreate the dir
        file:make_dir(HDir),
        set_permission(HDir),
        create_dtls_dir_struct(Restdirs)
    end;

create_dtls_dir_struct([]) ->
    ok.

delete_dtls_dir_struct() ->
    sysInitI:info_msg("~p: Deleting DTLS dir sturcture.~n", [?MODULE]),
    delete_dtls_dir_struct(lists:reverse(?DIRS)).
    
delete_dtls_dir_struct(Dirs) ->
   lists:foreach(fun(Dir) -> clear_dir(Dir), ok = file:del_dir(Dir) end, Dirs).

% Clean certs dir
clear_nc_dir() ->
    Dirs=[ ?CERTS_DIR, 
           ?PRIVATE_DIR],
    clear_dirs(Dirs).

% Cleans ca-certs dir
clear_tc_dir() ->
    Dirs=[?CA_CERTS_DIR],
    clear_dir(Dirs).

clear_dirs(Dirs) ->
    lists:foreach(fun(Dir) -> clear_dir(Dir) end, Dirs).

clear_dir(Dir) ->
   {ok, Filenames} =  file:list_dir_all(Dir),
   ?LOG("Clearing files from directory [~p], files ~p~n", [Dir, Filenames]),
   lists:foreach(fun(Filename)->  ok = file:delete(Dir ++ Filename) end, Filenames).

% Write nodeCredential to tls/certs dir 
write_nc_tls_dir(NC, NcKey) ->
    write(?CERTS_DIR ++ ?NC_NAME, NC),
    set_permission(?CERTS_DIR ++ ?NC_NAME, ?U_RWX_G_RX_O_X),
    write(?PRIVATE_DIR ++ ?NC_KEY_NAME, NcKey),
    set_permission(?PRIVATE_DIR ++ ?NC_KEY_NAME, ?U_RWX).
    
% Write trustedCert in tls/ca-certs dir
write_tc_tls_dir(TC)  ->
    write_tc(TC, 1).

write_tc([], _) ->
    ok;
write_tc([TC|TCtail], N) ->
    write(?CA_CERTS_DIR ++ ?TC_NAME(N) , TC),
    set_permission(?CA_CERTS_DIR ++ ?TC_NAME(N), ?U_RWX_G_RX_O_X),
    write_tc(TCtail, N +1 ).

% Helper for write certs or keys 
write(Path, CertOrKey) -> 
   ?LOG("Writing to file [~p]~n", [Path]),
   ok = file:write_file(Path, CertOrKey).


set_permission(Dir) ->
    CACertsDir =  ?CA_CERTS_DIR,
    CertsDir = ?CERTS_DIR,
    PrivateDir = ?PRIVATE_DIR,
    case Dir of 
        CACertsDir -> set_permission(?CA_CERTS_DIR, ?U_RWX_G_RX_O_X);
        CertsDir -> set_permission(?CERTS_DIR, ?U_RWX_G_RX_O_X);
        PrivateDir -> set_permission(?PRIVATE_DIR, ?U_RWX);
        _ -> ok
    end.

set_permission(Path, Mode) ->
    ?LOG("Setting permissions on [~p] [0~.8B]~n", [Path, Mode]),
    ok = file:change_mode(Path, Mode).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-spec initialize(#state{}) -> #state{}.
initialize(#state{oam_namespace = undefined} = State) ->
    %% if NS is undefined, no initialization
    State;
initialize(State) ->
    %% Don't stop/start snmpd in init, just set DTLS conf
    mnesia:subscribe({table, snmp, detailed}),
    mnesia:subscribe({table, snmpTargetV3Dtls, detailed}),
    
    %% Clear DTLS config
    delete_dtls_config(),
    
    %% Initial subscribe
    SnmpInfo = get_snmp_info(),
    {NodeCredential, TrustCategory} = get_nc_tc_mo_ref(SnmpInfo),
    subscribe_mo(NodeCredential, TrustCategory),

    %% Check if ok to start
    IsEnabled = is_snmp_dtls_enabled(SnmpInfo),
    config_dtls_state_machine({false, IsEnabled}, {SnmpInfo, SnmpInfo}), %% OldSnmp doesn't matter in this case
    
    State#state{is_dtls_enabled = IsEnabled}.

get_snmp_info() ->
    [SnmpInfo] = mnesia:dirty_read(snmp, {"1", "1", "1", "1"}),
    SnmpInfo.

get_nc_tc_mo_ref(SnmpInfo) ->
    {SnmpInfo#snmp.nodeCredential, SnmpInfo#snmp.trustCategory}.

%% reconfigure_snmp/1
%% ====================================================================
%% @doc Reconfigure snmp in case of NC or TC MO change

-spec reconfigure_snmp(#state{}) -> #state{}.
reconfigure_snmp(#state{is_dtls_enabled = WasEnabled} = State) ->
    SnmpInfo = get_snmp_info(),
    IsEnabled = is_snmp_dtls_enabled(SnmpInfo),
    DtlsReconfigured = config_dtls_state_machine({WasEnabled, IsEnabled}, {SnmpInfo, SnmpInfo}),
    sysInitI:info_msg("Handling SNMP MO change!~n"),
    handle_snmp_change(WasEnabled, IsEnabled),
    restart_snmpd(is_snmp_unlocked(SnmpInfo) andalso DtlsReconfigured),
    State#state{is_dtls_enabled = IsEnabled}.

%% snmp_conf_and_dir_update/1
%% ====================================================================
%% @doc Reload NodeCredential nad TrustCategory and store

-spec snmp_conf_and_dir_update(#snmp{}) -> ok.
snmp_conf_and_dir_update(#snmp{} = SnmpInfo) ->
    {NodeCredential, TrustCategory} = get_nc_tc_mo_ref(SnmpInfo),
    snmp_conf_and_dir_update(NodeCredential, TrustCategory).

snmp_conf_and_dir_update(NodeCredential, TrustCategory) ->
    store_nc_cert_info(NodeCredential),
    store_tc_cert_info(TrustCategory).

%% snmp_conf_and_dir_remove/0
%% ====================================================================
%% @doc Remove NodeCredential and TrustCategory from dir and config

snmp_conf_and_dir_remove() ->
    remove_nc_cert_info(),
    remove_tc_cert_info().

%% resubscribe_mo/2
%% ====================================================================
%% @doc Re-subscribe to MO reference

-spec resubscribe_mo(term(), term()) -> ok.
resubscribe_mo(_Old, _Old) ->
    ok;
resubscribe_mo(#snmp{nodeCredential = OldNC, trustCategory = OldTcat},
               #snmp{nodeCredential = NewNC, trustCategory = NewTcat}) ->
    resubscribe_mo(OldNC, NewNC),
    resubscribe_mo(OldTcat, NewTcat),
    ok;
resubscribe_mo(OldMoRef, NewMoRef) ->
    unsubscribe_mo(OldMoRef),
    subscribe_mo(NewMoRef),
    ok.


%% subscribe_mo/1
%% ====================================================================
%% @doc Subscribe to MO reference

-spec subscribe_mo(term()) -> atom().
subscribe_mo(undefined) ->
    ok;
subscribe_mo(MoRef) ->
    ?LOG("Subscribe to [~p]~n", [MoRef]),
    certI:subscribe(MoRef, ?MODULE).

%% subscribe_mo/2
%% ====================================================================
%% @doc Subscribe to MO references

-spec subscribe_mo(NodeCredential :: term(),
                   TrustCategory :: term()) -> atom().
subscribe_mo(NodeCredential, TrustCategory) ->
    subscribe_mo(NodeCredential),
    subscribe_mo(TrustCategory).

%% unsubscribe_mo/1
%% ====================================================================
%% @doc Unsubscribe from MO reference

-spec unsubscribe_mo(term()) -> atom().
unsubscribe_mo(undefined) ->
    ok;
unsubscribe_mo(MoRef) ->
    ?LOG("Unsubscribe from [~p]~n", [MoRef]),
    certI:unsubscribe(MoRef, ?MODULE).

%% store_tc_cert_info/1
%% ====================================================================
%% @doc Store TC in dir and fingerprints in snmpd.conf

store_tc_cert_info(TrustCategory) when TrustCategory =/= undefined ->
    case get_tcat_and_crlcheck(TrustCategory) of
        {ok, TrustedCertificates} ->
            FingerptintsTC = generate_tc_fingerprints(TrustedCertificates),
            config_set_tc(FingerptintsTC),
            write_tc_tls_dir(TrustedCertificates);
        {error, Reason} ->
            sysInitI:warning_msg("~p: Can't get TrustedCertificates from [~p], reason: [~p]~n", 
                                 [?MODULE, TrustCategory, Reason]),
            {error, Reason}
    end.

%% remove_tc_cert_info/0
%% ====================================================================
%% @doc Remove TC from dir and clear fingerprint in snmpd.conf

remove_tc_cert_info() ->
    clear_tc_dir(),
    config_delete_tc().

%% store_cipher_filter/0
%% ====================================================================
%% @doc Store cipher filter to snmpd.conf

store_cipher_filter() ->
    CipherFilter = rcsSecM:get_tls_cipher_filter(),
    config_set_cipher_filter(CipherFilter),
    ok.

%% store_agent_address_dtls_info/1
%% ====================================================================
%% @doc Store new agent address dtls info to snmpd.conf

store_agent_address_dtls_info(AgentAddressDtlsRecords) ->
    AgentAddressDtlsValues = agent_address_dtls_to_list(AgentAddressDtlsRecords),
    config_set_agent_address_dtls(AgentAddressDtlsValues),
    ok.

%% store_dtls_user_config/0
%% ====================================================================
%% @doc Store dtls user info to snmpd.conf

store_dtls_user_config() ->
    Fun = 
    fun() ->
        KeyList = mnesia:all_keys(snmpTargetV3Dtls),
        lists:map(fun(Key) ->
                    [SnmptargetV3DtlsInfo] = mnesia:read(snmpTargetV3Dtls, Key, read),
                    SnmptargetV3DtlsInfo
                  end, KeyList)
    end,
    
    case mnesia:transaction(Fun) of
    {atomic, V3DtlsTargets} ->
        lists:foreach(fun(V3Target) -> config_set_dtls_user(V3Target) end, V3DtlsTargets);
    Nok->
        sysInitI:warning_msg("~p: Mnesia transaction from [~p], not ok, reason: [~p]~n", 
                                 [?MODULE, Nok])
    end.

%% store_dtls_user_config/1
%% ====================================================================
%% @doc Store dtls user info to snmpd.conf
store_dtls_user_config(NewSnmpTargetV3Dtls) ->
    Fun = 
    fun() ->
        KeyList = mnesia:all_keys(snmpTargetV3Dtls),
        lists:map(fun(Key) ->
                    [SnmptargetV3DtlsInfo] = mnesia:read(snmpTargetV3Dtls, Key, read),
                    SnmptargetV3DtlsInfo
                  end, KeyList)
    end,
    
    case mnesia:transaction(Fun) of
    {atomic, V3DtlsTargets} ->
        %%Replacing mnesia:read info of new/changed target in case mnesia:read gets old value
        AllV3DtlsTargets = lists:keyreplace(element(2, NewSnmpTargetV3Dtls), 2, V3DtlsTargets, NewSnmpTargetV3Dtls),
        sysInitI:info_msg("AllV3DtlsTargets: ~p~n", [AllV3DtlsTargets]),
        lists:foreach(fun(V3Target) -> config_set_dtls_user(V3Target) end, AllV3DtlsTargets);
    Nok->
        sysInitI:warning_msg("~p: Mnesia transaction from [~p], not ok, reason: [~p]~n", 
                                 [?MODULE, Nok])
    end.

%% get_tcat_and_crlcheck/1
%% ====================================================================
%% @doc Get trusted certificates with TrustCategory

get_tcat_and_crlcheck(TrustCategory) ->
    case certI:get_tcat_and_crlcheck(TrustCategory) of
        {ok, TrustedCertificates, _Number} ->
            {ok, TrustedCertificates};
        {error, Reason} ->
            {error, Reason}
    end.

%% store_nc_cert_info/1
%% ====================================================================
%% @doc Store NC in dir and fingerprints in snmpd.conf

store_nc_cert_info(NodeCredential) when NodeCredential =/= undefined->
    {ok, NodeCert, PrivKey} = get_cert(NodeCredential),
    FingerprintNC = generate_nc_fingerprint(NodeCredential),
    config_set_nc(FingerprintNC),
    write_nc_tls_dir(NodeCert, PrivKey).

%% remove_nc_cert_info/0
%% ====================================================================
%% @doc Remove NC from dir and clear fingerprint in snmpd.conf

remove_nc_cert_info() ->
    clear_nc_dir(),
    config_delete_nc().

%% get_cert/1
%% ====================================================================
%% @doc Get node certificate with NodeCredential

get_cert(NodeCredential) ->
    case certI:get_cert(NodeCredential, pem) of
        {ok, NodeCert, PrivKey} ->
            {ok, NodeCert, PrivKey};
        _ ->
            undefined
    end.

%% generate_tc_fingerprints/1
%% ====================================================================
%% @doc Generate list of fingerprints from list of Trusted Certificates

generate_tc_fingerprints(TrustedCertificates) ->
    generate_tc_fingerprints(TrustedCertificates, []).

generate_tc_fingerprints([], Acc) ->
    Acc;
generate_tc_fingerprints([ H | T], Acc) ->
    generate_tc_fingerprints(T, [get_fingerprint(H) | Acc]).

generate_nc_fingerprint(NodeCredential) ->
    %% net-snmp requires FP of der format
    {ok, [Certificate | _], _} = certI:get_cert(NodeCredential, der),
    get_fingerprint(Certificate).

get_fingerprint(Certificate) ->
    certI:generate_fingerprint(Certificate).

%% agent_address_dtls_to_list/1
%% ====================================================================
%% @doc Generate list of values from records for snmpd.conf

agent_address_dtls_to_list(undefined) ->
    [];
agent_address_dtls_to_list([AgentAddressDtls | _] = AgentAddressDtlsList) when is_record(AgentAddressDtls, 'HostAndPort') ->
    lists:map(
        fun(AgentAddressDtlsRecord) ->
            agent_address_dtls_record_to_value(AgentAddressDtlsRecord)
        end, AgentAddressDtlsList);
agent_address_dtls_to_list(AgentAddressDtlsList) ->
    AgentAddressDtlsList.

%% handle_change/2
%% ====================================================================
%% @doc Returns true if Dtls parameter changed, false otherwise

-spec handle_change(OldSnmp :: #snmp{}, NewSnmp :: #snmp{}) -> 
    true | false.
handle_change(#snmp{trustCategory = OldTC, nodeCredential = OldNC, agentAddressDtls = OldAAD},
              #snmp{trustCategory = NewTC, nodeCredential = NewNC, agentAddressDtls = NewAAD}) ->
    NcTcChanged = handle_change(nctc, {OldTC, OldNC}, {NewTC, NewNC}),
    AADChanged = handle_change(agentAddressDtls, OldAAD, NewAAD),
    NcTcChanged orelse AADChanged.

%% handle_change/3
%% ====================================================================
%% @doc Returns true if Dtls parameter changed, false otherwise

-spec handle_change(Attr :: atom(), OldValue :: term(), NewValue :: term()) -> 
    true | false.
handle_change(_, _OldValue, _OldValue) ->
    false;
handle_change(agentAddressDtls, _OldAAD, NewAAD) ->
    sysInitI:info_msg("~p: Snmp MO changed:~nagentAddressDtls: [~p]~n", [?MODULE,  NewAAD]),
    config_delete_agent_address_dtls(),
    store_agent_address_dtls_info(NewAAD),
    true;

handle_change(nctc, {OldTC, OldNC}, {NewTC, NewNC}) ->
    sysInitI:info_msg("~p: Snmp MO changed:~nnodeCredential: [~p]~ntrustCategory: [~p]~n", [?MODULE, NewNC, NewTC]),
    resubscribe_mo(OldTC, NewTC),
    resubscribe_mo(OldNC, NewNC),
    snmp_conf_and_dir_remove(),
    snmp_conf_and_dir_update(NewNC, NewTC),
    subscribe_mo(NewNC, NewTC),
    true.

handle_change_snmptargetV3Dtls() ->
    config_delete_dtls_user(),
    store_dtls_user_config().

handle_change_snmptargetV3Dtls(NewSnmpTargetV3Dtls) ->
    config_delete_dtls_user(),
    store_dtls_user_config(NewSnmpTargetV3Dtls).

agent_address_dtls_record_to_value(AgentAddressDtls) ->
    #'HostAndPort'{host = Host, port = Port} = AgentAddressDtls,
    StringPort = integer_to_list(Port),
    Host++":"++StringPort.

-spec is_snmp_unlocked(#snmp{}) -> true | false.
is_snmp_unlocked(#snmp{administrativeState = ?BasicAdmState_UNLOCKED}) ->
    true;
is_snmp_unlocked(#snmp{administrativeState = ?BasicAdmState_LOCKED}) ->
    false.

-spec is_agent_address_dtls_defined(#snmp{}) -> true | false.
is_agent_address_dtls_defined(#snmp{agentAddressDtls = undefined}) ->
    false;
is_agent_address_dtls_defined(#snmp{agentAddressDtls = _DtlsAddress}) ->
    true.

-spec is_node_credential_defined(#snmp{}) -> true | false.
is_node_credential_defined(#snmp{nodeCredential = undefined}) ->
    false;
is_node_credential_defined(#snmp{nodeCredential = NC}) ->
    is_node_credential_installed(get_cert(NC)).

is_node_credential_installed({ok, _, _}) ->
    true;
is_node_credential_installed(undefined) ->
    false.
-spec is_trust_category_defined(#snmp{}) -> true | false.
is_trust_category_defined(#snmp{trustCategory = undefined}) ->
    false;
is_trust_category_defined(#snmp{trustCategory = Tcat}) ->
    is_trust_category_enabled(get_tcat_and_crlcheck(Tcat)).

is_trust_category_enabled({error, _}) ->
    false;
is_trust_category_enabled({ok, []}) ->
    false;
is_trust_category_enabled({ok, _}) ->
    true.

-spec is_snmp_dtls_enabled() -> true | false.
is_snmp_dtls_enabled() ->
    is_snmp_dtls_enabled(get_snmp_info()).

is_snmp_dtls_enabled(#snmp{} = SnmpMo) ->
    is_agent_address_dtls_defined(SnmpMo) andalso
    is_node_credential_defined(SnmpMo) andalso
    is_trust_category_defined(SnmpMo).

restart_snmpd(false) ->
    ok;
restart_snmpd(true) ->
    config_handler_restart().

delete_dtls_config() ->
    snmp_conf_and_dir_remove(),
    config_delete_agent_address_dtls(),
    config_delete_dtls_user(),
    config_delete_cipher_filter().

store_dtls_config(#snmp{trustCategory = Tcat, nodeCredential = NC, agentAddressDtls = AAD}) ->
    snmp_conf_and_dir_update(NC, Tcat),
    store_agent_address_dtls_info(AAD),
    store_dtls_user_config(),
    store_cipher_filter().

%% config_dtls_state_machine/2
%% ====================================================================
%% @doc State machine for writing/removing dtls configuration.
%%      Output specifies if snmpd needs to be restarted

-spec config_dtls_state_machine(OldNewEnabled :: {boolean(), boolean()},
                                OldNewSnmp :: {#snmp{}, #snmp{}}) ->
          false | true.

%% Change occured but dtls still disabled, do nothing
config_dtls_state_machine({false, false}, _) ->
    false;

%% Dtls becomes enabled, write config/dirs
config_dtls_state_machine({false, true}, {_, NewSnmp}) ->
    store_dtls_config(NewSnmp),
    true;

%% Dtls becomes disabled, remove from config/dirs
config_dtls_state_machine({true, false}, _) ->
    delete_dtls_config(),
    true;

%% Change occured, dtls still enabled, Snmp MO hasn't changed -> cert_event
config_dtls_state_machine({true, true}, {OldSnmp, OldSnmp}) ->
    snmp_conf_and_dir_remove(),
    snmp_conf_and_dir_update(OldSnmp),
    true;

%% Change occured, dtls still enabled, handle change
config_dtls_state_machine({true, true}, {OldSnmp, NewSnmp}) ->
    handle_change(OldSnmp, NewSnmp).

%% handle_snmp_change/2
%% ====================================================================
%% @doc Handling operationalState in case of change of Snmp MO

handle_snmp_change(true, false) ->
    set_operational_state_all_targets(?OperState_DISABLED);
handle_snmp_change(false, true) ->
    set_operational_state_all_targets(?OperState_ENABLED);
handle_snmp_change(_Enabled, _Enabled) ->
    ok.

%% set_operational_state_all_targets/1
%% ====================================================================
%% @doc Gets all snmpTargetV3Dtls MOs and sets oprationalState to given State

set_operational_state_all_targets(?OperState_DISABLED) ->
    AFun = fun() ->
                    TargetKeys = mnesia:all_keys(snmpTargetV3Dtls),
                    TargetsList = lists:map(fun(Key) ->
                                                    [Target] = mnesia:read(snmpTargetV3Dtls, Key, read),
                                                    Target
                                            end, TargetKeys),
                    lists:foreach(fun(Target) ->
                                          NewTarget = Target#snmpTargetV3Dtls{operationalState = ?OperState_DISABLED},
                                          mnesia:write(snmpTargetV3Dtls, NewTarget, write)
                                  end, TargetsList)
           end,
    mnesia:transaction(AFun);
set_operational_state_all_targets(?OperState_ENABLED) ->
    AFun = fun() ->
                    TargetKeys = mnesia:all_keys(snmpTargetV3Dtls),
                    TargetsList = lists:map(fun(Key) ->
                                                    [Target] = mnesia:read(snmpTargetV3Dtls, Key, read),
                                                    Target
                                            end, TargetKeys),
                    lists:foreach(fun(Target) ->
                                          case Target#snmpTargetV3Dtls.administrativeState of
                                              ?BasicAdmState_UNLOCKED ->
                                                  NewTarget = Target#snmpTargetV3Dtls{operationalState = ?OperState_ENABLED},
                                                  mnesia:write(snmpTargetV3Dtls, NewTarget, write);
                                              ?BasicAdmState_LOCKED ->
                                                  ok
                                          end
                                  end, TargetsList)
           end,
    mnesia:transaction(AFun).

%% handle_target_change/2
%% ====================================================================
%% @doc Handling operationalState in case of change of SnmpTargetV3Dtls MO

handle_target_change(undefined, Target = #snmpTargetV3Dtls{administrativeState = ?BasicAdmState_UNLOCKED}, true) ->
    set_operational_state_single_target(Target, ?OperState_ENABLED);
handle_target_change(undefined, Target = #snmpTargetV3Dtls{}, _) ->
    set_operational_state_single_target(Target, ?OperState_DISABLED);
handle_target_change(#snmpTargetV3Dtls{administrativeState = ?BasicAdmState_LOCKED}, Target = #snmpTargetV3Dtls{administrativeState = ?BasicAdmState_UNLOCKED}, true) ->
    set_operational_state_single_target(Target, ?OperState_ENABLED);
handle_target_change(#snmpTargetV3Dtls{administrativeState = ?BasicAdmState_LOCKED}, Target = #snmpTargetV3Dtls{administrativeState = ?BasicAdmState_UNLOCKED}, false) ->
    set_operational_state_single_target(Target, ?OperState_DISABLED);
handle_target_change(#snmpTargetV3Dtls{administrativeState = ?BasicAdmState_UNLOCKED}, Target = #snmpTargetV3Dtls{administrativeState = ?BasicAdmState_LOCKED}, _) ->
    set_operational_state_single_target(Target, ?OperState_DISABLED);
handle_target_change(_, _, _) ->
    ok.

set_operational_state_single_target(Target, State) ->
    NewTarget = Target#snmpTargetV3Dtls{operationalState = State},
    AFun = fun() ->
                   mnesia:write(snmpTargetV3Dtls, NewTarget, write)
           end,
    mnesia:transaction(AFun).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

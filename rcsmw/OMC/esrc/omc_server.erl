%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_server.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/1

%%% @doc ==Secure shell server==
%%% Start the erlang ssh-server to listen on ports for netconf, cli, and coli.
%%% Fetch and cache the roles used for authorization.

-module(omc_server).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/1').
-date('2017-11-28').

-include("omc.hrl").
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev        Date        Name         What
%%% -----      -------     --------	------------------------
%%% R2A/1      2014-01-17  etxlg	Replaces sysSshCli R2A/32
%%% R2A/2      2014-01-20  etxlg	Tried to silence dialyzer
%%% R2A/3      2014-01-27  etxlg	Disable netconf shell
%%%					prepare for config from OOT/CERTM
%%% R2A/4      2014-01-30  etxlg	fold -> foldl
%%% R2A/6      2014-01-30  etxlg	one more catch for gen_server:call
%%% R2A/7      2014-02-19  etxlg	is_activity_since_restart/0
%%% R2A/8      2014-03-11  etxlg	use the new ldap stuff
%%% R2A/9      2014-03-12  etxlg	wider catch for errors during shutdown
%%% R2A/10     2014-03-14  etxlg	soft stop when port changes
%%% R2A/11     2014-03-20  etxlg	return correct for is_activity...
%%% R2A/12     2014-03-25  etxlg	easy port change for netconf
%%%					api to enable lookup outside server
%%% R2A/13     2014-03-25  etxlg	part revert to enable sim w/o pw
%%% R2A/14     2014-03-25  etxlg	removed deadlock GNIIII!!
%%% R2A/15     2014-03-25  etxlg	fix for sim.....
%%% R2A/16     2014-04-09  etxlg	No keygen for TLM
%%% R2A/17     2014-04-10  etxlg	dbg function
%%% R2A/18     2014-04-14  etxlg	Handle bad return from
%%%					comsaI:get_all_security_roles()
%%% R2A/19     2014-06-16  etxlg	table of sessions
%%% R2A/20     2014-06-17  etxlg	logout_non_support_users().
%%%					is_only_support_users().
%%% R2A/21     2014-06-24  etxlg	change_notify(), respect adminstate
%%%				adminstate is only for netconf, cli, and coli
%%% R2A/22     2014-07-07  etxlg	log when ssh login failes
%%% R2A/23     2014-07-08  etxlg	backed out {parallel_login, true}
%%% R2A/24     2014-07-09  etxlg	backed out {max_sessions, 5}
%%% R2A/25     2014-08-13  eolaand	back in {max_sessions, 5}
%%% R2A/26     2014-09-02  etxlg	DSCP
%%% R2A/27     2014-09-10  etxlg	no crash when DSCP changes, however,
%%%					the change will need restart to become
%%%					active on the cli/netconf services.
%%% R2A/28     2014-10-02  etxlg	no restart needed for DSCP, enabled
%%%					parallel_login
%%% R2A/29     2014-10-13  etxlg	number of sessions increased to 20
%%%					ensure 'deprecated' is started
%%% R2A/30     2014-10-14  etxlg	ensure yesterdays fix doesn't break sim
%%% R2A/31     2014-10-16  etxlg	deprecated support REMOVED
%%% ----    ---------- -------  ------------------------------------------------
%%% R3A/1      2014-11-11  etxtory      Exported generate_host_keys (run in post_init)
%%% R3A/2      2014-12-04  etxlg        Network namespaces - here we go
%%% R3A/3      2015-01-14  etxpeno      Support for roles
%%% R3A/4      2015-01-20  erarafo      Separation of PMS and PES SFTP services
%%% R3A/5      2015-01-21  etxlg        Disconnect sessions when shutting down
%%% R3A/6      2015-01-27  etxlg        Prettified printout
%%% R3A/7      2015-01-28  etxlg        Safer waiting for startup_complete
%%% R3A/10     2015-03-17  etxlg        Work even if there is no dhcp (duh!)
%%% R3A/11     2015-04-10  etxlg        TR HT47659 (possibly only partly)
%%% R3A/12     2015-04-15  etxlg        TR HT64601 bind services to OaM
%%% R3A/13     2015-04-16  etxlg        Possible to (again) set OamAP in sim
%%% R3A/14     2015-04-17  etxlg        Ensure TN is ready before using the OaMAP
%%% R3A/15     2015-04-17  etxtory      HT66399; disable ssh banner grabbing
%%% ----    ---------- -------  ------------------------------------------------
%%% R4A/2      2015-08-25  etxlg        Always bind if OaMAP set (TR HU10106),
%%%					retry daemon-start.
%%% R4A/3      2015-08-27  etxlg        W/a oot slooow start after upgrade
%%% R4A/4      2015-08-27  etxlg        fort och fel
%%% R4A/5      2015-08-31  etxlg        Alternate OaM connection
%%% R4A/6      2015-09-09  etxtory      Increased timer for is_only_support_users
%%% R4A/8      2015-09-16  etxlg        Run on LCT even if OOT is awaiting TN
%%% R4A/9      2015-09-17  etxlg        Crash on OOT:lmt_ipv4 during upgrade
%%%% Commented out%%% R4A/10     2015-09-30  etxlg        OTP-18, fix TR HT82344
%%% R4A/10    2015-10-07  etxlg         Explictly close cons at warm restart
%%% R4A/11    2015-10-08  etxlg         Simplify filtering errors in dialyzer
%%% R4A/12    2015-10-14  etxlg         Fix TR HU26518
%%% R4A/13    2015-10-19  etxlg         Limit number of channels
%%%					TR HU25282
%%%					Alternate OaM revisited (new OOT)
%%%					Support same IP in multiple namespace
%%% R4A/14    2015-10-22  etxlg         max_channels=2 or no netconf on openssh
%%% R4A/15    2015-10-27  etxlg         TR HU29448
%%% R4A/16    2015-11-03  etxlg         IPv6
%%% R4A/17    2015-11-11  etxlg         Corrected name on alternative Oam DN
%%%					removed weak ssh algo
%%% R4A/18    2015-11-27  etxlg         One more IPv4 -> IPv[46]
%%% R4A/19    2015-11-30  etxlg         Weak ssh algo back for testframeworks
%%% R4A/20    2016-03-14  etxpeno       IPv6, wait for address to become 
%%%                                     available
%%% R4A/22    2016-04-01  eolaand       HU70271. IPv4, same as for IPv6 above
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-07 etxberb  Split generate generate_host_keys/0 into
%%%                             begin_generate_host_keys/0,
%%%                             end_generate_host_keys/0.
%%% R5A/2   2016-02-02 etxlg    Debug print when going into retry
%%% R5A/3   2016-02-12 etxlg    Removed weak ssh algo, GTE now signals ready
%%% R5A/4   2016-02-16 etxtory  Added weak ssh algo again; GSM securebridge ssh-client
%%%                             can only manage this algo
%%% R5A/5   2016-02-23 etxtory  Removed weak ssh algo, GSM is now ready
%%% R5A/6   2016-03-07 etxlg    IPv6, wait for address to become available
%%% R5A/7   2016-03-07 etxtory  Added weak ssh algo again; OSS-RC can only this algo
%%%                             Do not try again!!!
%%% R5A/8   2016-03-07 etxtory  Added weak ssh algo again; OSS-RC can only this algo
%%%                             Based on R5A/5 (changes in R5A/6 and R5A/7 is removed)
%%% R5A/9   2016-03-07 etxtory  Restored back R5A/7 (R5A/8 was special build for 16B 4.1 EP1)
%%% R5A/10  2016-04-01 eolaand  Merge HU70271 from R4
%%% ----    ---------- -------  ------------------------------------------------
%%% R6A/1   2016-08-30 emariad  CSUC feature, cipher configuration
%%% R6A/2   2016-08-30 uabesvi  vRC should be treated as target and not simulated
%%% R6A/3   2016-09-14 ehsake   Enable expert user for vRCS until AI is in place,
%%%                             SSH server startup fix.
%%% ----    ---------- -------  ------------------------------------------------
%%% R7A/1   2016-10-18 ehsake   WP6081, Generate ESI at rollback
%%% ----    ---------- -------  ------------------------------------------------
%%% R8A/3   2016-11-04 ehsake   Prevent exceptions being thrown from SSH password fun
%%% R8A/3   2016-11-04 emariad  Fixed a hole when hardcode expert user for vrcs and sim.
%%% ----    ---------- -------  ------------------------------------------------
%%% R9A/3   2017-02-22 etxtory  Listen to any for vrcs on no namespace (tenant)
%%% R9A/4   2017-04-07 etxarnu  Added more users/roles for sim/vrcs
%%% ----    ---------- -------  ------------------------------------------------
%%% R10A/1  2017-05-22 eivomat  HV86100 (Unique tables for each ssh daemon)
%%% ----    ---------- -------  ------------------------------------------------
%%% R11A/1  2017-11-27 etxtory  Added modify_algoritms (to enable diffie-hellman, again)
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%API functions
-export([authenticate_cli_user/2, authenticate_nc_user/2,
	 authenticate_coli_user/2, authenticate_sftp_user/2, 
	 authorize_user/1, store_authorization/2]).
-export([autointegration_fun/1]).
-export([config_update/1, change_notify/0, ssh_config_notify/0]).
-export([begin_generate_host_keys/0]).
-export([init_generate_host_keys/2]).
-export([end_generate_host_keys/0]).
-export([is_activity_since_restart/0]).
-export([store_session/4, store_session/5, store_session/6]).
-export([logout_non_support_users/0]).
-export([is_only_support_users/0]).
-export([restart_complete/0]).
-export([prepare_to_shutdown/0]).

%callback from APPM
-export([prep_warm/0]).

%%debug functions
-export([stop/0]).
-export([set_roles/2]).
-export([erase_roles/1]).
-export([get_daemon_refs/0]).
-export([get_sessions/0]).

%%uselful in testing to workaround ssh-bug that cause ssh to leak proceses
%%retarting will restore status-quo (maybe, perhaps need ssh:{start/stop} too).
%%stubbed now, but not removed incase Ivan uses it
-export([restart_ssh/0]).
%%middleware helper functions
-export([start_link/0, start_link/1]).

%for use when running through RCS-COLI
-export([ecoli_session_info/1]).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2]).


-define(SERVER, ?MODULE).
-define(EMERG_PORT, 22).   %placeholder - this is handled by the OS:sshd
-define(CLI_CHANNEL, omc_ssh_channel).
-define(NC_CHANNEL, omc_ssh_channel).
-define(COLI_CHANNEL, ecoli_ssh_channel).
-define(BLOCKPREFIX, "omc_").
-define(REINITTIMEOUT, 1000 * 60 * 2). %2 minutes
-define(SHORT_REINITTIMEOUT, 2000). %1s

-define(ALTALARM, 'AlternateOAMConnectionInUse').
-define(OOT_ALT_DN, [<<"ManagedElement=1">>, <<"SystemFunctions=1">>,
		     <<"SysM=1">>, <<"OamAccessPoint=Alternative">>]).

-define(genHostKeys_proc, omc_server_genHostKeys).

-define(MonoTime, erlang:monotonic_time()).

-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).

-record(st,
    {	started = false,	%becomes true after restart_complete
	nconf_admin,		%true|false  from sysM administrativeState
	cli_admin,		%true|false  from sysM administrativeState
	system_dir,		%where keys are located
	ets_az_id,              %identifier to ETS holding authorization
	ets_session_id,         %identifier to ETS holding session pids
	ssh_servers = [],       % list of servers
%ex. {[{{cli, oam}, Ssh_ref, Oap_ip_tuple, Cli_port,  Oam_opts_list},
        auto_fun,		%set by sysNetloader
        activity = false,	%changed to true as soon as any login succeeds
	is_alarm = false,	%status of 'AlternateOAMConnectionInUse'
	reinit_tmr}).		%if a daemon failed to start - retry

-record(session,
    {	pid,		%pid being monitored
	mref,		%monitor reference
	user,		%string() :: User
	type,		%cli | netconf | coli
	interface,	%oam | alt | lmt_* | any
	transport,	%ssh | tls
	is_super_oam,	%true | false
	roles,		%[Role1, Role2...]
	login_time,	%os:timestamp()
	ssh_ref}).	%the ssh connection, used during warm restart

%%-compile(export_all).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%API functions
start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%called from OOT through fun set in omc_api
%%dscp, oap_ip, oap_net_ns, nc_port, cli_port)
config_update(Proplist) ->
    gen_server:cast(?SERVER, {config_update, Proplist}).

%%called from COMSA through omc_api, means adminstate may have changed
change_notify() ->
    gen_server:cast(?SERVER, change_notify).

%%called through the omc_api whenever the ssh configuration changes
ssh_config_notify() ->
    gen_server:cast(?SERVER, ssh_config_notify).

prep_warm() ->
    gen_server:cast(?SERVER, prep_warm).

%%called by omc_datainit:post_init
begin_generate_host_keys() ->
    erlang:spawn(?MODULE,
		 init_generate_host_keys,
		 [get_ssh_key_dir(), self()]),
    receive
	{?MODULE, init_generate_host_keys_done} ->
	    ok
    end.

%%called by the ssh-daemon: true->allowed, false->denied
authenticate_nc_user(User, Pw) ->
    authenticate_user(User, Pw).

authenticate_coli_user(User, Pw) ->
    authenticate_user(User, Pw).

authenticate_sftp_user(User, Pw) ->
    case authenticate_user(User, Pw) of
        true ->
            Roles = authorize_user(User),
            sysSshSftpd:authorize_sftp_user(Roles);
        False ->
            False
    end.

authenticate_cli_user(User, Pw) ->
    authenticate_user(User, Pw).

%% This is were calls from COM to get roles should end up.
%% for now this works with strings
%% ex. assuming user admin is logged on
%% authorize_user("admin") -> ["write", "read", "whatever"]
authorize_user(User)->
    %% see comment from jotj above
    try gen_server:call(?SERVER, {authorize_user, User})
    catch
	 _:_ ->
	    []
    end.

store_authorization(User, Roles) ->
    gen_server:call(?SERVER, {store_authorization, User, Roles}).

autointegration_fun(Fun) when is_function(Fun); Fun =:= undefined ->
    gen_server:cast(?SERVER, {autointegration_fun, Fun}).

is_activity_since_restart() ->
    gen_server:call(?SERVER, is_activity_since_restart).

store_session(User, Type, Transport, Roles) ->
    store_session(User, Type, unknown, Transport, Roles, undefined).
store_session(User, Type, Interface, Transport, Roles) ->
    store_session(User, Type, Interface, Transport, Roles, undefined).
store_session(User, Type, Interface, Transport, Roles, Ssh_connection) ->
    Session = #session{	pid = self(),
			user = User,
			type = Type,
			interface = Interface,
			transport = Transport,
			is_super_oam = is_super_oam(Roles),
			roles = Roles,
			login_time = os:timestamp(),
			ssh_ref = Ssh_connection},
    gen_server:cast(?SERVER, {store_session, Session}).

logout_non_support_users() ->
    gen_server:call(?SERVER, logout_non_support_users).

is_only_support_users() ->
    %% This function is called from comsaUser (via omc_api).
    %% If omc_server is reconfiguration at the same time as 
    %% this function is called it might lead to a timeout.
    %% The timeout is tmp increased but this function should
    %% solve this in an other way (ets-table?).
    gen_server:call(?SERVER, is_only_support_users, 20000).

restart_complete() ->
    gen_server:cast(?SERVER, restart_complete).

prepare_to_shutdown() ->
    gen_server:cast(?SERVER, prepare_to_shutdown).

stop() ->
    gen_server:call(?SERVER, stop).

%%% For testing purposes
set_roles(User, Roles) ->
    gen_server:call(?SERVER, {set_roles, User, Roles}).
erase_roles(User) ->
    gen_server:call(?SERVER, {erase_roles, User}).

restart_ssh() ->
    io:format("~p: restart_ssh(), NOT implemented, do not use.~n", [?MODULE]).
get_daemon_refs() ->
    gen_server:call(?SERVER, get_daemon_refs).
get_sessions() ->
    Sessions = gen_server:call(?SERVER, get_sessions),
    %HERE format a bit
    Sessions.

ecoli_session_info(_Args) ->
    Sessions = omc_lib:get_session_info(),
    io:format("~10s~10s~20s~50s~n", ["Type", "Subtype", "User", "IP address"]),
    io:format("~90.0.=s~n", ["="]),
    F =
    fun({_, _,Type, Subtype, User, IpAddr}) ->
        io:format("~10s~10s~20s~50s~n", [Type, Subtype, User, IpAddr])
    end,
    lists:foreach(F, Sessions),
    io:format("~90.0.=s~n", ["="]),

    SshCli = omc_lib:get_session_count(ssh_cli),
    SshColi = omc_lib:get_session_count(ssh_coli),
    SshNetconf = omc_lib:get_session_count(ssh_netconf),
    SshFtp = omc_lib:get_session_count(ssh_sftp),

    TlsCli = omc_lib:get_session_count(tls_cli),
    TlsColi = omc_lib:get_session_count(tls_coli),
    TlsNetconf = omc_lib:get_session_count(tls_netconf),
    TlsFtp = omc_lib:get_session_count(tls_ftp),

    io:format("Total SSH sessions: ~2.B~n", [SshCli + SshColi + SshNetconf]),
    io:format("SSH cli:            ~2.B~n", [SshCli]),
    io:format("SSH coli:           ~2.B~n", [SshColi]),
    io:format("SSH netconf:        ~2.B~n~n", [SshNetconf]),
    io:format("SFTP sessions:      ~2.B~n~n", [SshFtp]),
    io:format("Total TLS sessions: ~2.B~n", [TlsCli + TlsColi + TlsNetconf + TlsFtp]),
    io:format("TLS cli:            ~2.B~n", [TlsCli]),
    io:format("TLS coli:           ~2.B~n", [TlsColi]),
    io:format("TLS netconf:        ~2.B~n", [TlsNetconf]),
    io:format("TLS FTP:            ~2.B~n", [TlsFtp]).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%gen_server callbacks

%%    process_flag(trap_exit, true),
%%    process_flag(sensitive, true),
init(_) ->
    case sysEnv:role() of
        active ->
	    %% Use fake user for VRCS until AI is in place
            %% profile is read in omc_ldap_server by calling
            %% omc_ldap_server localy instead of via gen_server call
	    case os:getenv("USE_LDAP") =:= false of
	        true ->
		    Mode = sysEnv:rcs_mode_2(),
		    case Mode =:= vrcs orelse Mode =:= simulated of			 
		    true ->
		        put({roles,"expert"},["expert","coli"]),
		        put({roles,"sysadm"},["SystemAdministrator"]),
		        put({roles,"secadm"},["SystemSecurityAdministrator"]),
		        put({roles,"tnadm"},["Transport_Application_Administrator"]),
		        put({roles,"tnsecadm"},["Transport_Application_SecurityAdministrator"]);
		    _ ->
			ok
		    end;
		_ ->
		    ok
	    end,
	    cont_init(sysInitI:restart_type());
        regular ->
            ignore
    end.

cont_init(Type) ->
    appmI:register_warm_cb(?MODULE), %mnesia write, appm need not be running
    %load State with stuff that are known to be always available
    S = #st{system_dir =  get_ssh_key_dir(),
	    ets_az_id = ets:new(list_to_atom(?BLOCKPREFIX ++ "auth"),
				[set, protected, {keypos, 1}]),
	    ets_session_id =
			ets:new(list_to_atom(?BLOCKPREFIX ++ "sessions"),
				[set, protected, {keypos, 2}])},
    cont_init(Type, S).

cont_init(local, S) ->
    %%start after a local server crash
    %%cease any alarm(s), clean up old daemons by restarting the ssh application
    case comsaI:get_alarms(?ALTALARM) of
	[] ->
	    ok;
	_ ->
	    comsaI:clear_alarm(?ALTALARM, ?OOT_ALT_DN)
    end,
    ssh:stop(), ssh:start(),
    New_s = start_services(S),
    {ok, New_s};

cont_init(vm_restart, S) ->
    % wait for restart_complete before continuing
    {ok, S}.

handle_call({authorize_user, User}, _From, S = #st{ets_az_id = Ets}) ->
    Reply = case ets:lookup(Ets, User) of
        [{User, Roles}] ->
            Roles;
        [] ->
            sysInitI:warning_msg(
                "User: ~p not in cache - returning empty rolelist.~n", [User]),
            []
    end,
    Mod_reply =
	if
	    S#st.auto_fun =:= undefined ->
		Reply;
	    true ->
		try (S#st.auto_fun)([{user, User}, {roles, Reply}]) of
		    Maybe_new_roles when is_list(Maybe_new_roles) ->
		         Maybe_new_roles;
		    _ -> Reply  %when AIC does something funky
		catch
                    _:_ ->
                        Reply
                end
        end,
        {reply, Mod_reply, S#st{activity = true}};
handle_call({authenticate_cli_user, User, Pw}, _From, S) ->
    %running from here we will get the process dictionary enabling
    %the simulator to run without LDAP config
    {Reply, Roles_or_error} = do_authenticate_user(User, Pw),
    if
	Reply =:= true ->
	    store_user_roles(S, User, Roles_or_error);
	true ->
	    ok
    end,
    {reply, Reply, S};
handle_call({store_authorization, User, Roles}, _From, S) ->
    store_user_roles(S, User, Roles),
    {reply, ok, S};
handle_call(stop, _From, S) ->
    %only for test
    {stop, shutdown, stop_ordered, S};
handle_call({set_roles, User, Roles}, _From, S ) ->
    % Used for testing if no access to LDAP
    put({roles,User},Roles),
    {reply, [], S};
handle_call({erase_roles, User}, _From, S ) ->
    % Used for testing  to activate LDAP in simulation
    erase({roles, User}),
    {reply, [], S};
handle_call(is_activity_since_restart, _From, S) ->
    {reply, S#st.activity, S};
handle_call(get_daemon_refs, _From, #st{ssh_servers = Servers} =S) ->
    {reply, Servers, S};
handle_call(get_sessions, _From, #st{ets_session_id = Ets_id} = S) ->
    {reply, ets:tab2list(Ets_id), S};
handle_call(logout_non_support_users, _From,
		#st{ets_session_id = Ets_id} = S) ->
    Message = "Forced logout by maintenanceuser",
    ets:foldl(fun logout_non_support_users/2, Message, Ets_id),
    {reply, ok, S};
handle_call(is_only_support_users, _From,
		#st{ets_session_id = Ets_id} = S) ->
    Match_non_support_users =
	{'_','_','_','_','_','_','_','false','_','_','_'},
    case ets:match(Ets_id, Match_non_support_users) of
	[] -> {reply, true, S};
	_ ->  {reply, false, S}
    end;
handle_call(Req, _From, S) ->
    sysInitI:warning_report([{module, ?MODULE},
            {reason, "illegal gen_server call"},
            {message, Req}]),
    {reply, illegal, S}.

handle_cast(restart_complete, S) ->
     {noreply, start_services(S)};
%    case ensure_oot_ready() of
%	true ->
%	    {noreply, start_services(S)};
%	false ->
%	    {noreply, S}
%    end;
handle_cast(prepare_to_shutdown, #st{ssh_servers = Servers,
				     ets_session_id = Ets_id} = S) ->
    %this will stop all listeners (rcs-coli, cli, netconf, sftp)
    New_s = lists:foldl(
	fun(Serv, Acc) ->
	    stop_ssh_daemon(Acc, Serv)
	end, S, Servers),
    %a chance of a race issue here if someone came in before closing the
    %listener but has not yet been but into ETS...
    %this will logout cli and netconf users (coli and sftp are not in ETS)
    ets:foldl(fun logout_any_user/2, "", Ets_id),
    {noreply, New_s#st{started = false}};

handle_cast({autointegration_fun, Fun}, S) ->
    {noreply, S#st{auto_fun = Fun}};

handle_cast({store_session, Session}, #st{ets_session_id = Ets} = S) ->
    Mref = monitor(process, Session#session.pid),
    true = ets:insert(Ets, Session#session{mref = Mref}),
    New_s = maybe_raise_alarm(S, Session),
    {noreply, New_s};

%change_notify and config_update are not accepted unless started==true
%something changed in COMSA (i.e. admin_state)
handle_cast(change_notify, #st{started = true} = S) ->
    {New_nc_admin, New_cli_admin} = get_admin_state(),
    {noreply, handle_changed_admin_state(S, New_nc_admin, New_cli_admin)};

%ssh_config_notify is only handled if started==true
handle_cast(ssh_config_notify, #st{started = true} = S) ->
    {noreply, handle_ssh_config_changed(S)};

%something changed in OOT (i.e. dscp, oap_ip, oap_net_ns, nc_port, cli_port)
%strategy: disregard the properties in input, read in new ssh_servers
%if any more are to be started - run them
%if less are needed stop those that are no longer needed
%for those that are still running (i.e. not just started) restart them
handle_cast({config_update, Props}, #st{started = true} = S) ->
    info_msg("handle_cast({config_update, ~p}, S)~n", [Props]),
    {noreply, handle_config_update(S)};
handle_cast(prep_warm, #st{started = true, ets_session_id = Ets} = S) ->
    New_set = sets:new(),
    {No_sessions, Cons_set} =
	%extract all SSH login sessions, not ECOLI, with ssh_ref defined
	%run through a sets() to get rid of duplicates
	ets:foldl(
	    fun(#session{transport = Transport,
			 type = Type,
			 ssh_ref = Ssh_ref}, {N, Acc})
		when Transport =:= ssh, Type =/= coli, Ssh_ref =/= undefined ->
		{N + 1, sets:add_element(Ssh_ref, Acc)};
	       (_, Acc) ->
		Acc
	    end, {0, New_set}, Ets),
    info_msg("Warm restart, terminating CLI/netconf SSH connections.~n"
	     "Number of sessions: ~b, Number of connections: ~b~n",
	     [No_sessions, sets:size(Cons_set)]),
    %%info_msg("SET: ~p", [sets:to_list(Cons_set)]),
    sets:fold(
	fun(E, null) ->
	    catch ssh:close(E),
	    null
	end, null, Cons_set),
    {noreply, S};
handle_cast(_Req, S) ->
    {noreply, S}.

handle_info({'DOWN', _Mref, process, Pid, _Info},
		#st{ets_session_id = Ets} = S) ->
    [Session] = ets:lookup(Ets, Pid),
    true =  ets:delete(Ets, Pid),
    New_s = maybe_clear_alarm(S, Session),
    {noreply, New_s};
handle_info(reinit_timeout, #st{ssh_servers = Servers} = S) ->
    New_s = lists:foldl(
	fun({_, undefined, _, _, _} = Server, Acc) ->
		start_ssh_daemon(Acc, Server);
	   (_, Acc) ->
		Acc
	end, S#st{reinit_tmr = undefined}, Servers),
    {noreply, New_s};
handle_info(_Info, S) ->
    {noreply, S}.

code_change(_Old, S, _Extra) ->
    {ok, S}.

terminate(_Why, S) ->
    [stop_daemon(Id) || {_, Id, _, _, _} <- S#st.ssh_servers],
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%logging function return a fun of arity 3 with the Type enclosed
return_failfunc(Type) ->
    fun (_User, Peer_ip, Reason) ->
    %the only reason I've seen sofar is: "Bad user or password"
    %lets try passing the reason straight through, however, the User
    %we discard here since it is a security risk due to users commonly
    %inputting their password instead of their user.../lg
	P_ip =
	    case Peer_ip of
		{Ip_tuple, _Port} ->
		    omc_lib:ip_to_string(Ip_tuple);
		Peer_ip ->
		    omc_lib:ip_to_string(Peer_ip)
	    end,
	Msg = "SSH: " ++ atom_to_list(Type) ++ " login failure: " ++ Reason,
	omc_lib:sec_log(P_ip,  Msg)
    end.

%% Common entry point for all password callback funs (coli,cli, netconf)
%% Catch all exceptions to prevent OTP SSH channel from crashing
authenticate_user(User,Pw) ->
    try
        authenticate_user_cont(User, Pw)
    catch
        _:_ -> false
    end.
  
authenticate_user_cont(User, Pw) ->
    %if sim we call through the server to enable arto's clever use of
    %process dictionary
    case sysEnv:rcs_mode_2() of
        target ->
        case do_authenticate_user(User, Pw) of
		{true, Roles} ->
		    gen_server:call(?SERVER,
					{store_authorization, User, Roles}),
		    true;
		{false, _} ->
		    false
	    end;
	_ -> %% sim or vrcs
        %% VRCS is handled like sim for LDAP until AI is in place for cloud.
 
	    %% It has happend that the server isn't running, which causes an
	    %% error printout in the erlang shell with the user and password
	    %% jotj 2013-01-31
	    gen_server:call(?SERVER, {authenticate_cli_user, User, Pw},
				20000)
    end.

%%moved this outside gen-server, prevent server from blocking
do_authenticate_user(User, Pw) ->
    %% skip delaying if simulated it causes timeout in gen_server:call
    Mode = sysEnv:rcs_mode_2(),
    (Mode =:= target orelse Mode =:= vrcs) andalso
	timer:sleep(omc_login_bucket:get_delay()),
    case comsaI:is_super_oam_user({User, Pw}) of
        true ->
            Roles = [?SUPER_ROLE],
            Msg = "Maintenance user login: " ++ User ++ ", Roles: " ++
            omc_lib:roles_to_string(Roles),
            omc_lib:sec_log("-", Msg),
            %store_authorization(User, Roles),
            {true, Roles};   
        false ->
            case omc_ldap_server:ldap_lookup(User, Pw) of
                {true, Roles} ->
                    Msg = "LDAP: lookup for user: " ++ User ++
                    ", Authenticated: true, Roles: " ++
                    omc_lib:roles_to_string(Roles),
                    omc_lib:sec_log("-", Msg),
                    %store_authorization(User, Roles),
                    {true, Roles};
                {false, Reason} ->
                    %%User and IP address is logged throught the failfun/3
		    Sleep = omc_login_bucket:fail(self()),
                    Msg = lists:flatten(
                        io_lib:format("LDAP: lookup, Authenticated: " ++
				      "false, Reason: ~p (~b)",
				      [Reason, Sleep div 1000])),
                    omc_lib:sec_log("-", Msg),
                    {false, Reason}
            end
    end.

start_services(S) ->
    {New_nc_admin, New_cli_admin} = get_admin_state(),
    New_s = S#st{cli_admin = New_cli_admin, nconf_admin = New_nc_admin},
    info_msg("SSH services activating, Netconf adminState: ~s~n"
	     "Cli adminState: ~s~n",
	[case New_nc_admin of true -> "UNLOCKED"; false -> "LOCKED" end,
            case New_cli_admin of true -> "UNLOCKED"; false -> "LOCKED" end]),
    %we've got restart_complete, surely ootServer must be running...
    ok = ootI:register_cfg_upd_cb(fun ?MODULE:config_update/1),
    Servers = get_all_servers(New_s), %return depends on S#st.admin_unlocked
    Last_s = lists:foldl(
	fun(Server, Acc) ->
	    start_ssh_daemon(Acc, Server)
	end, New_s, Servers),
    Last_s#st{started = true}.

%%return a list of all the servers
%% {{Type, Interface}, ServerRef, BindIP, Port, SockOptList}
%%{{cli, any}, undefind, any, 2023, []}
%%{{cli, oam}, undefind, {10,11,12,13}, 2023, [{netns, "/var/run/netns/OaM"},
%%					      {tos, 4}]}
%%{{cli, lmt},undefind, {10,86,153,49}, 2023, [{netns, "/var/run/netns/Lmt"}]}
get_all_servers(#st{cli_admin = Acli, nconf_admin = Anconf}) ->
    Types =
        case {Acli, Anconf} of
            {true, true} ->   [cli, netconf, coli, sftp];
            {true, false} ->  [cli, coli, sftp];
            {false, true} ->  [netconf, sftp];
            {false, false} -> [sftp]
	end,
    Port_props = [{Type, get_port(Type)} || Type <- Types],
    All_servers =
	get_all_servers(sysEnv:rcs_mode_2(),
			ootI:get_oap_namespace(),
			inet:parse_address(ootI:get_oap_ip_addr()),
			sysInitI:get_lmt_ns(),
			Port_props),
    All_servers.
    %%make_safe_serverlist(All_servers).

%%rewritten for TR fix and for cleaner, easier to understand logic,
%%the major cases are:
%%A0. Simulated!
%%	Run one set of servers without any binding (i.e. 'any')
%%A1. OaM AP  is set, namespace ready (meaning OaM IP is also ready)
%%	Run separate services for all IPs, namespace as required
%%	If alternate IP is set run services there as well.
%%A2. OaM AP  is set, but namespace unknown (meaning OaM IP may not be ready)
%%	Run servers only on LMT, i.e. NO service available on northbound
%%B.  No OaM AP no LMT namespace
%%	Run servers only on LMT, i.e. NO service available on northbound
%%	(TR HU10106)
%%C.  No OaM AP with LMT namespace
%%	Run servers only on LMT, i.e. NO service available on northbound

%% case A0
get_all_servers(simulated, _, _, _, Port_props) ->
    [{{Type, any}, undefined, any, Port, []} || {Type, Port} <- Port_props];
%% case A1
get_all_servers(Mode, {ok, _Ns}, {ok, Oap_ip}, Lmt_ns, Port_props) 
  when Mode =:= target orelse
       Mode =:= vrcs ->
    Oam_opts = omc_api:get_dscp_opt_list() ++ omc_api:get_oam_ns_opt_list(),
    [{{Type, oam}, undefined, Oap_ip, Port, Oam_opts} || {Type, Port} <-
	Port_props]
    ++
    case {ootI:get_oap_ip_addr_alt(), ootI:get_oap_alt_namespace()} of
	{[], _} ->
	    [];
	{Alt_ip_string, {ok, Alt_ns}} ->
	    info_msg("Running SSH services on alternative OamAccessPoint~n"),
	    {ok, Alt_ip} = inet:parse_address(Alt_ip_string),
	    Oam_alt_opts = omc_api:get_dscp_opt_list() ++
				omc_api:ns_to_opt_list(Alt_ns),
	    [{{Type, alt}, undefined, Alt_ip, Port, Oam_alt_opts} ||
		{Type, Port} <- Port_props];
	_ ->
	    []
    end
    ++
    get_lmt_servers(Mode, Lmt_ns, Port_props);
%% case A2
get_all_servers(Mode,  _, {ok, _Oap_ip}, Lmt_ns, Port_props) 
  when Mode == target orelse 
       Mode == vrcs ->
    info_msg("Oam namespace is pending - running SSH services only on LMT~n"),
    get_lmt_servers(Mode, Lmt_ns, Port_props);
%% case B
get_all_servers(Mode, _, _, <<>>, Port_props) 
  when Mode == target orelse 
       Mode == vrcs ->
    info_msg("The OamAccessPoint is not ready/configured, no SSH service will "
	     "be available on northbound interface~n"),
    get_lmt_servers(Mode, <<>>, Port_props);
%% case C
get_all_servers(Mode, _, _, Lmt_ns, Port_props) 
  when Mode == target orelse 
       Mode == vrcs ->
    info_msg("The LMT port is network namespaced, no SSH service will be "
	     "available on northbound interfaces unless the OamAccessPoint is "
	     "set~n"),
    get_lmt_servers(Mode, Lmt_ns, Port_props).

%%% LMT for hardware nodes (DUS/TCU) - Listen to link local and IPv4 address
%%% LMT for cloud nodes (vrcs) - Listen to "any" (no LMT in cloud, eth0 is tenant)
get_lmt_servers(_Mode = vrcs, Lmt_ns, Port_props) ->
    info_msg("Starting SSH services on LMT listening to any~n", []),
    Lmt_ns_opt = omc_api:ns_to_opt_list(Lmt_ns), % Is currently always <<>> 
    [{{Type, tenant_any}, undefined, any, Port, Lmt_ns_opt} ||
	{Type, Port} <- Port_props];

get_lmt_servers(_Mode, Lmt_ns, Port_props) ->
    %%only LMT don't bother with any Dscp options
    Lmt_ll_ip = ootI:get_lmt_ll_ipv4(), %always works, doesn't use ootServer
    Lmt_ns_opt = omc_api:ns_to_opt_list(Lmt_ns),
    [{{Type, lmt_ll}, undefined, Lmt_ll_ip, Port, Lmt_ns_opt} ||
	{Type, Port} <- Port_props]
	++
	case ootI:get_lmt_ipv4() of
	    {_, _, _, _} = Lmt_ip ->
		[{{Type, lmt_dhcp}, undefined, Lmt_ip, Port, Lmt_ns_opt} ||
		    {Type, Port} <- Port_props];
	    _ ->
		info_msg("No SSH service on LMT-DHCP available (yet)~n"),
		[]
	end.

%%return New_state
handle_changed_admin_state(#st{cli_admin = Cli_admin, nconf_admin = Nc_admin} = S,
    Nc_admin, Cli_admin) ->
    S; %no change
handle_changed_admin_state(S, Nc_admin, Cli_admin) ->
    {New_state, _} = do_admin_state_change(S, Nc_admin, Cli_admin),
    New_state.

do_admin_state_change(S, Nc_admin, Cli_admin) ->
    New_servers = get_all_servers(S#st{cli_admin = Cli_admin, nconf_admin = Nc_admin}),
    {Stop_srv, Start_srv} = changed_servers(S#st.ssh_servers, New_servers),
    info_msg("Netconf adminState: ~p~nCli adminState: ~p~nStopping servers: ~p,~nStarting servers: ~p~n",
	      [Nc_admin, Cli_admin, Stop_srv, Start_srv]),
    Next_s = lists:foldl(
	fun(Server, Acc) ->
	    stop_ssh_daemon(Acc, Server)
	end, S, Stop_srv),
    timer:sleep(1000), %TR HT47659 eaddrinuse (not bulletproof)
    New_s = lists:foldl(
	fun(Server, Acc) ->
	    start_ssh_daemon(Acc, Server)
	end, Next_s, Start_srv),
    {New_s#st{cli_admin = Cli_admin, nconf_admin = Nc_admin}, New_servers}.

%%% The SSH configuration changed. Go restart all servers
handle_ssh_config_changed(S) ->
    Servers = get_all_servers(S),
    info_msg("SSH configuration changed. Stopping and starting servers: ~p,~n",
          [Servers]),
    Next_s = lists:foldl(
    fun(Server, Acc) ->
       stop_ssh_daemon_hard(Acc, Server)
    end, S, S#st.ssh_servers),
    timer:sleep(1000), %TR HT47659 eaddrinuse (not bulletproof)
    New_s = lists:foldl(
    fun(Server, Acc) ->
        start_ssh_daemon(Acc, Server)
    end, Next_s, Servers),
    New_s.

%% return New_state
handle_config_update(#st{ssh_servers = Old_srvs} = S) ->
    %takes care of stopping/starting, after this - correct set is running
    {First_state, New_servers} = do_admin_state_change(S, S#st.nconf_admin,
        S#st.cli_admin),
    Last_state = lists:foldl(
	fun(Run, Acc) ->
	    case {Run, lists:keyfind(element(1, Run), 1, Old_srvs)} of
		{{A, _, C, D, E}, {A, _, C, D, E}} -> %no change, no touch
		    Acc;
		{Run, false} -> %just started, must be ok
		    Acc;
		{Run, Running} -> %a difference - restart this one
		    info_msg("New config, server restart:~n"
			     "~p ->~n"
			     "~p~n",
			     [Running, Run]),
		    New_state_acc = stop_ssh_daemon(Acc, Running),
		    timer:sleep(1000), %TR HT47659 eaddrinuse (not bulletproof)
		    start_ssh_daemon(New_state_acc, Run)
	    end
	end, First_state, New_servers),
    Last_state.


%% return: {Stop_srv, Start_srv}
%% return Those in Running but not in New as Stop_srv
%% return Those in New but not in Running as Start_srv
changed_servers(Running, New) ->
    Stop_srv = lists:filter(
	fun(E) ->
	    not lists:keymember(element(1, E), 1, New)
	end, Running),
    Start_srv = lists:filter(
	fun(E) ->
	    not lists:keymember(element(1, E), 1, Running)
	end, New),
    {Stop_srv, Start_srv}.

stop_daemon(undefined) ->
    ok;
stop_daemon(Dref) ->
    ssh:stop_daemon(Dref).

%% Add extra inet options for tenant_any.
get_inet_opts(tenant_any) ->
    [{inet, inet6}];
get_inet_opts(_) ->
    [].

%% FIXME new ssh stuff in OTP after R16B02, check if we can do this with
%% less workarounds/lg
get_ssh_opts(cli, Interface) ->
    %disable subsystems, strangeness if client runs -s sftp
    [
	{ssh_cli, {?CLI_CHANNEL, [cli, Interface]}},
	{pwdfun, fun authenticate_cli_user/2}, {subsystems, []}
    ];
get_ssh_opts(netconf, Interface) ->
    %% w/a to disable execution of erlang terms from ssh cmd line
    %% undocumented OTP feature
    [
	{exec, {dum, dum, []}},
	{shell, {dum, dum, []}},
	{pwdfun, fun authenticate_nc_user/2},
	{subsystems, [{"netconf", {?NC_CHANNEL, [netconf, Interface]}}]}
    ];
get_ssh_opts(coli, Interface) ->
    %disable subsystems, strangeness if client runs -s sftp
    [
	{ssh_cli, {?COLI_CHANNEL, [Interface]}},
	{pwdfun, fun authenticate_coli_user/2},
	{subsystems, []}
    ];
get_ssh_opts(sftp, Interface) ->
    %% w/a to disable execution of erlang terms from ssh cmd line
    %% undocumented OTP feature
    %%SftpdSubsysSpec = sysSshSftpd:subsystem_spec(),
    %% hack to get data into the channel:init() -function
    {Type, {Module, Options}} = sysSshSftpd:subsystem_spec(Interface),
    Extended_options = [{omc_interface, Interface} | Options],
    SftpdSubsysSpec = {Type, {Module, Extended_options}},
    [
	{exec, {dum, dum, []}},
	%% w/a to disable ssh shell for sftp client
	{shell, {dum, dum, []}},
	{pwdfun, fun authenticate_sftp_user/2}, 
	{subsystems, [SftpdSubsysSpec]}
    ].

cleanup_sftp(sftp,  [_Exec, _Shell, _Pwdfun, {subsystems, [SshOpts]}]) ->
	{_Type, {_Module, Options}} = SshOpts,
	sysSshSftpd:subsystem_spec_cleanup(Options),
	ok;
cleanup_sftp(_, _) ->
	ok.

%args: (State, {{cli, oam}, undefined, {1,2,3,4}, 2023, [sock_opts()]})
%returns New State
start_ssh_daemon(#st{ssh_servers = Servers} = S,
		 {{Type, Interface} = N, undefined, Local_ip, P, Opts} = T) ->
    InetOpts = get_inet_opts(Interface),
    Ssh_opts = get_ssh_opts(Type, Interface),
    %% Undocumented w/a to make sure keys are NOT found, i.e. force pw-auth
    Common_opts = [{system_dir, S#st.system_dir},
		   {max_sessions, 20},
		   %% 2 channels needed since some clients opens extra for netconf
		   {max_channels, 2},
		   {parallel_login, true},
		   {id_string, random},
		   {failfun, return_failfunc(Type)},
		   {auth_methods, "keyboard-interactive,password"},
		   {profile, Interface},
		   comsaI:get_ssh_preferred_algorithms(), %% {preferred_algorithms, algos()}
		   {dh_gex_groups, get_gex()}] ++
                     get_ssh_modify_algorithms(),
    
                %% these two are pending lab notification (TR HT82344)
		%% {auth_methods, "keyboard-interactive"}, 
		%% {auth_method_kb_interactive_data, fun auth_kb/3}],
    Work_around = [{user_dir, "/tmp"}],
    All_opts = lists:append([Opts, InetOpts, Ssh_opts, Common_opts, Work_around]),
    % do this from the channel {connectfun, fun connectfunc/3},
    % this seems to not work in 17.0.2 {disconnectfun, fun disconnectfunc/1}
    case workaround_listen_test(Local_ip, P, All_opts) of
	{error, eaddrnotavail} ->
	    %this happens because the local IPv6 is still "tentative"
	    %(possibly because DAD is in progress)
	    New_s = activate_retry(S, T, ?SHORT_REINITTIMEOUT,
				   {eaddrnotavail, undefined}),
	    New_servers = lists:keystore(N, 1, Servers, T),
	    New_s#st{ssh_servers = New_servers};
	_Ok_or_unexpected ->
	    case ssh:daemon(Local_ip, P, All_opts) of
		{ok, Ref} ->
		    New_servers = lists:keystore(N, 1, Servers,
						 setelement(2, T, Ref)),
		    S#st{ssh_servers = New_servers};
		{error, What__filter_out_this_in_runDialyze} ->
	            cleanup_sftp(Type, Ssh_opts), %% cleanup sysSshSftpd ETS
		    New_s = activate_retry(S, T, ?REINITTIMEOUT,
					   What__filter_out_this_in_runDialyze),
		    catch print_diag(New_s, T),
		    New_servers = lists:keystore(N, 1, Servers, T),
		    New_s#st{ssh_servers = New_servers}
	    end
    end.

get_ssh_modify_algorithms() ->
    case erlang:function_exported(comsaI, get_ssh_modify_algorithms, 0) of
	true ->
	    [comsaI:get_ssh_modify_algorithms()]; %% [{modify_algorithms, mod_algos()}]
	false ->
	    sysInitI:warning_msg("comsaI:get_ssh_modify_algorithms() not exported yet~n", []),       
	    []
    end.

activate_retry(#st{reinit_tmr = undefined} = S, T, Timeout, What) ->
    Timeout_string = timeout_to_string(Timeout),
    sysInitI:warning_report(
	[{module, ?MODULE},
	 {description, "Failed to start SSH-service, retry in " ++ Timeout_string},
	 {service, T},
	 {cause, catch element(1,What)}]),
    S#st{reinit_tmr = run_reinit_timer(Timeout)};
activate_retry(S, T, _Timeout, What) ->
    sysInitI:warning_report(
	[{module, ?MODULE},
	 {description, "Failed to start SSH-service, retry in progress"},
	 {service, T},
	 {cause, catch element(1,What)}]),
    S.

run_reinit_timer(Timeout) ->
    erlang:send_after(Timeout, self(), reinit_timeout).

print_diag(_S, Service) ->
    sysInitI:info_msg(
	"~w: IP address diagnostics printout for service: ~p~n~s~n",
	[?MODULE, Service, get_diag(Service)]).

get_diag({{_Type, _Interface} = _N, _, _Local_ip, _P, Opts}) ->
    case proplists:get_value(netns, Opts, <<>>) of
	<<>> -> %namespace isn't used
	    os:cmd("ip ad sh");
	Binary_filepath ->
	    Ns = lists:last(filename:split(binary_to_list(Binary_filepath))),
	    os:cmd(["sudo ip netns exec ", Ns, " ip ad sh"])
    end.

%args: (State, {{cli, oam}, Ref, {1,2,3,4}, 2023, [sock_opts()]})
%returns New State
%use stop_listener instead of stop_daemon, othervise crash when someone is
%logged in (also nicer behaviour, user locking is not thrown out)
stop_ssh_daemon(#st{ssh_servers = Servers} = S,
		 {{_, _} = N, Ref, _, _, _}) ->
    %it can be undefined while the reinit_timeout is going on
    case Ref of
	undefined ->
	    ok;
	Ref ->
	    ssh:stop_listener(Ref)
     end,
    New_servers = lists:keydelete(N, 1, Servers),
    S#st{ssh_servers = New_servers}.

% stops the daemons regardless if there are active sessions.
stop_ssh_daemon_hard(#st{ssh_servers = Servers} = S,
         {{_, _} = N, Ref, _, _, _}) ->
    %it can be undefined while the reinit_timeout is going on
    case Ref of
    undefined ->
        ok;
    Ref ->
        ssh:stop_daemon(Ref)    
    end,
    New_servers = lists:keydelete(N, 1, Servers),
    S#st{ssh_servers = New_servers}.


init_generate_host_keys(Dir, Parent) ->
    register(?genHostKeys_proc, self()),
    supervise_generate_host_keys(Dir, Parent, ?MonoTime).

supervise_generate_host_keys(Dir, Parent, T0) ->
    Parent ! {?MODULE, init_generate_host_keys_done},
    try
	begin
	    do_generate_host_keys(Dir, T0),
	    wait4end(Dir)
	end
    catch
	ErrClass : ErrReason ->
	    error_logger:error_report([{?MODULE, ?FUNCTION},
				       {ErrClass, ErrReason},
				       {stacktrace, erlang:get_stacktrace()}]),
	    Parent ! {?MODULE, error, generate_host_keys, ErrReason}
    end,
    exit(normal).

do_generate_host_keys(Dir, T0) ->
    %% Generate simulated keys in any case
    %% They will be overwritten on target, but we need a file that is owned
    %% by sirpa to get permissions right anyway
    case os:getenv("TARGET_TLM") of
	false ->
	    T1 = ?MonoTime,
	    cmd(["ssh-keygen -f ", Dir, "/ssh_host_rsa_key -N \"\" -t rsa;"]),
	    T2 = ?MonoTime,
	    cmd(["chmod 600 ", Dir, "/ssh_host_rsa_key;"]),
	    T3 = ?MonoTime,
	    cmd(["ssh-keygen -f ", Dir, "/ssh_host_dsa_key -N \"\" -t dsa;"]),
	    T4 = ?MonoTime,
	    cmd(["chmod 600 ", Dir, "/ssh_host_dsa_key;"]);
	_ ->
	    T1 = T2 = T3 = T4 = ?MonoTime,
	    ok
    end,
    T5 = ?MonoTime,
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  {'gen rsa_key', sysUtil:time_to_string(T2 - T1)},
			  {'chmod rsa_key', sysUtil:time_to_string(T3 - T2)},
			  {'gen dsa_key', sysUtil:time_to_string(T4 - T3)},
			  {'chmod dsa_key', sysUtil:time_to_string(T5 - T4)},
			  "-------------- Summary --------------",
			  {'TOTAL', sysUtil:time_to_string(T5 - T0)}]).

wait4end(Dir) ->
    T0 = ?MonoTime,
    receive
	{?MODULE, end_generate_host_keys, Sender} ->
	    Sender ! {?MODULE, begin_generate_host_keys_done, Dir, T0},
	    ok
    end.

end_generate_host_keys() ->
    whereis(?genHostKeys_proc) ! {?MODULE, end_generate_host_keys, self()},
    T1 = ?MonoTime,
    receive
	{?MODULE, begin_generate_host_keys_done, Dir, T0} ->
	    do_end_generate_host_keys(Dir, T0, T1),
	    ok;
	{?MODULE, error, generate_host_keys, ErrReason} ->
	    exit(ErrReason)
    end.

do_end_generate_host_keys(Dir, T0, T1) ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  {'begin proc waiting', sysUtil:time_to_string(T1 -
									T0)}]),
    case sysEnv:rcs_mode_2() of
        target ->
            proc_lib:spawn_link(fun() ->
					copy_ssh_files(Dir)
				end);
        _ -> %% simulated or vrcs 
            ok
    end.

copy_ssh_files(Dir) ->
    try do_copy_ssh_files(Dir)
    catch
        throw:enoent ->
            info_msg("Missing ssh key files, waiting~n"),
            timer:sleep(1000),
            copy_ssh_files(Dir);
	  ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    erlang:ErrClass(ErrReason)
    end.

do_copy_ssh_files(Dir) ->
    Files = ["/etc/ssh/ssh_host_dsa_key", "/etc/ssh/ssh_host_dsa_key.pub",
        "/etc/ssh/ssh_host_rsa_key", "/etc/ssh/ssh_host_rsa_key.pub"],
    Files_present = lists:all(fun(F) -> filelib:is_file(F) end, Files),
    if
        Files_present ->
            case cmd(["cd /tmp; sudo copy_host_keys.sh -d ", Dir]) of
                "" -> ok;
                Error ->
                    erlang:error(Error, [Dir])
            end;
        true ->
            throw(enoent)
    end,
    ok.

cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R=os:cmd(Cmd)]),
    R.

store_user_roles(#st{ets_az_id = Ets_id}, User, Roles) ->
            true = ets:insert(Ets_id, {User, Roles}).

is_super_oam(Roles) ->
    lists:member(?SUPER_ROLE, Roles).

logout_non_support_users(#session{type = coli}, Message) -> Message;
logout_non_support_users(#session{is_super_oam = true}, Message) -> Message;
logout_non_support_users(#session{pid = Pid, transport = ssh}, Message) ->
    omc_ssh_channel:close_session(Pid,  Message),
    Message;
logout_non_support_users(#session{pid = Pid, transport = tls}, Message) ->
    omc_tls_instance:close_session(Pid,  Message),
    Message.

logout_any_user(#session{pid = Pid, transport = ssh}, Message) ->
    omc_ssh_channel:close_session(Pid,  Message),
    Message;
logout_any_user(#session{pid = Pid, transport = tls}, Message) ->
    omc_tls_instance:close_session(Pid,  Message),
    Message.

get_ssh_key_dir() ->
    %% FIXME! This is the location of the ssh-daemon hostkeys.
    %% We could either generate this at first time startup or rely on a
    %% (unique) set installed from the factory.
    %% for now just dump them in .../rmw/base/priv
    sysEnv:releases_vsn_dir().

get_admin_state() ->
    Props_nc = comsaI:get_netconf_ssh_config(),
    Props_cli = comsaI:get_cli_ssh_config(),
    {case proplists:get_value(administrativeState, Props_nc) of
	'LOCKED' -> false;
	'UNLOCKED' -> true
    end,
    case proplists:get_value(administrativeState, Props_cli) of
	'LOCKED' -> false;
	'UNLOCKED' -> true
    end}.

get_port(Type) ->
    omc_api:get_port(ssh, Type).

%ensure that we have a config that won't crash the SSH service by filtering
%out any duplicates (ip/port), prefer oam before any other.
%this should now be handled by the option {profile, atom()} given to ssh:daemon
%make_safe_serverlist(All_servers) ->
%    %Sort oam first so that if there is a clash the LMT ones will be discarded
%    Oam_first =
%	lists:sort(
%	    fun(A, B) ->
%		element(2, element(1, A)) >=  element(2, element(1, B))
%	    end, All_servers),
%    Safe =
%	lists:usort(
%	    fun({_, _, Ip1, Port1, _}, {_, _, Ip2, Port2, _}) ->
%		    {Ip1, Port1} =< {Ip2, Port2}
%	    end, Oam_first),
%
%    if
%	length(All_servers) =/= length(Safe) ->
%	    sysInitI:warning_msg(
%		"~w: Erroneous SSH configuration, dropped some servers to "
%		"maintain operation.~nNew service list:~n~p~n",
%		[?MODULE, Safe]);
%	true -> ok
%    end,
%    Safe.

maybe_raise_alarm(S, #session{interface = Int}) when Int =/= alt ->
    S;
maybe_raise_alarm(#st{is_alarm = true} = S, _) ->
    S;
maybe_raise_alarm(S, _) ->
    comsaI:send_alarm(?ALTALARM, warning, ?OOT_ALT_DN, "", []),
    S#st{is_alarm = true}.

%not possible %maybe_clear_alarm(#st{is_alarm = false}S, _) ->
maybe_clear_alarm(S, #session{interface = Int}) when Int =/= alt ->
    S;
maybe_clear_alarm(#st{ets_session_id = Ets_id} = S, _) ->
    Match_alt_users = {'_','_','_','_','_','alt','_','_','_','_'},
    case ets:match(Ets_id, Match_alt_users) of
	[] ->
	    comsaI:clear_alarm(?ALTALARM, ?OOT_ALT_DN),
	    S#st{is_alarm = false};
	_ ->
	    S
    end.

info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%% perhaps nicefy this with macros
%%% putty requires 4096 length for Diffie-Hellman group exchange but the node does not support such length
%%% since it takes too long time to login to the node.
get_gex() ->
[
{2048,
2,
16#F7693FC11FDDEAA493D3BA36F1FFF9264AA9952209203192A88A697BE9D0E306E306A27430BD87AB9EE9DB4BC78C41950C2EB0E5E4C686E8B1BA6D6A2B1FE91EF40C5EA32C51018323E1D305FE637F35ACABDBFC40AD683F779570A76869EB90015A342B2D1F7C81602688081FCAAA8D623090258D9C5C729C8CDDC0C12CA2D561DD987DB79B6AD7A2A509EBC383BF223FD95BC5A2FCC26FB3F3A0DD3FDC1228E338D3290235A596F9465F7BF490974847E616229A9E60B8F4AA161C52F655843CCCAE8821B40C426B535DE087964778652BBD4EC601C0456AE7128B593FCC64402C891227AE6EE88CC839416FBF462B4852999C646BE0BED7D8CF2BE641C193},
{3072,
2,
16#EF9230A29EC5925FD89310E8F3002A60977FBF02543B6BE08667D8E970AD2468FF1B0892B7310073860FA7145250E6FE7A3902CBE70CC7DB0776031A0868780D47644BD8CD714E41B2AC2D744A51ECA4877FE3D2F496D8E6FC03F2F05B29284DA8682F9BCC93CC16846DF724BFAB53FAAC8492CFBFDCBA92A2F12C221E7FAFC4D8AF54156F649C44869592721FDC2AA65BEDC19DCD2CDD9E14F147F18F900A90B48D6D83197BBFE501FC4F19AE7C16D70DDCE3C757970D1CAFB474A0EA215E747E45C05E90935B721058DDF1BFE23C603A367E66C7E096547952663AFFC9A2B8F6292E12E81A6A61A47D032F7EF62DF4A9998D7B21A7917CC89BA68EB1D40BD9625087754618DC06AA486258DBEAA4B177A82ED0D9E2D9442C89D5354B04712F5159CE4EC6EDD608A5BB25700D24EA16E49A20931891C796B53227698132B5E9321905B81BBB7303BAE12DAB4C864A7B305FE9D6B196921F4890229171DE6DF09FA4D1067255F9D0F05F72DDAA6EB55E73F93EEA17B31A46CB33662E360CD0C3}].

%% Remove diffie-hellman-group1-sha1 since it uses too short key (1024)
%% get_algorithms() ->
%%     Algo = ssh:default_algorithms(),
%%     case lists:keyfind(kex, 1, Algo) of
%% 	{kex, Kexs} ->
%% 	    UpdatedKexs = Kexs -- ['diffie-hellman-group1-sha1'],
%% 	    [{kex, UpdatedKexs}];
%% 	_ ->
%% 	    Algo
%%     end.

%% it happens that the local IP address is still "tentative"
%% (possibly because DAD is in progress)
%% this will test using gen_tcp to avoid a crash printout from inets
workaround_listen_test(LocalIp, P, Opts) 
  when is_tuple(LocalIp) andalso 
       (tuple_size(LocalIp) =:= 8 orelse tuple_size(LocalIp) =:= 4) ->
    NetnsOpt = get_netns_opt(Opts),
    InetType = get_inet_type(LocalIp),
    case gen_tcp:listen(P, [InetType, {ip, LocalIp}] ++ NetnsOpt) of
        {ok, Lsock} ->
            gen_tcp:close(Lsock),
            ok;
        Err ->
            Err
    end;

workaround_listen_test(_LocalIp, _P, _Opts) ->
    ok.

get_netns_opt(Opts) ->
    case lists:keyfind(netns, 1, Opts) of
	false -> [];
	Tuple -> [Tuple]
    end.
    
get_inet_type(IPv6) when tuple_size(IPv6) =:= 8 ->
    inet6;
get_inet_type(_IPv4) ->
    inet.

%% not complete, just catch the important ones
%% also dialyzer figured out <1000 isn't used
%timeout_to_string(Msec) when Msec < 1000 ->
%    integer_to_list(Msec) ++ " msec";
timeout_to_string(Msec) when Msec < 60000 ->
    integer_to_list(Msec div 1000) ++ " second/s";
timeout_to_string(Msec) when Msec > 60000 ->
    integer_to_list(Msec div 60000) ++ " minute/s".

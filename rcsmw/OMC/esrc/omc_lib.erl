%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_lib.erl %
%%% Author:	etxbjca
%%% Description:  common functions within OMC
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(omc_lib).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R11A/1').
-date('2017-09-04').
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
%%% R2A/1      2014-01-19   etxlg     Created
%%% R2A/2      2014-01-29   etxlg     exteded ip_to_string/1
%%% R2A/3      2014-03-28   etxlg     fetch_* cert functions
%%% R2A/4      2014-04-08   etxlg     CLISS can be found in dev_patches
%%%                                   Args to pipe_pty
%%% R2A/5      2014-08-19   etxlg     log using correct MEId
%%% R2A/6      2014-09-01   etxlg     subscribe changes in CERT
%%% R3A/1      2015-02-03   etxlg     manage new API towards CERT verify_fun
%%% R3A/2      2015-02-24   etxlg     use the new verify_fun API
%%% R4A/1      2015-08-20   etxpejn   Added rpc:call for SecurityLog
%%% R4A/4      2015-09-25   etxpejn   Moved rpc:call to logI:write_log
%%% R4A/6      2015-11-09   etxlg     IPv6 address conversion
%%% R5A/2      2016-01-13   etxlg     Merged from R4
%%% R6A/1      2016-08-30   uabesvi   vRC should be treated as target and not simulated
%%% R7A/2      2016-10-18   ehsake    WP6081, Generate ESI at rollback
%%% R11A/1     2017-09-04   etxjotj   Copied db_op_dirty from swmLib
%%% ----------------------------------------------------------
-define(CLISS_BIN, "cliss").
-define(NCONF_BIN, "netconf").
-define(PIPE_PTY_BIN, "pipe_pty"). %needed for cliss
-define(COM_USER_FILE, "/etc/rcs_cs/com_user").
-define(OMC_ESI_LOG_DIR,"/tmp/omc").
-define(OMC_ESI_LOG_FILE,"/tmp/omc/omc.log").

%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1]).
-export([run_portprog/2]).
-export([run_portprog/3]).
-export([sec_log/2]).
-export([sec_log/3]).
-export([ip_to_string/1]).
-export([roles_to_string/1]).
-export([fetch_node_cert/1]).
-export([fetch_ca_certs/1]).
-export([fetch_verify_fun/3]).
-export([generate_rollback_esi/0]).

-export([get_session_info/0]).
-export([add_session_info/5]).
-export([remove_session_info/1]).

-export([add_session/1, add_session/2]).
-export([get_session_count/1]).
-export([remove_session/1, remove_session/2]).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%%-export([internal_function2/3]).

-include("omc.hrl").
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
init(DbNodes) ->
    {atomic, ok} =
    clhI:mnesia_create_table(omc_session_info,
                 [{type, set},
                  {ram_copies, DbNodes},
                  {attributes, record_info(fields,
                               omc_session_info)} |
                  omc_datainit:add_clh_option(omc_session_info)]),

    clhI:mnesia_create_table(omc_session_count,
                 [{type, set},
                  {ram_copies, DbNodes},
                  {attributes, record_info(fields,
                               omc_session_count)} |
                  omc_datainit:add_clh_option(omc_session_count)]).

run_portprog(User, cli, Args) ->
    {ok, Port_bin} = find_port_binary(cli),
    {ok, Pty_bin} = find_pipe_pty_binary(),
    {ok, Port} = application:get_env(comte, cli_port),
    Port_args = case sysEnv:rcs_mode_2() of
                    simulated ->
                        Args ++ ["--", Port_bin, integer_to_list(Port)];
                    Mode when Mode == target orelse Mode == vrcs ->
                        Args ++ ["--", Port_bin, "-c", ?COM_USER_FILE,
				 integer_to_list(Port)]
                end,
    open_port({spawn_executable, Pty_bin},
              [stream, {cd, "/tmp"}, {env, [{"USER", User}]},
               {args, Port_args}, binary,
               exit_status, use_stdio, stderr_to_stdout]).
run_portprog(User, cli) ->
    run_portprog(User, cli, []);
run_portprog(User, netconf) ->
    {ok, Port_bin} = find_port_binary(netconf),
    {ok, Port} = application:get_env(comte, netconf_port),
    
    Port_args = case sysEnv:rcs_mode_2() of
                    simulated ->
                        [integer_to_list(Port)];
                    Mode when Mode == target orelse Mode == vrcs ->
                        ["-c", ?COM_USER_FILE, integer_to_list(Port)]
                end,
    open_port({spawn_executable, Port_bin},
              [stream, {cd, "/tmp"}, {env, [{"USER", User}]},
               {args, Port_args},
               binary, exit_status, use_stdio, stderr_to_stdout]).

%%% ----------------------------------------------------------
%%% #           sec_log(SrcIp, Msg)
%%% Input: SrcIp::string() - User source IP address 
%%% 	 : Msg::string()
%%% Output:
%%% Exceptions:
%%% Description: Send a security event to the log block
%%% ----------------------------------------------------------
sec_log(SrcIp, Msg) ->
    logI:write_log("SecurityLog",  SrcIp, get_me_id_string(), 4, 
		   info, os:timestamp(), Msg).

sec_log(SrcIp, Msg, Count) ->
    NewMsg = lists:flatten(io_lib:format("~s, session count ~p", [Msg, Count])),
    logI:write_log("SecurityLog",  SrcIp, get_me_id_string(), 4, 
           info, os:timestamp(), NewMsg).


ip_to_string("-") -> "-";
ip_to_string(Any)->
    case inet:ntoa(Any) of
	{error, einval} ->
	sysInitI:warning_msg(
	    "~p: Failed to convert IP address: ~p to string~n",
	    [?MODULE, Any]),
	    "invalid address";
	Good ->
	    Good
    end.

roles_to_string(Roles) ->
    roles_to_string(Roles, []).

fetch_node_cert(Prop_list) ->
    case proplists:get_value(nodeCredential, Prop_list, undefined) of
        Dn when is_binary(Dn)->
	    cert_subscribe(Prop_list, Dn),
            case certI:get_cert(Dn) of
                {ok, Cert, {_Type, _Key} = Kt} ->
                    {Cert, Kt};
                _ ->
                    {<<>>,{undefined, <<>>}}
            end;
        _ ->
            {<<>>,{undefined, <<>>}}
    end.

fetch_ca_certs(Prop_list) ->
    case proplists:get_value(trustCategory, Prop_list, undefined) of
        Dn when is_binary(Dn)->
	    cert_subscribe(Prop_list, Dn),
            case certI:get_cert(Dn) of
                {ok, Cert_list} ->
                    Cert_list;
                _ ->
                    []
            end;
        _ ->
            []
    end.

fetch_verify_fun(Area, Prop_list, Info) ->
    case proplists:get_value(trustCategory, Prop_list, undefined) of
        Dn when is_binary(Dn)->
            try certI:mk_verify_fun(Dn, Info) of
                {ok, Fun_option, Partial_fun_option} ->
                    [Fun_option, Partial_fun_option];
                _ ->
                    [{verify_fun, {mk_fail_fun(Area), []}}]
            catch
                _:_ ->
                    [{verify_fun, {mk_fail_fun(Area), []}}]
            end;
        _ ->
            [{verify_fun, {mk_fail_fun(Area), []}}]
    end.

get_session_info() ->
    ets:tab2list(omc_session_info).

add_session_info(Key, Type, Subtype, User, IpAddr) ->
    Data = #omc_session_info{key = Key, type = Type, subtype = Subtype, user = User, ip_address = IpAddr},
    Fun = fun() -> mnesia:write(Data) end,
    db_op_dirty(Fun).

remove_session_info(Key) ->
    Fun = fun() -> mnesia:delete({omc_session_info, Key}) end,
    db_op_dirty(Fun).

get_omc_session_count_key(TlsOrSsh, Type)  ->
    case TlsOrSsh of
        tls ->
            case Type of
                cli -> tls_cli;
                netconf -> tls_netconf;
                coli -> tls_coli;
                ftp -> tls_ftp
            end;
        ssh ->
            case Type of
                cli -> ssh_cli;
                netconf -> ssh_netconf;
                coli -> ssh_coli;
                ftp -> ssh_ftp
            end
    end.

add_session(TlsOrSsh, Type) ->
    add_session(get_omc_session_count_key(TlsOrSsh, Type)).

add_session(Type) ->
    Fun =
    fun() ->
        X = mnesia:read({omc_session_count, Type}),
        Count = 
        case X of
            [] -> 0;
            [Val] -> Val#omc_session_count.count
        end,
        mnesia:write(#omc_session_count{key=Type, count = Count + 1}),
        Count + 1
    end,
    db_op_dirty(Fun).

get_session_count(Type) ->
    Fun =
    fun() ->
        X = mnesia:read({omc_session_count, Type}),
        Count = 
        case X of
            [] -> 0;
            [Val] -> Val#omc_session_count.count
        end,
        Count
    end,
    db_op_dirty(Fun).

remove_session(TlsOrSsh, Type) ->
    remove_session(get_omc_session_count_key(TlsOrSsh, Type)).

remove_session(Type) ->
    Fun =
    fun() ->
        X = mnesia:read({omc_session_count, Type}),
        Count = 
        case X of
            [] -> 0;
            [Val] -> Val#omc_session_count.count
        end,
        case Count of
            0 ->
                0;
            1 ->
                mnesia:delete({omc_session_count, Type}),
                0;
            _ ->
                mnesia:write(#omc_session_count{key=Type, count = Count - 1}),
                Count - 1
        end
    end,
    db_op_dirty(Fun).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
find_port_binary(cli) ->    
    Patched = filename:join([sysEnv:dev_patches_dir(), ?CLISS_BIN]),
    case file:read_file_info(Patched) of
	{ok, _} ->
	    {ok, Patched};
	_ ->
	    ComTop = sysEnv:com_top(),
	    {ok, filename:join([ComTop, "opt", "com", "bin", ?CLISS_BIN])}
    end;
find_port_binary(netconf) ->
    ComTop = sysEnv:com_top(),
    {ok, filename:join([ComTop, "opt", "com", "bin",
                        ?NCONF_BIN])}.
find_pipe_pty_binary() ->
    {ok, sysEnv:find_private_binary(omc, ?PIPE_PTY_BIN, ?PIPE_PTY_BIN)}.


roles_to_string([], []) -> [];
roles_to_string([Role], Acc) -> %last
    lists:reverse(role(Role, Acc));
roles_to_string([R | T], Acc) -> %last
    roles_to_string(T, [$\s, $, | role(R, Acc)]).

role([], Acc) -> Acc;
role([H | T], Acc) ->
    role(T, [H | Acc]).

mk_fail_fun(Area) -> % -> fun/3
    fun (_,_,_) ->
	sysInitI:warning_msg(
	    "~p: No CA-cert/s. There is no trustCategory set in ~s/"
	    "Cert configuration.~n",
	    [?MODULE, Area]),
	{fail, "missing trustCategory"}
    end.

get_me_id_string() ->
    case proplists:get_value(networkManagedElementId,
			     comsaI:get_managed_element_data(),
			     undefined) of
	undefined ->
	    "1";
        Me_string when is_list(Me_string) ->
	    Me_string
    end.

%%% ---------------------------------------------------------
%%% Collects information in OMC for troubleshooting purposes.
%%% This method is called by the LOG block prior to an upgrade
%%% is rolled back due to confirmation timeout.
%%% returns {Who,Result,LogDir} 
%%% ---------------------------------------------------------

generate_rollback_esi() ->
    
  
    
    Result =case  open_log_file() of
        {ok, Handle} ->
            collect_ssh_info(Handle),
            collect_tls_info(Handle),
            collect_other_info(Handle),
            collect_ldap_info(Handle),
            file:close(Handle),
            {omcEsi,"ok",?OMC_ESI_LOG_FILE};
        {error,Reason} ->
            {omcEsi,io:format("error: ~p",[Reason]),""}
    end,
    Result.
 

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
cert_subscribe(Prop_list, Dn) when is_list(Prop_list) ->
    cert_subscribe(proplists:get_value(cert_subscribe, Prop_list), Dn);
cert_subscribe(undefined, _) ->
    ok;
cert_subscribe(Module, Dn) when is_atom(Module) ->
    certI:subscribe(Dn, Module).

open_log_file() ->
    
    file:delete(?OMC_ESI_LOG_FILE),
    file:make_dir(?OMC_ESI_LOG_DIR),
    file:open(?OMC_ESI_LOG_FILE, [write]).
    


collect_ssh_info(Handle) ->
    
    SshServers = omc_server:get_daemon_refs(),
    SshSessions = omc_server:get_sessions(),
    do_write(Handle,"SSH SERVERS",SshServers),
    do_write(Handle,"SSH SESSIONS",SshSessions).

collect_tls_info(Handle) ->
    TlsInfo = omc_tls_server:info(),
    do_write(Handle,"TLS SERVER",TlsInfo).

collect_other_info(Handle) ->
    
    Netstat = os:cmd("netstat -a"),
    %% io_lib does not seem toformat newlines from os:cmd runs. 
    %% Tokenize before passing on to file.
    do_write(Handle,"NETSTAT",string:tokens(Netstat,"\n")).

collect_ldap_info(Handle) ->
    
    do_write(Handle,"LDAP TEST",""),
    CurrLeader = group_leader(),
    group_leader(Handle,self()),
    omc_ldap_server:ecoli_aatrace_lookup(["COMUser"]),
    group_leader(CurrLeader,self()).

do_write(Handle,Heading, Info) ->
    try io:fwrite(Handle, "~p: ~n ~p ~n",[Heading,Info]) of
        ok -> ok
    catch
        _:_->
        sysInitI:info_msg("~p: Could not write esi log info for ~p ~n",
                          [?MODULE,Heading])
    end.

db_op_dirty(Fun) ->
    case mnesia:is_transaction() of
	true ->
	    Fun();
	false ->
	    mnesia:async_dirty(Fun)
    end.



%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

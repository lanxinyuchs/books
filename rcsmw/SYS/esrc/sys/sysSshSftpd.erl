%%% ----------------------------------------------------------
%%% %CCaseFile:	sysSshSftpd.erl %
%%% Author:	erarafo
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sysSshSftpd).
-behaviour(ssh_daemon_channel).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R7A/R8A/R9A/R10A/4').
-date('2017-05-22').
-author('eivomat').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2015-01-20 erarafo     First version.
%%% R3A/4      2015-05-25 etxtory     Added EricssonSupport as allowed role
%%% R4A/1      2015-08-20 etxpejn     Added rpc:call for SecurityLog
%%% R4A/2      2015-09-07 etxlg       Alternate connection
%%% R4A/4      2015-09-25 etxpejn     Moved rpc:call to logI:write_log
%%% R4A/6      2015-11-03 eolaand     Remove call to ssh_sftpd:handle_msg at
%%%                                   close. Add catch to handle invalid table.
%%% R8A/3      2016-12-13 eolaand     Use sysEnv:vnf_dir instead of rcs_dir
%%% R9A/1      2016-02-09 etomist     Support for observability COLI command
%%% R9A/2      2016-02-13 etomist     HV62692, added 10 minute timeout for idle sessions
%%% R10A/1     2017-05-12 eivomat     HV86100
%%% R10A/3     2017-05-17 eivomat     Revert fix for HV86100
%%% R10A/4     2017-05-22 eivomat     HV86100 (extended)
%%% ----------------------------------------------------------

%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% These functions implement the ssh_daemon_channel behaviour.
-export([init/1]).
-export([handle_ssh_msg/2]).
-export([handle_msg/2]).
-export([terminate/2]).

%% How exactly are these functions used?
-export([subsystem_spec/1]).
-export([subsystem_spec_cleanup/1]).
-export([listen/1]).
-export([listen/2]).
-export([listen/3]).
-export([stop/1]).

-export([sftp_dir/0]).
-export([file_handler/0]).
-export([subsystem_spec/0]). %% Deprecated. Due to HV86100 use subsystem_spec/1
-export([authorize_sftp_user/1]).

-export([get_all_sessions_info/1,
	 get_session_info/1,
	 get_session_info/2,
	 get_session_info/3,
	 get_session_info/4]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-define(OTP_MOD, ssh_sftpd).
-define(FILE_HANDLER, sysSshSftpdFile).
-define(AUTHORIZED_ROLES, ["oss", 
			   "expert", 
			   "SystemAdministrator", 
			   "SystemReadOnly", 
			   "EricssonSupport"]).

-define(SFTP_TIMEOUT, 10 * 60 * 1000).
-record(additional_state, {tab,
                           channel_id,
                           ref,
                           timeout = ?SFTP_TIMEOUT,
                           misc,
                           reason = undefined}).
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

get_all_sessions_info(Tab) ->
    ets:tab2list(Tab).

get_session_info(Tab) ->
    get_session_info(Tab, self()).

get_session_info(Tab, Session) when is_pid(Session) ->
    case catch ets:lookup(Tab, Session) of
	[{_Pid, KeyVals}] ->
	    KeyVals;
	_ ->
	    []
    end;

get_session_info(Tab, Key) ->
    get_session_info(Tab, Key, []).

get_session_info(Tab, Key, Default) ->
    get_session_info(Tab, self(), Key, Default).

get_session_info(Tab, Session, Key, Default) ->
    case catch ets:lookup(Tab, Session) of
	[{_Pid, KeyVals}] ->
	    proplists:get_value(Key, KeyVals, Default);
	_ ->
	    Default
    end.

%%% ----------------------------------------------------------
%%% @doc Name of the SFTP service root directory. Use
%%% filename:join(sysEnv:rcs_dir(), sysSshSftpd:sftp_dir())
%%% if the absolute path is needed.
%%% @end
%%% ----------------------------------------------------------
sftp_dir() ->
    "sftp".

%%% ----------------------------------------------------------
%%% @doc 
%%% Name of the SFTP service default file handler callback module.
%%% @end
%%% ----------------------------------------------------------
file_handler() ->
    ?FILE_HANDLER.

%%% ----------------------------------------------------------
%%% @doc 
%%% Authorize sftp user for file access.
%%% Verifies that the user role is allowed to access files via sftp.
%%% @end
%%% ----------------------------------------------------------
-spec authorize_sftp_user(Roles::[string()]) -> boolean().
authorize_sftp_user(Roles) -> 
    lists:any(fun(Role) -> lists:member(Role, ?AUTHORIZED_ROLES) end, Roles).

%%% ----------------------------------------------------------
%%% @doc 
%%% Get pms sftpd subsystem spec.
%%% See OTP documentation ssh_sftpd:subsystem_spec/1 for a description
%%% of the output from this function.
%%% @end
%%% ----------------------------------------------------------
subsystem_spec() ->
    SftpRoot = filename:join(sysEnv:vnf_dir(), sftp_dir()),
    Tab = ets:new(session_tab, [public]),
    {Type, {_, Options}} = 
        ssh_sftpd:subsystem_spec(
          [{cwd, SftpRoot},
           {root, SftpRoot},
           {file_handler, {?FILE_HANDLER, Tab}},
           {max_files, 300},
           {sftpd_vsn, 3}]),
    {Type, {?MODULE, Options}}.

subsystem_spec(Interface) when is_atom(Interface) ->
    SftpRoot = filename:join(sysEnv:vnf_dir(), sftp_dir()),
    Name = list_to_atom("session_tab_" ++ atom_to_list(Interface)),
    Tab = public_named_table(Name),
    {Type, {_, Options}} = 
	ssh_sftpd:subsystem_spec(
	  [{cwd, SftpRoot},
	   {root, SftpRoot},
	   {file_handler, {?FILE_HANDLER, Tab}},
	   {max_files, 300},
	   {sftpd_vsn, 3}]),
    {Type, {?MODULE, Options}};

subsystem_spec(Options) ->
    sysInitI:warning_msg("~w subsystem_spec was called ++++++++++++++~n", [?MODULE]),
    ?OTP_MOD:subsystem_spec(Options).

subsystem_spec_cleanup(Ssh_opts) ->
    {_, {_, Tab}} = lists:keyfind(file_handler, 1, Ssh_opts),
    catch ets:delete(Tab),
    ok.

listen(Port) ->
    sysInitI:warning_msg("~w listen/1 was called ++++++++++++++~n", [?MODULE]),
    listen(any, Port, []).

listen(Port, Options) ->
    sysInitI:warning_msg("~w listen/2 was called ++++++++++++++~n", [?MODULE]),
    listen(any, Port, Options).

listen(Addr, Port, Options) ->
    sysInitI:warning_msg("~w listen/3 was called ++++++++++++++~n", [?MODULE]),
    ?OTP_MOD:listen(Addr, Port, Options).

stop(Pid) ->
    sysInitI:warning_msg("~w stop was called ++++++++++++++~n", [?MODULE]),
    ?OTP_MOD:stop(Pid).

init(Options) ->
    {ok, State} = ?OTP_MOD:init(Options),
    {_, Tab} = proplists:get_value(file_handler, Options),
    Misc = proplists:get_value(omc_interface, Options, unknown), %alternative connection
    Additional = #additional_state{tab = Tab, misc = Misc},
    {ok, {State, Additional}, Additional#additional_state.timeout}.

handle_ssh_msg(Msg, {State, Additional}) ->
    case ?OTP_MOD:handle_ssh_msg(Msg, State) of
	{ok, NewState} ->
	    {ok, {NewState, Additional}, Additional#additional_state.timeout};
	{stop, ChId, NewState} ->
	    {stop, ChId, {NewState, Additional}}
    end.

handle_msg({ssh_channel_up, ChannelId,  ConnectionManager} = Msg, 
	   {State, Additional}) ->
    User = get_ssh_user(ConnectionManager),
    PeerIp = get_ssh_peer(ConnectionManager),
    Roles = get_ssh_roles(User),
    Misc = Additional#additional_state.misc,
    Tab = Additional#additional_state.tab,
    omc_server:store_session(User, sftp, Misc, ssh, Roles),
    omc_lib:add_session_info(self(), "SSH", "sftp", User, PeerIp),
    Count = omc_lib:add_session(ssh_sftp),
    {ok, NewState} = ?OTP_MOD:handle_msg(Msg, State),
    security_log_event(Misc, PeerIp, User, "sftp session started", Count),
    ets:insert(Tab, {self(), [{user, User}, 
			      {roles, Roles}, 
			      {peer_ip, PeerIp}]}),
    {ok, {NewState,
          Additional#additional_state{channel_id = ChannelId, ref = ConnectionManager}},
          Additional#additional_state.timeout};

handle_msg(timeout, {State, Additional})->
    ssh:close(Additional#additional_state.ref),
    {stop, Additional#additional_state.channel_id,
           {State, Additional#additional_state{reason = timeout}}};

handle_msg({close, _}, {State, Additional}) ->
    %% Not implemented in OTP ssh_sftpd
    %% {ok, NewState} = ?OTP_MOD:handle_msg(Msg, State),
    %% {ok, {Tab, NewState, Misc}}.
    {ok, {State, Additional}, Additional#additional_state.timeout};

handle_msg(_Msg, {State, Additional}) ->
    %% Not implemented in OTP ssh_sftpd
    sysInitI:warning_msg("~p: unknown Msg ~p in handle_msg~n",
			 [?MODULE, _Msg]),
    {ok, {State, Additional}, Additional#additional_state.timeout}.


terminate(Reason, {State, Additional}) ->
    Misc = Additional#additional_state.misc,
    Tab = Additional#additional_state.tab,
    LogMsg =
    case Additional#additional_state.reason of
        undefined ->
            sysInitI:info_msg("~p: Terminate sftp session~p~n",[?MODULE, self()]),
            "sftp session ended";
        timeout ->
            sysInitI:info_msg("~p: Terminate sftp session (timeout) ~p~n",[?MODULE, self()]),
            "sftp session ended (timeout)"
    end,
    PeerIp = get_session_info(Tab, peer_ip, "-"),
    User = get_session_info(Tab, user, "-"),
    omc_lib:remove_session_info(self()),
    Count = omc_lib:remove_session(ssh_sftp),
    security_log_event(Misc, PeerIp, User, LogMsg, Count),
    catch ets:delete(Tab, self()),
    ?OTP_MOD:terminate(Reason, State).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% The following functions are mostly knicked from OMC.
security_log_event(Misc, IpString, User, InfoString, Count) ->
    Msg = 
	case Misc of
	    alt ->
		lists:flatten(io_lib:format("SSH-alternative: User: ~s, ~s, session count~p",
					    [User, InfoString, Count]));
	    _ ->
		lists:flatten(io_lib:format("SSH: User: ~s, ~s, session count ~p",
					    [User, InfoString, Count]))
	end,
    logI:write_log("SecurityLog", IpString, get_me_id_string(), 4, 
		   info, os:timestamp(), Msg).

get_ssh_user(Ref) ->
    %this is the new API, which will(maybe it is now) be documented by OTP
    [{user, User}] = ssh:connection_info(Ref, [user]),
    User.

get_ssh_roles(User) ->
    case omc_api:authorize_user(User) of
	Roles when is_list(Roles) ->
	    Roles;
	Error ->
	    sysInitI:warning_msg("~p: Failed to get roles for ssh user ~p"
				     "~nReason: ~p~n",
				     [?MODULE, User, Error]),
		[]
    end.
	    
get_ssh_peer(Ref) ->
    case ssh:connection_info(Ref, [peer]) of
	[{peer,{_,{IpTuple,_}}}] ->
	    ip_to_string(IpTuple);
	_ -> "-"
    end.

ip_to_string(Ip) when is_tuple(Ip) ->
    case inet:ntoa(Ip) of
	{error, _} ->
	    lists:flatten(io_lib:format("~p", [Ip]));
	IpString ->
	    IpString
    end;

ip_to_string("-") -> 
    "-";

ip_to_string(Any) ->
    lists:flatten(io_lib:format("~p", [Any])).

get_me_id_string() ->
    case proplists:get_value(networkManagedElementId,
			     comsaI:get_managed_element_data(),
			     undefined) of
	undefined ->
	    "1";
        Me_string when is_list(Me_string) ->
	    Me_string
    end.

public_named_table(Name) ->
    case ets:info(Name) of
        undefined ->
            ets:new(Name, [public, named_table]);
        _Info -> %% HV86100
            ets:delete_all_objects(Name),
            Name
    end.

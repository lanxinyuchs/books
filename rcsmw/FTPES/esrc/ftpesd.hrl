%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ftpesd.hrl %
%%% @author eivmiha
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R10A/R12A/2
%%%
%%% ----------------------------------------------------------

-hrl_vsn('/main/R8A/R9A/R10A/R12A/2').
-hrl_date('2017-11-24').
-hrl_author('eivmiha').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/4    2016-11-24   eivmiha    code editing
%%% R8A/6    2016-12-13   ekurnik    Changed ftpes root and default port
%%% R8A/9    2016-12-19   ekurnik    Added trusted_category parameter
%%% R8A/10   2016-12-22   ekurnik    Added ftpes_trace keyword
%%% R9A/1    2017-01-24   ekurnik    LOG_MODE set to false
%%% R9A/2    2017-01-27   ekurnik    Added linger variable
%%% R9A/4    2017-02-10   ekurnik    Moved common defines and types
%%% R9A/10   2017-04-06   ekurnik    Port is now fetched from port_conf
%%% R912A/1  2017-10-22   enekdav    Added default min and max data port values
%%% R12A/2   2017-11-24   eivmiha    Added idle timer
%%% ----------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTPD inner representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(ftpesd_hrl).
-define(ftpesd_hrl, true).

-define (DEFAULT_SERVER_DATA_CONNECTION_PORT, 9920).
-define (DEFAULT_SERVER_MIN_DATA_CONNECTION_PORT, 0).
-define (DEFAULT_SERVER_MAX_DATA_CONNECTION_PORT, 0).
-define (DEFAULT_SERVER_CONTROL_CONNECTION_PORT, sysEnv:get_port_conf(ftpes)). %% 9921
-define (IDLE_TIMER, ftpesLib:get_idle_timer()).
%% Defines

%% comment out in production, should be used only for debugging
%% -define(LOG_MODE, true).

-define(UTIL, ftpesd_util).
-define(RESP(Comm, Msg), ftpesd_util:response(Comm, Msg)).

-define(DEFAULT_ROOT_DIR, ftpesd_dir:ftpes_root()).
-define(DEFAULT_PWD_FUN,  fun(_,_) -> not_authorized end).
-define(DEFAULT_LOG_FUN,  fun(_,_) -> ok end).

-ifdef(LOG_MODE).
-define(LOG(Str, Args), io:format("ftpes_trace: " ++ Str, Args)).
-define(LOG(Str), io:format("ftpes_trace: " ++ Str)).
-else.
-define(LOG(Str, Args), ok).
-define(LOG(Str), ok).
-endif.

-define(is_anon(Args), ((Args#ctrl_conn_data.username == "anonymous") or
                            (Args#ctrl_conn_data.username == "ftp")) and
            (Args#ctrl_conn_data.allow_anonymous)).

%% Control connection data
%%  username     ~ actual username set by the user (using USER <loginname>)
%%  authed       ~ indicates whether a proper password
%%		 was given for the actual username
%%  control_sock ~ socket of the control connection
%%  pasv_pid     ~ PID of the passive connection used by this control connection
%%		 (if exists)
%%  curr_path    ~ current directory path

-record(user_data, {
                    peer_address = none,
                    peer_port = none,
                    peer_user = none,
                    username = none
                   }).

-record(ctrl_conn_data, {
                         chrootdir	= none,
                         type = none,
                         port = none,
                         address = none,
                         session_state	= none,
                         control_socket	= none,
                         data_socket = none,
                         auth_phase  = none, %% values: none --> started --> negotiation_done --> pbsz_done --> prot_done --> valid_seq
                         data_pid	= none,
                         data_proc_ref = none,
                         user_data_params = #user_data{},
                         authed		= false,
                         curr_path	= "/",
                         repr_type	= ["I"],
                         rename_from	= none,
                         inet = none,  %% inet4, inet6
                         tls_options = none,
                         ip = none,
                         ip_options = [],
                         listener_pid = none,
                         trusted_category = none,
                         idle_timer = none

                        }).

%% Types

-type proplist()   :: proplists:proplist().
-type socket()     :: gen_tcp:socket().

-type reply()      :: {reply, ReplyCode :: integer(), Message :: string()} | noreply.
-type argschange() :: sameargs | {newargs, #ctrl_conn_data{}}.


-type reason() :: any().
-type error_reason() :: reason().
-type property() :: atom() | tuple().

-type ftp_socket() :: socket() | ssl:sslsocket().
-type ip_interface() :: lmt | oam | alt_oam | sim_ipv4 | sim_ipv6.

-endif.

% Public macros for the log and trace functions

% events to trace
-define(CWD, <<"CWD">>).
-define(RETR, <<"RETR">>).
-define(STOR, <<"STOR">>).
-define(LIST, <<"LIST">>).
-define(MAX_SESSIONS, 20).
-define (BUCKET_SIZE, 15).
-define (BUCKET_TIMEOUT, 5000).

-define(DEFAULT_TIMEOUT, 30000).
-define(SERVER_SESSION_TIMEOUT, 60000).
-define(GEN_SERVER_TIMEOUT, 600000).
-define(LINGER_TIMEOUT, 1).
-define(CERT_EVENT_OFFSET, 100).

%% Macro for deciding if IP interface is enabled:
%% If lmt then only address is needed.
%% If oam or alt_oam both address and Ns are needed
-define(IP_IF_AVAIL(Type, Address, Ns),(Address =/= undefined
                                            andalso 
                                            (Type =:= lmt orelse %% no NS needed for LMT nor SIM IFs
                                                 Type =:= sim_ipv4 orelse 
                                                 Type =:= sim_ipv6 orelse 
                                                 Ns =/= <<>>))). %% NS required for OAM and ALT_OAM 

%% Priority list of interfaces
-define(IP_INTERFACES, [oam, alt_oam, lmt]).

-define(SIM_IP_INTERFACES, [sim_ipv4, sim_ipv6]).

-define(IS_TARGET_IF(Type), Type =:= lmt orelse 
            Type =:= oam orelse 
            Type =:= alt_oam).
-define(IS_SIM_IF(Type), Type =:= sim_ipv4 orelse 
            Type =:= sim_ipv6).

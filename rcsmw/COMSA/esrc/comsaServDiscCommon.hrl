%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaServDiscCommon.hrl %
%%% Author:	qostjoa
%%% Description: Common definitions for the SD module in COMSA
%%%
%%% ----------------------------------------------------------
-hrl_id('xx/190 55-CNA 113 348 Ux').
-hrl_vsn('/main/R12A/1').
-hrl_date('2017-11-24').
-hrl_author('qostjoa').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R12A/      2017-10-20 qostjoa     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------
-define(CONSUL_BIN, comsaServDiscCommon:get_consul_binary()).
-define(DEFAULT_DC_NAME, "vrcs").
-define(SERVER_NAME, ?MODULE).
-define(CONSUL_DIR,"/home/sirpa/consul/").
-define(LOCAL_CFG,    ?CONSUL_DIR ++ "local.json").
-define(FA_CFG,       ?CONSUL_DIR ++ "fa.json").
-define(GLOBAL_CFG,   ?CONSUL_DIR ++ "global.json").
-define(GLOBSRV_CFG,  ?CONSUL_DIR ++ "gsds.json").
-define(CONSUL_TMP, "/tmp/consul/").
-define(MOM_DATA_FILE,?CONSUL_TMP ++ "mom_data").
-define(LOCAL_TMP,    ?CONSUL_TMP ++ "local").
-define(FA_TMP,       ?CONSUL_TMP ++ "fa").
-define(GLOBAL_TMP,   ?CONSUL_TMP ++ "global").
-define(GLOBSRV_TMP,  ?CONSUL_TMP ++ "gsds").
-define(DOMAIN_FILE,  ?CONSUL_TMP ++ "domains").
-define(LOG_WRAPPER, log_wrapper() ).
-define(LOG_DIR,"/var/log/consul/").
-define(LOCAL_LOG,   ?LOG_DIR ++ "local.log").
-define(FA_LOG,      ?LOG_DIR ++ "fa.log").
-define(GLOBAL_LOG,  ?LOG_DIR ++ "global.log").
-define(GLOBSRV_LOG, ?LOG_DIR ++ "gsds.log").
-define(SERF_ENC_KEY, "x6R5vIx0rJJNuRR5KK3Gug=="). %Key for UDP encryption (does not use TSL)
-define(NO_TLS_FLAG, "/home/sirpa/NO_TLS"). %Existence of this file indicates that TLS should be disabled.
%%% Ports used by the various Consul domains
-define(CONSUL_DNS_PORT, -1).
%%% Local domain
-define(LOCAL_SERVER_PORT,   43400).
-define(LOCAL_SERF_LAN_PORT, 23501).
-define(LOCAL_SERF_WAN_PORT, 23502).
%%% FA domain
-define(FA_SERVER_PORT,   41400).
-define(FA_SERF_LAN_PORT, 21501).
-define(FA_SERF_WAN_PORT, 21502).
%%% Global domain
-define(GLOBAL_SERF_LAN_PORT, 22501).
-define(GLOBAL_SERF_WAN_PORT, 22502).
%%% GSDS domain
-define(GSDS_SERVER_PORT, 44400).
%%% Unix sockets used by the domains
-define(LOCAL_UNIX_SOCKET,  "unix://" ++ ?CONSUL_TMP ++ "consul_local_server_socket").
-define(FA_UNIX_SOCKET,     "unix://" ++ ?CONSUL_TMP ++ "consul_fa_server_socket").
-define(GLOBAL_UNIX_SOCKET, "unix://" ++ ?CONSUL_TMP ++ "consul_global_client_socket").
-define(GSDS_UNIX_SOCKET,   "unix://" ++ ?CONSUL_TMP ++ "consul_global_server_socket").
%%% Signal for terminating the wrapper script
-define(SIGHUP, 1).
%%% poll time to have vSD cluster ready.
-define(POLL_TIME, 1000).
-define(NUM_CLUSTER_MEMBERS,5).
-define(MAX_DNS_RETRY,10000).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(MODULE_STR, atom_to_list(?MODULE)).
-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(__Pid), sysUtil:pid_info(__Pid, {all, [error_handler]})).
-define(STATE_INFO(__Record),
	sysUtil:record_format(record_info(fields, state), __Record)).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description: 
%%% ----------------------------------------------------------


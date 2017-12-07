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
%%% %CCaseFile:	ftpesDataInit.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R10A/R12A/4
%%%
%%% ----------------------------------------------------------

-module(ftpesDataInit).
-vsn('/main/R8A/R9A/R10A/R12A/4').
-date('2017-12-06').
-author('enekdav').

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
%%% R8A/4    2016-11-24   eivmiha    doc&spec
%%% R8A/5    2016-12-22   ekurnik    Minor fixes
%%% R8A/6    2016-12-23   ekurnik    Fixed upg_tab
%%% R8A/7    2017-01-02   ekurnik    Fixed upg_tab 2nd try
%%% R9A/1    2017-01-24   ekurnik    Fixed warnings
%%% R9A/2    2017-02-10   ekurnik    Added ftpesClientHandler process
%%% R10A/1   2017-05-09   eivmiha    Fixed upg_tab
%%% R10A/1   2017-07-14   enekdav    Removed unnecesary info_msg
%%% R12A/1   2017-10-22   enekdav    Added FileTPM 2.1 model upgrade
%%% R12A/3   2017-11-28   emirbos    Added control connection port in children/0
%%% R12A/4   2017-12-06   enekdav    Added min and max data port in children/0
%%% ----------------------------------------------------------

%% ====================================================================
%% API functions
%% ====================================================================
-export([children/0,
     instPhParallel_init_data/0,
     activate/0]).

-include("RcsFileTPM.hrl").
-include("ftpesd.hrl").


children() ->
    ServerInfo = ftpesLib:get_ftp_tls_server_info(),
    ControlConnectionPort = ServerInfo#ftpTlsServer.port,
    MinDataPort = ServerInfo#ftpTlsServer.minDataPort,
    MaxDataPort = ServerInfo#ftpTlsServer.maxDataPort,
    {ok, [
            {ftpesServer, {ftpesServer, start_link, [[{port, ControlConnectionPort}, {idle_timer, ?IDLE_TIMER},
                                                      {minDataPort, MinDataPort}, {maxDataPort, MaxDataPort}]]},
                permanent, 1000, worker, [ftpesServer]},
            {ftpesClientHandler, {ftpesClientHandler, start_link, [[]]},
                permanent, 1000, worker, [ftpesClientHandler]}
         ]}.


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checks for upgrade
%%% ----------------------------------------------------------
%%% ###=====================================================================###
start_mode() ->
    start_mode(swmI:is_upgrade_ongoing()).

start_mode(true) ->
    upgrade;

start_mode(_) ->
    fromScratch.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES server
%%% ----------------------------------------------------------
-spec activate() ->
    ok | {error, reason}.
%%% ###=====================================================================###
activate() ->
    ftpesServer:start_ftpesd_server(),
    ftpesClientHandler:start_ftpes_client_handler(),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Initializes Mnesia tables
%%% ----------------------------------------------------------
%%% ###=====================================================================###
instPhParallel_init_data() ->
    StartMode = start_mode(),
    init_data(StartMode),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Initializes table content
%%% ----------------------------------------------------------
%%% ###=====================================================================###

init_data(upgrade) ->
    upg_tab(ets:lookup(olddb, ftpTls), ets:lookup(olddb, ftpTlsServer));

init_data(fromScratch) ->
    write_to_db(default_tab(ftpTls), default_tab(ftpTlsServer)).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Changes table content
%%% ----------------------------------------------------------
%%% ###=====================================================================###
upg_tab([{ftpTls, TlsEts}], [{ftpTlsServer, ServerEts}]) ->
    upg_case(ets:tab2list(TlsEts), ets:tab2list(ServerEts));

%% when upgrading from software which doesn't cointain this MO
upg_tab(_, _) ->
    write_to_db(default_tab(ftpTls), default_tab(ftpTlsServer)).

%% when upgrading from post-17B SW
upg_case([#ftpTls{} = FtpTlsEts], [{ftpTlsServer, ServerId, AdminState}]) ->
    NewFtpTlsServerEts = #ftpTlsServer{ftpTlsServerId = ServerId,
                                       administrativeState = AdminState,
                                       nodeCredential = FtpTlsEts#ftpTls.nodeCredential,
                                       trustCategory = FtpTlsEts#ftpTls.trustCategory,
                                       port = ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT,
                                       minDataPort = ?DEFAULT_SERVER_MIN_DATA_CONNECTION_PORT,
                                       maxDataPort = ?DEFAULT_SERVER_MAX_DATA_CONNECTION_PORT},
    write_to_db(FtpTlsEts, NewFtpTlsServerEts);

%% when upgrading from FileTPM 2.1
upg_case([#ftpTls{} = FtpTlsEts], [#ftpTlsServer{} = FtpTlsServerEts]) ->
    write_to_db(FtpTlsEts, FtpTlsServerEts);

%% when upgrading from pre-17B SW
upg_case(_, _) ->
    write_to_db(default_tab(ftpTls), default_tab(ftpTlsServer)).

write_to_db(Ets1, Ets2) ->
    [mnesia:dirty_write(X) || X <- [Ets1, Ets2]].

default_tab(ftpTls) ->
    #ftpTls{ftpTlsId = {"1","1", "1", "1", "1"},
            nodeCredential = undefined,
            trustCategory = undefined};

default_tab(ftpTlsServer) ->
    #ftpTlsServer{ftpTlsServerId = {"1","1", "1", "1", "1","1"}, 
                  administrativeState = ?BasicAdmState_UNLOCKED,
                  nodeCredential = undefined,
                  trustCategory = undefined,
                  port = ?DEFAULT_SERVER_CONTROL_CONNECTION_PORT,
                  minDataPort = ?DEFAULT_SERVER_MIN_DATA_CONNECTION_PORT,
                  maxDataPort = ?DEFAULT_SERVER_MAX_DATA_CONNECTION_PORT}.

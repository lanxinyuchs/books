%%% --------------------------------------------------------
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
%%% --------------------------------------------------------

-hrl_id({"ComFileTPM","1.0.0","/main/R11A/1"}).


%% -------------- CLASS FileTPM -------------------------

%% Description:
%% The top class for server and client configuration of different protocols implementing file transfer.

-record(fileTPM, {fileTPMId,
                  dummy}).

-define(fileTPM_types,
        [{fileTPMId, string},
         {dummy, atom}]).

-define(FileTPM_restricted, [fileTPMId]).


%% -------------- CLASS FtpTls -------------------------

%% Description:
%% Represents the common configuration parameters of FTP over Transport Layer Security (TLS) clients on the ME.

-record(ftpTls, {ftpTlsId,
                 nodeCredential,
                 trustCategory}).

-define(ftpTls_types,
        [{ftpTlsId, string},
         {nodeCredential, moRef},
         {trustCategory, moRef}]).

-define(FtpTls_restricted, [ftpTlsId]).


%% -------------- CLASS FtpTlsServer -------------------------

%% Description:
%% Represents the FTP over Transport Layer Security (TLS) server of the ME.

-record(ftpTlsServer, {ftpTlsServerId,
                       administrativeState,
                       nodeCredential,
                       trustCategory,
                       port,
                       minDataPort,
                       maxDataPort}).

-define(ftpTlsServer_types,
        [{ftpTlsServerId, string},
         {administrativeState, 'ComFileTPM.BasicAdmState'},
         {nodeCredential, moRef},
         {trustCategory, moRef},
         {port, uint16},
         {minDataPort, uint16},
         {maxDataPort, uint16}]).

-define(ftpTlsServer_port_default, 21).
-define(FtpTlsServer_restricted, [ftpTlsServerId]).


%% -------------- CLASS Sftp -------------------------

%% Description:
%% Represents the common configuration parameters of Secure Shell File Transfer Protocol (SFTP) clients on the ME.

-record(sftp, {sftpId,
               dummy}).

-define(sftp_types,
        [{sftpId, string},
         {dummy, atom}]).

-define(Sftp_restricted, [sftpId]).


%% -------------- CLASS SftpServer -------------------------

%% Description:
%% Represents the Secure Shell File Transfer Protocol (SFTP) server of the ME.

-record(sftpServer, {sftpServerId,
                     administrativeState,
                     port}).

-define(sftpServer_types,
        [{sftpServerId, string},
         {administrativeState, 'ComFileTPM.BasicAdmState'},
         {port, uint16}]).

-define(sftpServer_port_default, 115).
-define(SftpServer_restricted, [sftpServerId]).


%% -------------- CLASS FtpServer -------------------------

%% Description:
%% Generic configuration options for a file transfer server.

-record(ftpServer, {ftpServerId,
                    idleTimer}).

-define(ftpServer_types,
        [{ftpServerId, string},
         {idleTimer, 'ComFileTPM.ComFileTPM_FtpServer_idleTimer'}]).

-define(FtpServer_restricted, [ftpServerId]).


%% ------------------ ENUM BasicAdmState ----------------------
-ifndef('BasicAdmState').
-define('BasicAdmState', 1).

-define(BasicAdmState_LOCKED, 0).
-define(BasicAdmState_UNLOCKED, 1).

-endif. % BasicAdmState

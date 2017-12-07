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
%%% %CCaseFile:	ftpTestLib.hrl %
%%% Author: 
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

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
%%% -----      ---------  --------    ------------------------
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/4    2016-11-29   ekurnik    Added delete attribute macro
%%% R8A/5    2016-11-30   ekurnik    Added ssl error macros
%%% R8A/6    2016-12-06   ekurnik    Added OAM reference macros
%%% R8A/7    2017-01-04   ekurnik    Added FTPES_TEST_DIR
%%%--------------------------------------------------------------------

-define (TEST_DIR_NAME, "test").
-define (TEST_FILE_NAME, "testTransferFile.txt").
-define (NEW_FILE, "testFile.txt").
-define (TEST_FILE_CONTENT, "test file content").

-define (DEFAULT_DATA_PORT, 9920).
-define (DEFAULT_SERVER_CONTROL_CONNECTION_PORT, 9921).

-define (DEFAULT_FTP_CLIENT_CONTROL_PORT, 9001).
-define (DEFAULT_FTP_CLIENT_DATA_PORT, ?DEFAULT_FTP_CLIENT_CONTROL_PORT + 1).

-define(DEFAULT_TIMEOUT, 10000).

-define(FTP_RESPONSE_OPENING_DATA_CONNECTION, "150").

-define(FTP_RESPONSE_COMMAND_SUCCESSFUL, "200").
-define(FTP_FEAT_COMMAND_SUCCESSFUL, "211").
-define(FTP_SIZE, "213").
-define(FTP_RESPONSE_HELLO, "220").
-define(FTP_RESPONSE_SERVICE_CLOSING_CONTROL_CONNECTION, "221").
-define(FTP_RESPONSE_TRANSFER_COMPLETED, "226").
-define(FTP_RESPONSE_COMMAND_SUCCESSFUL_PASV ,"227").
-define(FTP_RESPONSE_COMMAND_SUCCESSFUL_EPASV ,"229").
-define(FTP_RESPONSE_LOGIN_OK, "230").
-define(FTP_USER_OK, "232").
-define(FTP_AUTHENTICATION_MODE_SUCCESSFUL, "234").
-define(FTP_DIRECTORY_COMMAND_SUCCESSFUL, "250").
-define(FTP_CREATING_DIRECTORY_SUCCESSFUL, "257").

-define(FTP_RESPONSE_UNSUCCESSFUL, "500").
-define(INVALID_NUMBER_OF_ARGUMENTS, "501").
-define(FTP_RESPONSE_COMMAND_NOT_IMPLEMENTED, "502").
-define(FTP_SECURITY_DATA_EXCHANGE_NOT_DONE, "503").
-define(UNSUPPORTED_STRUCTURE_TYPE, "504").
-define(NETWORK_PROTOCOL_NOT_SUPPORTED,"522").
-define(FTP_RESPONSE_NOT_LOGGED_IN, "530").
-define(FTP_PROTECTION_LEVEL_REJECTED, "534").
-define(FTP_DIRECTORY_COMMAND_FAILED, "550").

-define (TEST_FILE_NAME_STOR_EXISTING, "storExisting.txt").
-define (FTPES_TEST_DIR, "ftpes_test").

%% netconf delete attribute
-define (OPER_DELETE_ATTR, [{'xc:operation', "delete"}]).

-define (ERR_UNKNOWN_CA, {{error,{tls_alert,"unknown ca"}},_}).
-define (ERR_HANDSHAKE_FAIL, {{error,{tls_alert,"handshake failure"}},_}).

-define(COMTOP,  {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(OAM_ADDR_REF, "ManagedElement=1,Transport=1,Router=OAM,InterfaceIPv4=TN_A_OAM_IP,AddressIPv4=TN_A_OAM_IP").
-define(ALT_OAM_ADDR_REF, "ManagedElement=1,Transport=1,Router=OAM_ALT,InterfaceIPv4=TN_A_OAM_IP_ALT,AddressIPv4=TN_A_OAM_IP_ALT").

-define(OAM_ADDR6_REF, "ManagedElement=1,Transport=1,Router=OAM,InterfaceIPv6=TN_A_OAM_IP,AddressIPv6=TN_A_OAM_IP").
-define(ALT_OAM_ADDR6_REF, "ManagedElement=1,Transport=1,Router=OAM_ALT,InterfaceIPv6=TN_A_OAM_IP_ALT,AddressIPv6=TN_A_OAM_IP_ALT").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
          {'nc:operation', "delete"}]).

-define(INET(Inet), Inet =:= inet orelse Inet =:= ipv4).
-define(INET6(Inet6), Inet6 =:= inet6 orelse Inet6 =:= ipv6).

-define(RCS_DIR, rct_rpc:call(rpc, sysEnv, rcs_dir, [], 5000)).

-record(ftpDataConnection, 
        {mode, %% can be active and passive
         host, %% in active mode it is own ip address, in passive mode undefined (fetched from FTP server)
         port,  %% in active mode it is own data port, in passive mode fundefined (fetched from FTP server)
         ip_protocol = ipv4 ,%% default value is ipv4, ipv4_ext is ipv4 but invoked with EPRT and EPSV commands and ipv6 ,
         inet_mode = inet,
         data_dir, 
         certificate
        }).

-record(user_data, {
         peer_address = none,
         peer_port = none,
         peer_user = none,
         username = none
         }).

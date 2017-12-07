%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_oamap_ipv.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2016
%%% @version /main/R4A/1
%%% @doc ==Funtions to determine server and management interfaces if OaM accesspoint is configured==
%%%
%%% Management interfaces and server addresses changes depending on if OaM accesspoint is configured and if ipv4 or ipv6 is used. 
%%%
%%% This module has support functions to return the IPType for the management interfaces or servers depending on if -oamap_ipv4 or -oamap_ipv6 is given as arguments to rct_run.sh. or if config parameters {jenkins_config,[{oamap_ipv4, []}]} or {jenkins_config,[{oamap_ipv6, []}]} is given as a -config xxx.cfg argument to rct_run.sh
%%%
%%% The IPType is used by the testsuite to look up the IP address from board unique file /proj/rcs-tmp/stps/xxxxxx/config/stp.cfg using ct:get_config/1
%%% @end
-module(rct_oamap_ipv).
-id('Updated by CCase').
-vsn('/main/R4A/1').
-date('2016-02-12').
-author('etxkols').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R4A/1      2016-02-12 etxkols     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([mgmnt_iptype/1,
	 ntp_server_iptype/0,
	 sftp_server_iptype/0,
	 ldap_server_iptype/0]).

%% @spec mgmnt_iptype(IPType) -> IPType::atom() | ssh_lmt_ipv4 | ssh_TN_A_ipv4 | ssh_TN_A_ipv6
%% @doc Determines which management interface that should be used by management hooks.
%%
%% rct_cli, rct_coli, rct_netconf and rct_snmpmgr hooks will automatically use the return value for mangement.
%% ```IPType=oam_auto, rct_run.sh                   -> ssh_lmt_ipv4
%%    IPType=oam_auto, rct_run.sh -oamap_ipv4       -> ssh_TN_A_ipv4
%%    IPType=oam_auto, rct_run.sh -oamap_ipv6       -> ssh_TN_A_ipv6
%%    IPType=oam_auto, rct_run.sh -config xxx.cfg   -> ssh_TN_A_ipv4  if xxx.cfg contains {jenkins_config,[{oamap_ipv4, []}]}.
%%    IPType=oam_auto, rct_run.sh -config xxx.cfg   -> ssh_TN_A_ipv6  if xxx.cfg contains {jenkins_config,[{oamap_ipv6, []}]}.
%%    IPType=IPType                                 -> IPType
%%    Args to rct_run.sh has precedense over -config parameters'''
mgmnt_iptype(IPType) ->
        check_iptype(IPType,ssh_lmt_ipv4,ssh_TN_A_ipv4,ssh_TN_A_ipv6).

%% @spec ntp_server_iptype() -> ntp_server | ntp_server_ipv6
%% @doc Determines if ipv4 or ipv6 NTP server should be used
%% 
%% ```rct_run.sh                   -> ntp_server
%%    rct_run.sh -oamap_ipv4       -> ntp_server
%%    rct_run.sh -oamap_ipv6       -> ntp_server_ipv6
%%    rct_run.sh -config xxx.cfg   -> ntp_server       if xxx.cfg contains {jenkins_config,[{oamap_ipv4, []}]}.
%%    rct_run.sh -config xxx.cfg   -> ntp_server_ipv6  if xxx.cfg contains {jenkins_config,[{oamap_ipv6, []}]}.
%%    Args to rct_run.sh has precedense over -config parameters'''
ntp_server_iptype() ->
    check_iptype(oam_auto,ntp_server,ntp_server,ntp_server_ipv6).

%% @spec sftp_server_iptype() -> sftp_server | sftp_server_ipv6
%% @doc Determines if ipv4 or ipv6 SFTP server should be used
%% 
%% ```rct_run.sh                   -> sftp_server
%%    rct_run.sh -oamap_ipv4       -> sftp_server
%%    rct_run.sh -oamap_ipv6       -> sftp_server_ipv6
%%    rct_run.sh -config xxx.cfg   -> sftp_server       if xxx.cfg contains {jenkins_config,[{oamap_ipv4, []}]}.
%%    rct_run.sh -config xxx.cfg   -> sftp_server_ipv6  if xxx.cfg contains {jenkins_config,[{oamap_ipv6, []}]}.
%%    Args to rct_run.sh has precedense over -config parameters'''
sftp_server_iptype() ->
    check_iptype(oam_auto,sftp_server,sftp_server,sftp_server_ipv6).

%% @spec ldap_server_iptype() -> ldap_server | ldap_server_ipv6
%% @doc Determines if ipv4 or ipv6 LDAP server should be used
%% 
%% ```rct_run.sh                   -> ldap_server
%%    rct_run.sh -oamap_ipv4       -> ldap_server
%%    rct_run.sh -oamap_ipv6       -> ldap_server_ipv6
%%    rct_run.sh -config xxx.cfg   -> ldap_server       if xxx.cfg contains {jenkins_config,[{oamap_ipv4, []}]}.
%%    rct_run.sh -config xxx.cfg   -> ldap_server_ipv6  if xxx.cfg contains {jenkins_config,[{oamap_ipv6, []}]}.
%%    Args to rct_run.sh has precedense over -config parameters'''
ldap_server_iptype() ->
    check_iptype(oam_auto,ldap_server,ldap_server,ldap_server_ipv6).

check_iptype(oam_auto,LMT_ipv4,TN_A_ipv4,TN_A_ipv6) ->
    case init:get_argument(oamap_ipv4) of
        {ok,[[]]} ->
            TN_A_ipv4;
        _ ->
            case init:get_argument(oamap_ipv6) of
                {ok,[[]]} ->
                    TN_A_ipv6;
                _ ->
                    case ct:get_config({jenkins_config,oamap_ipv4}) of
                        [] ->
                            TN_A_ipv4;
                        _ ->
                            case ct:get_config({jenkins_config,oamap_ipv6}) of
                                [] ->
                                    TN_A_ipv6;
                                _ ->
                                    LMT_ipv4
                            end
                    end
            end
    end;
check_iptype(IPType,_,_,_) ->
    IPType.


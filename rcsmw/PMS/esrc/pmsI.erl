%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsI.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/R5A/1
%%%
%%% @doc == PMSI - Performance Management Subsystem Interface ==
%%% This module implements the CS internal API of the RBS CS Performance 
%%% Management Service.
%%% @end
%%% ----------------------------------------------------------
-module(pmsI).
-vsn('/main/R2A/R3A/R4A/R5A/1').
-date('2016-02-26').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-02-11 eolaand     Created
%%% R2A/8      2014-07-03 etxberb     Added coli_rp/1.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([authorize_sftp_user/1]).

-export([coli_rp/1]).
-export([rp_legacy/1]).
-export([rp_ecim/1]).

-export([prep_warm/0]).
-export([warm/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("pms.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(AUTHORIZED_ROLES, 
	["oss", "expert", "SystemAdministrator", "SystemReadOnly"]).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc 
%%% APPM callback. 
%%% Prepare for warm restart of all applications.
%%% @end
%%% ----------------------------------------------------------
-spec prep_warm() -> ok.
prep_warm() ->
    ok.

%%% ----------------------------------------------------------
%%% @doc 
%%% APPM callback. 
%%% All applications are restarted.
%%% @end
%%% ----------------------------------------------------------
-spec warm() -> ok.
warm() ->
    ok.



%%% ----------------------------------------------------------
%%% \@doc 
%%% Get pms sftpd ssh options.
%%% 
%%% See OTP SSH documentation for a description of the output 
%%% from this function.
%%%        
%%% \@end
%%% ----------------------------------------------------------
%% -spec get_ssh_opts() -> list().
%% get_ssh_opts() ->
%%     [
%%      {exec, {dum, dum, []}},
%%      %% w/a to disable ssh shell for sftp client
%%      {shell, {dum, dum, []}},
%%      {pwdfun, fun omc_server:authenticate_sftp_user/2}, % A bit ugly, but...
%%      {connectfun, fun connectfunc/3},
%%      {disconnectfun, fun disconnectfunc/1},
%%      {subsystems, [pmsI:sftpd_subsystem_spec()]}
%%     ].  

%% connectfunc(User, PeerAdress, Reason) ->
%%     io:format("(~p) connectfunc:~nUser = ~p~nPeerAdress = ~p~nReason ~p~n", 
%% 	      [self(), User, PeerAdress, Reason]).


%% disconnectfunc(Reason) ->
%%     io:format("(~p) disconnectfunc:~nReason = ~p~n", [self(), Reason]).


%% %%% ----------------------------------------------------------
%% %%% @doc 
%% %%% Get pms sftpd subsystem spec.
%% %%% 
%% %%% See OTP documentation ssh_sftpd:subsystem_spec/1 for a description
%% %%% of the output from this function.
%% %%%        
%% %%% @end
%% %%% ----------------------------------------------------------
%% -spec sftpd_subsystem_spec() -> tuple().
%% sftpd_subsystem_spec() -> 
%%     pmsSftpdEnv:subsystem_spec().

%%% ----------------------------------------------------------
%%% @doc 
%%% Authorize sftp user for ROP access.
%%% 
%%% Verifies that the user role is allowed to access ROP files via sftp.
%%%        
%%% @end
%%% ----------------------------------------------------------
-spec authorize_sftp_user(Roles::[string()]) -> boolean().
authorize_sftp_user(Roles) -> 
    lists:any(fun(Role) -> lists:member(Role, ?AUTHORIZED_ROLES) end, Roles).


%%% ----------------------------------------------------------
%%% @doc 
%%% COLI callback 
%%% 
%%% @end
%%% ----------------------------------------------------------
-spec coli_rp(list()) -> ok.
coli_rp(["ecim"]) ->
    rp_ecim([]);
coli_rp(["legacy"]) ->
    rp_legacy([]);
coli_rp([Arg]) ->
    Text = "Not allowed argument ~p for command RP.~n~n",
    ?LOG_RAM(?SEV_5, {Text, [Arg]}),
    io:format(Text, [Arg]),
    sysInitI:info_msg("PMS: " ++ Text, [Arg]),
    exit({error, illegal_parameter}).
    


%%% ----------------------------------------------------------
%%% @doc 
%%% Inhibit all other reporting periods but the ones valid in legacy 
%%% 
%%% See also rp_ecim
%%%        
%%% @end
%%% ----------------------------------------------------------
-spec rp_legacy([]) -> ok.
rp_legacy([]) -> 
    Text = "RP test mode is turned off. ~n"
	"Only legacy defined RP values for reporting period are allowed.~n"
	"To turn on the RP test mode use: rp ecim.~n",
    
    ?LOG_RAM(?SEV_5, {Text, []}),
    sysInitI:info_msg("PMS: " ++ Text),
    io:format(Text),
    pmsDb:pms_env_set(reporting_period, legacy),
    exit(ok);
rp_legacy(_) ->
    Text = "Sorry, no parameters are allowed for rp legacy command.~n",
    ?LOG_RAM(?SEV_5, {Text, []}),
    io:format(Text),
    sysInitI:info_msg("PMS: " ++ Text),
    exit({error, illegal_parameter}).

%%% ----------------------------------------------------------
%%% @doc 
%%% Enable all reporting periods defined in ECIM PM
%%% 
%%% See also rp_legacy
%%%        
%%% @end
%%% ----------------------------------------------------------
-spec rp_ecim([]) -> ok.
rp_ecim([]) -> 
    Text = "RP test mode is turned on. ~n"
	"All ECIM PM defined RP values for reporting period are allowed.~n"
	"To turn off the RP test mode use: rp legacy.~n",
    ?LOG_RAM(?SEV_5, {Text, []}),
    sysInitI:info_msg("PMS: " ++ Text),
    io:format(Text),
    pmsDb:pms_env_set(reporting_period, ecim),
    exit(ok);
rp_ecim(_) ->
    Text = "Sorry, no parameters are allowed for rp ecim command.~n",
    ?LOG_RAM(?SEV_5, {Text, []}),
    io:format(Text),
    sysInitI:info_msg("PMS: " ++ Text),
    exit({error, illegal_parameter}).




%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

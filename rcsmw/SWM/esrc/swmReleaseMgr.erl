%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmReleaseMgr.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2015
%%% @version /main/R1A/R4A/1
%%%
%%% @doc ==Software release manager==
%%% This model holds the interaction between software management and the
%%% OTP release handler which conducts the upgrade.

-module(swmReleaseMgr).
-vsn('/main/R1A/R4A/1').
-date('2015-08-31').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
%%% R1A/1      2012-07-03 etxjotj     Created skeleton
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% -export([run_upgrade/0, commit_upgrade/0]).
-export([upgrade_module/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").
-compile(nowarn_unused_vars).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

upgrade_module(Module, Extra) ->
    io:format("Upgrading module ~w (~p)~n",[Module, Extra]).

%%% Description: Register a new sw version as an unpacked set, then
%%%              run the upgrade.

%% run_upgrade(RelFile, AppDirs) ->
    
%%     NewVsn = 
%% 	case release_handler:set_unpacked(RelFile, AppDirs) of
%% 	    {ok, Vsn} ->
%% 		Vsn;
%% 	    {error, Reason} ->
%% 		sysInitI:error_report(
%% 		  [{mfa, {release_handler, set_upnacked, [RelFile, AppDirs]}},
%% 		   {error, Reason}]),
%% 		erlang:error(Reason, [])		
%% 	end,
%%     case release_handler:check_install_release(NewVsn, purge) of
%% 	{ok, OtherVsn, Descr} ->
%% 	    ok;
%% 	{error, Reason} ->
%% 	    sysInitI:warning_report(
%% 	      [{mfa, {release_handler, check_install_release, [NewVsn, purge]}},
%% 	       {error, Reason}])
%%     end,
%%     Opts = [{error_action, reboot},
%% 	    {code_change_timeout, 10000},
%% 	    {suspend_timeout, 10000},
%% 	    {update_paths, true}],
%%     spawn(release_handler, install_release, [NewVsn, Opts]).

%% commit_upgrade() ->
%%     release_handler:make_permanent(NewVsn),
%%     release_handler:remove_release(OldVsn).


    


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%% info_msg(Format) ->
%%    info_msg(Format, []).
%% info_msg(Format, Args) ->
%%    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%warning_msg(Format) ->
%    warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


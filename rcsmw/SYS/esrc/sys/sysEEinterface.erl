%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysEEinterface.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R4A/R11A/1
%%%
%%% @doc == EE - Execution Environment ==
%%% This module is an attempt to put stuff that directly and non-portably
%%% interacts with the EE (i.e. the Operating System) in one place.
%%% If it grows fat perhaps it should be put into a separate block (aka EA -
%%% Environment Adapter, what this is called in COM).
%%%
%%% For now the only thing here is the interface to the NTP-server.

-module(sysEEinterface).
-vsn('/main/R1A/R2A/R4A/R11A/1').
-date('2017-10-17').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-06-17 etxlg       Created
%%% -----   -------    --------   ------------------------
%%% R2A/1      2013-05-03 etxlg       print some
%%% R2A/4      2013-05-24 etxderb     Adapted for new sudo solution
%%%                                   Improved interface for configure_ntp
%%% R2A/5      2013-07-04 etxjotj     Adjusted ntp printout
%%% -----   -------    --------   ------------------------
%%% R11A/1  2017-10-17 etxberb    Adaptions to OTP20; Removed export_all.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([configure_ntp/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%%-export([internal_function1/2]).
%%%-export([internal_function2/3]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc Set NTP daemon according to the given info
-type proplist():: [{Key::atom(), Value::string()}].
-spec configure_ntp(proplist()) -> ok|error.

configure_ntp(Prop_list) ->
    %%info_msg("~p", [Prop_list]),
    Infos = proplists:get_value(info, Prop_list, "unknown"),
    Servers = proplists:get_value(servers, Prop_list),
    Drift_file = proplists:get_value(driftfile, Prop_list),
    Checked_servers = Servers, %FIXME dottedquad only, no DNS
    configure_ntp(sysEnv:rcs_mode(), Infos, Checked_servers, Drift_file).

configure_ntp(target, Infos, Servers, Drift_file) ->
    Ntp_prog = os:find_executable("config_ntp.sh"),
    Server_arg = lists:flatten(["-s " ++ Srv ++ " "|| Srv<-Servers]),
    Infos_arg = lists:flatten(["-c '" ++ Inf ++ "' "|| Inf<-Infos]),
    %% Usage:  config_ntp  -c 'comment line' -s <server> -d <driftfile>"
    %%         where -c and -s options can occur multiple times. 
    Cmd = io_lib:format("cd /tmp; sudo ~s ~s~s-d ~s; echo -n $?", 
			[Ntp_prog, Infos_arg, Server_arg, Drift_file]), 
    Res = cmd(Cmd),
    case {configure_ntp_rc(Res), get(attempts), check_time()} of
	{ok, _, ok} -> ok;
	{Return, _, ok} -> 
	    warning_msg("NTP config failed but it seems calendar time is "
			"close enough~n"
			"Current time UTC is ~p~n",
			[calendar:universal_time()]),
	    Return;
	{_, undefined, error} ->
	    warning_msg("Time is earlier than 2013 with NTP configured.~n"
			"Current time UTC is ~p~n"
			"Retrying NTP configure~n",[calendar:universal_time()]),
	    put(attempts, 2),
	    configure_ntp(target, Infos, Servers, Drift_file);
	{_, N, error} when is_integer(N), N > 0 ->
	    warning_msg("Time is earlier than 2013 with NTP configured.~n"
			"Current time UTC is ~p~n"
			"Retrying NTP configure~n",[calendar:universal_time()]),
	    put(attempts, N-1),
	    timer:sleep(2000),
	    configure_ntp(target, Infos, Servers, Drift_file);
	{_, 0, error} ->
	    warning_msg("Time is earlier than 2013 with NTP configured.~n"
			"Current time UTC is ~p~n",[calendar:universal_time()]),
	    error
    end;


configure_ntp(simulated, Infos, Servers, Drift_file) ->
    info_msg("configure_ntp: /etc/ntp.conf cannot be updated in RCSSIM~n"++
		 lists:append([Info++"~n"||Info<-Infos])++
		 "Servers: ~p~n"
	     "Drift file: ~p~n",[Servers, Drift_file]).

configure_ntp_rc(Result) ->
    case lists:last(string:tokens(Result, "\n")) of
	"0" -> 
	    info_msg("NTP configure: Success~n"),
	    ok;
	_ ->
	    error_msg("NTP configure: Failed~n"),
	    error
    end.
	
    
    
%% configure_ntp_rc("0") -> 
%% configure_ntp_rc(RC) ->
%%      info_msg("ntp configure: RC:~n~s~n", [RC]).

cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.


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
%%%-compile(export_all).
check_time() ->
    Up = swmI:get_current_up_metadata(),
    UpDate = proplists:get_value(productionDate, Up),
    Current = calendar:universal_time(),
    Diff = calendar:datetime_to_gregorian_seconds(Current)-
	calendar:datetime_to_gregorian_seconds(UpDate),
    
    case Diff of
	Diff when Diff > 0 ->
	    %% Current time is later than UP buildtime
	    ok;
	Diff when Diff < -86400 ->
	    %% Current time is more than a day behind UP build
	    error;
	_ ->
	    %% Current time is less than a day behind UP build
	    %% This may depend on the time zone used for UP metadata
	    ok
    end.    
	    


info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format) ->
    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

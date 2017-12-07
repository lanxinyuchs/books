%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsShowCounters.erl %
%%% 
%%% Description:     
%%%
%%% 
%%% 
%%% 
%%% 
%%% 
%%% 
%%% ----------------------------------------------------------
-module(pmsShowCounters).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/2').
-date('2017-04-19').
-shaid('60c493b7dad1de126485a475696a34cc668b7531').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R1A/1      2014-04-07 uabesvi     Created
%%% R10A/2     2017-04-18 eolaand     Merge contents from pmsShowCounters2.
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([show_counters/3]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pms.hrl").
-include("RcsPm.hrl").


%% show counter timeout for waiting for result from the application 
-define(SC_TO, 10).


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% show_counters(LDN, RequestedCounters, Options) -> ok.
%% 
%% Version 2 of show counters. 
%% 
%%========================================================================
show_counters(Ldn, ReqCounters, Options) ->
    sc(pmsShowCountersLib:filter_counters(Ldn, ReqCounters, Options), Ldn).

sc({error, _} = Error, _) ->
    sc_rc([Error], ?SC_NO_COUNTERS, [], []);    
sc(Filtered, Ldn) ->
    %%------------------------------------------------
    %% Find AppJob processes that handles the groups
    %% and request them to fetch the counter values.
    %%------------------------------------------------
    CheckGrps = [to_binary(G) || {G, _} <- Filtered],
    ?LOG_RAM(?SEV_5, {"Check Groups = ~p~n", [CheckGrps]}),
    case sc_get_group_pids(CheckGrps) of
	[] ->
	    sc_rc([], ?SC_NO_COUNTERS, [], []);    
	AppJobPidsGroups ->
	    ?LOG_RAM(?SEV_5, {"Found AppJob Pids = ~p~n", [AppJobPidsGroups]}),
	    Res = call_app_job(AppJobPidsGroups, Ldn, Filtered),
	    sc_rc(Res, ?SC_OK, [], [])
    end.


sc_rc([], Result, ErrorStr, Acc) ->
    {ok, {Result, ErrorStr, lists:append(lists:sort(Acc))}};
sc_rc([{ok, {?SC_OK, [], Counters}} | T], Res, ErrorStr, Acc) ->
    sc_rc(T, Res, ErrorStr, [Counters | Acc]);
sc_rc([{ok, {?SC_OK, NewErrorStr, Counters}} | T], Res, _ErrorStr, Acc) ->
    sc_rc(T, Res, NewErrorStr, [Counters | Acc]);
sc_rc([{ok, {Error, NewErrorStr, Counters}} | T], Res, _ErrorStr, Acc) ->
    sc_rc(T, choose(Res == ?SC_OK, Error, Res), NewErrorStr, [Counters | Acc]);
sc_rc([{error, Error} | T], Res, NewErrorStr, Acc) ->
    sc_rc(T, choose(Res == ?SC_OK, Error, Res), NewErrorStr, Acc).


sc_get_group_pids(Groups) ->
    PidsGroups = lists:append([get_group_pids(Group) || Group <- Groups]),
    pmsLib:key_collect(PidsGroups).


get_group_pids(Group) ->
    case pmsDb:app_reg_get(Group) of
	{ok, AppRegs} ->
	    Pids = [Pid || #pmsAppRegistry{job_pid = Pid} = AppReg <- AppRegs,
			   filter_reg_pids(AppReg)],
	    [{Pid, Group} || Pid <- Pids];
	_ ->
	    []
    end.


filter_reg_pids(#pmsAppRegistry{pmi_cb = {_CbMod, Callbacks}}) ->
    proplists:get_value(?REPORT_SC_CB, Callbacks, false);
filter_reg_pids(#pmsAppRegistry{pmi_cb = PmiCb}) when is_atom(PmiCb) ->
    true.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% 
%% 
%%========================================================================
call_app_job(PidsGroups, Ldn, Counters) ->
    [pmsAppJob:show_counters(Pid, Ldn, [{G, C} || 
					   {G, C} <- Counters, 
					   lists:member(to_binary(G), Groups)], 
			     ?SC_TO) || {Pid, Groups} <- PidsGroups],
    caj(length(PidsGroups), []).


caj(N, Acc) when N > 0 ->    
    R = receive
	    {show_counters_res, Res} ->
		Res
	after (?SC_TO *2) * 1000 ->
		{error, ?SC_INTERNAL_ERROR}
	end,
    caj(N - 1, [R | Acc]);
caj(_, Acc) ->
    Acc.




%%========================================================================
%% Misc functions
%%========================================================================


choose(true,  T, _) -> T;
choose(false, _, F) -> F.


to_binary(Term) ->
    pmsLib:to_binary(Term).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


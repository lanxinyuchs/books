%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsLib.erl %
%%% Author:	
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
%%% @private
-module(pmsLib).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/R12A/4').
-date('2017-11-30').
-author('eolaand').
-shaid('dc1d7b77eb5a114cfe9be1c485810a6de7091fac').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% Rev        Date          Name        What
%%% -----      ----------    --------    ------------------------
%%% R1A/1      2013-02-01    uabesvi     Created
%%% R5A/3      2016-02-01    uabesvi     HU52245 time accuracy
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


-export([get_interval/1, get_gp_enum/1]).
-export([get_report_offset/1]).
-export([get_rop_offset/1]).
-export([get_rop_data_to/1]).

%% Not used: it is not allowed to update a MR as today  
%%-export([gp_sync/2]).
-export([gp_sync_preset/3]).
-export([gp_tick/3]).
-export([gp_tick/4]).
-export([gp_tick/5]).
-export([gp_end_time/1]).
-export([cancel_granularity_tick/1]).
-export([key_collect/1]).

-export([sync_full_hour/2]). 

%% -export([get_rop_multi_val_format/0,
%% 	 set_rop_multi_val_format_3gpp/0,
%% 	 set_rop_multi_val_format_ecim/0]).

-export([epoch_time/0]). 

-export([aggregate_mt_for_sc/3]). 
-export([aggregate_mt_vals/6]). 

-export([log_trunc_bundle/1]).
-export([log_trunc_cm/1]).
-export([log_trunc_list/1]).
-export([log_trunc_sc_names/1]).
-export([log_trunc_sc_counters/1]).
-export([log_trunc_sc_spec/1]).
-export([log_trunc_pmi_vals/1]).
-export([log_trunc_pmi_sc_vals/1]).
-export([log_trunc_sc_vals/1]).

-export([rtime/1]).

-export([get_version_attributes/1]).

-export([is_flex_group/1]).

-export([get_proc_info/1]).
-export([choose/3]).

-export([to_list/1, 
	 to_binary/1]).

-export([parallel_exec/1,
	 pmapr_delay/3,
	 pmapr_delay/5,
	 pmapr/2, 
	 pmap/2, 
	 pmapr_old/2, 
	 pmap_old/3,
	 get_delay_slice/4,
	 get_curr_delay_time/4]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsPm.hrl").
-include("pms.hrl").


-define(ROP_MULTI_VAL_FORMAT, "ROP_MULTI_VAL_FORMAT"). 
-define('3GPP', "3gpp").
-define(ECIM, "ecim").

-define(MEAS_INFO_TICK, meas_info_tick).
-define(GP_SYNCED,      gp_synced_job).
-define(ET, pmsLib:epoch_time()).

%% User suspect
-define(SUSP_VALS, -1).
-define(SUSP_VAL,  -2).
-define(SUSP_NIL, " ").
%%-define(SUSP_NIL, "NIL").

-define(MT_KEY(G, T), {"1", "1", "1", to_list(G), to_list(T)}).
%% -define(MI_ID(PmGroup), "PM=1,PmGroup=" ++ to_list(PmGroup)).
-define(ME, "ManagedElement").

-define(DEFAULT_BL_TO, 10).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% gp_sync(Msg, GP, Offset) -> ok
%% 
%% This function will syncronize the granularity periond to the wall clock.
%% The requesting process will receive {gp_synced, GP} message when synced.
%% 
%% Offset will add the defined number of seconds to the wall clock time.
%% Used from PmJob and PmJobGroup to find when to build the measInfo
%% and the ROP files, respectively.
%%========================================================================
%% Not used: it is not allowed to update a MR as today  
%% gp_sync(Msg, GP) ->
%%     gp_sync(Msg, GP, 0).

%% gp_sync(Msg, GP, Offset) ->
%%     log(gp_sync_full_hour, GP),
%%     {MegS, S, MicS} = os:timestamp(),
%%     MSOffset = 1000 - erlang:round(MicS/1000),
%%     {_, Time}   = calendar:now_to_local_time({MegS, S, 0}),
%%     WaitSeconds = sync_full_hour(GP, Time) - 1,
%%     timer:send_after((WaitSeconds + Offset) * 1000 + MSOffset, self(), 
%% 		     {Msg, GP}),
%%    ok.
    

gp_sync_preset(Msg, GP, ReqPreset) ->
    log(gp_sync_offset, GP),
    {MegS, S, MicS} = Now = os:timestamp(),
    MilSOffset   = 1000 - erlang:round(MicS/1000),
    {_, Time}    = calendar:now_to_universal_time({MegS, S, 0}),
    SecOffset    = sync_full_hour(GP, Time) - 1,
    NextSyncTime = get_next_sync_time(SecOffset, MilSOffset, Now),
    %% Count the wait time (in milli seconds) by adding
    %% SecOffset  = number of whole seconds to sync with the wall clock
    %% ReqSOffset = requested number of seconds delay from the wall clock
    %% MilSOffset = number of milli seconds to sync with the wall clock
    %% 10         = extra delay to be sure enough time has passed
    TO = count_to(SecOffset, ReqPreset, 0, 0),
    ?LOG_RAM(?SEV_5, 
	     {"Resynching with preset. GP SYNC New time out = ~p~n~p   ~p~n",
	      [TO, SecOffset, ReqPreset]}),
    {ok, Ref} = send_after(TO, self(), Msg, GP, NextSyncTime),
    {Ref, NextSyncTime, TO}.

    

%%========================================================================
%% gp_tick(Msg, GP, ReqOffset) -> {Ref, NextSyncTime, TimeOut}
%% 
%% This function will syncronize the granularity periond to the wall clock.
%% The requesting process will receive {gp_synced, GP, NextSyncTime} message 
%% when synced.
%% NextSyncTime is an absolute time when the next sync should be received.
%% When the gp_synced message is received a check should be done that 
%% the current timestamp is greater than NextSyncTime, otherwise a new
%% gp_tick should be called to be sure that NextSyncTime is passed.
%% This is because the Erlang clock may go 1 procent faster or slower than
%% the real clock.
%% 
%% Offset will add the defined number of seconds to the wall clock time.
%% Used from PmJob and PmJobGroup to find when to build the measInfo
%% and the ROP files, respectively.
%%========================================================================
gp_tick(Msg, GP, ReqOffset) ->
    gp_tick(Msg, GP, ReqOffset, 0).

gp_tick(Msg, GP, ReqSOffset, ReqMilSecOffset) ->
    log(gp_sync_full_hour, GP),
    {MegS, S, MicS} = Now = os:timestamp(),
    MilSOffset   = 1000 - erlang:round(MicS/1000),
    {_, Time}    = calendar:now_to_universal_time({MegS, S, 0}),
    SecOffset    = sync_full_hour(GP, Time) - 1,
    NextSyncTime = get_next_sync_time(SecOffset + ReqSOffset, 
				      MilSOffset + ReqMilSecOffset, 
				      Now),
    %% Count the wait time (in milli seconds) by adding
    %% SecOffset  = number of whole seconds to sync with the wall clock
    %% ReqSOffset = requested number of seconds delay from the wall clock
    %% MilSOffset = number of milli seconds to sync with the wall clock
    %% 10         = extra delay to be sure enough time has passed
    TO = count_to(SecOffset, ReqSOffset, MilSOffset, ReqMilSecOffset),
    ?LOG_RAM(?SEV_5, 
	     {"Resynching 1. GP TICK New time out = ~p~n~p   ~p~n",
	      [TO, SecOffset, ReqSOffset]}),
    {ok, Ref} = send_after(TO, self(), Msg, GP, NextSyncTime),
    {Ref, NextSyncTime, TO}.


gp_tick(Msg, GP, ReqSOffset, ReqMilSecOffset, Now) ->
    ?LOG_RAM(?SEV_5, 
	     {"Resynching. GP TICK = ~p  ~p~n", [Now, os:timestamp()]}),
    log(gp_sync_full_hour, GP),
    {MegS, S, MicS} = Now,
    MilSOffset   = 1000 - erlang:round(MicS/1000),
    {_, Time}    = calendar:now_to_universal_time({MegS, S, 0}),
    SecOffset    = sync_full_hour(GP, Time) - 1,
    NextSyncTime = get_next_sync_time(SecOffset + ReqSOffset, 
				      MilSOffset + ReqMilSecOffset, 
				      Now),
    %% Count the wait time (in miilli seconds) by adding
    %% SecOffset  = number of wholw seconds to sync with the wall clock
    %% ReqSOffset = requested number of seconds delay from the wall clock
    %% MilSOffset = number of milli seconds to sync with the wall clock
    %% 10         = extra delay to be sure enough time has passed
    TO = count_to(SecOffset, ReqSOffset, MilSOffset, ReqMilSecOffset),
    ?LOG_RAM(?SEV_5, 
	     {"Resynching 2. GP TICK New time out = ~p~n~p   ~p~n",
	      [TO, SecOffset, ReqSOffset]}),
    {ok, Ref} = send_after(TO, self(), Msg, GP, NextSyncTime),
    {Ref, NextSyncTime, TO}.

get_next_sync_time(WaitSec, _WaitMicS, {MegS, S, _MicS}) ->
    NextSyncSec = MegS * 1000000 + S + WaitSec + 1,
    {NextSyncSec div 1000000, NextSyncSec rem 1000000, 0}. 


%% ReqSOffset can be negative. 
%% Do not decrease SecOffset if the sum is below 5 seconds
count_to(SecOffset, ReqSOffset, MilSOffset, ReqMilSecOffset) 
  when (SecOffset + ReqSOffset) < 5 ->
    SecOffset * 1000 + MilSOffset + ReqMilSecOffset + 10;
count_to(SecOffset, ReqSOffset, MilSOffset, ReqMilSecOffset) ->
    (SecOffset + ReqSOffset) * 1000 + MilSOffset + ReqMilSecOffset + 10.


%% If the remaining time is more than 12 seconds remove
%% 2 procent of the wait time to ensure that the tick is
%% coming before the wall clock. Then a new tick will be sent
%% for the short time remaining to the wall clock.
%% By doing this we prevent the rop data request to be sent
%% too late to the application if Erlang is synchronizing the time.
%% (that can differ up to 1 procent).
send_after(TO, Pid, Msg, GP, NextSyncTime) when TO < 12000 ->
    timer:send_after(TO, Pid, {Msg, GP, NextSyncTime});
send_after(TO, Pid, Msg, GP, NextSyncTime) ->
    timer:send_after(TO - (TO div 50), Pid, {Msg, GP, NextSyncTime}).
    

%%========================================================================
%% cancel_granularity_tick(TimerRef) -> ok
%%
%% cancel the report tick, i.e. the time intervals at PMS
%% should report to OSS.
%%========================================================================
cancel_granularity_tick(TimerRef) ->
    log(delete_granularity_tick, {self(), TimerRef}), 
    timer:cancel(TimerRef),
    ok.

%%========================================================================
%% get_interval(GP) -> Interval
%%
%% Transform the GP ENUM to seconds.
%% 
%%========================================================================
get_interval(?TimePeriod_TEN_SECONDS)    -> 10;
get_interval(?TimePeriod_THIRTY_SECONDS) -> 30;
get_interval(?TimePeriod_ONE_MIN)        -> 60;
get_interval(?TimePeriod_FIVE_MIN)       -> 60*5;
get_interval(?TimePeriod_FIFTEEN_MIN)    -> 60*15;
get_interval(?TimePeriod_THIRTY_MIN)     -> 60*30;
get_interval(?TimePeriod_ONE_HOUR)       -> 60*60;
get_interval(?TimePeriod_TWELVE_HOUR)    -> 60*60*12;
get_interval(?TimePeriod_ONE_DAY)        -> 60*60*24.

%%========================================================================
%% get_gp_enum(Interval) -> TimePeriod.
%%
%% Transform interval seconds to GP ENUM.
%% 
%%========================================================================
get_gp_enum(10)       -> ?TimePeriod_TEN_SECONDS;
get_gp_enum(30)       -> ?TimePeriod_THIRTY_SECONDS;
get_gp_enum(60)       -> ?TimePeriod_ONE_MIN;
get_gp_enum(60*5)     -> ?TimePeriod_FIVE_MIN;
get_gp_enum(60*15)    -> ?TimePeriod_FIFTEEN_MIN;
get_gp_enum(60*30)    -> ?TimePeriod_THIRTY_MIN;
get_gp_enum(60*60)    -> ?TimePeriod_ONE_HOUR;
get_gp_enum(60*60*12) -> ?TimePeriod_TWELVE_HOUR;
get_gp_enum(60*60*24) -> ?TimePeriod_ONE_DAY;
get_gp_enum(GP)       -> {error, {invalid_gp, GP}}.
    
%%========================================================================
%% get_report_offset(GP) -> ReportTime
%%
%% This function returns how many seconds the application
%% is allowed to spend before the measurement data has to
%% be sent to PMS.
%%
%%========================================================================
get_report_offset(?TimePeriod_TEN_SECONDS) ->     5;
get_report_offset(?TimePeriod_THIRTY_SECONDS) -> 15;
get_report_offset(?TimePeriod_ONE_MIN) ->        30;
get_report_offset(?TimePeriod_FIVE_MIN) ->      240;
get_report_offset(_GP) ->                       240.
%% get_report_offset(GP) -> get_offset(GP, 8).

%%========================================================================
%% get_rop_data_to(GP) -> ReportTime
%%
%% time when the rop data should have been delivered 
%% from the applications to pmsAppJob
%%
%%========================================================================
get_rop_data_to(?TimePeriod_TEN_SECONDS)    ->   5;
get_rop_data_to(?TimePeriod_THIRTY_SECONDS) ->  17;
get_rop_data_to(?TimePeriod_ONE_MIN)        ->  32;
get_rop_data_to(?TimePeriod_FIVE_MIN)       -> 243;
get_rop_data_to(_GP)                        -> 243.


%%========================================================================
%% get_rop_offset(GP) -> ReportTime
%%
%% This function returns the offset from RP when pmsJobGroup starts
%% to build the ROP file. 
%%
%% Different PM Jobs may have different GPs but they must have the
%% same RP. Further, a GP must not be greater than the RP. 
%%
%% The application will get 1/8:th granularity period to send the data.
%% pmsJob will start at GP + 1/6 GP to build the measInfo data.
%% pmsJobGroup will start at RP + 1/4 to build the ROP file.
%% OSS will start to read the ROP file at RP + 1/3 RP.
%% 
%% 10 seconds GP is handled separately to get the correct offset time.
%%========================================================================
get_rop_offset(?TimePeriod_TEN_SECONDS) ->     8;
get_rop_offset(?TimePeriod_THIRTY_SECONDS) -> 20;
get_rop_offset(?TimePeriod_ONE_MIN) ->        45;
get_rop_offset(?TimePeriod_FIVE_MIN) ->      280;
get_rop_offset(_RP) ->                       280.
%% get_rop_offset(RP) -> get_offset(RP, 4).


%%===========================================================================
%% key_collect(KeyValueList) -> MergedKeyValueList
%% 
%% Convert a list of {Key, Value} pairs with multiple instances of Key
%% into a sorted list of {Key, [Values]} with a single instance of each Key.
%%===========================================================================
key_collect(KVList) ->
    SL = lists:sort(KVList),
    lists:foldr(fun({K, V}, [{K, AV} | Acc]) ->
			[{K, [V | AV]} | Acc];
		   ({K, V}, Acc) ->
			[{K, [V]} | Acc]
		end, [], SL).

%%===========================================================================
%% gp_end_time(GP) -> integer()
%% 
%% Calculates the end time of a granularity period in seconds since 1 jan 1970.
%% GP should be given as enumeration value.
%%===========================================================================
gp_end_time(GP) ->
    {MeSec, Sec, MiSec} = os:timestamp(),
    GPSec = get_interval(GP),
    GPSec * (erlang:round(MeSec * 1000000 + Sec + MiSec/1000000) div GPSec).
 

%%===========================================================================
%% epoch_time() -> integer()
%% 
%% Calculates the current time in seconds since 1 jan 1970.
%%===========================================================================
epoch_time() ->
    {MeSec, Sec, MiSec} = os:timestamp(),
    erlang:round(MeSec * 1000000 + Sec + MiSec/1000000).
 


%%===========================================================================
%% aggregate_mt_for_sc(Vals, Grp, Ldn) -> 
%% 
%% This is called from pmsShowCountersI when aggregating for show-counters
%%===========================================================================
aggregate_mt_for_sc(Vals, Grp, Ldn) ->
    amsc(Vals, Grp, Ldn, []).


amsc([], Grp, Ldn, Acc) ->
    {Grp, [{Ldn, Acc}]};
%% flex counter
amsc([{{BaseMTId, MTId}, Vals} | T], Grp, Ldn, Acc) when Vals =/= [] ->
    {ok, [MTRec]}   = pmsDb:measurement_type_dirty_get(?MT_KEY(Grp, BaseMTId)),
    {AggVals, Susp} = aggregate_meas_vals(MTId, MTRec, _Flex = true, Vals),
    Y = {MTId, AggVals, Susp}, 
    amsc(T, Grp, Ldn, [Y | Acc]);
%% common counter
amsc([{MTId, Vals} | T], Grp, Ldn, Acc) when Vals =/= [] ->
    {ok, [MTRec]}   = pmsDb:measurement_type_dirty_get(?MT_KEY(Grp, MTId)),
    {AggVals, Susp} = aggregate_meas_vals(MTId, MTRec, _Flex = false, Vals),
    Y = {MTId, AggVals, Susp}, 
    amsc(T, Grp, Ldn, [Y | Acc]);
%% suspect marked
amsc([{MTId, _Vals} | T], Grp, Ldn, Acc) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Empty measurement value received for ~p. "
	      "Suspect marked. ~n", [{Grp, MTId}]}, ?DEFAULT_BL_TO),
    Y = {MTId, [], true},
    amsc(T, Grp, Ldn, [Y | Acc]).





%%===========================================================================
%% aggregate_mt_vals(MtVals, Grp, MeasTable) -> 
%% 
%% This is called from pmsJob when aggregating for ROP
%%===========================================================================
aggregate_mt_vals(Group, MTId, Flex, SeqNo, Vals, AccSusp) when Vals =/= [] ->
    Key = ?MT_KEY(Group, MTId),
    case pmsDb:measurement_type_dirty_get(Key) of
	{ok, [MT]} ->
	    {Val, Susp} = aggregate_meas_vals(MTId, MT, Flex, Vals),
	    {{SeqNo, remove_susp(Val)}, Susp or AccSusp};
	_Error ->	    
	    ?LOG_RAM(?SEV_WARNING, {"~p not found in database~n", [Key]}, 
		     ?DEFAULT_BL_TO),
	    {[], AccSusp}
    end;

aggregate_mt_vals(Group, MTId, _Flex, SeqNo, _Vals, _Susp) ->
    ?LOG_RAM(?SEV_WARNING, 
	      {"Empty measurement value received for ~p. "
	       "Suspect mark measValue.~n", [{Group, MTId}]}, ?DEFAULT_BL_TO),
	     {{SeqNo, [?SUSP_NIL]}, true}.


remove_susp(Vals) ->
    F = fun(Val) when Val =:= ?SUSP_VALS;
		      Val =:= ?SUSP_VAL ->
		?SUSP_NIL;
	   (Val) ->
		Val
	end,
    lists:map(F, Vals).
    

aggregate_meas_vals(MTId, MT, Flex, Vals) ->
    Aggr = MT#measurementType.aggregation,
    Mult = MT#measurementType.multiplicity,
    if
	MT#measurementType.isCompressed ->
	    aggregate_meas_vals_comp(Aggr, Vals, Mult);
	true ->
	    aggregate_meas_vals_normal(Aggr, Vals, Mult, MTId, Flex)
    end.
	    

%% Normal counters should be suspect marked if the length of any of the
%% received values is other than the configured multiplicity. 
%%
%% For aggregation Last Update we only check the last received value.
aggregate_meas_vals_normal(?Aggregation_LAST_UPDATE, [Val | _], Mult, _MT, 
			   Flex) ->
    {Val, lists:member(?SUSP_VALS, Val) orelse 
     not Flex andalso length(Val) =/= Mult};

aggregate_meas_vals_normal(?Aggregation_AVG, Vals, Mult, MT, Flex) ->  
    {SumVals, Susp} = 
	aggregate_meas_vals_normal(?Aggregation_SUM, Vals, Mult, MT, Flex),
    N = length(Vals),
    F = fun(Sum) when Sum =/= ?SUSP_VALS, Sum =/= ?SUSP_VAL ->
		erlang:round(Sum/N);
	   (Val) ->
		Val
	end,
    {lists:map(F, SumVals), Susp};

aggregate_meas_vals_normal(Aggr, [FVal | Vals], Mult, MT, Flex) ->
    F = fun([N1] = Val, {[N2] = Acc, Susp}) 
	      when Mult =:= 1, N1 >= 0, N2 >= 0 ->
		{aggregate_meas_val(Aggr, Val, Acc), Susp}; 
	    (Val, {Acc, Susp}) when length(Val) =:= length(Acc),
				    Flex orelse length(Val) =:= Mult -> 
		AggVal = aggregate_meas_val(Aggr, Val, Acc),
		{AggVal, (Susp orelse (lists:member(?SUSP_VALS, Val)))};
	   (Val, {_Acc, _Susp}) when length(Val) =/= Mult ->
		?LOG_RAM(?SEV_WARNING, 
			 ?LFUN({"Suspect marked. Invalid Multiplicity. ~n"
				"  MT         = ~p~n"
				"  Mult       = ~p~n"
				"  Rcv length = ~p~n",
				[MT, Mult, length(Val)]}), ?DEFAULT_BL_TO),
		{Val, true};
	   (_Val, {Acc, _Susp}) ->
		?LOG_RAM(?SEV_WARNING,
			 {"Suspect marked. Unknown reason. ~n"
			  "  MT         = ~p~n",
			  [MT]}, ?DEFAULT_BL_TO),
		{Acc, true}
	end,
    FSusp = lists:member(?SUSP_VALS, FVal) orelse not Flex andalso 
	length(FVal) =/= Mult,
    FSusp andalso not lists:member(?SUSP_VALS, FVal) andalso
	?LOG_RAM(?SEV_WARNING, 
		 ?LFUN({"Suspect marked. Invalid Multiplicity. ~n"
			"  MT         = ~p~n"
			"  Mult       = ~p~n"
			"  Rcv length = ~p~n",
			[MT, Mult, length(FVal)]}), ?DEFAULT_BL_TO),
    lists:foldl(F, {FVal, FSusp}, Vals).

%% On request from TN, compressed PDF counters should not be suspect 
%% marked even if the length is other than the configured multiplicity.
aggregate_meas_vals_comp(_Aggr, [Val], _Mult) ->
    %% Normal scenario for compressed PDF counters. One value per GP.
    %% No aggregation needed and aggregation type can be ignored.
    {Val, lists:member(?SUSP_VALS, Val)};

aggregate_meas_vals_comp(?Aggregation_LAST_UPDATE, [Val | _], _Mult) ->
    %% Last Update is the only aggregation type for compressed PDF where we 
    %% can support reception of several measurement values in one GP since 
    %% we can safely ignore all values but the last.
    {Val, lists:member(?SUSP_VALS, Val)};

aggregate_meas_vals_comp(_Aggr, [Val, _ | _], _Mult) ->
    %% At the moment we don't support aggregation of compressed PDF
    %% counters for other aggregation types than Last Update. 
    %% If more than one value is received and the type is other than LU,
    %% it will result in suspect marking. 
    {Val, true}.


aggregate_meas_val(Aggr, Val, Acc) ->
    Vs = lists:zip(Val, Acc),
    [aggregate_meas_val(Aggr, V) || V <- Vs].


aggregate_meas_val(_, {V1, V2}) when V1 =:= ?SUSP_VALS; V2 =:= ?SUSP_VALS ->
    ?SUSP_VALS;
aggregate_meas_val(_, {V1, V2}) when V1 =:= ?SUSP_VAL; V2 =:= ?SUSP_VAL ->
    ?SUSP_VAL;
aggregate_meas_val(?Aggregation_SUM, {V1, V2}) ->
    V1 + V2;
aggregate_meas_val(?Aggregation_MIN, {V1, V2}) when V1 < V2 ->
    V1;
aggregate_meas_val(?Aggregation_MIN, {_V1, V2}) ->
    V2;
aggregate_meas_val(?Aggregation_MAX, {V1, V2}) when V1 > V2->
    V1;
aggregate_meas_val(?Aggregation_MAX, {_V1, V2}) ->
    V2.


%%=============================================
%% human readable time
%%=============================================
rtime(Time) ->
    {_, {H, M, S}} = calendar:seconds_to_daystime(Time),
    integer_to_list(H) ++ ":" ++
	rtime_fill(M) ++ integer_to_list(M) ++ ":" ++
	rtime_fill(S) ++ integer_to_list(S).

rtime_fill(X) when X < 10 -> "0";
rtime_fill(_)             -> "".


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% sync_full_hour(GP, os:timestamp()) -> Seconds
%%========================================================================
sync_full_hour(?TimePeriod_TEN_SECONDS, {_, _, S}) ->
    10 - (S rem 10);
sync_full_hour(?TimePeriod_THIRTY_SECONDS, {_, _, S}) ->
    30 - (S rem 30);
sync_full_hour(?TimePeriod_ONE_MIN, {_, _, S}) ->
    60 - S;
sync_full_hour(?TimePeriod_FIVE_MIN, {_, M, S}) ->
    ((4 - (M rem 5)) * 60) + (60 - S);
sync_full_hour(?TimePeriod_FIFTEEN_MIN, {_, M, S}) ->
    ((14 - (M rem 15)) * 60) + (60 - S);
sync_full_hour(?TimePeriod_THIRTY_MIN, {_, M, S}) ->
    ((29 - (M rem 30)) * 60) + (60 - S);
sync_full_hour(?TimePeriod_ONE_HOUR, {_, M, S}) ->
    ((59 - (M rem 60)) * 60) + (60 - S);
sync_full_hour(?TimePeriod_TWELVE_HOUR, {_, M, S}) ->
    ((59 - (M rem 60)) * 60) + (60 - S);
sync_full_hour(?TimePeriod_ONE_DAY, {_, M, S}) ->
    ((59 - (M rem 60)) * 60) + (60 - S).





%%===========================================================================
%% get_offset(Period, Dividend) -> Time
%% 
%% The ROP file must be created at latest 5 min after the reporting period,
%% even if the reporting period is longer than 15 minutes.
%% If the reporting period is shorter than 15 min the rop file must be
%% ready at 1/3 of the reporting period.
%%===========================================================================
%% get_offset(Period, Dividend) ->
%%      case get_interval(Period) of
%% 	%% The ROP file should be ready at maximum 5 min after RP
%% 	%% even if the RP is very long
%% 	Interval when Interval > 60*15 ->
%% 	    get_interval(?TimePeriod_FIFTEEN_MIN) div Dividend;
%% 	%% The ROP file should be ready latest at 1/3 of the RP
%% 	%% if the RP is less than 15 min
%% 	Interval ->
%% 	    Interval div Dividend
%%     end.
   

%%===========================================================================
%% ROP formatting (for demo)
%%===========================================================================
%% set_rop_multi_val_format_3gpp() ->
%%     os:putenv(?ROP_MULTI_VAL_FORMAT, ?'3GPP').

%% set_rop_multi_val_format_ecim() ->
%%     os:putenv(?ROP_MULTI_VAL_FORMAT, ?ECIM).


%% get_rop_multi_val_format() ->
%%     os:getenv(?ROP_MULTI_VAL_FORMAT).

%%===========================================================================
%% log truncate functions
%%===========================================================================

%% counter map
log_trunc_cm(CounterMaps) ->
    log_trunc_cm(CounterMaps, pmsDb:pms_env_get(log_max)).


log_trunc_cm(CounterMaps, Max) when length(CounterMaps) >= Max ->
    lists:append(lists:sublist(CounterMaps, Max), ['...']);
log_trunc_cm(CounterMaps, _) ->
    CounterMaps.


%% tuncate a list
log_trunc_list(L) ->
    log_trunc_list(L, pmsDb:pms_env_get(log_max)).

log_trunc_list(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(L, Max), ['...']);
log_trunc_list(L, _) ->
    L.

%% truncate value bundles
log_trunc_bundle(Bundles) ->
    ltb_grp(Bundles, 0, pmsDb:pms_env_get(log_max), []).

ltb_grp([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltb_grp(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltb_grp([], _, _, Acc) ->
    lists:reverse(Acc);
ltb_grp([{Grp, Ldns} | T], N, Max, Acc) ->
    Res = ltb_ldn(Ldns, 0, Max, []),
    ltb_grp(T, N + 1, Max, [{Grp, Res} | Acc]).


ltb_ldn([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltb_ldn(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltb_ldn([], _, _, Acc) ->
    lists:reverse(Acc);
ltb_ldn([{Ldn, Vals} | T], N, Max, Acc) ->
    ltb_ldn(T, N + 1, Max, [{Ldn, log_trunc_list(Vals)} | Acc]).


%% tuncate show counter names
log_trunc_sc_names(L) when is_list(L)->
    log_trunc_sc_names(L, pmsDb:pms_env_get(log_max));
log_trunc_sc_names(Other) ->
    Other.

log_trunc_sc_names(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(lt_sc_names(L, Max, []), Max), ['...']);
log_trunc_sc_names(L, Max) ->
    lt_sc_names(L, Max, []).

lt_sc_names([], _, Acc) ->
    lists:reverse(Acc);
lt_sc_names([{Name, L, X} | T], Max, Acc) ->
    lt_sc_names(T, Max, [{Name, lt_scn(L, Max), X} | Acc]);
lt_sc_names([H | T], Max, Acc) ->
    lt_sc_names(T, Max, [H | Acc]).
    
lt_scn(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(L, Max), ['...']);
lt_scn(L, _) ->
    L.


%% tuncate show counter counters
log_trunc_sc_counters(L) when is_list(L)  ->
    log_trunc_sc_counters(L, pmsDb:pms_env_get(log_max));
log_trunc_sc_counters(Other) ->
    Other.

log_trunc_sc_counters(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(lt_sc_counters(L, Max, []), Max), ['...']);
log_trunc_sc_counters(L, Max) ->
    lt_sc_counters(L, Max, []).

lt_sc_counters([], _, Acc) ->
    lists:reverse(Acc);
lt_sc_counters([{Name, C} | T], Max, Acc) ->
    lt_sc_counters(T, Max, [{Name, lt_scc(C, Max)} | Acc]);
lt_sc_counters([H | T], Max, Acc) ->
    lt_sc_counters(T, Max, [H | Acc]).
    
lt_scc(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(L, Max), ['...']);
lt_scc(L, _) ->
    L.

%% tuncate show counter counter spec
log_trunc_sc_spec(L) when is_list(L) ->
    log_trunc_sc_spec(L, pmsDb:pms_env_get(log_max));
log_trunc_sc_spec(Other) ->
    Other.

log_trunc_sc_spec(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(lt_sc_spec(L, Max, []), Max), ['...']);
log_trunc_sc_spec(L, Max) ->
    lt_sc_spec(L, Max, []).

lt_sc_spec([], _, Acc) ->
    lists:reverse(Acc);
lt_sc_spec([{Name, L} | T], Max, Acc) ->
    lt_sc_spec(T, Max, [{Name, lt_scs(L, Max)} | Acc]).
    
lt_scs(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(L, Max), ['...']);
lt_scs(L, _) ->
    L.


%% tuncate show counters result
log_trunc_sc_vals(L) when is_list(L) ->
    log_trunc_sc_vals(L, pmsDb:pms_env_get(log_max));
log_trunc_sc_vals(Other) ->
    Other.

log_trunc_sc_vals(L, Max) when length(L) > Max ->
    lists:append(lists:sublist(lt_sc_vals(L, Max, []), Max), ['...']);
log_trunc_sc_vals(L, Max) ->
    lt_sc_vals(L, Max, []).


lt_sc_vals([], _, Acc) ->
    lists:reverse(Acc);
lt_sc_vals([{Name, V, O} | T], Max, Acc) ->
    lt_sc_vals(T, Max, [{Name, lt_scv(V, Max), O} | Acc]).
    
lt_scv(V, Max) when length(V) > Max ->
    lists:append(lists:sublist(V, Max), ['...']);
lt_scv(V, _) ->
    V.


%% truncate PMI bundle values
log_trunc_pmi_vals(Vals) ->
    ltv(Vals, 0, pmsDb:pms_env_get(log_max), []).


log_trunc_pmi_sc_vals(Vals) ->
    ltv2(Vals, 0, pmsDb:pms_env_get(log_max), []).


ltv([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltv(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltv([], _, _, Acc) ->
    lists:reverse(Acc);
ltv([{Grp, Vals} | T], N, Max, Acc) ->
    Res = {Grp, ltv2(Vals, 0, Max, [])}, 
    ltv(T, N + 1, Max, [Res | Acc]).


ltv2([], N, Max, Acc) when N >= Max ->
    lists:reverse(Acc);
ltv2(_, N, Max, Acc) when N >= Max ->
    lists:reverse(['...' | Acc]);
ltv2([], _, _, Acc) ->
    lists:reverse(Acc);
ltv2([{_, _} = Val | T], N, Max, Acc) ->
    ltv2(T, N + 1, Max, [Val | Acc]);
ltv2([{_, _, _} = Val | T], N, Max, Acc) ->
    ltv2(T, N + 1, Max, [Val | Acc]).
   

%%===========================================================================
%% get_version_attributes(Prefix) -> string()
%%===========================================================================
get_version_attributes(Prefix) ->
    vsn_attrs(modules(Prefix),
	      fun(W, M) -> ltf_vsn(W, M, vsn, fun vsn/1) end).

%%===========================================================================
%% get_compiled_attributes(Prefix) -> string()
%%===========================================================================

vsn_attrs(Modules, Fun)
  when is_list(Modules) ->
    W = 2 + widest(Modules),
    lists:foldl(fun(M, Acc) -> S = Fun(W, M), [S | Acc]  end,
		[], 
		Modules).

ltf_vsn(Width, Mod, Attr, VFun) ->
    Time = attr(Mod, date),
    Str  = io_lib:format(": ~*s ~s | ~s", 
			 [-Width, Mod, Time, attr(Mod, Attr, VFun)]),
    lists:flatten(Str).



vsn("/main/" ++ V) ->
    V;
vsn(T) when is_atom(T) ->
    vsn(atom_to_list(T)).

attr(Mod, Attr, VFun) ->
    try
	VFun(val(Attr, keyfetch(Attr, Mod:module_info(attributes))))
    catch
	_:_ ->
	    "-"
    end.

attr(Mod, Attr) ->
    attr(Mod, Attr, fun attr/1).

attr(T) when is_atom(T) ->
    atom_to_list(T);
attr(N) when is_integer(N) ->
    integer_to_list(N);
attr(V) ->
    case is_list(V) andalso lists:all(fun is_char/1, V) of
        true ->  %% string
            V;
        false ->
            io_lib:format("~p", [V])
    end.

is_char(C) ->
    0 =< C andalso C < 256.





keyfetch(Key, List) ->
    {Key, V} = lists:keyfind(Key, 1, List),
    V.


val(_, [V]) ->
    V.


%% widest/1

widest(List) ->
    lists:foldl(fun widest/2, 0, List).

widest(T, Max)
  when is_atom(T) ->
    widest(atom_to_list(T), Max);

widest(T, Max)
  when is_integer(T) ->
    widest(integer_to_list(T), Max);

widest(T, Max)
  when is_list(T) ->  %% string
    max(length(T), Max).



%%% ----------------------------------------------------------
%%% # modules(Prefix|Prefixes)
%%%
%%% Input: Prefix = atom()
%%%
%%% Description: Return the list of all loaded modules with the
%%%              specified prefix.
%%% ----------------------------------------------------------

modules(Prefix)
  when is_atom(Prefix) ->
    lists:sort(mods(Prefix));
modules(Prefixes)
  when is_list(Prefixes) ->
    lists:sort(lists:flatmap(fun modules/1, Prefixes)).

mods(Prefix) ->
    P = atom_to_list(Prefix),
    lists:filter(fun(M) ->
                         lists:prefix(P, atom_to_list(M))
                 end,
                 erlang:loaded()).


%%===========================================================================
%% Flex counter functions
%%===========================================================================
is_flex_group(PmGroup) when is_list(PmGroup); is_binary(PmGroup) ->
    lists:prefix(?FLEX_PREFIX, to_list(PmGroup));

is_flex_group({_ME, _SF, _PM, PmGroup}) ->
    is_flex_group(PmGroup).    

parallel_exec(Funs) ->
    Opts = [{timeout, 30000}, {results_order, match_task_reverse}],
    {ok, Res} = sysUtil:parallel_call(Funs, Opts),
    Res.


%%===========================================================================
%% sysUtil parallel execution functions
%%===========================================================================
%% pmapr_delay(Fun, Items, Time) when Time > 0 ->
%%     Opts = [{timeout, Time + 30000}, {results_order, match_task_reverse}],
%%     Length = length(Items),
%%     TimeSlice = Time/Length,
%%     DFun = fun({Item, N}) ->
%% 		   timer:sleep(round(N*TimeSlice)), 
%% 		   Fun(Item)
%% 	   end,
%%     ItemsN = lists:zip(Items, lists:seq(1, Length)),
%%     {ok, NewItems} = sysUtil:parallel_call(DFun, ItemsN, Opts),
%%     NewItems;

%% pmapr_delay(Fun, Items, _Time) ->
%%     pmapr_old(Fun, Items).


pmapr_old(Fun, Items) ->
    Opts = [{timeout, 30000}, {results_order, match_task_reverse}],
    pmap_old(Fun, Items, Opts).


pmap_old(Fun, Items, Opts) ->
    {ok, NewItems} = sysUtil:parallel_call(Fun, Items, Opts),
    NewItems.

%%===========================================================================
%% Parallel map functions
%%===========================================================================
pmapr_delay(Fun, Items, Time) ->
    pmapr_delay(Fun, Items, Time, 0, Time).


pmapr_delay(Fun, Items, Time, Min, Max) when Items =/= [] ->
    pmapr(Fun, Items, Time, Min, Max);

pmapr_delay(_Fun, Items, _Time, _Min, _Max) ->
    Items.


pmap(Fun, Items) ->
    pmapr(Fun, lists:reverse(Items)).


pmapr(Fun, Items) ->
    pmapr(Fun, Items, 0, 0, 0).


pmapr(Fun, Items, Delay, Min, Max) ->
    NProcs = 2*erlang:system_info(schedulers_online),
    pmapr(Fun, Items, Delay, Min, Max, NProcs).


pmapr(Fun, Items, Delay, Min, Max, N) when Items =/= [] ->
    Len = length(Items),
    NW = choose(Len >= N, N, Len),
    Receiver = spawn_receiver(Len),
    Workers = spawn_workers(NW, Fun, Receiver, []),
    Receiver ! {workers, self(), Workers},
    TimeSlice = get_delay_slice(Len, Delay, Min, Max),
    send_to_workers(TimeSlice, Workers, Items),     
    Res = receive_result(Receiver),
    stop_workers(Workers),
    Res;

pmapr(_Fun, Items, _Delay, _Min, _Max, _N) ->
    Items.

spawn_receiver(Len) ->
    Pid = self(),
    spawn(fun() ->
		  Workers = get_workers(Pid),
		  Res = receive_from_workers(Workers, Len),
		  Pid ! {res, self(), Res}
	  end).


get_workers(Pid) ->
    receive
	{workers, Pid, Workers} ->
	    Workers
    after 30000 ->
	    exit(normal)
    end.
    

receive_from_workers(Workers, N) ->
    receive_from_workers(N, Workers, Workers, []).


receive_from_workers(N, [Worker | RWorkers], Workers, Acc) when N > 0 ->
    receive
	{Worker, Res} when RWorkers =/= [] ->
	    receive_from_workers(N - 1, RWorkers, Workers, [Res | Acc]);
	{Worker, Res} ->
	    receive_from_workers(N - 1, Workers, Workers, [Res | Acc])
    after 30000 ->
	    throw({error, worker_receive_timeout})
    end;

receive_from_workers(_N, _RW, _W, Acc) ->
    Acc.


spawn_workers(N, Fun, Pid, Acc) when N > 0 ->
    Worker = spawn(fun() ->
			   worker_loop(Fun, Pid, self())
		   end),
    spawn_workers(N - 1, Fun, Pid, [Worker | Acc]);

spawn_workers(_N, _Fun, _Pid, Acc) ->
    Acc.
    

worker_loop(Fun, Pid, Self) ->
    receive
	{map, Item} ->
	    Pid ! {Self, Fun(Item)},	  
	    worker_loop(Fun, Pid, Self);
	stop ->
	    ok
    after 30000 ->
	    ok
    end.    
    

get_delay_slice(Len, Delay, Min, Max) when Len > 1, Delay > 0 ->
    Slice = erlang:trunc(Delay/Len),
    if Min =< Slice, Slice =< Max ->
	    Slice;
       Slice < Min ->
	    0;
       Slice > Max ->
	    Max
    end;

get_delay_slice(_Len, _Delay, _Min, _Max) ->
    0.


send_to_workers(Sleep, Workers, Items) when Sleep == 0 ->
    send_to_workers_no_sleep(Workers, Items, Workers);

send_to_workers(Sleep, Workers, Items) ->
    send_to_workers_sleep(Workers, Items, Sleep, Workers).
    

send_to_workers_no_sleep(RWorkers, [Item | RItems], Workers) ->
    NewWorkers = do_send_to_workers(RWorkers, Item, Workers),  
    send_to_workers_no_sleep(NewWorkers, RItems, Workers);

send_to_workers_no_sleep(_RWorkers, _Items, _Workers) ->
    ok.


send_to_workers_sleep(RWorkers, [Item | RItems], Sleep, Workers) ->
    timer:sleep(Sleep),
    NewWorkers = do_send_to_workers(RWorkers, Item, Workers),  
    send_to_workers_sleep(NewWorkers, RItems, Sleep, Workers);

send_to_workers_sleep(_RWorkers, _Items, _Sleep, _Workers) ->
    ok.


do_send_to_workers([Worker | RWorkers], Item, Workers) ->
    Worker ! {map, Item},
    if
	RWorkers =/= [] ->
	    RWorkers;
	true ->
	    Workers
    end.


receive_result(Pid) ->
    receive
	{res, Pid, Result} ->
	    Result
    after 30000 ->
	    throw({error, parallel_map_timeout})
    end.


stop_workers(Workers) ->
    lists:foreach(fun(Worker) ->    
			  Worker ! stop
		  end, Workers).


get_curr_delay_time(GP, GPEnd, Time, MinTime)
  when GP =:= ?TimePeriod_FIVE_MIN;
       GP =:= ?TimePeriod_ONE_MIN;
       GP =:= ?TimePeriod_FIFTEEN_MIN ->
    RemTime = 1000 * (GPEnd + get_rop_offset(GP) - epoch_time()),
    ?LOG_RAM(?SEV_1, 
	     {"Time remaining until ROP deadline = ~p ms~nMinTime = ~p ms~n",
	      [RemTime, MinTime]}),
    case RemTime - MinTime of
	Interval when Interval > Time ->
	    Time;
	Interval when Interval > 0 ->
	    Interval;
	_ ->
	    0
    end;

get_curr_delay_time(_GP, _GPEnd, _Time, _MinTime) ->
    0.


%%===========================================================================
%% Misc functions
%%===========================================================================
log(_, _) -> ok.


get_proc_info(Pid) ->
    Keys = [total_heap_size, message_queue_len, reductions],
    List = process_info(Pid),
    [{Key, proplists:get_value(Key, List)} || Key <- Keys].
			 


choose(true,  T, _) -> T;
choose(false, _, F) -> F.


to_list(L) when is_list(L) ->
    L;
to_list(B) when is_binary(B) ->
    binary_to_list(B);
to_list(I) when is_integer(I) ->
    integer_to_list(I);
to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(T) when is_tuple(T) ->
    tuple_to_list(T).


to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L).


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------




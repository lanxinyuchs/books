%%% ------------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ------------------------------------------------------------------------
%%% %CCaseFile:	pmsJobMeasInfo.erl %
%%% @private
%%% 
%%% Description:
%%%
%%% pmsJobMeasInfo process is started...
%%% ------------------------------------------------------------------------
-module(pmsJobMeasInfo).
-vsn('/main/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-30').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    --------------------------------------
%%% R6A/1      2016-05-12 eolaand     Created
%%% ------------------------------------------------------------------------
%%%
%%% ------------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ------------------------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ------------------------------------------------------------------------

-export([start/5, stop/1]).
-export([create_meas_info/2]).
-export([get_meas_info/1]).
-export([get_loop_data/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1]).

%% -compile(export_all).

-include("RcsPm.hrl").
-include("pms.hrl").

-define(GET_TO,   5000).
-define(ET, pmsLib:epoch_time()).
-define(MI_ID(PmGroup), "PM=1,PmGroup=" ++ PmGroup).
-define(ME, "ManagedElement").
-define(JID(Jid), element(4, Jid)).
-define(DEFAULT_BL_TO, 10).

-record(loop, {job_id, 
	       parent,
	       pm_group,
	       pm_group_dn,
	       gp         = 0,  %% granularity period   
	       rp         = 0,  %% reporting period
	       gp_end     = 0,
	       meas_tab,        %% ets table with measured types.
	       meas_vals  = [],
	       meas_info  = <<>>,
	       proc_type  = ?PROC_TYPE_UNDEFINED
	      }).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% start(JobId, PmGroup, RP, MeasTab) -> {ok, Pid}
%%
%% Start a new PM Job MeasInfo process.
%%========================================================================
start(JobId, PmGroup, RP, GP, MeasTab) ->
    Self = self(),
    {ok, spawn(fun() -> init({Self, JobId, PmGroup, RP, GP, MeasTab}) end)}.


%%========================================================================
%% stop(Pid) -> {ok, Pid}
%%
%% Stop a PM Job MeasInfo process.
%%========================================================================
stop(Pid) ->
    Pid ! stop,
    receive
	{'DOWN', _Ref, process, Pid, _Reason} ->
	    ok;
	{Pid, ok} ->
	    stop(Pid)
    after 10 ->
	    exit(Pid, kill),
	    ok
    end.

%%========================================================================
%% create_meas_info(Pid) -> ok.
%%
%% Create MeasInfo.
%%========================================================================
create_meas_info(Pid, NeMEId) ->
    Pid ! {create_meas_info, NeMEId}.

%%========================================================================
%% get_meas_info(Pid) -> {ok, MeasInfo}
%%
%% Get MeasInfo.
%%========================================================================
get_meas_info(Pid) ->
    Pid ! {get_meas_info, self()},
    receive
	{ok, Pid, MeasInfo} ->
	    {ok, MeasInfo};
	{'DOWN', _Ref, process, Pid, Reason} ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"pmsJobMeasInfo ~p is DOWN: ~p~n", [Pid, Reason]}),
	    {error, slave_is_down}
    end.

%%========================================================================
%% get_loop_data(Pid) -> ok
%%========================================================================
get_loop_data(Pid) ->
    Pid ! {get_loop_data, self()},
    receive
	{loop_data, Res} ->
	    Res
    after 10000 ->
	    undefined
    end.
    
%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% init({Parent, JobId, PmGroup, RP, GP}) -> loop/1
%%
%% Init function for the PM Job MeasInfo slave process.
%%========================================================================
init({Parent, JobId, PmGroup, RP, GP, JobMeasTab} = _InData) -> 
    put(name, {?MODULE, JobId}), %% used by pms_cli to print process data
    ?LOG_RAM_SLAVE(Parent, ?SEV_5, 
		   {"Started PmJob MeasInfo slave ~p~nPmGroup = ~s~n"
		    "PmJob ~p (~p)~n", 
		    [self(), PmGroup, JobId, Parent]}), 
    erlang:monitor(process, Parent),
    ProcType = get_proc_type(PmGroup),
    MeasTab = get_meas_tab(ProcType, PmGroup, JobMeasTab, Parent),
    Loop = #loop{job_id    = JobId,
		 %% pm_group  = to_list(PmGroup), 
		 pm_group  = to_binary(PmGroup), 
		 meas_tab  = MeasTab,
		 gp        = GP,
		 rp        = RP,
		 meas_vals = [],
		 meas_info = <<>>,
		 parent    = Parent,
		 proc_type = ProcType
		},
    loop(Loop).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%========================================================================
%% loop(#loop{}) -> void() 
%%
%% Job MeasInfo loop. Loop until the PM Job is deleted.
%% 
%%========================================================================
loop(#loop{job_id    = JobId,
	   parent    = Parent,
	   pm_group  = PmGroup, 
	   meas_tab  = _MeasTab,
	   gp        = GP,
	   gp_end    = _GPEnd,
	   rp        = _RP} = Loop) ->
    receive
	{meas_values, From, RGPEnd, MeasVals} ->
	    ?LOG_RAM_SLAVE(Parent, 
			   [{?SEV_1, 
			     {" meas_values ~n"
			      "  PmGroup    = ~p~n",
			      [PmGroup]}},
			    {?SEV_5, 
			     ?LFUN({" meas_values ~n"
				    "  JobId    = ~p~n"
				    "  GP       = ~p~n"
				    "  GPEnd    = ~p~n"
				    "  MeasVals = ~p~n",
				    [JobId, GP, RGPEnd, 
				     log_trunc_mv([{PmGroup, MeasVals}])]})}
			   ]),
	    NewLoop = handle_meas_info_data(RGPEnd, MeasVals, Loop),
	    From ! {self(), ok},
	    loop(NewLoop);
	{create_meas_info, NeMEId} ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_5, 
			   {" create_meas_info ~n"
			    "  PmGroup    = ~p~n",
			    [PmGroup]}),
	    NewLoop = prepare_build_if_flex(Loop),
	    MeasInfo = build_meas_info(NewLoop, NeMEId),
	    loop(NewLoop#loop{meas_info = MeasInfo, meas_vals = []});
	{get_meas_info, From} ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_5, 
			   {" get_meas_info ~n"
			    "  PmGroup    = ~p~n",
			    [PmGroup]}),
	    From ! {ok, self(), Loop#loop.meas_info},
	    loop(Loop#loop{meas_info = <<>>});
	%%-------------------------------------------------------------
	%% Read loop data
	%%-------------------------------------------------------------
	{get_loop_data, UserPid} ->
	    UserPid ! {loop_data, format_loop(Loop)},
	    loop(Loop);

	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    ok;
	stop ->
	    ok
    end.
	    

%%========================================================================
%% get_meas_tab(ProcType, PmGroup, MeasTab) -> MeasTab.
%%
%% Create new measTab if flex PmGroup.
%% 
%%========================================================================
get_meas_tab(?PROC_TYPE_FLEX, PmGroup, _MeasTab, Parent) ->
    ?LOG_RAM_SLAVE(Parent, 
		   ?SEV_1, 
		   {" Create new measTab for Flex counters ~n"
		    "  PmGroup    = ~p~n",
		    [PmGroup]}),
    ets:new(measuredFlex, [ordered_set]);

get_meas_tab(_Type, _PmGroup, MeasTab, _Parent) ->
    MeasTab.


%%========================================================================
%% handle_meas_info_data(RGPEnd, MeasVals, #loop{}) -> #loop{} 
%%
%% Accumulate MeasValues
%% 
%%========================================================================
handle_meas_info_data(_RGPEnd, [], Loop) ->
    Loop; 

handle_meas_info_data(RGPEnd, GroupMeasVals, Loop) 
  when Loop#loop.gp_end =:= RGPEnd ->
    NewAccVals = acc_meas_values(GroupMeasVals, Loop#loop.meas_vals),
    Loop#loop{meas_vals = NewAccVals};

handle_meas_info_data(RGPEnd, [{LDN, MTMeasVals}], Loop) ->
    AccMeasVals = [{LDN, format_meas_vals(MTMeasVals)}],
    Loop#loop{meas_vals = AccMeasVals, gp_end = RGPEnd};

handle_meas_info_data(RGPEnd, GroupMeasVals, Loop) ->
    Fun = fun({LDN, MTMeasVals}) -> {LDN, format_meas_vals(MTMeasVals)} end,
    %% TS1 = os:timestamp(),
    AccMeasVals = pmsLib:pmapr_delay(Fun, GroupMeasVals, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time handle_meas_info_data: ~p ms~n", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    Loop#loop{meas_vals = AccMeasVals, gp_end = RGPEnd}.


format_meas_vals(MeasVals) ->
    [{MT, format_mt_vals(Vals)} || {MT, Vals} <- MeasVals, Vals =/= []].


format_mt_vals([Val | _] = Vals) when is_integer(Val) ->
    [Vals].


acc_meas_values(Values, OldVals) ->
    acc_meas_values(Values, OldVals, []).


acc_meas_values([{_MT, [Val | _]} | _] = MTVals, OldVals, _Acc) 
  when is_integer(Val) ->
    acc_mt_values(MTVals, OldVals);

acc_meas_values([{Key, Vals} | T], OldVals, Acc) when Vals =/= [] ->
    case lists:keytake(Key, 1, OldVals) of
	false ->
	    NewAcc = [{Key, format_meas_vals(Vals)} | Acc],
	    acc_meas_values(T, OldVals, NewAcc);
	{value, {_Key, AccVals}, RemOldVals} ->
	    NewAccVals = acc_meas_values(Vals, AccVals, []),
	    NewAcc = [{Key, NewAccVals} | Acc],
	    acc_meas_values(T, RemOldVals, NewAcc)
    end;
    
acc_meas_values([{_Key, []} | T], OldVals, Acc) ->
    acc_meas_values(T, OldVals, Acc);

acc_meas_values([], [], Acc) ->
    lists:keysort(1, Acc);

acc_meas_values([], OldVals, Acc) ->
    lists:keysort(1, Acc ++ OldVals).


acc_mt_values(MTVals, AccMTVals) ->
    lists:foldl(fun({MT, Val}, Acc) ->
			case lists:keyfind(MT, 1, AccMTVals) of
			    {_MT, Vals} ->
				NewMTVal = {MT, [Val | Vals]},
				lists:keyreplace(MT, 1, Acc, NewMTVal); 
			    _ ->
				[{MT, [Val]} | Acc]
			end
		end, AccMTVals, MTVals).

%%========================================================================
%% prepare_build_if_flex(Loop) -> NewLoop.
%% 
%%========================================================================
prepare_build_if_flex(Loop) when Loop#loop.proc_type =/= ?PROC_TYPE_FLEX ->
    Loop;

prepare_build_if_flex(#loop{parent    = Parent,
			    pm_group  = PmGroup, 
			    meas_tab  = MeasTab,
			    meas_vals = MeasVals} = Loop) ->
    update_meas_tab(PmGroup, MeasVals, MeasTab, Parent),
    SortedMeasVals = [{LDN, sort_flex_vals(MTVs)} || {LDN, MTVs} <- MeasVals],
    Loop#loop{meas_vals = SortedMeasVals}.


sort_flex_vals(MTVs) ->
    lists:sort(fun({{_BaseMT1, FlexMT1}, _V1}, {{_BaseMT2, FlexMT2}, _V2}) ->
		       FlexMT1 =< FlexMT2
	       end, MTVs).

%%===================================================================
%% update_meas_tab(PmGroup, MeasVals, MeasTab, Parent) -> void()
%%
%%  MeasVals = [{LDN, [{MT, Vals}]}]
%%
%% Populate measTab if flex counters
%%===================================================================
update_meas_tab(PmGroup, MeasVals, MeasTab, Parent) ->
    AllFlexMTs = lists:append([[FlexMT || {{_BaseMT, FlexMT}, _V} <- MTVs] || 
				  {_LDN, MTVs} <- MeasVals]),
    FlexMTs = lists:usort(AllFlexMTs),
    ?LOG_RAM_SLAVE(Parent, 
		   [{?SEV_1, 
		     {" Populate Flex measTab ~n"
		      "  PmGroup    = ~p~n",
		      [PmGroup]}},
		    {?SEV_5, 
		     ?LFUN({" Populate Flex measTab ~n"
			    "  PmGroup    = ~p~n~n  MeasurementTypes:~n" ++ 
				lists:append(["    " ++ to_list(FlexMT) ++ 
						  "~n" || FlexMT <- FlexMTs]),
			    [PmGroup]})}
		   ]),
    ets:delete_all_objects(MeasTab),
    lists:foldl(fun(FlexMT, N) ->
			ets:insert(MeasTab, {{PmGroup, FlexMT}, N}),
			N + 1
		end, 1, FlexMTs),
    ok.

%%========================================================================
%% build_meas_info(Loop) -> MeasInfo.
%% 
%%========================================================================
build_meas_info(#loop{job_id    = JobId,
		      parent    = Parent,
		      pm_group  = PmGroup, 
		      meas_tab  = MeasTab,
		      gp        = GP,
		      gp_end    = GPEnd,
		      rp        = RP,
		      meas_vals = MeasVals}, NeMEId) when MeasVals =/= [] ->
    MeasValues = build_meas_values(MeasVals, PmGroup, MeasTab, NeMEId, Parent),
    MiId = ?MI_ID(PmGroup),
    case group_mts_from_tab(PmGroup, MeasTab) of
	MeasTypes when MeasValues =/= [], MeasTypes =/= []  ->
	    build_meas_data_xml(MiId, JobId, GP, GPEnd, RP, MeasTypes, 
				MeasValues);
	[] ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_ERROR,
			   {"PmGroup ~p not found among subscribed in "
			    "measTab~nwhen building xml~n",
			    [PmGroup]}),
	    [];
	_ ->
	    []
    end;

build_meas_info(#loop{parent    = Parent,
		      pm_group  = PmGroup}, _NeMEId) ->
    ?LOG_RAM_SLAVE(Parent, 
		   ?SEV_WARNING,
		   {"No measurement values received for PmGroup ~p~n",
		    [PmGroup]}),
    [].


build_meas_data_xml(MiId, JobId, GP, GPEnd, RP, MeasTypes, MeasValues) ->
    Job   = pmsRopXml:job(?JID(JobId)),
    GranP = pmsRopXml:granPeriod(GP, GPEnd),
    RepP  = pmsRopXml:repPeriod(RP),
    Types = pmsRopXml:measTypes(MeasTypes),
    %% use ecim for now
    %% MValFormat = pmsLib:get_rop_multi_val_format(),
    Fun = fun({LDN, Suspect, MTVals}) -> 
		  pmsRopXml:measValue(LDN, Suspect, MTVals) 
	  end,
    %% TS1 = os:timestamp(),
    Values = pmsLib:pmapr_delay(Fun, MeasValues, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time build_meas_data_xml: ~p ms", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    pmsRopXml:measInfo(MiId, Job, GranP, RepP, Types, Values).

%%===================================================================
%% build_meas_values(MeasValData, Group, MeasTab, NeMEId, Parent) -> 
%%     MeasValues
%%
%%  MeasValData = [{LDN, [{MT, Vals}]}]
%%
%%  MeasValues = [{LDN, Susp, MeasVals}]
%%
%% Aggregate measurement values and perform basic sanity check.
%% Also set the ME of the LDN to correct value.
%%===================================================================
build_meas_values(MeasValData, Group, MeasTab, NeMEId, Parent) ->
    Fun = 
	fun({LDN, MTVals}) ->
		case aggregate_group_mt_vals(MTVals, Group, MeasTab, Parent) of
		    {MeasVals, SuspV} when MeasVals =/= [] ->
			Susp = SuspV 
			    orelse 
			    not num_of_gmt_ok(MTVals, Group, MeasTab, Parent)
			    orelse 
			    not num_of_mtv_ok(MTVals, Parent),
			[{update_ldn_me_id(NeMEId, LDN), Susp, MeasVals}];
		    _ ->
			[]
		end
	end,
    %% TS1 = os:timestamp(),
    AccMeasVals = pmsLib:pmapr_delay(Fun, MeasValData, 0),
    %% TS2 = os:timestamp(),
    %% ?LOG_RAM(?SEV_9, {"Execution time build_meas_values: ~p ms", 
    %% 		      [timer:now_diff(TS2, TS1)/1000]}),
    lists:append(AccMeasVals).



%%===================================================================
aggregate_group_mt_vals(MTVals, Group, MeasTab, Parent) ->
    F = fun({{BaseId, FlexMTId}, Vals}, AccSusp) ->
		Key = {Group, FlexMTId},
		aggregate_mt_vals(BaseId, _Flex = true, Vals, AccSusp, Group, 
				  Key, MeasTab, Parent);
	   ({MTId, Vals}, AccSusp) ->
		Key = {Group, MTId},
		aggregate_mt_vals(MTId, _Flex = false, Vals, AccSusp, Group, 
				  Key, MeasTab, Parent)
 	end,
    {MeasVals, SuspV} = lists:mapfoldl(F, false, MTVals),
    {[MeasVal || MeasVal <- MeasVals, MeasVal =/= []], SuspV}.
		 

aggregate_mt_vals(MTId, Flex, Vals, AccSusp, Group, Key, MeasTab, Parent) ->
    case ets:lookup(MeasTab, Key) of
	[{_Key, SeqNo}] ->
	    pmsLib:aggregate_mt_vals(Group, MTId, Flex, SeqNo, Vals, 
				     AccSusp);
	_ ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_WARNING, 
			   {"~p not found among subscribed in "
			    "measTab~n", [Key]}),
	    {[], AccSusp}
    end.
 
%%===================================================================
num_of_gmt_ok(MTVs, Group, MeasTab, Parent) ->
    case group_mts_from_tab(Group, MeasTab) of
	MeasTypes when length(MTVs) =:= length(MeasTypes) ->
	    true;
	MeasTypes when MeasTypes =/= [] ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_WARNING,
			   ?LFUN({"Unexpected number of measurements received "
				  "for ~p~nSuspect mark measValue~n"
				  "  Expected = ~p~n"
				  "  Got      = ~p~n", 
				  [Group, length(MeasTypes), length(MTVs)]}),
			   ?DEFAULT_BL_TO),
	    false;
	_ ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_WARNING, 
			   {"Group ~p not found among subscribed in measTab~n"
			    "Suspect mark measValue~n",
			    [Group]},
			   ?DEFAULT_BL_TO),
	    false
    end.


num_of_mtv_ok(MTVals, Parent) ->
    case is_num_of_mtv_ok(MTVals) of
	true = Res ->
	    Res;
	False ->
	    ?LOG_RAM_SLAVE(Parent, 
			   ?SEV_WARNING, 
			   {"Inconsistent length of measurement values~n"
			    "Suspect mark measValue~n~p~n",
			    [MTVals]},
			   ?DEFAULT_BL_TO),
	    False
    end.


is_num_of_mtv_ok([{_MT, Vals} | MTVals]) ->
    Len = length(Vals),
    F = fun({_, V}) ->
		length(V) =:= Len
	end,
    lists:all(F, MTVals);

is_num_of_mtv_ok([]) ->
    false.



update_ldn_me_id(NeMEId, LDN) when is_list(NeMEId) ->
    {ME, T} = lists:splitwith(fun(Ch) -> Ch =/= $, end, to_list(LDN)),
    case ME of
	?ME ++ _ ->
	    ?ME ++ "=" ++ NeMEId ++ T;
	_ ->
	    LDN
    end;

update_ldn_me_id(_, LDN) ->
    LDN.


group_mts_from_tab(Group, Tab) ->
    ets:select(Tab, [{{{'$1', '$2'}, '$3'}, [{'==', '$1', Group}], 
		      [{{'$3','$2'}}]}]).

%%===================================================================
get_proc_type(PmGroup) ->
    case pmsLib:is_flex_group(PmGroup) of
	true ->
	    ?PROC_TYPE_FLEX;
	_False ->
	    ?PROC_TYPE_COMMON
    end.

%%===================================================================
to_list(Term) ->
    pmsLib:to_list(Term).


to_binary(Term) ->
    pmsLib:to_binary(Term).


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
log_trunc_mv(Bundles) ->
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
    ltb_ldn(T, N + 1, Max, [{Ldn, pmsLib:log_trunc_list(Vals)} | Acc]).


format_loop(Loop) ->
    F       = record_info(fields, loop),
    [_ | L] = tuple_to_list(Loop), 
    lists:zip(F,L).

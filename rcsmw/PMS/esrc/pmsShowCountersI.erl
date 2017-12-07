%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsShowCountersI.erl %
%%%
%%% Description:
%%%
%%%
%%%
%%% ----------------------------------------------------------
-module(pmsShowCountersI).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/3').
-date('2017-05-03').
-author('eolaand').
-shaid('7f3132ad36385027be85c6fdb86205e06b909ae5').
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
%%% Rev        Date         Name        What
%%% -----      -------      --------    ------------------------
%%% R1A/1      2014-05-23   uabesvi     Created
%%%
%%% R8A/1      2016-12-20   edamkon     Attempted to fix HV49197
%%% R8A/2      2016-12-20   edamkon     Fix wrong TR number in R8A/1 comment
%%% R8A/4      2017-01-12   edamkon     Fix for HV49197; Merge SPI1 and SPI2
%%% R9A/2      2017-02-22   eolaand     Remove support for obsolete SPI1
%%% R9A/5      2017-02-28   eolaand     Improve logging and clean up 
%%% R9A/6      2017-03-21   eolaand     First part of removal of PMI/PMI2 
%%%                                     knowledge above pmsAppJob.
%%%                                     Still some parts left for backwards 
%%%                                     compatibility. To be removed.
%%% R10A/2     2017-04-19   eolaand     Final part of removal of PMI/PMI2. 
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/1]).
-export([get_measurements/3]).
-export([get_measurement_names/1]).
-export([get_job_ids/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-include("pms.hrl").
-include("RcsPm.hrl").

-define(DEFAULT_BL_TO, 10).
%% stolen from comte_types.hrl
-define(INT64, 4).
-define(INT64(Int), {?INT64, Int}).

%%=====================================================================
%% start(Args) -> ok
%%
%% COM callback function
%%=====================================================================
start(_) ->
    ok.

%%=====================================================================
%% get_measurements(Dn, Names, Options) -> 
%%
%% Get measurements for the given MO.
%%
%% Tries PMI first, and if that fails, then try PMI2.
%% If both fail, return PMI error, for legacy reasons.
%%
%% As of R8A/3, same function handles both SPI v1 and v2.
%%=====================================================================
get_measurements(DnBin, NamesBin, Options) when DnBin =/= <<>> ->
    DnString = to_list(DnBin),
    DnSplit = string:tokens(DnString, ","),
    DN = gm_ldn_format(DnSplit),
    Names = [to_list(N) || N <- NamesBin],
    TruncatedNames = pmsLib:log_trunc_sc_names(Names),
    IsVerbose = proplists:get_value(verbose, Options, false),
    ?LOG_RAM(?SEV_1, 
	     {"Get measurements~n"
	      "  Ldn     = ~p~n"
	      "  Names   = ~p~n"
	      "  Options = ~p~n",
	      [DnBin, TruncatedNames, Options]}, ?DEFAULT_BL_TO),
    Res = pmsShowCounters:show_counters(DN, Names, Options),
    ParsedRes = gm2_rc(Res, IsVerbose),
    ?LOG_RAM(?SEV_1, {"Get measurements. Result for LDN = ~p~n  ~p~n",
		      [DN, pmsLib:log_trunc_sc_vals(ParsedRes)]}, 
	     ?DEFAULT_BL_TO),
    ParsedRes;

get_measurements(_, _Names, _Options) ->
    {error, not_exist}.


%%------------------------
%% ok, no values
%%------------------------
gm2_rc({ok, {?SC_OK, _, []}}, _) ->
    [];
%%------------------------
%% ok, verbose mode
%%------------------------
gm2_rc({ok, {?SC_OK, "", Data}}, true) ->
    gm2_verbose(aggregate(Data), undefined, []);
gm2_rc({ok, {?SC_OK, ErrorStr, Data}}, true) ->
    ErrorInfo = {error_information, to_binary(ErrorStr)},
    gm2_verbose(Data, ErrorInfo, []);
%%------------------------
%% ok, non verbose mode
%%------------------------
gm2_rc({ok, {?SC_OK, "", Data}}, _) ->
    gm2_non_verbose(aggregate(Data), [], []);
gm2_rc({ok, {?SC_OK, ErrorStr, Data}}, _) ->
    Opts = [{error_information, to_binary(ErrorStr)}],
    gm2_non_verbose(Data, Opts, []);
%%------------------------
%% error
%%------------------------
gm2_rc({ok, {Error, "", _Res}}, _) ->
    {error, gm_error(Error)};
gm2_rc({ok, {Error, ErrorStr, _Res}}, _) ->
    {error, gm_error(Error), to_binary(ErrorStr)}.


gm2_verbose([], _, Acc) ->
    lists:usort(lists:append(Acc));
gm2_verbose([{PmGrp, [{_LDN, Vals}]} | T], ErrorInfo, Acc) ->
    Empty = gm2_v_get_empty(PmGrp, Vals),
    Res   = gm2v(get_verbose_data(PmGrp, Vals, ErrorInfo, []), Vals) ++ Empty,
    gm2_verbose(T, ErrorInfo, [Res | Acc]).


gm2v([], Vals) ->
    [gm2nv(V, []) || V <- Vals];
gm2v(VerboseData, Vals) ->
    Res = [[gm2nv(V, tuple_to_list(VD)) || V <- Vals] || VD <- VerboseData],
    lists:append(Res).
	

gm2_non_verbose([], _Opts, Acc) ->
    lists:sort(lists:append(Acc));
gm2_non_verbose([{_, [{_, Data}]} | T], Opts, Acc) ->
    Res = [gm2nv(D, Opts) || D <- Data],
    gm2_non_verbose(T, Opts, [Res | Acc]).


%% Flex counter
gm2nv({{_, FlexC}, V}, Opts) ->
    gm2nv({FlexC, V}, Opts);
%% Common counter
gm2nv({N, {V, S}}, Opts) ->
    {to_binary(N), gm2_values(V, S), Opts};
gm2nv({N, V}, Opts) ->
    {to_binary(N), gm2_values(V), Opts}.
    

gm2_v_get_empty(PmGrp, Vals) ->
    {ok, MTs}  = pmsDb:measurement_type_match({"1", "1", "1", to_list(PmGrp)}),
    Cnts = [to_binary(C) || 
	       #measurementType{measurementTypeId = {_,_,_,_,C}} <- MTs],
    [{NE, [], []} || NE <- Cnts, proplists:get_value(NE, Vals) == undefined].
    


%%=====================================================================
%% get_measurement_names(Dn) -> ok
%%=====================================================================
get_measurement_names(Dn) ->
    ?LOG_RAM(?SEV_5, 
	     {"Get measurement names.~n"
	      "  Ldn = ~p~n",
	      [Dn]}),
    DnF = gm_ldn_format(string:tokens(to_list(Dn), ",")),
    Res = pmsShowCountersLib:show_counter_names(DnF),
    ?LOG_RAM(?SEV_5, 
	     {"Get measurement names result for LDN = ~p~n"
	      "  ~p~n",
	      [Dn, Res]}),
    Res.


%%=====================================================================
%% get_job_ids(Dn) -> ok
%%
%% COM callback function.
%% Find all jobs associated with the Dn.
%%=====================================================================
get_job_ids(Dn) ->
    ?LOG_RAM(?SEV_5, 
	     {"Get job IDs.~n"
	      "  Ldn   = ~p~n",
	      [Dn]}),
    DnF = gm_ldn_format(string:tokens(to_list(Dn), ",")),

    Res = pmsShowCountersLib:get_job_ids(DnF),

    ?LOG_RAM(?SEV_5, 
	     {"Get job IDs result for LDN = ~p~n"
	      "  ~p~n",
	      [Dn, Res]}),
    Res.
    
%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

gm_error(?SC_NO_COUNTERS) ->
    ?LOG_RAM(?SEV_WARNING, {"Error = ~p~n", [?SC_NO_COUNTERS]}),
    not_exist;
gm_error(Error) ->
    ?LOG_RAM(?SEV_WARNING, {"Error = ~p~n", [Error]}),
    failure.


gm_ldn_format([_ME | T]) ->
    string:join(["ManagedElement=1" | T], ",").
    
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%=====================================================================
%% aggregate(ScRes) -> FromattedRes
%% 
%% ScRes        = [{Group, [{Ldn, [{Counter, [Value]}]}]}]
%% FormattedRes = [{Group, [{Ldn, [{Counter, {[Value], Suspect}}]}]}]
%% Group        = string()
%% Ldn          = string()
%% Counter      = string()
%% Value        = integer()
%% Suspect      = boolean()
%% 
%% Aggregate if multi answers from the application
%% Reusing the same aggretation as for ROP files
%% thus the LDN is used also for show counters
%% even if there is only the one (the requested) LDN involved.
%%
%%=====================================================================
aggregate(Res) ->
    %%------------------------------------------------------------
    %% CollectedGrps = [{Group, [[{Ldn, [{Counter, [Value]}]}]]}]
    %%------------------------------------------------------------
    CollectedGrps = pmsLib:key_collect(Res),
    %%------------------------------------------------------------
    %% CollectedCnts = [{Group, [{Ldn, [[{Counter, [Value]}]]}]}]
    %%------------------------------------------------------------
    CollectedCnts = [{Grp, pmsLib:key_collect(lists:flatten(LDN))} || 
			{Grp, LDN} <- CollectedGrps],
    %%------------------------------------------------------------
    %% AggregatedCnts = [{Group, [{Ldn, NotUsed}], [{Counter, [[Value]]}]}] 
    %%------------------------------------------------------------
    AggregatedCnts = aggregate_cnt(CollectedCnts, []),
    %%------------------------------------------------------------
    %% AggregatedRes = [{Group, [{Ldn, [{Counter, [Value], Suspect}]}]}]
    %%------------------------------------------------------------
    AggregatedRes = [pmsLib:aggregate_mt_for_sc(Vals, Grp, Ldn) || 
			{Grp, [{Ldn, _}], Vals} <- AggregatedCnts],
    %%------------------------------------------------------------
    %% FormattedRes = [{Group, [{Ldn, [{Counter, {[Value], Suspect}}]}]}]
    %%------------------------------------------------------------
    aggregate_format(AggregatedRes, []).


aggregate_cnt([], Acc) ->
    Acc;
aggregate_cnt([{Grp, Ldn} | T], Acc) ->
    Vals = flat([pmsLib:key_collect(flat(V)) || {_, V} <- Ldn]),
    aggregate_cnt(T, [{Grp, Ldn, Vals} | Acc]).


aggregate_format([], Acc) ->
    Acc;
aggregate_format([{Grp, [{Ldn, Vals}]} | T], Acc) ->
    ValsF = af(Vals, []),
    aggregate_format(T, [{Grp, [{Ldn, ValsF}]} | Acc]).


af([], Acc) ->
    Acc;
af([{C, V, S} | T], Acc) ->
    af(T, [{C, {(V), S}} | Acc]).

%%=====================================================================
%% format the counter values to COM format
%%
%% if one counter has value -1 the all should be suspect marked
%% if a counter has value -2 only that counter is suspect marked 
%%=====================================================================
gm2_values(Vals) ->
    Suspect = [] /= [Susp || -1 = Susp <- Vals],
    gm2_v(Vals, Suspect, []).
    
gm2_values(Vals, false) ->
    Suspect = [] /= [Susp || -1 = Susp <- Vals],
    gm2_v(Vals, Suspect, []);
%% aggregation says suspect mark
gm2_values(Vals, true) ->
    gm2_v(Vals, true, []).
    
    

gm2_v([], _, Acc) ->
    lists:reverse(Acc);
%% suspect mark if -2 
gm2_v([-2 = H | T], Suspect, Acc) ->
    gm2_v(T, Suspect, [{?INT64(H), true} | Acc]);
gm2_v([H | T], Suspect, Acc) ->
    gm2_v(T, Suspect, [{?INT64(H), Suspect} | Acc]).


%%=====================================================================
%% 
%%=====================================================================
get_verbose_data(_PmGrp, [], ErrorInfo, Acc) ->
    JobIds  = lists:append(Acc),
    Jobs    = [{job_id, to_binary(Job)} || {_, _, _, Job} <- JobIds],
    JobRecs = [pmsDb:pm_job_dirty_get(JobId) || JobId <- JobIds],
    GPs     = [{gp_in_seconds,pmsLib:get_interval(GP)} || 
		  {ok, [#pmJob{granularityPeriod = GP}]} <- JobRecs],
    gvd_rc(Jobs, GPs, ErrorInfo);
get_verbose_data(PmGrp, [{Type, _Vals} | T], ErrorInfo, Acc) ->
    MrG  = pmsDb:measurement_reader_match_pm_group(to_list(PmGrp)),
    MrT  = pmsDb:measurement_reader_match_meas_type(to_list(PmGrp), 
						    to_list(Type)),
    Jobs = lists:usort(gvd(MrG, MrT)),
    get_verbose_data(PmGrp, T, ErrorInfo, [Jobs | Acc]).


gvd_rc(Jobs, GPs, undefined) ->
    lists:zip(Jobs, GPs);
gvd_rc(Jobs, GPs, ErrorInfo) ->
    EI      = lists:duplicate(length(Jobs), ErrorInfo),
    lists:zip3(Jobs, GPs, EI).


    
gvd({ok, MrG}, {ok, MrT}) ->
    Mids = [Mid || #measurementReader{measurementReaderId = Mid} <- MrG ++ MrT],
    [{ME, SF, PM, J} || {ME, SF, PM, J, _} <- Mids].



flat(List) ->
    lists:flatten(List).


to_binary(Term) ->
    pmsLib:to_binary(Term).


to_list(Term) ->
    pmsLib:to_list(Term).

%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

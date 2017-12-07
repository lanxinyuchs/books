%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_load_test_lib.erl %
%%% @private
%%% @doc
%%% == Library functions for load test suite ==
%%% 
%%% @end

-module(pms2_load_test_lib).
-vsn('/main/R5A/R6A/R8A/R9A/R10A/1').
-date('2017-04-12').
-author('eolaand').
-shaid('d72ab94947b83b0675b19cea84fbe5442686de2a').

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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-04-13 edamkon     Created
%%% R6A/3      2016-06-29 edamkon     Implement parsing ROP with SAX
%%% ----------------------------------------------------------

-export([run_load_test/2,
	 start_load_test/1,
	 end_load_test/0,
	 wait_n_rp/2,
	 check_rop/1,
	 check_rop/3,
	 log_rop_files/1,
	 log_memory_usage/4,
	 test/1
	]).


-include("pms2_test.hrl").
-include("pms2_load_test.hrl").
-include_lib("common_test/include/ct.hrl").


%% ====================================================================
%% SAX parser record definitions
%% ====================================================================
-record(measVal, {
    measType :: string(),
    value    :: string()
}).

-record(object, {
    name        :: string,
    values = [] :: [#measVal{}]
}).

-record(pmGroup, {
    name           :: string(),
    measTypes = [] :: [string()],
    objects   = [] :: [#object{}]
}).

-record(pmJob, {
    jobId    :: string(),
    pmGroups :: [#pmGroup{}]
}).

%% -------------------------------------------
%% Record describing the SAX parser state. All
%% the fields beginning with "curr" hold state
%% for the current "measInfo" ROP XML fragment
%% -------------------------------------------
-record(state, {
    currJobId             :: string(),
    currPmGroup           :: string(),
    currMeasTypes = []    :: [string()],
    currObjects   = []    :: [#object{}],
    currObjName           :: string(),
    currObjVals   = []    :: [#measVal{}],
    currXmlText           :: string(),
    
    jobs          = []    :: [#pmJob{}],
    suspectFound  = false :: boolean()
}).


%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%--------------------------------------------------------------------
%% run_load_test(NumOfRPs, Options) -> ok.
%%
%% Runs the PMS Load test with given options, and waits until a
%% specified number of RPs pass before checking ROP file contents.
%%--------------------------------------------------------------------
run_load_test(NumOfRPs, Options) ->
    ct:pal("Starting test with following options:~n~p", [Options]),

    RP    = proplists:get_value(rp, Options, 1),
    NProcsBefore = rpc(erlang,system_info,[process_count]),
    BytesBefore  = rpc(erlang,memory,[MemType = processes]),
    _Pid  = start_load_test(Options),
    %% ok   = ?LOAD:subscribe(),
    ok    = wait_until_subscribe_rop(),
    ok    = wait_n_rp(RP, NumOfRPs),
    NProcsDuring = rpc(erlang,system_info,[process_count]),
    wait_n_rop_files(NumOfRPs - 1),	
    NProcsAfter = rpc(erlang,system_info,[process_count]),
    %% Sleep = ?RP_TO_GP(RP) div 2,
    %% ct:pal("Timer sleep value: ~ps~n", [Sleep]),
    %% timer:sleep(1000 * Sleep),

    ct:pal("=== Number of processes on CS Node ===~n"
           "Before: ~p~n"
           "During: ~p~n"
           "After:  ~p~n"
           "Delta:  ~p~n",
           [NProcsBefore, 
	    NProcsDuring, 
	    NProcsAfter, 
	    NProcsAfter - NProcsBefore]),

    Fun = fun(MT) -> rpc(erlang,memory,[MT]) end,
    log_memory_usage("CS Node", BytesBefore, MemType, Fun),

    ok    = end_load_test(),
    ok    = check_rop(Options).

%%--------------------------------------------------------------------
%% start_load_test(PartialOptions) -> PmsLoadAppPid.
%%
%% Starts up the PMS load test.
%%--------------------------------------------------------------------
start_load_test(Options) ->
    ct:pal("=== Starting load test ==="),

    ct:pal("Starting pmsLoadApp with given options..."),
    {ok, Pid} = ?LOAD:start(Options),
    
    ok   = ?LOAD:subscribe(),
    ct:pal("initializing pmsLoadApp..."),
    ok = ?LOAD:initialize(),
    timer:sleep(5000),
    ct:pal("Creating pmJobs..."),
    ok = ?LOAD:create_pm_jobs(),
    %% timer:sleep(10000),
    %% ct:pal("initializing pmsLoadApp..."),
    %% ok = ?LOAD:initialize(),

    ct:pal("=== Load test started ==="),

    Pid.


%%--------------------------------------------------------------------
%% end_load_test() -> ok.
%%
%% Gracefully ends the PMS load test.
%%--------------------------------------------------------------------
end_load_test() ->
    ct:pal("=== Stopping pmsLoadApp ==="),

    ok = ?LOAD:finalize(),
    ok = ?LOAD:delete_pm_jobs(),
    ok = ?LOAD:stop(),

    ct:pal("=== pmsLoadApp stopped ===").


%%--------------------------------------------------------------------
%% wait_n_rp(ReportingPeriod, NumOfRPs) -> ok | {error, reason}.
%%
%% Waits for a given number RPs of specified length to pass.
%%--------------------------------------------------------------------
wait_n_rp(RP, NumOfRPs) ->  
    GP       = ?RP_TO_GP(RP),
    Timeout  = (GP + ?RP_TO_BUFFER) * 1000,
    WaitRP   = fun() -> wait_until_report_rop(Timeout) end,

    repeat(WaitRP, NumOfRPs).

%%--------------------------------------------------------------------
%% wait_n_rop_files(NumOfFiles) -> ok | {error, reason}.
%%
%% Waits for a given number of ROP files to be written
%%--------------------------------------------------------------------
wait_n_rop_files(NumOfRPs) ->
    {ok, Files} = pms_rop_hook:list_rop_files(),
    case length(Files) of
	Length when Length >= NumOfRPs ->
	    ok;
	_ ->
	    timer:sleep(1000),
	    wait_n_rop_files(NumOfRPs)
    end.

%%--------------------------------------------------------------------
%% wait_subscribe_rop() -> ok | {error, reason}.
%%
%% Listens until pmi2SubscribeRop is received, or timeout occurs.
%%--------------------------------------------------------------------
wait_until_subscribe_rop() ->
    wait_until_signal(pmi2SubscribeRop, ?ROP_SUB_TO).


%%--------------------------------------------------------------------
%% wait_until_report_rop() -> ok | {error, reason}.
%%
%% Listens until pmi2ReportRop is received, or timeout occurs.
%%--------------------------------------------------------------------
wait_until_report_rop(Timeout) ->
    wait_until_signal(pmi2ReportRop, Timeout).


%%--------------------------------------------------------------------
%% wait_signal() -> ok | {error, reason}.
%%
%% Listens for response from PMS (via pmsLoadApp) for a given signal.
%%--------------------------------------------------------------------
wait_until_signal(Signal, Timeout) ->
    ct:pal("=== Waiting for ~p signal for ~p ms ===", 
           [Signal, Timeout]),

    receive
        {Signal, Data, _From} ->

            ct:pal("Received ~p signal.~n"
                   "Data: ~p", 
                   [Signal, Data]),

            ok;

        Unhandled ->
            ct:pal("Unhandled response while waiting for ~p:~n"
                   "~p",
                   [Signal, Unhandled]),

            wait_until_signal(Signal, Timeout)
    after
        Timeout ->
            ct:pal("~p timeout (~pms)", [Signal, Timeout]),
            ct:fail({wait_signal_timeout, Signal, Timeout})
    end.


%%--------------------------------------------------------------------
%% repeat(Fun, Args, NoOfTimes) -> ok.
%%
%% Repeats a call to the given function for specified number of times.
%%--------------------------------------------------------------------
repeat(Fun, NoOfTimes) ->
    repeat(Fun, [], NoOfTimes).

repeat(_Fun, _Args, 0) ->
    ok;

repeat(Fun, Args, NoOfTimes) ->
    apply(Fun, Args),
    repeat(Fun, Args, NoOfTimes - 1),
    ok.


%%========================================================================
%% all_expected([X], Expected) -> ok | [X]
%%
%% Checks whether all the items in the provided list
%% are the same as the expected value. If they are,
%% ok is returned. If not, that same list is returned.
%%========================================================================
all_expected(Xs, Expected) ->
    Res = lists:all(fun (X) -> Expected == X end, Xs),
    case Res of
        true -> ok;
           _ -> Xs
    end.


all_true(Xs) ->
    all_expected(Xs, true).


all_ok(Xs) ->
    all_expected(Xs, ok).


%%--------------------------------------------------------------------
%% test(RopPath) -> {RopStats, WasSuspectFound}.
%%
%% Used for testing the load test lib without creating ROP files
%% (using existing example ROP from disk).
%%--------------------------------------------------------------------
test(RopPath) ->
    {ok, RopBin}   = file:read_file(RopPath),
    {ok, State, _} = xmerl_sax_parser:stream(
        RopBin,
        [{event_fun, fun parse_rop/3},
        {event_state, #state{}}]
    ),
    
    #state {jobs = PmJobs, suspectFound = Suspect} = State,
    RopStats = get_rop_statistics(PmJobs),
    {RopStats, Suspect}.


%%--------------------------------------------------------------------
%% get_meas_info_data(PmJobs) -> measInfo.
%%
%% Gets data about the individual measurement
%% info fragment from the given pmJob list
%% (acquired from SAX parsed ROP).
%%--------------------------------------------------------------------
get_rop_statistics(PmJobs) ->
    %%TODO: Implement parsing SAX result 
    NoOfJobs        = length(PmJobs),
    
    AllGroups       = lists:map(fun(PmJob) -> PmJob#pmJob.pmGroups end, PmJobs),
    FlattenedGroups = lists:flatten(AllGroups),
    NoOfGroupsMulti = lists:map(fun length/1, AllGroups),
    [NoOfGroups]    = lists:usort(NoOfGroupsMulti),
    
    AllObjects      = lists:map(fun(Group) -> Group#pmGroup.objects end, FlattenedGroups),
    FlattenedObjs   = lists:flatten(AllObjects),
    NoOfObjsMulti   = lists:map(fun length/1, AllObjects),
    [NoOfObjs]      = lists:usort(NoOfObjsMulti),
    
    AllCounters     = lists:map(fun(Obj) -> Obj#object.values end, FlattenedObjs),
    FlattenedCntrs  = lists:flatten(AllCounters),
    NoOfCountMulti  = lists:map(fun length/1, AllCounters),
    [NoOfCounters]  = lists:usort(NoOfCountMulti),
    
    NoOfValues      = round(length(FlattenedCntrs) / NoOfJobs),
    
    [{n_counters, NoOfCounters},
     {n_groups,   NoOfGroups},
     {n_jobs,     NoOfJobs},
     {n_objects,  NoOfObjs},
     {n_values,   NoOfValues}].
    

%%--------------------------------------------------------------------
%% check_rop(LoadTestOptions) -> ok | {error, reason}.
%%
%% Checks if the actual statistics retrieved from the
%% ROP files correspond with the options that the load
%% test was run with.
%%--------------------------------------------------------------------
check_rop(LoadTestOptions) ->
    {ok, RawXmls} = pms_rop_hook:get_rop_files(),
    XmlsCount     = length(RawXmls),
    ExpectedStats = options_to_stats(LoadTestOptions),

    case XmlsCount of
        0 -> ct:fail(no_rop_files_to_check);
        _ -> ct:pal("=== Total number of ROPs to check: ~p", 
                    [XmlsCount])
    end,

    CheckRop = fun({_Name, Zipped}, Index) ->
        MemType      = processes,
        MemBefore    = erlang:memory(MemType),
        RopBin = zlib:gunzip(Zipped),
        {ok, State, _} = xmerl_sax_parser:stream(
            RopBin,
            [{event_fun, fun parse_rop/3},
             {event_state, #state{}}]
        ),
        
        #state{
            jobs         = PmJobs,
            suspectFound = Suspect
        } = State,
        
        case Suspect of
            true -> ct:fail(rop_suspect_found);
               _ -> ok
        end,
        
        Res = check_rop(PmJobs, Index, ExpectedStats),
        
        Msg = io_lib:format("check_rop #~p", [Index]),
        log_memory_usage(Msg, MemBefore, MemType),
               
        Res
    end,

    Results = map_with_index(CheckRop, RawXmls),
    Passed  = all_ok(Results) == ok,
    
    case Passed of
        true -> ok;
           _ -> {error, rop_check_mismatch}
    end.

check_rop(RopData, Index, ExpectedStats) ->
    AcquiredStats = get_rop_statistics(RopData),

    CheckStat = fun({Key, ExpectedVal}) ->
        ActualVal = proplists:get_value(Key, AcquiredStats),
        ExpectedVal == ActualVal
    end,

    Results = lists:map(CheckStat, ExpectedStats),
    AllTrue = all_true(Results),
    Passed  = AllTrue == ok,

    ct:pal("=== CHECK ROP FILE #~p ===~n~n"
           "ACQUIRED:~n~p~n~n"
           "EXPECTED:~n~p~n~n"
           "=== Has ROP passed: ~p ===",
           [Index, AcquiredStats, ExpectedStats, Passed]),
    
    case Passed of
        true -> ok;
           _ -> {error, {mismatch, [{acquired, AcquiredStats},
                                    {expected, ExpectedStats}]}}
    end.

%%--------------------------------------------------------------------
%% map_with_index(Fun, ListIn) -> ListOut.
%%
%% Maps through the given list, just like the regular lists:map,
%% but the given function is also provided with a second argument
%% which is the index of the current element in the given list.
%%--------------------------------------------------------------------
map_with_index(Fn, Xs) ->
    {Res, _Total} = lists:mapfoldl(fun(X, Idx) -> 
                        Res = apply(Fn, [X, Idx]),
                        NewIdx = Idx + 1,
                        {Res, NewIdx}
                    end, 1, Xs),
    Res.

%%--------------------------------------------------------------------
%% options_to_stats(LoadTestOpts) -> Stats.
%%
%% Converts the load test options prop list to statistics
%% prop list needed for checking ROP file.
%%--------------------------------------------------------------------
options_to_stats(LoadTestOpts) ->
    Delete = fun(Key, Stats) ->
        proplists:delete(Key, Stats)
    end,

    %%PmiDataPerObj = get_val(pmi_data_per_obj, LoadTestOpts, false),

    KeysToDelete = [rp, 
                    mr_per_mt, 
                    pmi_data_per_obj,
		    dirty_jobs],

    lists:foldl(Delete, LoadTestOpts, KeysToDelete).


%%--------------------------------------------------------------------
%% log_rop_files(Config) -> ok.
%%
%% Logs the ROP files to the CT log.
%%--------------------------------------------------------------------
log_rop_files(Config) ->
    {ok, RopData} = pms_rop_hook:get_rop_files(),
    pms_test_lib:log_rop_files(RopData, Config).


%%--------------------------------------------------------------------
%% log_memory_usage(Tag, BytesBefore, MemType) -> ok.
%%
%% Logs the memory usage in CT log, formatted for easier readability.
%% MemType param is defined as the Type argument for erlang:memory/1.
%%--------------------------------------------------------------------
log_memory_usage(Tag, BytesBefore, MemType) ->
    Fun = fun erlang:memory/1,
    log_memory_usage(Tag, BytesBefore, MemType, Fun).

log_memory_usage(Tag, BytesBefore, MemType, Fun) ->  
    BytesAfter  = Fun(MemType),
    BytesDelta  = BytesAfter - BytesBefore,
    Sizes       = [BytesBefore, BytesAfter, BytesDelta],
    Formatted   = lists:map(fun format_memory_size/1, Sizes),
    
    ct:pal("=== Memory usage for ~s (measured with '~s' memory type) ===~n"
           "Before: ~s~n"
           "After:  ~s~n"
           "Delta:  ~s~n",
           [Tag, MemType | Formatted]).


%%--------------------------------------------------------------------
%% format_memory_size(Bytes) -> string().
%%
%% Formats the given memory size for easier readability.
%%--------------------------------------------------------------------
format_memory_size(Bytes) ->
    Sizes        = ["B", "KiB", "MiB", "GiB"],
    Power        = format_memory_size(Bytes, 1),
    Formatted    = Bytes / math:pow(1024, Power - 1),
    FormattedStr = io_lib:format("~.2f", [Formatted]), 
    Unit         = lists:nth(Power, Sizes),
    
    FormattedStr ++ " " ++ Unit.

format_memory_size(Size, Power) when abs(Size) < 1024 ->
    Power;

format_memory_size(Size, Power) ->
    format_memory_size(Size / 1024, Power + 1).

%%========================================================================
%% rpc(M, F, A) -> Result
%%
%%
%%========================================================================
rpc(M, F, A) ->
    pms2_test_lib:rpc(M, F, A).

%% ====================================================================
%% SAX ROP parsing functions
%% ====================================================================
parse_rop({startElement, _, "measInfo", _, 
           [{_, _, "measInfoId", MeasInfoId}]}, _, State) ->
    PmGroupName = get_mo_id(MeasInfoId, "PmGroup"),
    State#state{currPmGroup = PmGroupName};

parse_rop({endElement, _, "measInfo", _}, _, State) ->
    #state{
        jobs         = update_jobs(State),
        suspectFound = State#state.suspectFound
    };

parse_rop({startElement, _, "job", _, [{_, _, "jobId", JobId}]}, _, 
          State) ->
    State#state{currJobId = JobId};

parse_rop({startElement, _, "measType", _, [{_, _, "p", MeasTypeId}]}, _, 
          #state{currMeasTypes = MeasTypes} = State) ->
    State#state{currMeasTypes = [MeasTypeId | MeasTypes]};

parse_rop({startElement, _, "measValue", _, [{_, _, "measObjLdn", LDN}]}, _, 
          State) ->
    ObjectName = get_mo_id(LDN, "Mo"), 
    State#state{
        currObjName = ObjectName,
        currObjVals = []
    };

parse_rop({endElement, _, "measValue", _}, _, 
          #state{currObjects = CurrObjs} = State) ->
    NewObj = #object{
        name   = State#state.currObjName,
        values = State#state.currObjVals
    },
    
    State#state{currObjects = [NewObj | CurrObjs]};

parse_rop({startElement, _, "r", _, [{_, _, "p", MeasType}]}, _, 
          #state{currObjVals = Vals} = State) ->
    NewVal = #measVal{measType = MeasType},
    State#state{currObjVals = [NewVal | Vals]};

parse_rop({endElement, _, "r", _}, _,
          #state{
            currObjVals = [CurrVal | Vals],
            currXmlText = ValueText
          } = State) ->
    
    NewVal = CurrVal#measVal{value = ValueText},
    
    State#state{
        currObjVals = [NewVal | Vals],
        currXmlText = ""
    };

parse_rop({characters, Text}, _, State) ->
    State#state{currXmlText = Text};

parse_rop({startElement, _, "suspect", _, _}, _, State) ->
    State#state{suspectFound = true};

parse_rop(_,_, State) ->
    State.


%% ====================================================================
%% SAX Helper functions
%% ====================================================================

%%--------------------------------------------------------------------
%% get_mo_id(LDN, MoKey) -> string() | undefined.
%%
%% Gets the MO ID from the given LDN by the given MO Type. Example:
%%
%% LDN   = ""PM=1,PmGroup=pmsLoadAppPmGroup1"
%% MoKey = "PmGroup",
%% MoId  = "pmsLoadAppPmGroup1"
%%--------------------------------------------------------------------
get_mo_id(LDN, MoKey) ->
    GetMoFromRdn = fun(RDN) -> 
        [MoType, MoId] = string:tokens(RDN, "="),
        {MoType, MoId}
    end,
    
    RDNs = string:tokens(LDN, ","),
    MOs  = lists:map(GetMoFromRdn, RDNs),
    
    proplists:get_value(MoKey, MOs).


%%--------------------------------------------------------------------
%% update_jobs(Jobs, State) -> Jobs.
%%
%% Updates the the given jobs list with info from the provided state.
%%
%% If the job with the same ID as the one found in the given state
%% exists in the job list, then the current state data is merged into
%% the matching pmJob. If not, then a new pmJob (with the current state
%% data) is added to the jobs list.
%%--------------------------------------------------------------------
update_jobs(#state{jobs = Jobs} = State) ->
    update_jobs(Jobs, State, []).

update_jobs([#pmJob{jobId = IdMatch} = CurrJob | Rest], 
             #state{currJobId = IdMatch} = State, Acc) ->
    MergedJob = updateJobFromState(CurrJob, State),
    [MergedJob | Acc] ++ Rest;

update_jobs([CurrJob | Rest], State, Acc) ->
    update_jobs(Rest, State, [CurrJob | Acc]);

update_jobs([], State, Acc) ->
    NewJob = get_curr_pmJob_from_state(State),
    [NewJob | Acc].
    

%%--------------------------------------------------------------------
%% get_curr_pmJob_from_state(MeasInfo) -> PmJob.
%%
%% Converts the current pmJob data from state into pmJob record.
%%--------------------------------------------------------------------
get_curr_pmJob_from_state(State) ->
    PmGroup = #pmGroup{
        name      = State#state.currPmGroup,
        measTypes = State#state.currMeasTypes,
        objects   = State#state.currObjects
    },
    
    #pmJob{
        jobId     = State#state.currJobId,
        pmGroups  = [PmGroup]
    }.
    
    
%%--------------------------------------------------------------------
%% updateJobFromState(PmJob, State) -> PmJob.
%%
%% Merges the current pmJob data from state into the provided pmJob.
%%--------------------------------------------------------------------
updateJobFromState(#pmJob{jobId = MatchId} = PmJob, 
                    #state{currJobId = MatchId} = State) ->
    NewJob         = get_curr_pmJob_from_state(State),
    [NewGroup]     = NewJob#pmJob.pmGroups,
    ExistingGroups = PmJob#pmJob.pmGroups,
    
    PmJob#pmJob{pmGroups = [NewGroup | ExistingGroups]}.

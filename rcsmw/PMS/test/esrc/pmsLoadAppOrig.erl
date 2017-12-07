%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsLoadAppOrig.erl %
%%% Author:	eolaand
%%% 
%%% Description: A PMS application with a configurable number of counters.
%%% Used mainly for testing large number counters.
%%%
%%% Modules used: pms_pmi_proxy
%%%
%%% ========================
%%% Instructions how to use:
%%% ========================
%%% 
%%% Compile the module
%%% ------------------
%%% 
%%% cd  $RCS_TOP/PMS/PMS_CNX9012618/PMS_CAX1033070/test/esrc
%%% 
%%% erlc pmsLoadApp.erl
%%% 
%%% 
%%% Go to your test directory
%%% -------------------------
%%% 
%%% cd ~/test_dir
%%% 
%%% 
%%% Start Erlang shell
%%% ------------------
%%% 
%%% $RCT_TOP/test/bin/rct_run.sh -sim <USER_ID>_tmp -shell -pa $RCS_TOP/PMS/PMS_CNX9012618/PMS_CAX1033070/test/esrc -pa $RCS_TOP/PMS/PMS_CNX9012618/test/suites
%%% 
%%% 
%%% An example how to run the load app
%%% ----------------------------------
%%% 
%%% Opts = [{rp, 1}, 
%%%         {n_jobs, 1},
%%%         {mr_per_mt, false},
%%%         {n_groups, 1},
%%%         {n_counters, 1},
%%% 	{n_objects, 1},
%%% 	{pmi_data_per_obj, false}]. 
%%% 
%%% 
%%% pmsLoadApp:start(Opts).
%%% 
%%% pmsLoadApp:initialize().
%%% pmsLoadApp:create_pm_jobs().
%%% 
%%% 
%%% pmsLoadApp:finalize().
%%% pmsLoadApp:delete_pm_jobs().
%%% 
%%% pmsLoadApp:stop().
%%% 
%%% 
%%% ----------------------------------------------------------
-module(pmsLoadAppOrig).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R4A/R5A/R6A/1').
-date('2016-08-23').
-author('etxkols').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% Rev      Date       Name        What
%%% -----    -------    --------    ------------------------
%%% R4A/1    2015-06-04 eolaand     Created
%%% R6A/1    2016-08-23 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------

%% Start and stop
-export([start/0,
	 start/1,
	 stop/0]).

%% API
-export([initialize/0,
	 finalize/0,
	 create_pm_jobs/0,
	 delete_pm_jobs/0]).

%% Exported Internal Functions called via RPC
-export([rpc_delete_mts/1,
	 rpc_delete_groups/1,
	 rpc_write_db/1,
	 rpc_delete_jobs/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% Debug
-export([dump/0,
	 dump/1]).

%% -compile(export_all).

-include("../../out/RcsPm.hrl").


-define(SERVER, ?MODULE). 
-define(PROXY, pms_pmi_proxy).
-define(CALLBACKS, {true, true, true}).
-define(COUNTER_MAP, undefined).



-record(state, {name,
		rp,
		n_jobs,
		mr_per_mt,
		n_groups,
		n_counters,
		n_objects,
		pmi_data_per_obj,
		handle,
		counterMap,
		pmiDataVals}).

%%%===================================================================
%%% API
%%%===================================================================

initialize() ->
    initialize(?SERVER).


initialize(Server) ->
    gen_server:cast(Server, initialize).


finalize() ->
    finalize(?SERVER).


finalize(Server) ->
    gen_server:cast(Server, finalize).


create_pm_jobs() ->
    create_pm_jobs(?SERVER).
    

create_pm_jobs(Server) ->
    gen_server:call(Server, create_pm_jobs, 60000).
    

delete_pm_jobs() ->
    delete_pm_jobs(?SERVER).
    

delete_pm_jobs(Server) ->
    gen_server:call(Server, delete_pm_jobs, 60000).
    

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start([]).


start(Opts) ->
    start(?SERVER, Opts).


start(Server, Opts) ->
    pms_test_lib:set_ct_log(false),
    case whereis(?PROXY) of
	undefined ->
	    {ok, _} = ?PROXY:start([{erl_api, false}, 
				    {trace_ift, false}, 
				    {trace_child, false}]);
	_Pid ->
	    ok
    end,
    State = init_state(Opts, #state{name = Server}),
    gen_server:start({local, Server}, ?MODULE, State, []).


%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    stop(?SERVER, normal).


stop(Server, Reason) ->
    gen_server:call(Server, {stop, Reason}).


%%%===================================================================
%%% Debug
%%%===================================================================
dump() ->
    dump(?SERVER).


dump(Server) ->
    gen_server:call(Server, dump).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(State) ->
    remote_load(),
    CM = init_counter_map(State),
    LDNAliases = init_ldn_aliases(State),
    Vals = init_meas_values(State, LDNAliases),
    ok = create_mts(CM),
    {ok, State#state{counterMap = CM,
		     pmiDataVals = Vals}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(create_pm_jobs, _From, State) ->
    ok = create_jobs(State),
    rpc_call(gen_server, cast, [pmsServer, {test_terminate, reset}]),
    {reply, ok, State};

handle_call(delete_pm_jobs, _From, State) ->
    ok = delete_jobs(State),
    rpc_call(gen_server, cast, [pmsServer, {test_terminate, reset}]),
    {reply, ok, State};

handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call({stop, Reason}, _From, State) ->
    delete_mts(State#state.counterMap),
    {stop, Reason, ok, State}.

%% handle_call(_Request, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(initialize, State) ->
    {ok, Handle} = ?PROXY:pmi2Initialize(?CALLBACKS, ?COUNTER_MAP),
    ok = ?PROXY:pmi2CounterMap(Handle, State#state.counterMap),
    {noreply, State#state{handle = Handle}};

handle_cast(finalize, #state{handle = Handle} = State) 
  when Handle =/= undefined ->
    ok = ?PROXY:pmi2Finalize(Handle),
    {noreply, State#state{handle = undefined}};

handle_cast(finalize, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% handle_info(_Info, State) ->
%%     io:format("Unknown Info: ~p~n", [_Info]),
%%     {noreply, State};

%% Unknown Info: {pmi2SubscribeRop,{60,[{1,[1,2,3,4,5]}]}}
%% Unknown Info: {pmi2ReportRop,{60,1432846080,7}}

handle_info({pmi2SubscribeRop = CB, _}, State) ->
    io:format("Received Callback: ~p~n", [CB]),
    {noreply, State};

handle_info({pmi2ReportRop, {RP, Id, _TO}} = CB, State) ->
    io:format("Received Callback: ~p~n", [CB]),
    io:format("Send pmi2DataRop~n", []),
    Res = send_pmi_data(RP, Id, State),
    io:format("Result of pmi2DataRop: ~p~n", [Res]),
    {noreply, State};

handle_info(_Info, State) ->
    io:format("Unknown Info: ~p~n", [_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_state(Opts, State) ->
    RP = proplists:get_value(rp, Opts, 1),
    NJobs = proplists:get_value(n_jobs, Opts, 1),
    MRMT = proplists:get_value(mr_per_mt, Opts, false),
    NGroups = proplists:get_value(n_groups, Opts, 1),
    NCounts = proplists:get_value(n_counters, Opts, 1),
    NObjs = proplists:get_value(n_objects, Opts, 1),
    PDPerObj = proplists:get_value(pmi_data_per_obj, Opts, false),
    State#state{rp = RP,
		n_jobs = NJobs,
		mr_per_mt = MRMT,
		n_groups = NGroups,
		n_counters = NCounts,
		n_objects = NObjs,
		pmi_data_per_obj = PDPerObj}.
    

remote_load() ->
    {M, Bin, _F} = code:get_object_code(?MODULE),
    F = "dummy",
    {module, M} = rpc_call(code, load_binary, [M, F, Bin]).
   

init_counter_map(#state{name = Name, 
			n_groups = NGroups, 
			n_counters = NCounts}) ->
    init_counter_map(Name, NGroups, NCounts).


init_counter_map(Name, NGroups, NCounts) ->
    [init_group_counter_map(group_name(Name, GN), GN, NCounts) || 
	GN <- lists:seq(1, NGroups)].

init_group_counter_map(GName, GN, NCounts) ->
    {GName, GN, [{mt_name(GName, MTN), MTN} || MTN <- lists:seq(1, NCounts)]}.

init_meas_values(#state{pmi_data_per_obj = PDPO,
			n_groups = NGroups, 
			n_counters = NCounts}, LDNAliases) ->
    PmiData = [{GN, [{LA, [{CN, [CN]} || CN <- lists:seq(1, NCounts)]} || 
			LA <- LDNAliases]} || GN <- lists:seq(1, NGroups)],
    if
	PDPO ->
	    pmi_data_per_obj(PmiData);
	true ->
	    PmiData
    end.


pmi_data_per_obj(PmiData) ->
    lists:append([[[{G, [{LDN, Vals}]}] || {LDN, Vals} <- LDNVals] || 
		     {G, LDNVals} <- PmiData]).


init_ldn_aliases(State) ->
    Name = State#state.name,
    [get_ldn_alias(Name, N) || N <- lists:seq(1, State#state.n_objects)].


get_ldn_alias(Name, N) ->
    LDN = ["ManagedElement=1," ++ atl(Name) ++ "=1,Mo=" ++ itl(N)],
    {ok, Alias} = rpc_call(gmfI, get_integer, LDN),
    Alias.


create_mts(CM) ->
    Recs = lists:flatmap(fun({Group, _, MTs}) ->
				 [group_rec(Group) | 
				  [mt_rec(Group, MT) || {MT, _} <- MTs]]
		  end, CM),
    write_db(Recs).


delete_mts(CM) ->
    GMTs = [{Group, [MT || {MT, _} <- MTs]} || {Group, _, MTs} <- CM],
    delete_group_mts(GMTs),
    GroupKeys = [group_key(Group) || {Group, _} <- GMTs], 
    rpc_call(?MODULE, rpc_delete_groups, [GroupKeys]).


delete_group_mts([{Group, MTs} | T] ) ->
    MTKeys = [mt_key(Group, MT) || MT <- MTs],
    rpc_call(?MODULE, rpc_delete_mts, [MTKeys]),
    delete_group_mts(T);
			
delete_group_mts([]) ->
    ok.


rpc_delete_mts(MTKeys) ->
    lists:foreach(fun(MTKey) ->
			  mnesia:dirty_delete(measurementType, MTKey)
		  end, MTKeys).
		       

rpc_delete_groups(GroupKeys) ->
    lists:foreach(fun(GroupKey) ->
			  mnesia:dirty_delete(pmGroup, GroupKey)
		  end, GroupKeys).
			       

create_jobs(State) ->
    CM = State#state.counterMap,
    Name = State#state.name,
    NJobs = State#state.n_jobs,
    RP = State#state.rp,
    case State#state.mr_per_mt of
	true ->    
	    create_jobs_mr_per_mt(NJobs, Name, RP, CM);
	false ->
	    create_jobs_mr_per_group(NJobs, Name, RP, CM)
    end.

create_jobs_mr_per_mt(NJobs, Name, RP, CM) when is_integer(NJobs) ->
    Recs = lists:flatmap(fun(N) ->
				 JName = job_name(Name, N), 
				 [job_rec(JName, RP) | 
				  create_mt_mrs(CM, JName)]
			 end, lists:seq(1, NJobs)),
   write_db(Recs);

create_jobs_mr_per_mt(_NJobs, Name, RP, CM) ->
    {Recs, _N} = lists:mapfoldl(fun({GMTs, N}) ->
					JName = job_name(Name, N), 
					{[job_rec(JName, RP) | 
					  create_mt_mrs([GMTs], JName)], 
					 N + 1}
				end, 1, CM),
    write_db(lists:append(Recs)).


create_mt_mrs(CM, JName) ->
    lists:flatmap(fun({Group, _A, MTs}) ->
			  create_mt_mrs(Group, MTs, JName)
		  end, CM).


create_mt_mrs(Group, MTs, JName) ->
    {Recs, _N} = lists:mapfoldl(fun({MT, _}, N) ->
					MRName = mr_name(JName, N),
					MSpec = meas_spec(Group, MT),
					{mr_rec(JName, MRName, MSpec), N + 1}
				end, 1, MTs),
    Recs.


create_jobs_mr_per_group(NJobs, Name, RP, CM) when is_integer(NJobs) ->
    Recs = lists:flatmap(fun(N) ->
				 JName = job_name(Name, N), 
				 [job_rec(JName, RP) | 
				  create_group_mrs(CM, JName)]
			 end, lists:seq(1, NJobs)),
    write_db(Recs);

create_jobs_mr_per_group(_NJobs, Name, RP, CM) ->
    {Recs, _N} = lists:mapfoldl(fun(GMTs, N) ->
					JName = job_name(Name, N), 
					{[job_rec(JName, RP) | 
					  create_group_mrs([GMTs], JName)], 
					 N + 1}
				end, 1, CM),
    write_db(lists:append(Recs)).


create_group_mrs(CM, JName) ->
    {Recs, _N} = lists:mapfoldl(fun({Group, _, _}, N) ->
					MRName = mr_name(JName, N),
					MSpec = meas_spec(Group),
					{mr_rec(JName, MRName, MSpec), N + 1}
				end, 1, CM),
    Recs.


write_db(Recs) ->
    rpc_call(?MODULE, rpc_write_db, [Recs]).


rpc_write_db(Recs) ->
    TF = fun() ->
		 lists:foreach(fun(Rec) -> mnesia:write(Rec) end, Recs)
	 end,
    {atomic, ok} = mnesia:transaction(TF),
    ok.


delete_jobs(State) ->
    rpc_call(?MODULE, rpc_delete_jobs, [atl(State#state.name)]).


rpc_delete_jobs(Prefix) ->
    AllJobs = mnesia:dirty_all_keys(pmJob),
    LoadJobs = [Job || {"1", "1", "1", Id} = Job <- AllJobs,
		       lists:prefix(Prefix, Id)],
    ok = delete_job_mrs(LoadJobs),
    lists:foreach(fun(JobId) ->
			  ok = mnesia:dirty_delete(pmJob, JobId)
		  end, LoadJobs).


delete_job_mrs(JobKeys) ->
    F = fun({ME, SF, PM, PmJob}) ->
		MrIdMatch = {ME, SF, PM, PmJob, '_'},
		MRMatch = #measurementReader{measurementReaderId = MrIdMatch,
					     _ = '_'},
		MRs = mnesia:dirty_match_object(measurementReader, MRMatch),
		delete_mrs(MRs)
	end,
    lists:foreach(F, JobKeys).


delete_mrs(MRs) ->
    lists:foreach(fun(#measurementReader{measurementReaderId = MrId}) ->
			  ok = mnesia:dirty_delete(measurementReader, MrId)
		  end, MRs).


send_pmi_data(RP, Id, State) ->
    Handle = State#state.handle,
    PmiData = State#state.pmiDataVals,
    case State#state.pmi_data_per_obj of
	false ->
	    send_pmi_data_ff(Handle, PmiData, RP, Id);
	_True ->
	    send_pmi_data_per_obj(PmiData, Handle, RP, Id)
    end.


send_pmi_data_ff(Handle, PmiData, RP, Id) ->
    ?PROXY:pmi2DataRop(Handle, RP, Id, PmiData, true).
    

send_pmi_data_per_obj([LDNVals], Handle, RP, Id) ->
    send_pmi_data_ff(Handle, LDNVals, RP, Id);

send_pmi_data_per_obj([LDNVals | T], Handle, RP, Id) ->
    ?PROXY:pmi2DataRop(Handle, RP, Id, LDNVals, false),
    send_pmi_data_per_obj(T, Handle, RP, Id).


group_rec(Name) ->
    #pmGroup{pmGroupId = group_key(Name)}.


group_key(Name) ->
    {"1", "1", "1", Name}.


mt_rec(Group, Name) ->
    #measurementType{measurementTypeId = mt_key(Group, Name),
		     measurementName = Name}.


mt_key(Group, Name) ->
    {"1", "1", "1", Group, Name}.


job_rec(Name, RP) ->
    job_rec(Name, RP, RP).


job_rec(Name, RP, GP) ->
    #pmJob{pmJobId = {"1", "1", "1", Name},
	   reportingPeriod = RP,
	   granularityPeriod = GP}.


mr_rec(JName, Name, MSpec) ->   
    #measurementReader{measurementReaderId = {"1", "1", "1", JName, Name},
		       measurementSpecification = MSpec}.

meas_spec(Group) ->	    
    Ref = "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=" ++ Group,
    #'MeasurementSpecification'{groupRef = ltb(Ref)}.


meas_spec(Group, MT) ->	    
    Ref = "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=" ++ Group ++
	",MeasurementType=" ++ MT,
    #'MeasurementSpecification'{measurementTypeRef = ltb(Ref)}.
			  

group_name(Prefix, N) ->
    atl(Prefix) ++ "PmGroup" ++ itl(N).


mt_name(Prefix, N) ->
    atl(Prefix) ++ "MeasType" ++ itl(N).


job_name(Prefix, N) ->
    atl(Prefix) ++ "PmJob" ++ itl(N).


mr_name(Prefix, N) ->
    atl(Prefix) ++ "MeasReader" ++ itl(N).


rpc_call(M, F, A) ->
    pms_pmi_proxy:cs_node_call(M, F, A).


atl(A) when is_atom(A) ->
    atom_to_list(A);

atl(L) when is_list(L) ->
    L.


itl(I) ->
    integer_to_list(I).


ltb(L) ->
    list_to_binary(L).

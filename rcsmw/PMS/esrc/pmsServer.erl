%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsServer.erl %
%%% 
%%% @doc
%%% Description:
%%% Server process for Performance Management.     
%%%
%%% ```
%%% =====================================
%%% =====================================
%%% PMS Process handling
%%% =====================================
%%% =====================================
%%%  
%%% There are two gen_server processes: pmsServer and pmsAppRegistry.
%%% pmsServer process handles the interface towards OSS and 
%%% pmsAppRegistry is the master of the application interface part of PMS.
%%% 
%%% The COM half of PMS contains the following processes:
%%%  - pmsServer
%%%  - pmsJobGroup
%%%  - pmsJob
%%%  - pmsJobMeasInfo
%%%  
%%% The application half of PMS contains the following processes:
%%%  - pmsAppRegistry
%%%  - pmsSession and pmsSession2 (two flavors, one for PMI and one for PMI2)
%%%  - pmsAppJob
%%%  
%%%  
%%% The following figure depicts the associations between the processes
%%%  
%%%                                                   pmsAppRegistry  
%%%                                                    |         |
%%%                                                    |         |
%%% pmsServer ---- pmsJobGroup ---- pmsJob ---- pmsAppJob ---- pmsSession
%%%                                   |
%%%                                   |
%%%                             pmsJobMeasInfo           
%%%  
%%% The following table shows the number of associations between the processes
%%%  
%%% pmsServer        1..N    pmsJobGroups
%%% pmsJobGroup      0..N    pmsJob
%%% pmsJob           0..N    pmsAppJob
%%% pmsJob           0..N    pmsJobMeasInfo
%%% pmsAppJob        0..N    pmsJob
%%% pmsAppJob        1..1    pmsSession[2]
%%% pmsAppRegistry   1..N    pmsAppJob
%%% pmsAppRegistry   1..N    pmsSession[2]
%%%  
%%%  
%%% --------------------------------------  
%%% pmsServer
%%% --------------------------------------  
%%% pmsServer subscribes to pmJob and measurementReader mnesia table events. 
%%% 
%%% pmsServer starts one pmsJobGroup process for each unique combination
%%% of jobGroups and RPs, as defined in the pmJobs.
%%% 
%%% pmsComteI invokes pmsServer:prepare_transaction when a COM
%%% transaction is executed. pmsServer stores the included MO updates
%%% and waits for mnesia events for those MOs.
%%% 
%%% When all mnesia events are received pmsServer starts one pmsJobGroup
%%% process for each unique jobGroup and RP combination in the transaction,
%%% if not already started.
%%% Further, it orders the pmsJobGroup processes to create/delete/update
%%% pmsJob processes included in the transaction.
%%%
%%% pmsServer keeps track of the pmsJobGroups and what pmJobs each of the
%%% pmsJobGroups own.
%%% 
%%% pmsServer always creates a pmsJobGroup with jobGroup = undefined 
%%% for each existing RP even if there are no pm jobs with that RP.
%%% These processes are used when PMS should only generate a signle ROP file
%%% for all jobGroups, see below.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsJobGroup
%%% --------------------------------------  
%%% pmsJobGroup handles the pmsJob processes and creates the ROP files
%%% for its jobGroup; i.e. there will be as many ROP-files as there
%%% are combinations of pmGroups and RPs.
%%% 
%%% pmsJobGroup creates a pmsJob process for each new pmJob it handles.
%%% pmsJobGroup keeps track of all its pmsJob processes: the pmJobId,
%%% Pid, and GP.
%%% 
%%% 
%%% 
%%% 
%%% --------------------------------------  
%%% pmsJob
%%% --------------------------------------  
%%% pmsJob handles one pmJob. It is the bridge between the pmJobs and
%%% the pmsAppJob processes, described below. It will find all
%%% pmsAppJob processes that handles the same counters as the pmJob
%%% and requests the pmsAppJob to send the counters values at GP intervals.
%%% 
%%% pmsJob subscribes to pmsAppRegistry mnesia table events.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsJobMeasInfo
%%% --------------------------------------  
%%% pmsJobMeasInfo is a slave process belonging to a pmsJob. 
%%% Each slave process is used to collects measurements belonging to one 
%%% PmGroup. These measurement are used to produce a measInfo element that
%%% is fetched by the pmsJob process. 
%%% 
%%% 
%%% --------------------------------------  
%%% pmsAppRegistry
%%% --------------------------------------  
%%% pmsAppRegistry is the heart of the PMS half taking care of 
%%% the applications
%%% 
%%% pmsAppRegistry monitors pmsSession[2] and pmsAppJob[2] processes. 
%%% It also stores the PIDs to these processes and what pmGroups and
%%% pmMeasurementTypes these handles.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsSession[2]
%%% --------------------------------------  
%%% A pmsSession process handles the message passing between 
%%% an application (c-kode) and PMS on the Erlang node.
%%% When an application invokes PMI[2] pmiInitialize a notification
%%% is sent to CEC module, which will create a pmsSession or pmsSession2 
%%% process, depending if PMI or PMI2 was used.
%%%  
%%% pmsSession will inform pmsAppRegistry about the new PMI[2] session.
%%% pmsAppRegistry will then create a pmsAppJob process,
%%% which will be associated with the corresponding pmsSession process.
%%% These two processes will always work in pairs.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsAppJob
%%% --------------------------------------  
%%% pmsAppJob process controls what counters are active and requests
%%% the application to start subscription of them. It also requests
%%% the counter values with GP interval, and forwards the data to 
%%% the pmsJob processes that has requested the data.
%%% 
%%% 
%%% 
%%% 
%%% =====================================
%%% =====================================
%%% PMS sequences
%%% =====================================
%%% =====================================
%%%
%%% Below is a short presentation of what happens when 
%%%  - a pmJob is created, updated, or deleted
%%%  - an application has created a PMI session
%%%  - a ROP file is generated
%%%  - one/multi ROP file mode
%%% 
%%% 
%%% --------------------------------------  
%%% Create a pm job
%%% --------------------------------------  
%%% .....................
%%% pmsComteI
%%% .....................
%%% pmsComteI calls pmsServer with prepare_transaction. This is actually
%%% done twice, because the first call is just for checking that the
%%% MO updates in the transaction are valid. The second prepare call
%%% is the actual transaction.
%%% 
%%% 
%%% ...............
%%% pmsServer
%%% ...............
%%% pmsServer stores the included updates in its state and waits for
%%% the mnesia events.
%%% When the events are received pmsServer builds a list of all pmJobs that
%%% were affected. When all events for a transaction are received pmsServer
%%% creates possible new pmsJobGroup processes. It also invokes the
%%% affected pmsJobGroup process about any created/deleted/updated pmJobs.
%%% 
%%% pmsServer remembers (and monitors) all pmsJobGroup processes and what
%%% pmJobs they manage.
%%% 
%%% 
%%% ...............
%%% pmsJobGroup
%%% ...............
%%% pmsJobGroup creates (and monitors) pmsJob process on demand from pmsServer.
%%% pmsJobGroup stores what pmsJob processes it controls in the loop data.
%%% 
%%% 
%%% ...............
%%% pmsJob
%%% ...............
%%% When a pmsJob process is started it checks in Mnesia what
%%% measurementReaders it owns. The transaction is finished by this time so
%%% all MRs are already created.
%%% 
%%% pmsJob also checks in the pmsAppRegistry if there are any applications
%%% handling the counters included in the pmJob. In that case it sends a
%%% subscribe message to all the affected pmsAppJob processes.
%%% 
%%% 
%%% ...............
%%% pmsAppJob
%%% ...............
%%% pmsAppJob stores the counters and pmsJob PIDs when it receives 
%%% a subscribe request.
%%% 
%%% It counts a union of all requested counters from all pmsJobs and 
%%% sends a subscribe message to pmsSession[2].
%%% 
%%% 
%%% ...............
%%% pmsSession[2]
%%% ...............
%%% pmsSession[2] encodes the subscribe message to the PMI[2] internal
%%% format and sends it to the c-side.
%%% 
%%% 
%%% ...............
%%% PMI[2]
%%% ...............
%%% The c-side decodes the messages and calls the application callbacks.
%%% How the application is invoced is actually a little more complicated
%%% than that, for details see the PMI[2] IWDs.
%%% 
%%% 
%%% --------------------------------------  
%%% Update a pm job
%%% --------------------------------------  
%%% ...............
%%% pmsJobGroup
%%% ...............
%%% pmsJobGroup is notified by pmsServer of all pmJobs that are updated
%%% in the transaction. pmsJobGroup forwards the information to the 
%%% affected pmsJob processes
%%% 
%%% 
%%% ...............
%%% pmsJob
%%% ...............
%%% When a pmsJob process is updated it checks in Mnesia if the
%%% pmJob attributes are changed and also if there were any 
%%% changeds in the measurementReaders it controls.
%%% If will send a new subscribe message to pmsAppJob, if needed.
%%% 
%%% 
%%% ...............
%%% pmsAppJob
%%% ...............
%%% pmsAppJob remembers the new set of counters that the pmsJob has
%%% requested.
%%% 
%%% It also checks if the new set of counters will affect the union
%%% of all counters. In that case a new subscribe message is sent
%%% to the application via pmsSession[2]
%%% 
%%% 
%%% --------------------------------------  
%%% Delete a pm job
%%% --------------------------------------  
%%% ...............
%%% pmsJobGroup
%%% ...............
%%% pmsJobGroup is notified by pmsServer of all pmJobs that are deleted
%%% in the transaction. pmsJobGroup forwards the information to the 
%%% affected pmsJob processes, and removes the pmsJobs from its list.
%%% 
%%% If the list of jobs becomes empty the pmsJobGroup process could
%%% be deleted because it does not handle any more pmJobs.
%%% However, there may be need to generate a last ROP file for the
%%% previous RP. Therefore, the process waits for next ROP tick
%%% to possibly create a ROP file and only after that the process is 
%%% deleted.
%%% 
%%% If the process is deleted a message is sent to pmsServer.
%%% pmsServer will then remove all data about that pmsJobGroup process.
%%% 
%%% 
%%% ...............
%%% pmsJob
%%% ...............
%%% pmsJob process sends a subscribe message to its associated pmsAppJob[2]
%%% processes with an empty counter list to indicate that the job is 
%%% deleted.
%%% 
%%% The pmsJob process also counts a current timestamp and compares
%%% that to the timestamp received in the last meas_value message, 
%%% refer to the ROP data collection chapter below.
%%% 
%%% If the current timestamp is greater, it implies that we have passed
%%% to a new GP for which we have not yet received the measurement data.
%%% In this case the pmsJob process will wait for the counter values
%%% from all its associated pmsAppJob processes. When all pmsAppJobProcesses
%%% have responed the pmsJob processes is deleted.
%%% 
%%% If the timestamps are equal the process will be deleted because that
%%% implies that we are in the last incomplete GP and for the last incomplete
%%% RP measurement data is not to be written into the ROP file.
%%% 
%%% 
%%% ...............
%%% pmsAppJob
%%% ...............
%%% When pmsAppJob receives an empty subscribe it will remove 
%%% the pmsJob process from its list of job processes.
%%%
%%% It also checks if the removed counters will affect the union
%%% of all counters. In that case a new subscribe message is sent
%%% to the application via pmsSession[2]
%%% 
%%% 
%%% --------------------------------------  
%%% PMI[2] Initialize
%%% --------------------------------------  
%%% ...............
%%% CEC
%%% ...............
%%% When PMI[2] on the c-side is invoked with pmiInitialize it will send 
%%% a message to a predefined CEC port with a tag PMI or PMI2, depending
%%% of which interface is requested by the application.
%%% 
%%% CEC will order pmsSession or pmsSession2 to create a session process. 
%%% The new socket is returned to the c-side and the initialize message
%%% is sent to that socket
%%% 
%%% 
%%% ...............
%%% pmsSession[2]
%%% ...............
%%% pmsSession[2] decodes the incoming message and invokes pmsAppRegistry
%%% process. 
%%% 
%%% 
%%% ...............
%%% pmsAppRegistry
%%% ...............
%%% pmsAppRegistry starts a pmsAppJob process.
%%% The initialize message may or may not contain information
%%% about the counters that the calling application handles.
%%% 
%%% If there was no counters defined in the initialize message
%%% then the application has to defined the counters by sending 
%%% counterMap messages. 
%%% 
%%% pmsAppRegistry will store the counters and the pmsAppJob[2]
%%% process PIDs in a pmsAppRegistry Mnesia table.
%%% 
%%% 
%%% ...............
%%% pmsAppJob
%%% ...............
%%% pmsAppJob stores the counters that it will handle and the PID to
%%% its associated pmsSession process.
%%% 
%%% 
%%% ...............
%%% pmsJob
%%% ...............
%%% pmsJob process is notified of all changes in the pmsAppRegistry table.
%%% pmsJob ignores events that are not affecting the counters that are
%%% associated with the pm job.
%%% 
%%% If a new pmsAppJob process is handling the same counters as the pmJob
%%% a subscribe message is sent to that process.
%%% 
%%% If a pmsAppJob is removed pmsJob process removes the association to that
%%% process.
%%% 
%%% 
%%% --------------------------------------  
%%% PMI[2] Finalize
%%% --------------------------------------  
%%% Normally this will never happen, but the sequence is similar to the
%%% initialize sequence, but reversed, i.e. all traces of that session
%%% are deleted.
%%% 
%%% 
%%% --------------------------------------  
%%% ROP handling
%%% --------------------------------------  
%%% 
%%% ...............
%%% pmsAppJob
%%% ...............
%%% These processes will send a message to the applications every GP and
%%% waits for the counter values. The message is tagged with an integer
%%% that identifies the current GP. This tag is also included when the
%%% measurement data is sent to the pmsJob processes.
%%% 
%%% The counter values may fragmented into several messages. In PMI2 there
%%% is a final fragment flag that informs that all counter values are sent.
%%% In PMI the final flag may be indicated by sending a message without
%%% any counter values.
%%% 
%%% An application has only a certain time to response. If not all data
%%% has been received within the time limit, a timeout message is sent
%%% to the pmsJob processes.
%%% 
%%% The counter names and LDNs are received as integers, aka aliases, 
%%% to decrease the number of bytes in the messages.
%%% 
%%% pmsAppJob sorts the data and decodes the names to strings.
%%% Further, it forwards the counter values to the subscribing pmsJob processes.
%%% 
%%% 
%%% ...............
%%% pmsJob
%%% ...............
%%% pmsJob receives only the counters from pmsAppJob that it has subscribed on.
%%% pmsJob stores the received values until it finds a final flag or receives
%%% the timeout message. It also stores the GP identity that is received from
%%% the pmsAppJob[2] process. This is used when a pmsJob is deleted.
%%% 
%%% When all data is received pmsJob formats the data to ROP file format and
%%% sends it to the pmsJobGroup process.
%%% 
%%% 
%%% ...............
%%% pmsJobGroup
%%% ...............
%%% pmsJobGroup waits for data from all its pmsJob processes.
%%% When all jobs have responded the ROP file is generated by each of 
%%% the pmsJobGroup processes if PMS is set to be in multi ROP file mode.
%%% If in single ROP file mode only the default jobGroup process will
%%% genrate the ROP file; all the other pmJobGroup processes will forward
%%% the received counter values to default process.
%%% 
%%% pmsJobGroup has also a timer that indicates that it is time to
%%% generate the ROP file despite if all jobs have replied.
%%% This is because the ROP file must be generated 5 min after the RP
%%% (in case of 15 min RP).
%%% 
%%% 
%%% --------------------------------------  
%%% Single/multi ROP file mode
%%% --------------------------------------  
%%% 
%%% ...............
%%% pmsJobGroup
%%% ...............
%%% 
%%% In the NodeSupport -> PmsSupport the operator can define if PMS
%%% should generate a ROP file fore each defined jobGroup and RP pair,
%%% or if only one ROP file should be generated for each RP.
%%% 
%%% pmsJobGroup subscribes to the NodeSupport mnesia events to check 
%%% the ROP file generation mode. 
%%% 
%%% When the mode is changed the non-default pmsJobGroup processes
%%% will inform the default process if it will forward the counter data
%%% or if it will generate the ROP files by itself.
%%% 
%%% 
%%% =====================================
%%% =====================================
%%% PMS tables
%%% =====================================
%%% =====================================
%%%  
%%% --------------------------------------  
%%% ECIM model tables
%%% --------------------------------------  
%%%  
%%% The Pm=1 and pmMeasurementCapabilities=1 instancea are created by PMS
%%% when the node is started.
%%%  
%%% PMS parses the appdata files provided by the application and
%%% creates the requested pmGroup and measurementType instances.
%%%  
%%% There may be an appdata file defining predefined jobs, in that case
%%% also pmJob and measurementReader instances will be created.
%%%  
%%% pmJob and measurementReader instances will also be created when
%%% an operator creates pm jobs.
%%%  
%%%  
%%% --------------------------------------  
%%% pmsCounterAliases
%%% --------------------------------------  
%%% pmsCounterAliases mnesia table is created at PMS start.
%%% 
%%% It is used to store the counter aliases when parsing 
%%% the alias appdata file.
%%% 
%%%
%%% --------------------------------------  
%%% pmsAppsInfo
%%% --------------------------------------  
%%% pmsAppsInfo mnesia table is created at PMS start.
%%% 
%%% It is used to store the CXP ids and versions of the applications
%%% that has created pm counters. 
%%% I.e. applications defining pmsCounter appdata files.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsAppRegistry
%%% --------------------------------------  
%%% pmsAppRegistry mnesia table is created at PMS start.
%%% 
%%% It is used to store which applications are handling which counters.
%%% pmsJob processes subscribe to the events to be able to associate
%%% pm jobs with applications handling the counters of the pm job.
%%% 
%%% It also contains information what callbacks the application handles.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsScAppMoLdns
%%% --------------------------------------  
%%% pmsScAppMoLdns mnesia table is created at PMS start.
%%% 
%%% It is only used for PMI sessions, i.e. not for PMI2. 
%%% 
%%% The pmiInitialize_2 message contains an attribute where the 
%%% application can define a top LDN. This LDN is used when
%%% show-counter is invoked to find which PMI session is to be 
%%% used to collect the counter values.
%%% 
%%% 
%%% --------------------------------------  
%%% pmsEnv
%%% --------------------------------------  
%%% pmsEnv mnesia table is created at PMS start.
%%% 
%%% It contains miscelaneous key value pairs, e.g. if the node
%%% is in PMS test mode, i.e. if other RPs are allowed besides
%%% the default 15 min RP.
%%% 
%%% '''
%%% 
%%% @end 
%%% ----------------------------------------------------------
-module(pmsServer).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/1').
-date('2017-08-17').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-04-10 etxbjca     Created
%%% R3A/17     2015-01-20 erarafo     Separation of PMS and PES SFTP services
%%% R5A/1      2015-11-11 uabesvi     Created r5 branch
%%% R5A/3-13   2015-11-19 uabesvi     Allow update of pm jobs
%%% R5A/14     2015-12-15 uabesvi     Reorg
%%% R5A/15     2015-12-15 uabesvi     Single/multi rop files
%%% R5A/23     2016-02-01 uabesvi     HU53044
%%% R5A/24     2016-02-11 eolaand     Move ROP-file dir registration to 
%%%                                   pmsDataInit
%%% ----------------------------------------------------------
%%% R11A/1     2017-08-17 eolaand     Increase max ROP-file age from 24h to 48h 
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2]).

-export([job_create_res/3]). 
-export([job_group_exit/4]). 

-export([check_alarm/0]). 

-export([get_loop_data/0]). 

-include("pms.hrl").
-include("RcsPm.hrl").

%% -define(ROP_EXPIRY_TIME,    {hours, 24}).
-define(ROP_EXPIRY_TIME,    {hours, 48}).
-define(ROP_CLEAN_INTERVAL, 10*60*1000).
-define(DEFAULT_BL_TO, 5).


%%========================================================================
%% Actions          = [Action]
%% Action           = {created, NewRec, [OldRec]} | {deleted, NewRec, [OldRec]}
%% NewRec = OldRec  = Rec
%% RemainingActions = [RemAction]
%% RemAction        = {created, Rec} | {deleted, MoId}
%% Rec              = #pmJob | #measurementReader
%% MoId             = pmJobId | measurementReaderId
%%========================================================================
-record(state, 
	{job_groups  = #{},   %% {{JobGrp, RP}, {JobGrpPid, JobGrpRef, [JobId]}}  
	 actions     = [],    %% {TransId, ActionId, Actions}
	                      %% contains the actions for which events have
                              %% been received
	 rem_actions = [],    %% {TransId, ActionId, RemainingActions}
	                      %% contains the actions received in prepare
	 meas_cnt    = 0,     %% current number of measurements
	 pms_alarm   = false  %% is large number of counter alarm sent
	}).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% start() -> ok.
%% 
%%========================================================================
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).


%%========================================================================
%% job_create_res() -> ok.
%% 
%% Result of a pm job creation
%%========================================================================
job_create_res(Result, JgPid, JobId) ->
    ?MODULE ! {job_create_res, {Result, JgPid, JobId}}.

%%========================================================================
%% job_group_exit() -> ok.
%% 
%% A pmsJobGroup has intentionaly exited.
%%========================================================================
job_group_exit(Id, JgPid, RP, Reason) ->
    ?MODULE ! {job_group_exit, {JgPid, Id, RP, Reason}}.

%%========================================================================
%% check_alarm() -> ok.
%% 
%% Ordered from pmsJob to check if no of jobs or no counter has
%% exeeded the max values.
%%========================================================================
check_alarm() ->
    gen_server:cast(?MODULE, check_alarm).


%%========================================================================
%% get_loop_data() -> ok.
%% 
%% Debug function
%%========================================================================
get_loop_data() ->
    gen_server:call(?MODULE, get_loop_data).



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% init(_) -> {ok, State}
%%========================================================================
init(_) ->
    Options = [{maxNoFiles, 30}, 
	       {size,       500},
	       {header,     {pmsDebug, get_versions, []}},
	       {zip,        true},
	       {local,      true}],
    logRamI:create_log(?PM_LOG, Options),
    Filters = os:getenv("rcs_pms_counter_filters", "off"),
    Filters == "on" andalso logRamI:set_severity(?PM_LOG, 9),

    init_active(sysEnv:role()).


init_active(active) ->
    process_flag(trap_exit, true), %% to receive terminate at shutdown
    put(name, {?MODULE, ?MODULE}), %% used by pms_cli to print process data
    ?LOG_RAM(?SEV_1, "Started~n"),
    pmsDb:mnesia_subscribe({table, pmJob, detailed}),
    pmsDb:mnesia_subscribe({table, measurementReader, detailed}),
    erlang:start_timer(?ROP_CLEAN_INTERVAL, self(), clean_rop_files),

    {ok, init_check(#state{})};
    
init_active(_Regular) ->
    ignore.

%%-------------------------------------------------------------
%% Check if there is already created jobs
%%-------------------------------------------------------------
init_check(State) ->
    ic(ets:tab2list(pmJob), State).

ic([], State) ->
    State;
ic([PmJob | T], State) -> 
    ic(T, ic_job(PmJob, [], State)).

ic_job(#pmJob{reportingPeriod = RP,
	      jobGroup        = JobGroup} = PmJob,
       Old,
       #state{job_groups = TJG} = State) ->
    TjgVal = maps:get({JobGroup, RP}, TJG, undefined),
    init_jobs(TjgVal, PmJob, Old, State).



%%========================================================================
%% handle_call(Msg, From, State) -> {reply, Res, State}
%%========================================================================
handle_call(get_loop_data, _From, State) ->
    {reply, format_state(State), State};

handle_call(get_proc_info, _From, State) ->
    {reply, 
     {server, 
      [{loop_size, erts_debug:flat_size(State)}, 
       {proc_info, pmsLib:get_proc_info(self())}]}, 
     State};

handle_call(reread_pmjobs, _From, State) ->
    ?LOG_RAM(?SEV_5, "Reread PmJob from mnesia~n"),
    {reply, ok, init_check(State)};

handle_call(Msg, _From, State) ->
    ?LOG_RAM(?SEV_WARNING, {"Received unknown handle_call msg: ~p~n", [Msg]}),
    {reply, ok, State}.


%%========================================================================
%% handle_cast(Msg, State) -> {noreply, State}
%%========================================================================
handle_cast({prepare_transaction, TransId, ActionId, Objects, NewNoOfJobs} = X, 
	    #state{actions = Actions} = State) ->
    p("### prepare_transaction~n  ~p~n  ~p~n", [X, Actions]),
    {ok, NewState} = prepare_transaction(lists:keymember(TransId, 1, Actions),
					 TransId, 
					 ActionId, 
					 Objects, 
					 NewNoOfJobs,
					 State),
    {noreply, NewState};

handle_cast({abort_transaction, TransId},
	    #state{rem_actions = RemAct,
		   actions     = Actions} = State) ->
    RemRemAct  = lists:keydelete(TransId, 1, RemAct),
    RemActions = lists:keydelete(TransId, 1, Actions),
    {noreply, State#state{rem_actions = RemRemAct,
			  actions     = RemActions}};

handle_cast(check_alarm, State) ->
    NewState = handle_check_alarm(State),
    {noreply, NewState};

handle_cast({test_terminate, Reason}, State) ->
    ?LOG_RAM(?SEV_1, {"Test terminate. Reason = ~p~n", [Reason]}),
    {stop, Reason, State};

handle_cast(Msg, State) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Received unknown handle_cast msg: ~p~n", [Msg]}),
    {noreply, State}.

%%========================================================================
%% handle_info(Info, State) -> {noreply, State}
%%========================================================================
handle_info({mnesia_table_event, Event},
	    State) ->
    p("### mnesia_table_event~n  ~p~n", [Event]),
    case check_event(Event, State) of
	{ok, NewState} ->
	    {noreply, NewState};
	Error ->
	    ?LOG_RAM(?SEV_ERROR, {"MNESIA EVENT ~n~p~n~p~n", [Error, Event]},
		     ?DEFAULT_BL_TO),
	    {noreply, State}
    end;

handle_info({job_group_exit, {Id, RP, Reason}}, 
	    #state{job_groups = TJG} = State) ->
    handle_job_group_exit(maps:get({Id, RP}, TJG, not_found), Id, Reason),
    {noreply, State#state{job_groups = maps:remove({Id, RP}, TJG)}};

handle_info({timeout, _Ref, clean_rop_files}, State) ->
    ?LOG_RAM(?SEV_5, {"Remove old ROP files.~n", []}),
    pmsDb:rop_file_delete_older(?ROP_EXPIRY_TIME),
    erlang:start_timer(?ROP_CLEAN_INTERVAL, self(), clean_rop_files),
    {noreply, State};

handle_info({disk_log, Node, Log, Reason}, State) ->
    ?LOG_RAM(?SEV_5, 
	     {"disk_log: node = ~p  Log = ~p  Reason = ~p~n", 
	      [Node, Log, Reason]}),
    {noreply, State};

handle_info({'DOWN', MRef, process, Pid, Reason}, State)
  when Reason == no_jobs orelse 
       Reason == normal ->
    ?LOG_RAM(?SEV_1,
	     {"PmJobGroup ~p terminated. Reason: ~p~n", 
	      [Pid, Reason]}),
    {noreply, handle_monitor_tjg(MRef, State)};

handle_info({'DOWN', MRef, process, Pid, Reason}, State) ->
    ?LOG_RAM(?SEV_1, 
	     {"PmJobGroup ~p terminated unexpectedly. Reason: ~p~n", 
	      [Pid, Reason]}),
    {stop, job_group_down, handle_monitor_tjg(MRef, State)};

handle_info(Info, State) ->
    ?LOG_RAM(?SEV_WARNING, {"Received unknown handle_info msg: ~p~n", [Info]}),
    {noreply, State}.


%%========================================================================
%% code_change(OldVsn, State, Extra) -> {ok, State}
%%========================================================================
code_change(OldVsn, State, _Extra) ->
    ?LOG_RAM(?SEV_1, {"code change ~p~n", [OldVsn]}),
    {ok, State}.


%%========================================================================
%% terminate(Reason, State) -> Reason
%%========================================================================
terminate(Reason, #state{job_groups = TJG} = State) ->
    ?LOG_RAM(?SEV_1, {"terminate(~p, ~p)~n", [Reason, State]}),
    maps:fold(fun terminate_job_grp/3, [], TJG),
    Reason.

terminate_job_grp({JobGrpId, _RP}, {JobGrpPid, JobGrpRef, _}, Acc) ->
    erlang:demonitor(JobGrpRef, [flush, info]),
    [pmsJobGroup:terminate(JobGrpPid, JobGrpId) | Acc].
    



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% prepare_transaction(IsInActions, TransId, ActionId, Objects, State) ->
%%   {ok, State} | {error, Reason}
%% 
%% This function is run twice from comsaTransaction server with
%% the same transaction id.
%% If the transaction id is found in 'actions', replace the old 
%% with the new.
%% 
%% There are two attributes in state handling the transactions,
%% 'actions', and 'rem_actions'. Both contains a proplist with
%% the transaction data as the key, and a list of events as the value.
%% 
%% The objects received in prepare_transaction are stored as values
%% in 'rem_actions'. 
%% 
%% When a mnesia event is received that particular event is removed from
%% 'rem_actions' and added to 'actions'. 
%% When rem_actions is empty the server starts to update
%% the affected pmsJobGroup(s). 
%% 
%% All this handling of events is done to collect all events for each job
%% and handle them in one go per job.
%%  
%% Another reason is not to disturb the application with unnecessary
%% subscribe messages. 
%% An example:
%% If a job contains several MR, and if the mnesia events were handle as 
%% they arrive, each MR event would cause a subscribe to be sent
%% to the application.
%% 
%%========================================================================
prepare_transaction(true, 
		    TransId, 
		    ActionId, 
		    Objects,
		    NewNoOfJobs,
		    #state{actions     = Actions,
			   rem_actions = RemAct} = State) ->
    RmActions = lists:keydelete(TransId, 1, Actions),
    RmRemAct  = lists:keydelete(TransId, 1, RemAct),
    prepare_transaction(false, 
			TransId, 
			ActionId, 
			Objects,
			NewNoOfJobs,
			State#state{actions     = RmActions,
				    rem_actions = RmRemAct});
prepare_transaction(false,
		    TransId, 
		    ActionId, 
		    Objects,
		    NewNoOfJobs,
		    #state{actions     = Actions,
			   rem_actions = RemAct} = State) ->

    PrintObjs = [{What, element(1, Rec), element(2, Rec)} ||
		    {What, Rec} <- Objects],

    ?LOG_RAM(?SEV_5, 
	     {"Prepare transaction.~n"
	      "  TransactionId = ~p~n"
	      "  Objects = ~n    ~p~n", [ActionId, PrintObjs]}),
	     
    {ok, MaxJobs} = pmsDb:meas_capabilities_get(maxNoOfJobs),
    NewState      = check_alarm({NewNoOfJobs, MaxJobs}, State),

    NewActions    = [{TransId, ActionId, []} | Actions],
    NewRemActions = [{TransId, ActionId, Objects} | RemAct],
    {ok, NewState#state{actions     = NewActions,
			rem_actions = NewRemActions}}.



%%========================================================================
%% check_event(Event, State) -> {ok, State} | {error, Reason}
%% 
%% Mnesia events are received after the prepare call and after the transaction
%% is commited. 
%% If the rem_action for this action id is empty all the events for that
%% transaction are received. In that case start to update the PMS internal
%% processes and possibly affected applications.
%% 
%%========================================================================
check_event(Event, State) ->
    p("~n~n#### check_event event     ~n  ~p~n", [Event]),
    p("#### check_event actions   ~n  ~p~n", [State#state.actions]),
    p("#### check_event remaining ~n  ~p~n", [State#state.rem_actions]),
    ce_print(Event),
    ce_rc(ce(Event, State)).

ce_print({What, Class, Rec, _, Aid}) when is_tuple(Rec) ->
    ?LOG_RAM(?SEV_5, 
	     ?LFUN({"Check event.~n"
		    "  TransactionId = ~p~n"
		    "  Event         = ~p~n",
		    [Aid, {What, Class, element(2, Rec)}]}), ?DEFAULT_BL_TO);
ce_print({What, Class, Id, _, Aid}) ->
    ?LOG_RAM(?SEV_5, 
	     {"Check event.~n"
	      "  TransactionId = ~p~n"
	      "  Event         = ~p~n",
	      [Aid, {What, Class, Id}]}, ?DEFAULT_BL_TO).

%%-------------------------------------------------------
%% result code
%%-------------------------------------------------------
ce_rc({ok, State}) ->
    {ok, trans_end(State)};
ce_rc(Error) ->
    Error.


%%=======================================================
%% dispatch the event
%%=======================================================
%%------------------------------
%% pmJob
%%------------------------------
ce({write, pmJob, New, Old, Aid}, State) ->
    {ok, ce_write(Aid, New, Old, State)};
%% This is a probably a dirty delete from a test suite.
ce({delete, pmJob, PmJobId, Old, {dirty, _}}, State) ->
    ?LOG_RAM(?SEV_WARNING, {"Unexpected delete of ~p ~p~n", [PmJobId, Old]}),
    {ok, _NewState} = delete_pm_job(Old, delete, State);
%% Mnesia has two flavors of delete events
ce({delete, pmJob, #pmJob{pmJobId = PmJobId}, Old, Aid}, State) ->
    {ok, ce_delete_job(Aid, {pmJob, PmJobId}, Old, State)};
ce({delete, pmJob, PmJobId, Old, Aid}, State) ->
    {ok, ce_delete_job(Aid, PmJobId, Old, State)};
%%------------------------------
%% measurementReader
%%------------------------------
ce({write, measurementReader, New, Old, Aid}, State) ->
    State2 = ce_write(Aid, New, Old, State),
    {ok, State2};
%% Mnesia has two flavors of delete events
ce({delete, 
    measurementReader,
    #measurementReader{measurementReaderId = MrId}, 
    Old, 
    Aid}, 
   State) ->
    {ok, ce_delete_mr(Aid, {measurementReader, MrId}, Old, State)};
ce({delete, measurementReader, MrId, Old, Aid}, State) ->
    {ok, ce_delete_mr(Aid, MrId, Old, State)};
%%------------------------------
%% unknown record 
%%------------------------------
ce(Event, _State) ->
    ?LOG_RAM(?SEV_WARNING, {"UNKNOWN MNESIA EVENT ~p~n", [Event]}),
    {error, {unknown_event, Event}}.


%%=======================================================
%% write
%% 
%% move the received event from rem_actions to actions
%%=======================================================
ce_write(Aid,
	 New, 
	 Old,
	 #state{rem_actions = RemAct,
		actions     = Actions} = State) ->
    cew(lists:keytake(Aid, 2, RemAct),
	lists:keytake(Aid, 2, Actions),
	Aid,
	New, 
	Old,
	State).

cew({value, {Tid, _, TidRemAct},  RemRemAct},
    {value, {Tid, _, TidActions}, RemActions},
    Aid,
    New, 
    Old,
    State) ->
    NewTidRemAct  = TidRemAct -- [{created, New}],
    NewTidActions = [{created, New, Old} | TidActions], 
    State#state{rem_actions = [{Tid, Aid, NewTidRemAct}  | RemRemAct],
		actions     = [{Tid, Aid, NewTidActions} | RemActions]};
%% This clause is for the pmJob current state updates from pmsJob
cew(_, _, _, _, _, State) ->
    State.


%%=======================================================
%% delete
%% 
%% move the received event from rem_actions to actions
%%=======================================================
ce_delete_job(Aid,
	      {_, DelId}, 
	      Old,
	      #state{rem_actions = RemAct,
		     actions     = Actions} = State) ->
    {value, {Tid, _, TidRemAct},  RemRemAct}  = lists:keytake(Aid, 2, RemAct),
    {value, {Tid, _, TidActions}, RemActions} = lists:keytake(Aid, 2, Actions),
 
    NewTidRemAct  = TidRemAct -- [{deleted, DelId}],
    NewTidActions = [{deleted, Old} | TidActions], 

    State#state{rem_actions = [{Tid, Aid, NewTidRemAct}  | RemRemAct],
		actions     = [{Tid, Aid, NewTidActions} | RemActions]}.


ce_delete_mr(Aid,
	     {_, DelId}, 
	     Old,
	     #state{rem_actions = RemAct,
		    actions     = Actions} = State) ->
    RemActVal  = lists:keytake(Aid, 2, RemAct),
    ActionsVal = lists:keytake(Aid, 2, Actions),
    ce_delete_mr_2(Aid, DelId, Old, RemActVal, ActionsVal, State).
   
%%-------------------------------------------------------
%% If only the pmJob is deleted in a transaction, 
%% all the associated MRs will also be deleted.
%% However, prepare transaction contains only the pmJob,
%% thus, the possible MRs will not be found in 
%% Actions/RemainingActions
%%-------------------------------------------------------
ce_delete_mr_2(_Aid, _DelId, _Old, false, false, State) ->
    State;
ce_delete_mr_2(Aid,
	       DelId, 
	       Old,
	       {value, {Tid, _, TidRemAct},  RemRemAct}, 
	       {value, {Tid, _, TidActions}, RemActions},
	       State) ->
    NewTidRemAct  = TidRemAct -- [{deleted, DelId}],
    NewTidActions = [{deleted, Old} | TidActions], 
    
    State#state{rem_actions = [{Tid, Aid, NewTidRemAct}  | RemRemAct],
		actions     = [{Tid, Aid, NewTidActions} | RemActions]}.


%%========================================================================
%% trans_end
%%  
%% Create/delete/modify the job instances included in the transaction.
%% 
%% 'rem_actions' contains a list of events that have not yet been received.
%% 'actions'     contains a list of events that have been received
%% 
%% If there is an action id in 'rem-actions' which event list is empty,
%% then all events are received for that transaction. In such cases
%% find the action id in 'actions'; its event list contains the
%% pm jobs that where affect in this transaction.
%% 
%% When a pm job is created, updated or deleted the event list contains 
%% both a pm job and zero or several measurement readers associated
%% with the pm job. In these cases the measurement readers are ignored here.
%% (pmsJob module will find its measurement readers in the database 
%%  when it is notified, via pmsJobGroup that the job is changed)
%% 
%% If new measurement readers are added/deleted there will not
%% be a pmJob event in the transaction. However, the pmsJob should still
%% be notified that the pm job is updated. In these case the parent pm job
%% must be found so it can be notified.
%% 
%%========================================================================
trans_end(#state{actions = A, rem_actions = RemAct} = State) ->
    p("~n~n#### trans_end actions   ~n  ~p~n", [A]),
    p("#### trans_end remaining ~n  ~p~n", [RemAct]),
    te([Aid || {_, Aid, []} <- RemAct], State).


%% No transaction found
te([],
   State) ->
    State;
%% Found a transaction for which all included events have been received
te([Aid],
   #state{rem_actions = RemAct,
	  actions     = Actions} = State) ->
    {value, {_, _, []},         RemRemAct}  = lists:keytake(Aid, 2, RemAct),
    {value, {_, _, TidActions}, RemActions} = lists:keytake(Aid, 2, Actions),

    UpdatedJobs = te_get_updated_jobs(TidActions, #{}),
    NewState = maps:fold(fun te_job_groups_cut/3, State, UpdatedJobs),
    p("#### trans_end updated jobs new state~n  ~p~n", [NewState]),
    NewState#state{rem_actions = RemRemAct,
		   actions     = RemActions}.


%%=======================================================
%% check what pm jobs and groups are affected 
%% by the transaction
%%=======================================================
te_get_updated_jobs([], Jobs) ->
    Jobs;
%%------------------------------
%% pmJob
%%------------------------------
te_get_updated_jobs([{created, #pmJob{pmJobId = JobId} = PmJob, []} | T],
		    Jobs) ->
    te_get_updated_jobs(T, Jobs#{JobId => {updated, [PmJob], []}}); 
te_get_updated_jobs([{created, #pmJob{pmJobId = JobId} = PmJob, Old} | T],
		    Jobs) ->
    te_get_updated_jobs(T, Jobs#{JobId => {updated, [PmJob], Old}}); 
te_get_updated_jobs([{deleted, [#pmJob{pmJobId = JobId} = PmJob]} | T],
		    Jobs) ->
    te_get_updated_jobs(T, Jobs#{JobId => {deleted, [PmJob]}}); 
%%------------------------------
%% measurementReader
%%------------------------------
te_get_updated_jobs([{created, 
		      #measurementReader{measurementReaderId = MrId},
		      _} | T],
		    Jobs) ->
    {ME, SF, PM, J, _} = MrId,
    {ok, PmJob} = pmsDb:pm_job_get({ME, SF, PM, J}),
    NewVal = maps:get({ME, SF, PM, J}, Jobs, {updated, PmJob, PmJob}),
    te_get_updated_jobs(T, Jobs#{{ME, SF, PM, J} => NewVal});
te_get_updated_jobs([{deleted, 
		      [#measurementReader{measurementReaderId = MrId}]} | T],
		    Jobs) ->
    {ME, SF, PM, J, _} = MrId,
    {ok, PmJob} = pmsDb:pm_job_get({ME, SF, PM, J}),
    NewVal = maps:get({ME, SF, PM, J}, Jobs, {updated, PmJob, PmJob}),
    te_get_updated_jobs(T, Jobs#{{ME, SF, PM, J} => NewVal});
%%------------------------------
%% This can happen if there are
%% two parallel transacetions
%% deleting the same MO
%%------------------------------
te_get_updated_jobs([{deleted, []} | T],
		    Jobs) ->
    te_get_updated_jobs(T, Jobs).



%%=======================================================
%% create/update/takeover job groups 
%%=======================================================
te_job_groups_cut(_, 
		  {updated, 
		   [#pmJob{reportingPeriod = RP, 
			   jobGroup        = NewJobGrp} = NewJob],
		   []},
		  #state{job_groups = TJG} = State) ->
    TjgVal = maps:get({NewJobGrp, RP}, TJG, undefined),
    te_jg_cut(TjgVal, NewJob, [], State);
te_job_groups_cut(_, 
		  {updated, 
		   [#pmJob{reportingPeriod = RP, 
			   jobGroup        = _NewJobGrp} = NewJob],
		   [#pmJob{jobGroup = OldJobGrp}] = OldJob},
		  #state{job_groups = TJG} = State) ->
    TjgVal = maps:get({OldJobGrp, RP}, TJG, undefined),
    te_jg_cut(TjgVal, NewJob, OldJob, State);
te_job_groups_cut(_, 
		  {deleted, 
		   [#pmJob{reportingPeriod = RP, 
			   jobGroup        = JobGrp} = PmJob]},
		  #state{job_groups = TJG} = State) ->
    TjgVal = maps:get({JobGrp, RP}, TJG, undefined),
    NewTGJ = te_jg_delete(TjgVal, TJG, PmJob, {JobGrp, RP}),
    State#state{job_groups = NewTGJ}.




init_jobs(JobGroupVals, NewJob, OldJob, State) ->
    te_jg_cut(JobGroupVals, NewJob, OldJob, State).

%%-------------------------------------------------------------
%% te_jg_cut(JobGroupVals, NewJob, OldJob, State) -> State
%% 
%% PmJob created/updated/takeover. 
%% 
%% Create a new job group if needed.
%% Also create a default job group for the requested RP, 
%% if not already created.
%%  
%% Invoke update for the requested job group
%% if a job is created or updated.
%% 
%%-------------------------------------------------------------
%%----------------
%% Create
%%----------------
te_jg_cut(undefined, 
	  #pmJob{pmJobId         = JobId, 
		 reportingPeriod = RP, 
		 jobGroup        = JobGrp} = NewJob,
	  OldJob,
	  #state{job_groups = TJG} = State) ->
    case job_group_create(JobGrp, JobId, RP, TJG) of
	{{ok, JgPid}, NewTJG} -> 
	    job_group_update(false, JgPid, NewJob, OldJob),
	    State#state{job_groups = NewTJG};
	{_Error, NewTJG} -> 
	    State#state{job_groups = NewTJG}
    end;
    
%%----------------
%% Takeover
%%----------------
te_jg_cut({_JobGrpPid, _Ref, _JobIds}, 
	  #pmJob{pmJobId         = JobId, 
		 reportingPeriod = RP, 
		 jobGroup        = NewJobGrp},
	  [#pmJob{jobGroup = OldJobGrp}],
	  #state{job_groups = TJG} = State) 
  when NewJobGrp /= OldJobGrp ->
    Maps = maps:get({NewJobGrp, RP}, TJG, false),
    {_, NewState} = job_group_takeover(Maps,
				       NewJobGrp,
				       OldJobGrp, 
				       JobId, 
				       RP, 
				       State),
    NewState;
%%----------------
%% Updated
%%----------------
te_jg_cut({JobGrpPid, Ref, JobIds}, 
	  #pmJob{pmJobId         = JobId, 
		 reportingPeriod = RP, 
		 jobGroup        = JobGrp} = NewJob,
	  OldJob,
	  #state{job_groups = TJG} = State) ->
    Member = lists:member(JobId, JobIds),
    job_group_update(not Member, JobGrpPid, NewJob, OldJob),
    NewTJG = TJG#{{JobGrp, RP} => {JobGrpPid,
				   Ref, 
				   tjc_jobs(Member, JobId, JobIds)}},
    State#state{job_groups = NewTJG}.
    


tjc_jobs(true,  _JobId, JobIds) ->
    JobIds;
tjc_jobs(false, JobId, JobIds) ->
    [JobId | JobIds].


%%-------------------------------------------------------------
%% PmJob deleted
%%-------------------------------------------------------------
te_jg_delete(undefined, TJG, _, _) ->
    TJG;
te_jg_delete({JobGrpPid, Ref, JobIds},
	  TJG, 
	  #pmJob{pmJobId = JobId} = PmJob,
	  Key) ->
    delete_job(JobGrpPid, PmJob, delete),
    TJG#{Key => {JobGrpPid, Ref, lists:delete(JobId, JobIds)}}.

 



%%========================================================================
%% delete_pm_job([PmJob], Reason, State) -> {ok, State} | {error, Reason}
%% 
%% pmJob instance deleted.
%% 
%%========================================================================
delete_pm_job([], _, State) ->
    {ok, State};
delete_pm_job([#pmJob{pmJobId         = JobId,
		      reportingPeriod = RP,
		      jobGroup        = JobGroup} | Rem],
	      Reason,
	      #state{job_groups = TJG} = State) ->
    GvGid  = maps:get({JobGroup, RP}, TJG, undefined),
    NewTJG = dpj(GvGid, Reason, {JobGroup, RP}, JobId, TJG),
    delete_pm_job(Rem, Reason, State#state{job_groups = NewTJG}).
    
dpj(undefined, _Reason, _Key, _PmJob, TJG) ->   
    TJG;
dpj({Pid, Ref, JobIds}, Reason, Key, JobId, TJG) ->
    ok = delete_job(Pid, JobId, Reason),
    TJG#{Key => {Pid, Ref, JobIds}}.
    


%%===========================================================================
%% pmsJobGroup functions
%%===========================================================================


%%=======================================================
%% job_group_create(GroupId, JobId, RP) -> 
%%    {ok, JobGroupVal} |  {error, Reason}
%% 
%% create a job group
%%=======================================================
job_group_create(JobGrpId, JobId, RP, TJG) ->
    Maps = maps:get({?COMMON_JG, RP}, TJG, create),
    JgRes = pmsJobGroup:create(JobGrpId, {create, JobId, RP}),
    {Res, TJG2} = jg_create(JgRes, TJG, JobGrpId, RP, [JobId]),
    {_,   TJG3} = jg_create_default(JobGrpId, Maps, RP, TJG2),
    {Res, TJG3}.
	


jg_create({ok, exists}, TJG, _JobGrpId, _RP, _JobId) ->
    {{ok, exists}, TJG};
jg_create({ok, JobGrpPid}, TJG, JobGrpId, RP, JobId) ->
    MRef = erlang:monitor(process, JobGrpPid),
    {{ok, JobGrpPid}, TJG#{{JobGrpId, RP} => {JobGrpPid, MRef, JobId}}};
jg_create(Error, TJG, JobGrpId, RP, JobId) ->
    ?LOG_RAM(?SEV_ERROR, 
	     {"Job Group create error."
	      "  JobGroupId = ~p~n"
	      "  RP         = ~p~n"
	      "  JobId      = ~p~n"
	      "  Reason     = ~p~n", 
	      [JobGrpId, RP, JobId, Error]}),
    {{error, Error}, TJG}.


%%=======================================================
%% jg_create_default(GroupId, PmGroups, RP) -> 
%%    {ok, JobGroupVal} |  {error, Reason}
%% 
%% create a default job group if not already existing
%%=======================================================
%% creating default jobGroup
jg_create_default(?COMMON_JG, _, _, TJG) ->
    {ok, TJG};
%% default job group not defined
jg_create_default(_, create, RP, TJG) ->
    JgRes = pmsJobGroup:create(?COMMON_JG, {default_grp, no_job_id, RP}),
    jg_create(JgRes, TJG, ?COMMON_JG, RP, []);
%% default job group already defined
jg_create_default(_, _, _, TJG) ->
    {ok, TJG}.



%%=======================================================
%% job_group_takeover(IsDefined, NewGroupId, OldGroupId, JobId, RP, State) -> 
%%    {NewJobGrpPid, State}
%% 
%% takeover a job
%%=======================================================
job_group_takeover(false, NewGrpId, OldGrpId, JobId, RP, State) ->
    case pmsJobGroup:create(NewGrpId, {takeover, JobId, RP}) of
	{ok, JobGrpPid} ->
	    jg_takeover(JobId, RP, OldGrpId, NewGrpId, JobGrpPid, State);
	Error ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"Job Group takeover. Create error."
		      "  NewJobGroupId = ~p~n"
		      "  OldJobGroupId = ~p~n"
		      "  JobId         = ~p~n"
		      "  Reason        = ~p~n", 
		      [NewGrpId, OldGrpId, JobId, Error]}),
	    {error_pid, State}
    end;
job_group_takeover({JobGrpPid, _, _}, NewGrpId, OldGrpId, JobId, RP, State) ->
    jg_takeover(JobId, RP, OldGrpId, NewGrpId, JobGrpPid, State).


jg_takeover(JobId, 
	    RP,
	    OldGrpId,
	    NewGrpId,
	    NewJobGrpPid,
	    #state{job_groups = TJG} = State) ->
    {Pid, Ref, JobIds} = maps:get({OldGrpId, RP}, TJG, not_found),
    ok = pmsJobGroup:job_takeover(Pid, JobId, NewJobGrpPid, NewGrpId),
    MRef = erlang:monitor(process, NewJobGrpPid),
    ?LOG_RAM(?SEV_5, {"Monitor pm_grp ~p ~p~n", [MRef, NewJobGrpPid]}),
    OldTJG = TJG#{{OldGrpId, RP} => {Pid, Ref, lists:delete(JobId, JobIds)}},
    NewTJG = OldTJG#{{NewGrpId, RP} => {NewJobGrpPid, MRef, [JobId]}},
    {NewJobGrpPid, State#state{job_groups = NewTJG}}.



%%=======================================================
%% update a job group
%%=======================================================
job_group_update(false, JgPid, NewJob, OldJob) ->
    pmsJobGroup:job_update(JgPid, NewJob, OldJob);
job_group_update(true, JgPid, #pmJob{pmJobId = JobId} = NewJob, OldJob) ->
    pmsJobGroup:job_create(JgPid, NewJob, OldJob),
    receive
	{job_create_res, {ok, JgPid, JobId}} ->
	    ok;
	{job_group_exit, {JgPid, JgId, RP, Reason}} ->
	    {error, {job_group_exit, {JgPid, JgId, RP, Reason}}}
    after 20000 ->
	    Reason = {error, {job_create, timeout}},
	    ?LOG_RAM(?SEV_ERROR, 
		     {"Job create error."
		      "  JobGroupPid = ~p~n"
		      "  JobId       = ~p~n"
		      "  Reason      = ~p~n", 
		      [JgPid, JobId, Reason]}),
	    Reason
    end.



%%=======================================================
%% delete a job
%%=======================================================
delete_job(Pid, PmJob, Reason) ->
    ok = pmsJobGroup:job_delete(Pid, PmJob, Reason).






%%===========================================================================
%% Misc functions
%%===========================================================================


%%========================================================================
%% check_alarm(Jobs, State) -> State 
%% 
%% 
%% 
%%========================================================================
check_alarm({JobCnt,  MaxJobs}, 
	    #state{pms_alarm = true} = State) 
  when JobCnt =< MaxJobs ->
    ?LOG_RAM(?SEV_1, 
	     {"Cleared alarm LargeNumberOfCounters:"
	      " max number of jobs running~n",
	      []}),
    comsaI:clear_alarm('LargeNumberOfCounters', get_pm_ldn()),
    State#state{pms_alarm = false};
check_alarm({JobCnt, MaxJobs}, 
	    #state{pms_alarm = false} = State) 
  when JobCnt > MaxJobs ->
    %% send alarm
    ?LOG_RAM(?SEV_1, 
	     {"Sent alarm LargeNumberOfCounters:"
	      " max number of jobs running~n",
	      []}),
    comsaI:send_alarm('LargeNumberOfCounters', 
		      warning,
		      get_pm_ldn(), 
 		      "max number of jobs running"),
    State#state{pms_alarm = true};
check_alarm(_, State) ->
    State.



%%========================================================================
%% handle_job_group_exit(Result, Id, Reason) -> ok. 
%% 
%% 
%% 
%%========================================================================
handle_job_group_exit(not_found, Id, Reason) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Job Group exited but Group Id not found.~n"
	      "  JobGroupId = ~p~n"
	      "  Reason     = ~p~n",
	      [Id, Reason]});
handle_job_group_exit({_Pid, _Ref, []}, Id, Reason) ->
    ?LOG_RAM(?SEV_1, 
	     {"Job Group exited.~n"
	      "  JobGroupId = ~p~n"
	      "  Reason     = ~p~n",
	      [Id, Reason]});
handle_job_group_exit({_Pid, _Ref, RemJobs}, Id, Reason) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"Job Group exited. Remaining Jobs."
	      "  JobGroupId = ~p~n"
	      "  Jobs       = ~p~n"
	      "  Reason     = ~p~n", 
	      [Id, RemJobs, Reason]}).



%%========================================================================
%% handle_check_alarm(State) -> State
%% 
%% send alarm if noof active jobs exceeds the max value
%% clear alarm if noof active jobs is less than the max value
%%========================================================================
handle_check_alarm(State) ->
    hca(get_active_jobs(),
	pmsDb:meas_capabilities_get(maxNoOfJobs),
	State).

hca(N, {ok, MaxJobs},  #state{pms_alarm = false} = State)
  when N > MaxJobs ->
    ?LOG_RAM(?SEV_1, 
	     {"Sent alarm LargeNumberOfCounters:"
	      " max number of jobs running~n",
	      []}),
    comsaI:send_alarm('LargeNumberOfCounters', 
		      warning,
		      get_pm_ldn(), 
 		      "max number of jobs running"),
    State#state{pms_alarm = true};
hca(N, {ok, MaxJobs},  #state{pms_alarm = true} = State)
  when N > MaxJobs ->
    State;
hca(_, _, #state{pms_alarm = true} = State) ->
    ?LOG_RAM(?SEV_1, 
	     {"Cleared alarm LargeNumberOfCounters:"
	      " max number of jobs running~n",
	      []}),
    comsaI:clear_alarm('LargeNumberOfCounters', get_pm_ldn()),
    State#state{pms_alarm = false};
hca(_, _, State) ->
    State.
    



%%========================================================================
%% handle_monitor_tjg(Ref, State) -> State
%% 
%%========================================================================
handle_monitor_tjg(MRef, #state{job_groups = TJG} = State) ->
    Fun = fun(_, {_, Ref, _}) -> MRef /= Ref end,
    State#state{job_groups = maps:filter(Fun, TJG)}.



%%========================================================================
%% get_active_jobs() -> integer()
%% 
%%========================================================================
get_active_jobs() -> 
    ets:foldl(fun(#pmJob{currentJobState = ?JobState_ACTIVE}, Acc) -> Acc + 1;
		 (_,                                          Acc) -> Acc 
	      end,
	      0,
	      pmJob).		      


%%========================================================================
%% get_pm_ldn() -> LDN to Pm=1
%% 
%%========================================================================
get_pm_ldn() ->
    MEData = comsaI:get_managed_element_data(),
    NeMEId = case proplists:get_value(networkManagedElementId, MEData) of
		 undefined -> "1";
		 Id        -> Id
	     end,
    [list_to_binary("ManagedElement=" ++ NeMEId),
     <<"SystemFunctions=1">>,
     <<"Pm=1">>].

   

%%===========================================================================
%% Misc functions
%%===========================================================================


format_state(State) ->
    F       = record_info(fields, state),
    [_ | L] = tuple_to_list(State), 
    lists:zip(F,L).
    


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
 

p(_,_) -> ok.

%%p(S,A) -> io:format(S,A).


%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_netconf.erl %
%%% @version /main/R3A/R4A/1

%%% @doc 
%%% 
%%% 
%%% @end

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R3A/1      2014-11-28 uabesvi     Created
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-module(pes_netconf).

-include("pes_test.hrl").

-export([create_job/2]).
-export([delete_job/2]).

-export([update_me/3]).


-define(ME, "ManagedElement=1").
%%-define(ME, "ManagedElement=kalle").

-define(MO_ID, "1").
%%-define(ME_ID, "kalle").

-define(COMTOP,  {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(ECIM_PM, {xmlns, "urn:com:ericsson:ecim:ECIM_PMEventM"}).

-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).



    
%%========================================================================
%% update_me(UserLabel, LogicalName) -> ok | {error, Reason}
%%
%% JobId - string()
%% Args  - [Arg]
%% Arg   - {"granularityPeriod", P} | {"reportingPeriod", P} |
%%         {"requestedJobState", Rjs} | {"jobPriority", Jp}
%% P     - 1..9  (See defines on top of the file)
%% Rjs   - 1..2  (See defines on top of the file)
%% Jp    - 1..3  (See defines on top of the file)
%%========================================================================
update_me(ME, UserLabel, LogicalName) ->

    ct:pal(" pes_netconf:update_me ~p~n", [{UserLabel, LogicalName}]),

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], [ME]},
			      userLabel(UserLabel),
			      networkManagedElementId(LogicalName)
			      ]
			    }).

userLabel(undefined) ->
    {userLabel, ?DELETE, []};
userLabel(Val) ->
    {userLabel, [], [Val]}.

networkManagedElementId(undefined) ->
    {networkManagedElementId, ?DELETE, []};
networkManagedElementId(Val) ->
    {networkManagedElementId, [], [Val]}.


%%========================================================================
%% create_job(JobId, Args) -> ok | {error, Reason}
%%
%% JobId - string()
%% Args  - [Arg]
%% Arg   - {"granularityPeriod", P} | {"reportingPeriod", P} |
%%         {"requestedJobState", Rjs} | {"jobPriority", Jp}
%% P     - 1..9  (See defines on top of the file)
%% Rjs   - 1..2  (See defines on top of the file)
%% Jp    - 1..3  (See defines on top of the file)
%%========================================================================
create_job(Producer, Attrs) ->

    ct:pal(" Attrs ~p~n", [Attrs]),


    {JobId, FormattedAttrs} = format_attrs(Attrs), 

    JobArgs = [{eventJobId, [], [JobId]}] ++ FormattedAttrs,
    
    ct:pal(" JOB ARGS ~p~n", [JobArgs]),

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], [?MO_ID]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], [?MO_ID]},
				{'PmEventM',
				 [?ECIM_PM],
				 [{pmEventMId, [], [?MO_ID]},
				  {'EventProducer', 
				   [], 
				   [{eventProducerId, [], [Producer]},
				    {'EventJob', [], JobArgs}]}]
				}]}]
			    }).



%%========================================================================
%% delete_job(JobId) -> ok | {error, Reason}
%%
%% JobId - string()
%%========================================================================
delete_job(Producer, Attrs) ->

    {JobId, _FormattedAttrs} = format_attrs(Attrs), 

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], [?MO_ID]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], [?MO_ID]},
				{'PmEventM',
				 [?ECIM_PM],
				 [{pmEventMId, [], [?MO_ID]},
				  {'EventProducer', 
				   [], 
				   [{eventProducerId, [], [Producer]},
				    {'EventJob', 
				     ?DELETE, 
				     [{eventJobId, [], [JobId]}]}]}]
				}]}]
			    }).


%% {eventJobId,
%%  description,
%%  eventFilter,
%%  requestedJobState,
%%  currentJobState,
%%  fileOutputEnabled,
%%  streamDestinationIpAddress,
%%  streamDestinationPort,
%%  reportingPeriod,
%%  streamOutputEnabled,
%%  jobControl,
%%  eventGroupRef,
%%  eventTypeRef,
%%  fileCompressionType,
%%  streamCompressionType}



format_attrs(#eventJob{eventJobId = Job} = EventJob) ->
    Fields = record_info(fields, eventJob),
    Zip = lists:zip([eventJob | Fields], tuple_to_list(EventJob)),
    {Job, cj_attrs(Zip, [])}.
	      


%% Done
cj_attrs([], Acc) ->
    Acc;
%% Ignored attributes
cj_attrs([{eventJob, _Val} | T], Acc) ->
    cj_attrs(T, Acc);
cj_attrs([{eventJobId, _Val} | T], Acc) ->
    cj_attrs(T, Acc);
cj_attrs([{currentJobState, _Val} | T], Acc) ->
    cj_attrs(T, Acc);
cj_attrs([{description, _Val} | T], Acc) ->
    cj_attrs(T, Acc);
cj_attrs([{jobControl, _Val} | T], Acc) ->
    cj_attrs(T, Acc);
%% Real attributes
cj_attrs([{eventFilter, Val} | T], Acc) ->
    Add = eventFilter(Val),
    cj_attrs(T, r(a(f(Add), Acc)));
cj_attrs([{requestedJobState, Val} | T], Acc) ->
    Add = requestedJobState(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{eventGroupRef, Val} | T], Acc) ->
    Add = eventGroupRef(Val),
    cj_attrs(T, a(f(Add), Acc));
cj_attrs([{eventTypeRef, Val} | T], Acc) ->
    Add = eventTypeRef(Val),
    cj_attrs(T, a(f(Add), Acc));
cj_attrs([{filterIds, Val} | T], Acc) ->
    Add = filterIds(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{fileOutputEnabled, Val} | T], Acc) ->
    Add = fileOutputEnabled(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{reportingPeriod, Val} | T], Acc) ->
    Add = reportingPeriod(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{fileCompressionType, Val} | T], Acc) ->
    Add = fileCompressionType(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{streamOutputEnabled, Val} | T], Acc) ->
    Add = streamOutputEnabled(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{streamCompressionType, Val} | T], Acc) ->
    Add = streamCompressionType(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{streamDestinationIpAddress, Val} | T], Acc) ->
    Add = streamDestinationIpAddress(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([{streamDestinationPort, Val} | T], Acc) ->
    Add = streamDestinationPort(Val),
    cj_attrs(T, a(Add, Acc));
cj_attrs([H | T], Acc) ->
    ct:pal("~p ~p Unknown attribute ~p~n", [?MODULE, create_job, H]),
    cj_attrs(T, Acc).


requestedJobState(Val) ->
    [{requestedJobState, [], [state(Val)]}].


eventFilter(undefined) ->
    [{eventFilter, ?DELETE, []}];
eventFilter(Val) ->
    [[{eventFilter, [], ef(Name, Value)}] || {Name, Value} <- Val].


ef(Name, Value) ->
    [{filterName,  [], [Name]},
     {filterValue, [], [Value]}].


eventGroupRef(undefined) ->
    [{eventGroupRef, ?DELETE, []}];
eventGroupRef(Val) ->
    [[{eventGroupRef, [], [V]}] || V <- Val].

eventTypeRef(undefined) ->
    [{eventTypeRef, ?DELETE, []}];
eventTypeRef(Val) ->
    [[{eventTypeRef, [], [V]}] || V <- Val].

filterIds(_Val) ->
    [].

fileOutputEnabled(true) ->
    [{fileOutputEnabled, [], ["true"]}];
fileOutputEnabled(_) -> 
    [{fileOutputEnabled, [], ["false"]}].

streamOutputEnabled(true) ->
    [{streamOutputEnabled, [], ["true"]}];
streamOutputEnabled(_) -> 
    [{streamOutputEnabled, [], ["false"]}].

fileCompressionType(undefined) ->
    [{fileCompressionType, ?DELETE, []}];
fileCompressionType(Val) ->
    [{fileCompressionType, [], [Val]}].

streamCompressionType(undefined) ->
    [{streamCompressionType, ?DELETE, []}];
streamCompressionType(Val) ->
    [{streamCompressionType, [], [Val]}].

streamDestinationIpAddress(undefined) ->
    [{streamDestinationIpAddress, ?DELETE, []}];
streamDestinationIpAddress(Val) ->
    [{streamDestinationIpAddress, [], [Val]}].

streamDestinationPort(undefined) ->
    [{streamDestinationPort, ?DELETE, []}];
streamDestinationPort(Val) ->
    [{streamDestinationPort, [], [integer_to_list(Val)]}].

reportingPeriod(undefined) ->
    [{reportingPeriod, ?DELETE, []}];
reportingPeriod(Val) ->
    [{reportingPeriod, [], [rp(Val)]}].

rp(?TEN_SECONDS)    -> "TEN_SECONDS";
rp(?THIRTY_SECONDS) -> "THIRTY_SECONDS";
rp(?ONE_MIN)        -> "ONE_MIN";
rp(?FIVE_MIN)       -> "FIVE_MIN";
rp(?FIFTEEN_MIN)    -> "FIFTEEN_MIN";
rp(?THIRTY_MIN)     -> "THIRTY_MIN";
rp(?ONE_HOUR)       -> "ONE_HOUR";
rp(?TWELVE_HOUR)    -> "TWELVE_HOUR";
rp(?ONE_DAY)        -> "ONE_DAY".

state(?ACTIVE)  -> "ACTIVE";
state(?STOPPED) -> "STOPPED".


a(L, A) -> lists:append([L, A]).
f(L)    -> lists:flatten(L).
r(L)    -> lists:reverse(L).

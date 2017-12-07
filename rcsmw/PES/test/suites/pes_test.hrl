%%% --------------------------------------------------------
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% --------------------------------------------------------



%%=========================================================
%% 
%%=========================================================
-define(LOW,    1).
-define(MEDIUM, 2).
-define(HIGH,   3).

%%=========================================================
%% RP and GP timers
%%=========================================================
-define(TEN_SECONDS, 1).
-define(THIRTY_SECONDS, 2).
-define(ONE_MIN, 3).
-define(FIVE_MIN, 4).
-define(FIFTEEN_MIN, 5).
-define(THIRTY_MIN, 6).
-define(ONE_HOUR, 7).
-define(TWELVE_HOUR, 8).
-define(ONE_DAY, 9).

%%=========================================================
%% Job state
%%=========================================================
-define(ACTIVE, 1).
-define(STOPPED, 2).

%%=========================================================
%% Compression type
%%=========================================================
-define(GZIP, "GZIP").

%%=========================================================
%% Default PEI initialize callbacks
%% {subscribe, rop, show_counters}
%%=========================================================
%%-define(CB_DEF, {true, true}).
-define(CB_DEF, [{peiEventJobCallback, true}, 
		 {peiMEAttrUpdateCallback, true}]).

%%=========================================================
%% Default PEI initialize event map
%%=========================================================
-define(EVENT_MAP_DEF, "fake").

%%=========================================================
%% Default event producer
%%=========================================================
-define(PRODUCER_DEF, "first").
-define(PRODUCER_2ND, "second").



%%=========================================================
%% Default values 
%%=========================================================

-define(EVENT_GRP_1, ["ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1"]).
-define(EVENT_GRP_2, ["ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2"]).

-define(EVENT_TYPE_1, ["ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp1,EventType=EvType1"]).
-define(EVENT_TYPE_2, ["ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType2"]).
-define(EVENT_TYPE_3, ["ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=first,EventGroup=EventGrp2,EventType=EvType3"]).



-define(JobControl_FULL, 0).
-define(JobControl_STARTSTOP, 1).
-define(JobControl_VIEWONLY, 2).


-define(DEF_JOB(Job), #eventJob{eventJobId                 = Job,
				eventFilter                = undefined,
				requestedJobState          = ?ACTIVE,
				jobControl                 = undefined,
				eventGroupRef              = undefined,
				eventTypeRef               = undefined,
				fileOutputEnabled          = false,
				reportingPeriod            = ?FIFTEEN_MIN,
				fileCompressionType        = undefined,
				streamOutputEnabled        = false,
				streamDestinationIpAddress = undefined,
				streamDestinationPort      = undefined,
				streamCompressionType      = undefined
			       }).

-define(DEF_ME_ATTR, #peiMEAttrUpdate{userLabel   = undefined,
				      logicalName = undefined}).
-define(ME_ATTR(UL, LN), #peiMEAttrUpdate{userLabel   = UL,
					  logicalName = LN}).


-define(FIN_DATA(Job),  [#peiEventJob{jobId       = Job, 
				      reqJobState = ?STOPPED,
				      types       = []}]).

-define(FIN_DATA(Job, Rec),  [Rec#peiEventJob{reqJobState = ?STOPPED},
			      Rec#peiEventJob{reqJobState = ?STOPPED,
					      types       = [],
					      filterIds   = [],
					      fileCtrl    = undefined, 
					      streamCtrl  = undefined
					     }]).


%%=========================================================
%% test recordsyes
%%=========================================================

-record(peiEventJob, {producerId  = "first", 
		      jobId       = undefined, 
		      reqJobState = ?ACTIVE, 
		      types       = undefined, 
		      filterIds   = [], 
		      fileCtrl    = undefined, 
		      streamCtrl  = undefined}).


-record(peiMEAttrUpdate, {userLabel, 
			  logicalName}).


-record(eventJob, {eventJobId,
                   description,
                   eventFilter,
                   requestedJobState,
                   currentJobState,
                   fileOutputEnabled,
                   streamDestinationIpAddress,
                   streamDestinationPort,
                   reportingPeriod,
                   streamOutputEnabled,
                   jobControl,
                   eventGroupRef,
                   eventTypeRef,
                   fileCompressionType,
                   streamCompressionType}).


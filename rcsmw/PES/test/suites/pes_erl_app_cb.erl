%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_erl_app_cb.erl %
%%% @copyright Ericsson AB 2014
%%% @version /main/R3A/3

-module(pes_erl_app_cb).


-export([peiEventJobCallback/8]).
-export([peiMEAttrUpdateCallback/3]).

-include("pes_test.hrl").

%%% ----------------------------------------------------------
%%% @doc 
%%% Default implementation of peiEventJobCallback. Sends an asynchronous 
%%% message to the AppPid provided in {@link peiInitialize/4}.
%%% @end
%%% ----------------------------------------------------------
peiEventJobCallback(AppPid, 
		    ProducerId, 
		    JobId, 
		    ReqJobState, 
		    Types, 
		    FilterIds, 
		    FileCtrl, 
		    StreamCtrl) -> 

    Rec = #peiEventJob{producerId  = ProducerId, 
		       jobId       = JobId, 
		       reqJobState = ReqJobState, 
		       types       = Types, 
		       filterIds   = FilterIds, 
		       fileCtrl    = FileCtrl, 
		       streamCtrl  = StreamCtrl},
    AppPid ! {peiEventJobCallback, Rec},
    ok.

%%% ----------------------------------------------------------
%%% @doc 
%%% Default implementation of peiMEAttrUpdateCallback. Sends an asynchronous 
%%% message to the AppPid provided in {@link peiInitialize/4}.
%%% @end
%%% ----------------------------------------------------------
peiMEAttrUpdateCallback(AppPid, UserlLabel, LogicalName) ->

    Rec = #peiMEAttrUpdate{userLabel   = UserlLabel,
			   logicalName = LogicalName},
    AppPid ! {peiMEAttrUpdateCallback, Rec},
    ok.


